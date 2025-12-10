pub const root = @import("root.zig");
pub const strings = root.strings;
pub const std = @import("std");

pub const hash_map = std.AutoHashMap;
pub const mem_allocator = std.mem.Allocator;
pub const array_list = std.array_list.Managed;
pub const trait_identifier = root.trait_identifier;
pub const type_identifier = root.type_identifier;
pub const stack = array_list(value);
pub const call_stack = array_list(call_frame);
pub const string = strings.string;
pub const comparison = root.comparison;
pub const binary = root.binary;
pub const number = root.number;
pub const unary = root.unary;

pub const type_id = struct { id: u64 };

pub const foreign_function = struct {
    id: u16,
};

pub const bytes = struct { count: usize };

pub const trait_object = struct {
    data: usize,
    vtable: *const virtual_table,
};

pub const struct_reference = struct { ptr: usize };

pub const function_index = struct { index: usize };

pub const value = union(enum) {
    unit: void,
    integer: u64,
    float: f64,
    boolean: bool,
    ptr: struct_reference,
    dyn: trait_object,
};

pub const virtual_table = struct {
    owner: type_identifier,
    implemented: trait_identifier,
    methods: []const function_index,
    destructor: function_index,
};

pub const runtime_type = struct {
    id: type_identifier,
    size: bytes,
    name: identifier,
};

pub const executable = struct {
    constants: []value,
    instructions: []operation,

    pub const header = struct {
        constant_count: usize,
        instruction_count: usize,
    };
};

pub const address = struct { 
    block: *memory, where: usize,

    pub fn add(self: *memory, comptime ty: type, count: i) ty {
        return memory {
            .block = self.block,
            .where = self.where + @sizeOf(ty) * count
        };
    }

    pub fn read(self: *memory, comptime ty: type) *ty {
        return @as(*ty, @ptrCast(self.block.read(self.where)));
    }

    pub fn write(self: *memory, comptime ty: type, val: ty) void {
        self.read(ty).* = val;
    }
};

pub const operation = union(enum) {
    state: control,
    arithmetic: math, 
    block: memory,

    pub const control = union(enum) {
        //no operation
        unit: void,
        //halt operation
        bang: void,
        jump: struct { to: address, condition: jump_condition },
        call: struct { argc: u8, argp: address, ret: address, location: call_location },
        ret: struct { value: ?address },
        match: struct {
            dst: address,
            condition: jump_condition,
            true_if: address,
            false_if: address,
        },

        pub const jump_condition = union(enum) {
            always,
            check_if: struct { cmp: bool, subject: address },
            check_comparison: struct { cmp: comparison, lhs: address, rhs: address, ty: number },
        };

        pub const call_location = union(enum) {
            direct: struct { where: address, },
            native: struct { id: foreign_function },
            indirect: struct { which: address },
            virtual: struct { vtable: virtual_table, offset: u8 }
        };

    };
    pub const math = union(enum) {
        compute_binary: struct {
            dst: address,
            lhs: address,
            rhs: address,
            op: binary,
            ty: number,
        },
        compute_unary: struct {
            dst: address,
            src: address,
            op: unary,
            ty: number,
        },
        compare: struct {
            dst: address, // Stores a boolean result (0 or 1)
            lhs: address,
            rhs: address,
            op: comparison,
            ty: number,
        },
    };
    pub const memory = union(enum) {
        copy: struct { dst: address, src: address, count: address, },
        swap: struct { lhs: address, rhs: address, count: address, },
        alloc: struct { dst: address, element: type_identifier, count: address },
        free: struct { ptr: address },
        //immutable data like execution context or constants
        load: struct { dst: address },
        //heap or stack
        read: struct { dst: address, ptr: address, offset: u16 },
        write: struct { ptr: address, src: address, offset: u16 },
        field_get: struct { dst: address, object: address, field_id: u16 },
        field_set: struct { object: address, field_id: u16, src: address },
        element_get: struct { dst: address, array: address, index: address },
        element_set: struct { array: address, index: address, src: address },
        make_trait_object: struct {
            dst: address, 
            src: address,        
            impl_type_id: u16,  
            trait_id: u16  
        },
        copy_generic: struct { 
            dst: address, 
            src: address, 
            type_descriptor: address 
        },
        check_trait: struct {
            dst: address,
            src: address,       
            trait_id: u16
        },
        resolve_method: struct {
            dst: address,        
            object: address,     
            method_index: u16   
        }
    };
};

pub const location = struct {
    start: usize,
    end: usize
};

pub const identifier = struct {
    name: string,
};

pub const ownership = enum(u8) {
    ref,
    mut,
    owned
};

pub const trap_type = error {
    bad_type
};

pub const control_flow = enum {
    cont,
    halt
};



pub const machine = struct { 
    pub const header = struct {
        constant_count: u64,
        trap_count: u64,
        instruction_count: u64,
    };
    heap: *memory,
    def: header,
    frames: call_stack,

    stack: register_stack,

    ip: address, 
    fp: address,
    constant: address,
    trap: address,
    code: address,

    pub fn init(exe: executable, allocator: mem_allocator) machine {
        const heap = allocator.create(memory);
        heap.* = memory.init(allocator);

        const code = heap.allocate_head(operation, exe.instructions);
        const constant = heap.allocate_head(value, exe.constants);
        const trap = heap.allocate_head(value, [_]value{value {.boolean = false}} ** @typeInfo(trap_type).@"union".fields.len);

        const reg = register_stack.init(heap, 8192);

        const ip = code;
        const fp = ip;

        const frames = call_stack.init(allocator); 

        return machine {
            .frames = frames,
            .heap = heap,
            .ip = ip,
            .fp = fp,
            .constant = constant,
            .trap = trap,
            .code = code,
            .stack = reg,
        };
    }

    pub fn step(self: *machine) control_flow {
        self.instruct() catch |trap_condition| {
            self.trap.add(@intFromEnum(trap_condition)).write(value, .{.boolean = true});
            return .halt;
        };
    } 

    fn instruct(self: *machine) !control_flow {
        const instruction: *operation = @ptrCast(self.ip.read(operation));
        self.ip.next();
        
        switch(instruction) {
            .state => |control_op| switch (control_op) {
                .unit => {},
                .bang => return true,
                .jump => |jump| switch (jump.condition) {
                    .always => self.ip.jump(jump.to),
                    .check_if => |check_if| {
                        const check_against: *bool = check_if.subject.read(bool);
                        if (check_against.* == check_if.cmp) {
                            self.ip.jump(jump.to);
                        } 
                    }
                    ,
                    .check_comparison => |check_cmp| {
                        const num = if (check_cmp.ty == .number) u64 else f32; 
                        const should_jump = root.compare(
                            check_cmp.cmp,
                            check_cmp.lhs.read(num),
                            check_cmp.rhs.read(num)
                        );
                        if (should_jump) {
                            self.ip.jump(jump.to);
                        }
                     }
                },
                .call  => |call| {
                    var frame: call_frame = undefined;
                    frame.dest = call.ret.read(machine.register); 
                    frame.ret = address { .where = self.ip.where + 1 };

                    try self.frames.push(frame);

                    switch (location) {
                        .direct => |loc| {
                            self.ip.jump(loc.where);
                        },
                        .indirect => |loc| {
                            self.ip.jump(loc.where.read(address).*);
                        },
                        else => unreachable
                    }
                },
                .ret => |ret| {
                    const frame = try self.frames.pop();
                    self.ip.jump(frame.ret);

                    if (ret.value) |ret_value| {
                        frame.dest.write_bytes(ret_value, ret.size);
                    }
                }
            },
            .math => |math_op| switch(math_op) {
                .compute_binary => |op| {

            },
                .compute_unary  => |op| {},
                .compare  => |op| {}
            },
            .block => |memory_op| switch(memory_op) {
                else => {}
            }
        }
        return false;
    }

    pub const memory = struct {
        block: []u64,
        allocator: mem_allocator,
        head: usize,
        tail: usize,

        pub fn init_with_capacity(allocator: mem_allocator, initial_capacity: usize) memory {
            return memory {
                .allocator = allocator,
                .block =  try allocator.alloc(u8, initial_capacity),
            };
        }

        pub fn init(allocator: mem_allocator) memory {
            const megabyte: usize = 1024 * 1024;
            return memory.init_with_capacity(allocator, megabyte);
        }

        pub fn allocate_head(self: *memory, comptime ty: type, values: []ty) !address {
            const size = @sizeOf(ty) * values.count;
            if (self.head + size >= self.tail) {
                return error.out_of_memory;
            }
            const allocation = self.allocate_at(ty, values, self.head);
            self.head += size;
            return allocation; 
        }

        pub fn allocate_tail(self: *memory, comptime ty: type, values: []ty) !address {
            const size = @sizeOf(ty) * values.count;
            const dest = self.tail - size;
            if (dest <= self.head) {
                return error.out_of_memory;
            }
            const allocation = self.allocate_at(ty, values, dest);
            self.tail -= size;
            return allocation;
        }

        fn allocate_at(self: *memory, comptime ty: type,  values: []ty, offset: usize) address {
            var block: []u8 = @ptrCast(self.block);
            block += offset;
            const data: []ty = @ptrCast(block);
            for (0..values.count) |i| {
                (data + i).* = values[i];
            }
            return address { .memory = self, .where = offset }; 
        }
    };

    pub const register_stack = struct {
        size: usize,
        data: address,
        
        pub fn init(mem: *memory, size: usize) register_stack {
            return register_stack {
                .size = size,
                .data = mem.allocate_head(value, [_]value{.unit} ** size), 
            };
        }
    };

    pub const register = struct {
        which: u64,
    };
};

pub const call_frame = struct {
    ret: instruction_pointer,
    prev: frame_pointer,
    dest: machine.register,
};
