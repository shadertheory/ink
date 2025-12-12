pub const root = @import("root");
pub const object = root.object;
pub const word = root.vm.machine.word;
const std = @import("std");
pub const field_enum = std.meta.FieldEnum;
pub const fields = std.meta.fields;

pub fn handle(comptime h: type, comptime t: type) type {
    return struct { header: type = h, layout: type = t };
}
pub fn view(comptime header_type: type, comptime layout_type: type) type {

    const has_payload = @hasField(layout_type, "payload");
    const payload_child_type = if(has_payload) blk: {
        const field_info = fields(layout_type)[@intFromEnum(field_enum(layout_type).payload)];
        const ptr_info = @typeInfo(field_info.type);
        
        if (ptr_info != .Pointer or ptr_info.Pointer.size != .Slice) {
            @compileError("'payload' must be a slice (e.g., []u8).");
        }
        break :blk ptr_info.Pointer.child;
    } else void;
        
    const header_bit_size = @bitSizeOf(header_type);
    const word_size = @bitSizeOf(word);

    const header_words = (header_bit_size + word_size - 1) / word_size;
    const misalignment = header_bit_size % word_size;

    if (misalignment != 0) {
        @compileError("Header must be aligned to 64-bit words.");
    }

    const payload_offset_bits = if(has_payload) @bitOffsetOf(layout_type, "payload") else @bitSizeOf(layout_type);

    const fixed_words = (payload_offset_bits + word_size - 1) / word_size;

    return struct {
        base: usize,
        tape: []u64,

        const self = @This();

        pub fn init(tape: []u64, index: usize) self {
            return .{ .tape = tape, .index = index };
        }

        pub const field = field_enum(layout_type);

        pub fn header(this: self) *header_type {
            const h_index = this.base - header_words;

            return &this.tape[h_index];
        }
        
        pub fn payload(this: self) []payload_child_type {
            const size = this.header().length;
            const start_index = this.base + fixed_words;
            const raw_slice =  this.tape[start_index .. start_index + size.tape_words];

            if (payload_child_type == u64) {
                return raw_slice;
            } else {
                const bytes = std.mem.sliceAsBytes(raw_slice);
                const casted = std.mem.bytesAsSlice(payload_child_type, bytes);
                return casted[0..]; 
            }
        }

        pub fn get(this: self, comptime which: field) field_type(field) {
            const offset = @intFromEnum(which);
            const index = this.base + @sizeOf(object.header) + offset;

            const raw = this.tape[index];
            const what = field_type(field);

            if (what == u64) {
                return raw;
            } else if (is_handle(what)) {
                return view(what.h, what.t);
            } else if (is_view(what)) {
                return what.init(self.tape, @intCast(raw));
            }
        }

        fn field_type(comptime which: field) type {
            const field_info = std.meta.fields(layout_type) [@intFromEnum(which)];
            const raw_type = field_info.type;

            if(is_handle(raw_type)) {
                return view(raw_type.target);
            }
            return raw_type;
        }

        fn is_handle(comptime which: type) bool {
            return @typeInfo(which) == .@"struct" and @hasField(which, "target");
        }

        fn is_view(comptime which: type) bool {
            return @typeInfo(which) == .@"struct" and @hasDecl(which, "init") and @hasDecl(which, "field");
        }
    };
}

pub fn array(comptime ty: type) type {
    return struct {
        length: u64,
        payload: []ty
    };
}

pub fn hash_map(comptime k: type, comptime v: type) type {
    return struct 
    {
        count: u64,
        capacity: u64,
        keys: handle(array(k)),
        values: handle(array(v))
    };
} 
pub const string = struct {
    byte_count: u64,
    payload: []u8,
};

pub const bytecode = struct {
    word_count: u64,
    payload: []u32,
};

pub const function = struct {
    name: handle(string),
    register_count: u64,
    code: handle(bytecode),
    constants: handle(array),
};

pub const module = struct {
    name: handle(string),
    exports: handle(hash_map),
    imports: handle(array),
};

pub const virtual_table = struct {

};

pub const trait_object = struct {

};
pub const type_definition = struct {

};

pub const trap = struct {

};

pub const stack_frame = struct {

};

pub const closure = struct {

};
