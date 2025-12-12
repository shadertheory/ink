pub const object = @import("root").vm.object;
pub const string = []const u8;
pub fn intern(comptime ty: type) type {

}
pub const ir = union(enum) {
    unit: void,
    boolean: bool,
    integer: i64,
    float: f64,
    
    string: intern(string),
    
    array: struct { 
        obj_type: object.identnifier, 
        items: []const field_value, 
    },

    dictionary: struct { 
        items: []const pair, 
    },
    
    structure: struct {
        type_name: []const u8, 
        fields: []const field_value, 
    },

    variant: struct {
        enum_name: identifier, 
        tag_name: type_identifier, 
        payload: ?*const value,
    },

    tensor: struct {
        order: u8,
        size: []const u8,
        payload: ?*const value,
    },

    type_ref: TypeId, 

    function_ref: []const u8, 
    
    //ast_fragment: *const ast_node,
};

pub const field_value = struct {
    name: identifier,
    value: value,
};

pub const pair = struct {
    key: value,
    entry: value,
};

pub const ir = union(enum) {
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
