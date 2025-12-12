pub const vm = @import("root").vm;
pub const object = struct {
    pub const header = packed struct {
        ty: identifier,
        gen: generation,
        flags: flag,
        size: u64,
    };

    pub const view = struct {
        slice: []vm.word,
    };

    pub const flag = enum(u64) {
        freed = 1 << 0,
    };

    pub const generation = struct { gen: u64 };

    pub const identifier = struct { 
        id: u64,
        var next: u64 = @typeInfo(special).@"enum".fields.len;

        pub fn reserve_special(which: special) identifier {
            return identifier { .id = @intFromEnum(which) };
        }

        pub fn reserve_next() identifier {
            const id = identifier.next;
            identifier.next += 1;
            return identifier { .id = id };
        }

        pub const special = enum {
            free,
            raw_data,
            function,
            trap_signal,
            stack_frame,
            array,
            string,
        };
    };
};

pub const reference = packed struct {
    where: tape_index,
    when: object.generation,
};
