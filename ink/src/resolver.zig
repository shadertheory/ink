const std = @import("std");

pub fn resolver(comptime input_type: type, comptime context_type: type, comptime return_type: type, comptime logic_type: type) type {
    return struct {
        const self = @This();

        fn invoke(comptime func_name: []const u8, context: *context_type, payload: anytype) return_type {
            if (@hasDecl(logic_type, "before")) {
                logic_type.before(context, func_name, payload);
            }

            if (@hasDecl(logic_type, func_name)) {
                const handler = @field(logic_type, func_name);
                const out = @call(.auto, handler, .{ context, payload });
                if (@hasDecl(logic_type, "before")) {
                    logic_type.before(context, func_name, payload, out);
                }
                return out;
            }

            if (@hasDecl(logic_type, "on_missing")) {
                return logic_type.on_missing(context, func_name, payload);
            }

            return error.unexpected_token;
        }

        pub fn dispatch(context: *context_type, input: input_type) return_type {
            const input_info = @typeInfo(input_type);

            switch (input_info) {
                .@"union" => {
                    switch (input) {
                        inline else => |payload, tag| {
                            return invoke(@tagName(tag), context, payload);
                        },
                    }
                },
                else => if (@hasField(input_type, "which")) {
                    const enum_type = @TypeOf(input.which);
                    inline for (std.meta.fields(enum_type)) |field| {
                        if (input.which == @field(enum_type, field.name)) {
                            return invoke(field.name, context, input);
                        }
                    }
                },
            }

            return error.failed_dispatch;
        }
    };
}
