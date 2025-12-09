const std = @import("std");
const inkc = @import("inkc");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // --- Args Parsing ---
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <source_file>\n", .{args[0]});
        return;
    }

    const source_path = args[1];
    const source = try std.fs.cwd().readFileAlloc(allocator, source_path, 1_000_000);
    defer allocator.free(source);

    // --- Lexer Setup ---
    var lexer = try inkc.lexer.init(source);
    var tokens: [1024]inkc.token = undefined;
    var i: usize = 0;

    while (true) {
        if (i >= tokens.len) break; 
        
        // Safety: handle potential null from lexer
        if (try lexer.next()) |tok| {
            tokens[i] = tok;
            if (tokens[i].which == .end_of_file) break;
            i += 1;
        } else {
            break;
        }
    }

    // --- Parser Setup ---
    var parser = try inkc.parser.parse(source, &tokens, allocator);

    // --- Parsing & Printing Loop ---
    while (true) {

        const node = try parser.process();
        
        // Use the debug printer wrapper
        print_node(node, 0);
        std.debug.print("\n", .{});
    }
}

// --- Pretty Print Function (Uses std.debug.print) ---

fn print_node(node: *const inkc.node, depth: usize) void {
    // Helper to print indentation using std.debug.print
    var i: usize = 0;
    while (i < depth) : (i += 1) { 
        std.debug.print("  ", .{}); 
    }

    switch (node.*) {
        .integer => |val| std.debug.print("Integer: {}\n", .{val}),
        .float => |val| std.debug.print("Float: {d}\n", .{val}),
        .identifier => |name| std.debug.print("Identifier: \"{s}\"\n", .{name.string}),
        
        .decl => |func| {
            std.debug.print("Function Decl: {s}\n", .{func.name.string});
            
            // Print Params
            var k: usize = 0;
            while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
            std.debug.print("Params:\n", .{});
            
            for (func.params) |param| {
                k = 0;
                while (k < depth + 2) : (k += 1) { std.debug.print("  ", .{}); }
                std.debug.print("{s}: {s}\n", .{param.name.string, param.ty.string});
            }

            // Print Return Type
            if (func.return_type) |rt| {
                k = 0;
                while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
                std.debug.print("Return Type: {s}\n", .{rt.string});
            }

            // Print Body
            k = 0;
            while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
            std.debug.print("Body:\n", .{});
            print_node(func.body, depth + 2);
        },

        .binary => |bin| {
            std.debug.print("Binary Op: {}\n", .{bin.op});
            print_node(bin.left, depth + 1);
            print_node(bin.right, depth + 1);
        },

        .unary => |un| {
            std.debug.print("Unary Op: {}\n", .{un.op});
            print_node(un.right, depth + 1);
        },

        .if_expr => |ife| {
            std.debug.print("If Expr:\n", .{});
            
            var k: usize = 0;
            while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
            std.debug.print("Condition:\n", .{});
            print_node(ife.condition, depth + 2);
            
            k = 0;
            while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
            std.debug.print("Then:\n", .{});
            print_node(ife.then_branch, depth + 2);

            if (ife.else_branch) |el| {
                k = 0;
                while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
                std.debug.print("Else:\n", .{});
                print_node(el, depth + 2);
            }
        },

        .match_expr => |me| {
            std.debug.print("Match Expr:\n", .{});
            
            var k: usize = 0;
            while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
            std.debug.print("Target:\n", .{});
            print_node(me.target, depth + 2);

            for (me.arms) |arm| {
                k = 0;
                while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
                std.debug.print("Arm Pattern:\n", .{});
                print_node(arm.pattern, depth + 2);
                
                k = 0;
                while (k < depth + 1) : (k += 1) { std.debug.print("  ", .{}); }
                std.debug.print("Arm Body:\n", .{});
                print_node(arm.body, depth + 2);
            }
        },
    }
}
