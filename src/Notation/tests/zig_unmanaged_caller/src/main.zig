const std = @import("std");

extern fn typeset (buffer:[*c]u8, len:*i64, tex:[*c]const u8, transparent:bool) void;

pub fn main() !void {
    const tex: [:0]const u8 = "f(x) = \\frac{A + B^2}{-g(x) \\cdot k_A}  \x00";
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const buffer = try allocator.alloc(u8, 18 * 1025);
    defer allocator.free(buffer);
    
    var len: i64 = undefined;
    typeset (buffer.ptr, &len, tex.ptr, false);

    var fs = try std.fs.cwd().createFile("tex_image.png", .{.read = true});
    defer fs.close();

    const n: usize = @intCast(len);
    try fs.writeAll(buffer[0..n]);
}

