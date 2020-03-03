const fs = require("fs");

// Read a single character from stdin synchronously
exports.readInputCharImpl = function() {
  const buff = Buffer.alloc(1, undefined, "utf-8");
  fs.readSync(process.stdin.fd, buff, 0, 1, 0);
  return buff.toString()[0];
};
