const fs = require("fs");

// Read a single character from stdin synchronously
exports.readInputCharImpl = Just => {
  return Nothing => {
    return () => {
      const buff = Buffer.alloc(1, undefined, "utf-8");
      const read = fs.readSync(process.stdin.fd, buff, 0, 1, 0);
      if (read <= 0) {
        return Nothing;
      } else {
        return Just(buff.toString()[0]);
      }
    };
  };
};
