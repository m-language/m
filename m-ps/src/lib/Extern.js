exports.externImpl = key => {
  return globalThis[key];
};

exports.require = path => {
  return () => {
    return require(path);
  };
};

exports.merge = a => b => Object.assign({}, a, b);

exports.emptyObject = {};
