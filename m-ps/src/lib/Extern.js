exports.externImpl = key => {
  return globalThis[key];
};

exports.require = path => {
  return () => {
    return require(path);
  };
};

exports.hasProperty = object => property => {
  return object.hasOwnProperty(property) || Object.getPrototypeOf(object).hasOwnProperty(property);
};

exports.merge = a => b => Object.assign({}, a, b);

exports.emptyObject = {};
