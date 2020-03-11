exports.callForeign = thisValue => arguments => foreignFunction => {
  if (typeof foreignFunction !== "function") {
    throw new Error("Expected function, found extern value " + foreignFunction.toString());
  }
  if (foreignFunction.length !== arguments.length) {
    throw new Error("Expected " + foreignFunction.length + " arguments, found " + arguments.length);
  }
  return foreignFunction.apply(thisValue, arguments);
};

exports.arity = None => Just => fn => {
  if (typeof fn === "function") {
    return Just(fn.length);
  } else {
    return None;
  }
};
