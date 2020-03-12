exports.callForeign = thisValue => arguments => foreignFunction => {
  if (typeof foreignFunction !== "function") {
    throw new Error("Expected function, found extern value " + foreignFunction.toString());
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
