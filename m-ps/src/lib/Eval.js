exports.callForeign = thisValue => arguments => foreignFunction => {
  if (typeof foreignFunction !== "function") {
    throw new Error("Expected function, found extern value " + foreignFunction);
  }
  return foreignFunction.apply(thisValue, arguments);
};

exports.arity = None => Just => fn => {
  if (typeof fn === "function") {
    console.log(fn.length);
    return Just(fn.length);
  } else {
    return None;
  }
};
