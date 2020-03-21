exports.callForeign = thisValue => arguments => foreignFunction => throwError => pure => {
  try {
    if (typeof foreignFunction !== "function") {
      throw new Error("Expected function, found extern value " + JSON.stringify(foreignFunction));
    }
    return pure(foreignFunction.apply(thisValue, arguments));
  } catch (e) {
    return throwError(e);
  }
};

exports.arity = None => Just => fn => {
  if (typeof fn === "function") {
    console.log(fn.length);
    return Just(fn.length);
  } else {
    return None;
  }
};
