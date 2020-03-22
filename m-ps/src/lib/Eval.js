exports.callForeign = arguments => foreignFunction => throwError => pure => {
  try {
    if (typeof foreignFunction !== "function") {
      throw new Error("Expected function, found extern value " + JSON.stringify(foreignFunction));
    }
    return pure(foreignFunction.apply(null, arguments));
  } catch (e) {
    return throwError(e);
  }
};

exports.arity = None => Just => fn => {
  if (typeof fn === "function") {
    return Just(fn.length);
  } else {
    return None;
  }
};
