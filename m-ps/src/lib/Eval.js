exports.callForeign = arguments => foreignFunction => {
  return foreignFunction.apply(null, arguments);
};
