module.exports = {
  index: (o, k) => o[k],
  js: {
    undefined,
    null: null,
    undefined: undefined,
    true: true,
    false: false,
    call: (o, t) => o.bind(t)
  }
};
