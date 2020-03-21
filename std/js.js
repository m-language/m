module.exports = {
  index: (o, k) => o[k],
  js: {
    Number: Number,
    Function: Function,
    String: String,
    Array: Array,
    Boolean: Boolean,
    Object: Object,
    null: null,
    undefined: undefined,
    bind: (a, b, c) => a.bind(b, c),
    apply: (a, b, c) => a.apply(b, c),
    with: (o, fn) => {
      fn(o);
      return o;
    },
    force: (a, b) => a.apply(b, []),
    console
  }
};
