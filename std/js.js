module.exports = {
  index: (o, k) => o[k],
  js: {
    Number: Number,
    Function: Function,
    String: String,
    Array: Array,
    Boolean: Boolean,
    Object: Object,
    undefined,
    null: null,
    undefined: undefined,
    bind: (a, b, c) => a.bind(b, c),
    apply: (a, b, c) => a.call(b, c),
    with: (o, fn) => {
      fn(o);
      return o;
    },
    console
  }
};
