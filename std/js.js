module.exports = {
  index: (o, k) => o[k],
  js: {
    Number: Number,
    Function: Function,
    String: String,
    Array: Array,
    undefined,
    null: null,
    undefined: undefined,
    true: true,
    false: false,
    empty: {},
    call: (o, t) => o.bind(t),
    apply: (o, fn) => {
      fn(o);
      return o;
    },
    console
  }
};
