module.exports = {
  index: o => k => o[k],
  js: {
    Number: Number,
    JSON: JSON,
    Function: Function,
    String: String,
    Array: Array,
    Boolean: Boolean,
    Object: Object,
    null: null,
    undefined: undefined,
    apply: a => b => c => {
      return a.apply(b, c);
    },
    with: a => fn => {
      fn(a);
      return a;
    },
    console
  }
};
