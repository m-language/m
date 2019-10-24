;; Macroexpands an expression.
(defnrec macroexpand-expr expr local-env global-env
  (expr.match expr
    (fn name l (right (symbol-expr name l)))
    (fn exprs l (macroexpand-list-expr macroexpand-expr exprs l local-env global-env))))

;; Macroexpands a list expression.
(defn macroexpand-list-expr macroexpand-expr exprs l local-env global-env
  (if (nil? exprs)
    (right (list-expr exprs l))
    ((macroexpand-expr (car exprs) local-env global-env) left
      (fn expanded-car
        (expr.match expanded-car
          (fn name _
            ((env.get local-env global-env name)
              (fn _
                (if (| (symbol.= name (symbol fn))
                    (| (symbol.= name (symbol def))
                    (| (symbol.= name (symbol macro))
                       (symbol.= name (symbol symbol)))))
                  (right (list-expr (cons expanded-car (cdr exprs)) l))
                  (left (right (list name)))))
              (fn variable
                (if (& (global-variable? variable) (global-variable.macro? variable))
                  (macroexpand-apply macroexpand-expr name (cdr exprs) l local-env global-env)
                  (right (list-expr (cons expanded-car (cdr exprs)) l))))))
          (fn _ _ (right (list-expr (cons expanded-car (cdr exprs)) l))))))))

;; Applies a macro during macroexpansion.
(defn macroexpand-apply macroexpand-expr name exprs l local-env global-env
  (let function (heap/get (global-env.heap global-env) name)
       env (global-env->env global-env)
       result (function env exprs)
    (result/match result
      (fn new-expr (macroexpand-expr (expr.with-path (location.path l) new-expr) local-env global-env))
      (fn errors (left (left errors)))
      (fn dependencies (left (right dependencies))))))
