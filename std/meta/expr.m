;; An expression representing an M symbol.
(def symbol-expr
  (fn name path start end
    (pair true (cons name (cons path (cons start (cons end ())))))))

(def symbol-expr? first)
(def symbol-expr.name (compose car second))
(def symbol-expr.path (compose cadr second))
(def symbol-expr.start (compose caddr second))
(def symbol-expr.end (compose cadddr second))

;; An expression representing an M list.
(def list-expr
  (fn exprs path start end
    (pair false (cons exprs (cons path (cons start (cons end ())))))))

(def list-expr? (compose not first))
(def list-expr.exprs (compose car second))
(def list-expr.path (compose cadr second))
(def list-expr.start (compose caddr second))
(def list-expr.end (compose cadddr second))

(defn expr.path expr
  (if (list-expr? expr)
    (list-expr.path expr)
    (symbol-expr.path expr)))

(defn expr.start expr
  (if (list-expr? expr)
    (list-expr.start expr)
    (symbol-expr.start expr)))

(defn expr.end expr
 (if (list-expr? expr)
   (list-expr.end expr)
   (symbol-expr.end expr)))

; (def symbol-expr0
;   (fn name
;     (symbol-expr name (symbol expr.m) start-position start-position)))

; (def list-expr0
;   (fn exprs
;     (list-expr exprs (symbol expr.m) start-position start-position)))

; ;; Creates a symbol expression.
; (macro macro/symbol
;   (fn exprs
;     (list-expr0
;       (cons (symbol-expr0 (symbol symbol-expr0))
;       (cons (car exprs)
;         ())))))

; ;; Creates a list expression.
; (macro macro/list
;   (fn exprs
;     (list-expr0
;       (cons (symbol-expr0 (symbol list-expr0))
;       (cons (car exprs)
;         ())))))

(def symbol-expr0 left)
(def list-expr0 right)

;; Converts an expression to a list.
(defn expr->list expr
  (if (symbol-expr? expr)
    (left (symbol-expr.name expr))
    (right (map (list-expr.exprs expr) expr->list))))

;; Converts a list to an expression.
(defn list->expr expr either
  (either
    (fn name (symbol-expr name (expr.path expr) (expr.start expr) (expr.end expr)))
    (fn list (list-expr (map list (list->expr expr)) (expr.path expr) (expr.start expr) (expr.end expr)))))