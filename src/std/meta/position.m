;; A position in a file.
(def position pair)

(def position.line first)
(def position.char second)

(def start-position (position nat.1 nat.1))

;; The position of the next char.
(defn next-char p
  (position
    (position.line p)
    (nat.+ nat.1 (position.char p))))

;; The position of the next line.
(defn next-line p
  (position
    (nat.+ nat.1 (position.line p))
    nat.1))