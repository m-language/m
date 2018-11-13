;; The root file for this program.
(def local-file ())

(import predef.data)

;; The child of a file given a name.
(def file.child (field (symbol file) (symbol child)))

;; A list of child files.
(def file.child-files (field (symbol file) (symbol child-files)))

;; True if a file is a directory.
(def file.directory? (field (symbol file) (symbol directory?)))

;; The name of a file.
(def file.name (field (symbol file) (symbol name)))

;; Reads the contents of a file as a list of characters.
(def file.read (field (symbol file) (symbol read)))

(import predef.list)
(import predef.bool)
(import predef.char)

;; The name of a file without its extension.
(def file.name-without-extension
  (lambda file
    (run-with (file.name file)
      (lambda name
        (take-while name
          (lambda char
            (not (eq-char char dot))))))))