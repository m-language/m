;;; File.m

;; A file specified by a path
(extern file.from-path)

;; The root file for this program.
(def file.local-file (file.from-path (symbol ".")))

;; The name of a file.
(extern file.name)

;; The name of a file without its extension.
(extern file.name-without-extension)

;; The child of a file given a name.
(extern file.child)

;; Tests if a file exists.
(extern file.exists?)

;; Reads the contents of a file as a list of characters.
(extern file.read)

;; Writes a list of characters to a file.
(extern file.write)

;; True if a file is a directory.
(extern file.directory?)

;; A list of child files.
(extern file.child-files)

;; Copys a file.
(extern file.copy)
