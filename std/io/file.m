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

;; Copies a file.
(extern file.copy)

;; Converts a file to a map of paths to files.
(defn file->tree-map file
  (file->tree-map' () (impure (empty-tree-map (compare-list compare-symbol))) file))

(defn file->tree-map' path !tree-map file 
  (do directory? (file.directory? file)
    (if directory?
      (do child-files (file.child-files file)
        (fold child-files !tree-map 
          (file->tree-map' (append path (file.name file)))))
      (do tree-map !tree-map
        (impure 
          (tree-map.put tree-map
            (append path (file.name file))
            file))))))
