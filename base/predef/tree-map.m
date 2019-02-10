;; A node in a tree map.
(def tree-map-node
  (new-data (symbol tree-map-node)
    (list4 (symbol left) (symbol right) (symbol key) (symbol value))))

(def tree-map-node.left (field (symbol tree-map-node) (symbol left)))
(def tree-map-node.right (field (symbol tree-map-node) (symbol right)))
(def tree-map-node.key (field (symbol tree-map-node) (symbol key)))
(def tree-map-node.value (field (symbol tree-map-node) (symbol value)))

;; The empty tree map node.
(def tree-map-node-nil (object (symbol tree-map-node-nil)))

;; Tests if a value is tree-map-node-nil.
(def tree-map-node-nil? (is? (symbol tree-map-node-nil)))

;; A tree map.
(def tree-map
  (new-data (symbol tree-map)
    (list2 (symbol node) (symbol compare))))

(def tree-map.node (field (symbol tree-map) (symbol node)))
(def tree-map.compare (field (symbol tree-map) (symbol compare)))

;; Creates an empty tree map with a given compare.
(def empty-tree-map
  (fn compare
    (tree-map tree-map-node-nil compare)))

;; Gets the value of a key in a tree map node.
(def tree-map-node.get
  (fn node
    (fn compare
      (fn key
        (if (tree-map-node-nil? node)
          null
          (fold-compare (compare key (tree-map-node.key node))
            (fn <
              (tree-map-node.get (tree-map-node.left node) compare key))
            (fn >
              (tree-map-node.get (tree-map-node.right node) compare key))
            (fn =
              (some (tree-map-node.value node)))))))))

;; Gets the value of a key in a tree map.
(def tree-map.get
  (fn map
    (fn key
      (tree-map-node.get (tree-map.node map) (tree-map.compare map) key))))

;; Puts the value of a key in a tree map node.
(def tree-map-node.put
  (fn node
    (fn compare
      (fn key
        (fn value
          (if (tree-map-node-nil? node)
            (tree-map-node tree-map-node-nil tree-map-node-nil key value)
            (fold-compare (compare key (tree-map-node.key node))
              (fn <
                (tree-map-node
                  (tree-map-node.put
                    (tree-map-node.left node)
                    compare
                    key
                    value)
                  (tree-map-node.right node)
                  (tree-map-node.key node)
                  (tree-map-node.value node)))
              (fn >
                (tree-map-node
                  (tree-map-node.left node)
                  (tree-map-node.put
                    (tree-map-node.right node)
                    compare
                    key
                    value)
                  (tree-map-node.key node)
                  (tree-map-node.value node)))
              (fn =
                (tree-map-node
                  (tree-map-node.left node)
                  (tree-map-node.right node)
                  key
                  value)))))))))

;; Puts the value of a key in a tree map.
(def tree-map.put
  (fn map
    (fn key
      (fn value
        (tree-map
          (tree-map-node.put
            (tree-map.node map)
            (tree-map.compare map)
            key
            value)
          (tree-map.compare map))))))

;; Folds a node with an accumulator and function.
(def tree-map-node.fold
  (fn node
    (fn acc
      (fn f
        (if (tree-map-node-nil? node)
          acc
          (tree-map-node.fold (tree-map-node.right node)
            (tree-map-node.fold (tree-map-node.left node)
              (f acc
                (tree-map-node.key node)
                (tree-map-node.value node))
            f)
          f))))))

;; Folds a map with an accumulator and function.
(def tree-map.fold
  (fn map
    (fn acc
      (fn f
        (tree-map-node.fold (tree-map.node map) acc f)))))

;; Adds two maps.
(def tree-map.+
  (fn map1
    (fn map2
      (tree-map.fold map1 map2
        (fn map
          (fn key
            (fn value
              (tree-map.put map key value))))))))

;; Converts a tree map to a list.
(def tree-map->list
  (fn map
    (tree-map.fold map ()
      (fn list
        (fn key
          (fn value
            (cons (pair key value) list)))))))