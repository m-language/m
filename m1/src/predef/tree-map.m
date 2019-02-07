;; A node in a tree map.
(def tree-map-node
  (ap new-data (symbol tree-map-node)
    (ap list4 (symbol left) (symbol right) (symbol key) (symbol value))))

(def tree-map-node.left (ap field (symbol tree-map-node) (symbol left)))
(def tree-map-node.right (ap field (symbol tree-map-node) (symbol right)))
(def tree-map-node.key (ap field (symbol tree-map-node) (symbol key)))
(def tree-map-node.value (ap field (symbol tree-map-node) (symbol value)))

;; The empty tree map node.
(def tree-map-node-nil (ap object (symbol tree-map-node-nil)))

;; Tests if a value is tree-map-node-nil.
(def tree-map-node-nil? (ap is? (symbol tree-map-node-nil)))

;; A tree map.
(def tree-map
  (ap new-data (symbol tree-map)
    (ap list2 (symbol node) (symbol compare))))

(def tree-map.node (ap field (symbol tree-map) (symbol node)))
(def tree-map.compare (ap field (symbol tree-map) (symbol compare)))

;; Creates an empty tree map with a given compare.
(def empty-tree-map
  (fn compare
    (ap tree-map tree-map-node-nil compare)))

;; Gets the value of a key in a tree map node.
(def tree-map-node.get
  (fn node
    (fn compare
      (fn key
        (if (ap tree-map-node-nil? node)
          null
          (ap fold-compare (ap compare key (ap tree-map-node.key node))
            (fn <
              (ap tree-map-node.get (ap tree-map-node.left node) compare key))
            (fn >
              (ap tree-map-node.get (ap tree-map-node.right node) compare key))
            (fn =
              (ap some (ap tree-map-node.value node)))))))))

;; Gets the value of a key in a tree map.
(def tree-map.get
  (fn map
    (fn key
      (ap tree-map-node.get (ap tree-map.node map) (ap tree-map.compare map) key))))

;; Puts the value of a key in a tree map node.
(def tree-map-node.put
  (fn node
    (fn compare
      (fn key
        (fn value
          (if (ap tree-map-node-nil? node)
            (ap tree-map-node tree-map-node-nil tree-map-node-nil key value)
            (ap fold-compare (ap compare key (ap tree-map-node.key node))
              (fn <
                (ap tree-map-node
                  (ap tree-map-node.put
                    (ap tree-map-node.left node)
                    compare
                    key
                    value)
                  (ap tree-map-node.right node)
                  (ap tree-map-node.key node)
                  (ap tree-map-node.value node)))
              (fn >
                (ap tree-map-node
                  (ap tree-map-node.left node)
                  (ap tree-map-node.put
                    (ap tree-map-node.right node)
                    compare
                    key
                    value)
                  (ap tree-map-node.key node)
                  (ap tree-map-node.value node)))
              (fn =
                (ap tree-map-node
                  (ap tree-map-node.left node)
                  (ap tree-map-node.right node)
                  key
                  value)))))))))

;; Puts the value of a key in a tree map.
(def tree-map.put
  (fn map
    (fn key
      (fn value
        (ap tree-map
          (ap tree-map-node.put
            (ap tree-map.node map)
            (ap tree-map.compare map)
            key
            value)
          (ap tree-map.compare map))))))

;; Folds a node with an accumulator and function.
(def tree-map-node.fold
  (fn node
    (fn acc
      (fn f
        (if (ap tree-map-node-nil? node)
          acc
          (ap tree-map-node.fold (ap tree-map-node.right node)
            (ap tree-map-node.fold (ap tree-map-node.left node)
              (ap f acc
                (ap tree-map-node.key node)
                (ap tree-map-node.value node))
            f)
          f))))))

;; Folds a map with an accumulator and function.
(def tree-map.fold
  (fn map
    (fn acc
      (fn f
        (ap tree-map-node.fold (ap tree-map.node map) acc f)))))

;; Adds two maps.
(def tree-map.+
  (fn map1
    (fn map2
      (ap tree-map.fold map1 map2
        (fn map
          (fn key
            (fn value
              (ap tree-map.put map key value))))))))

;; Converts a tree map to a list.
(def tree-map->list
  (fn map
    (ap tree-map.fold map ()
      (fn list
        (fn key
          (fn value
            (ap cons (ap pair key value) list)))))))