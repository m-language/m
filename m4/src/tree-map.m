(include symbol)
(include data)

;; A node in a tree map.
(def tree-map-node
  (lambda left
    (lambda right
      (lambda key
        (lambda value
          (derive (symbol tree-map-node) (symbol left) left
          (derive (symbol tree-map-node) (symbol right) right
          (derive (symbol tree-map-node) (symbol key) key
          (derive (symbol tree-map-node) (symbol value) value
            (object (symbol tree-map-node)))))))))))

(def tree-map-node.left (field (symbol tree-map-node) (symbol left)))
(def tree-map-node.right (field (symbol tree-map-node) (symbol right)))
(def tree-map-node.key (field (symbol tree-map-node) (symbol key)))
(def tree-map-node.value (field (symbol tree-map-node) (symbol value)))

;; The empty tree map node.
(def tree-map-node-nil (object (symbol tree-map-node-nil)))

;; Tests if a value is tree-map-node-nil.
(def is-tree-map-node-nil
  (lambda x
    (eq-symbol (type-name x) (symbol tree-map-node-nil))))

;; A tree map.
(def tree-map
  (lambda node
    (lambda compare
      (derive (symbol tree-map) (symbol node) node
      (derive (symbol tree-map) (symbol compare) compare
        (object (symbol tree-map)))))))

(def tree-map.node (field (symbol tree-map) (symbol node)))
(def tree-map.compare (field (symbol tree-map) (symbol compare)))

;; Creates a new tree map.
(def tree-map
  (lambda node
    (lambda compare
      (tree-map (cons node (cons compare nil))))))

;; Creates an empty tree map with a given compare.
(def empty-tree-map
  (lambda compare
    (tree-map tree-map-node-nil compare)))

(include compare)
(include maybe)
(include list)

;; Gets the value of a key in a tree map node.
(def tree-map-node.get
  (lambda node
    (lambda compare
      (lambda key
        (if (is-tree-map-node-nil node)
          none
          (fold-compare (compare key (tree-map-node.key node))
            (lambda <
              (tree-map-node.get (tree-map-node.left node) compare key))
            (lambda >
              (tree-map-node.get (tree-map-node.right node) compare key))
            (lambda =
              (some (tree-map-node.value node)))))))))

;; Gets the value of a key in a tree map.
(def tree-map.get
  (lambda map
    (lambda key
      (tree-map-node.get (tree-map.node map) (tree-map.compare map) key))))

;; Puts the value of a key in a tree map node.
(def tree-map-node.put
  (lambda node
    (lambda compare
      (lambda key
        (lambda value
          (if (is-tree-map-node-nil node)
            (tree-map-node tree-map-node-nil tree-map-node-nil key value)
            (fold-compare (compare key (tree-map-node.key node))
              (lambda <
                (tree-map-node
                  (tree-map-node.put
                    (tree-map-node.left node)
                    compare
                    key
                    value)
                  (tree-map-node.right node)
                  (tree-map-node.key node)
                  (tree-map-node.value node)))
              (lambda >
                (tree-map-node
                  (tree-map-node.left node)
                  (tree-map-node.put
                    (tree-map-node.right node)
                    compare
                    key
                    value)
                  (tree-map-node.key node)
                  (tree-map-node.value node)))
              (lambda =
                (tree-map-node
                  (tree-map-node.left node)
                  (tree-map-node.right node)
                  key
                  value)))))))))

;; Puts the value of a key in a tree map.
(def tree-map.put
  (lambda map
    (lambda key
      (lambda value
        (tree-map
          (tree-map-node.put
            (tree-map.node map)
            (tree-map.compare map)
            key
            value)
          (tree-map.compare map))))))

;; Folds [node] with an accumulator [acc] and function [f].
(def tree-map-node.fold
  (lambda node
    (lambda acc
      (lambda f
        (if (is-tree-map-node-nil node)
          acc
          (tree-map-node.fold (tree-map-node.right node)
            (tree-map-node.fold (tree-map-node.left node)
              (f acc (tree-map-node.key node) (tree-map-node.value node))
            f)
          f))))))

;; Folds [map] with an accumulator [acc] and function [f].
(def tree-map.fold
  (lambda map
    (lambda acc
      (lambda f
        (tree-map-node.fold (tree-map.node map) acc f)))))

;; Adds [map1] and [map2].
(def tree-map.add
  (lambda map1
    (lambda map2
      (tree-map.fold map1 map2
        (lambda map
          (lambda key
            (lambda value
              (tree-map.put map key value))))))))

;; Converts a tree map to a list.
(def tree-map->list
  (lambda map
    (tree-map.fold map nil
      (lambda list
        (lambda key
          (lambda value
            (cons (pair key value) list)))))))