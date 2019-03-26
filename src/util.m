;; Tests if a file exists.
(def file.exists? ())

;; Removes a key in a tree map node.
(def tree-map-node.remove
  (fn node
  (fn compare
  (fn key
    (if (tree-map-node-nil? node) node
    (fold-compare (compare key (tree-map-node.key node))
      (fn <
        (tree-map-node
          (tree-map-node.remove (tree-map-node.left node) compare key)
          (tree-map-node.right node)
          (tree-map-node.key node)
          (tree-map-node.value node)))
      (fn >
        (tree-map-node
          (tree-map-node.left node)
          (tree-map-node.remove (tree-map-node.right node) compare key)
          (tree-map-node.key node)
          (tree-map-node.value node)))
      (fn =
        (if (tree-map-node-nil? (tree-map-node.left node)) (tree-map-node.right node)
        (if (tree-map-node-nil? (tree-map-node.right node)) (tree-map-node.left node)
          (with (tree-map-node.min-node (tree-map-node.right node))
          (fn min
            (tree-map-node
              (tree-map-node.left node)
              (tree-map-node.right (tree-map-node.remove (tree-map-node.right node) compare key))
              (tree-map-node.key min)
              (tree-map-node.value min)))))))))))))

;; Removes a key in a tree map.
(def tree-map.remove
  (fn map
  (fn key
    (tree-map
      (tree-map-node.remove (tree-map.node map) (tree-map.compare map) key)
      (tree-map.compare map)))))

;; The minimum node of a tree map node.
(def tree-map-node.min-node
  (fn node
    (if (tree-map-node-nil? (tree-map-node.left node)) node
      (tree-map-node.min-node (tree-map-node.left node)))))