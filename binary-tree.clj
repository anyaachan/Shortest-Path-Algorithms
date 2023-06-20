(defrecord BST [root])
(defn createBST [] (BST. (ref nil)))

(def bst (createBST))
(defn bst? [bst] (= (class bst) BST))
(defn bst-empty? [bst] (nil? @(:root bst)))

(defrecord BSTNode [data left right])
(defn make-bst-node! [val] (BSTNode. val (ref nil) (ref nil)))

(bst? bst)
(bst-empty? bst)

(defn traverseBST [bst val] (loop [currentNode (:root bst)]
                              (if (nil? @currentNode)
                                (dosync 
                                  (ref-set currentNode (BSTNode. val (ref nil) (ref nil))))
                                (let [data (:data @currentNode)]
                                  (if (< val data) 
                                    (recur (:left @currentNode))
                                    (if (> val data) 
                                      (recur (:right @currentNode)))))))
  val)

(traverseBST bst 300)
(traverseBST bst 200)

(bst? bst)


(defn print-node [data] (println data))
(defn node-iter [node func] (when (not (nil? @node))
                       (node-iter (:left @node) func)
                         (func (:data @node))
                          (node-iter (:right @node) func)
                        ))                  

(defn iterBST [bst] (let [node (:root bst)] 
                      (node-iter node print-node))) ;; Iterating through the tree from left to right


(iterBST bst)


;;creating a dictionary 
(defrecord Dict [root count])
(defn make-dict [] (Dict. (ref nil) (ref 0)))
(def dict (make-dict))

(defrecord DictNode [key value left right])
(defn make-dict-node [key value] (DictNode. key (ref value) (ref nil) (ref nil)))

(defn dict? [dict] (= (class dict) Dict))
(dict? dict)

(defn dict-empty? [dict] (nil? @(:root dict)))
(dict-empty? dict)

(defn dict-set! [dict key value] (loop [currentNode (:root dict)]
                                   (if (not= (:key @currentNode) key)
                                     (if (nil? @currentNode)
                                       (dosync
                                        (ref-set currentNode (make-dict-node key value))
                                         (ref-set (:count dict) (inc @(:count dict)))) 
                                       (let [key_node (:key @currentNode)]
                                         (if (< key key_node)
                                           (recur (:left @currentNode))
                                           (if (> key key_node)
                                             (recur (:right @currentNode))))))
                                     (dosync 
                                       (ref-set (:value @currentNode) value)))
                                   ) dict)

(dict-set! dict 10 15)
(dict-set! dict 2 "105")
(dict-set! dict 105 "Hello")


(defn dict-contains? [dict key] (loop [currentNode (:root dict) i 1] 
                                      (if (nil? @currentNode)
                                        (println "There is not such key in a list")
                                        (if (not= (:key @currentNode) key)
                                           (let [key_node (:key @currentNode)]
                                             (if (< key key_node)
                                               (recur (:left @currentNode) (inc i))
                                               (recur (:right @currentNode) (inc i))))
                                          (println "The dictionary contains the key, the depth is" i)))
                                      ))
(dict-contains? dict 2)

(defn dict-count [dict] (println "The number of nodes in the dictionary is" @(:count dict)))
(dict-count dict)

(defn dict-get [dict key] (loop [currentNode (:root dict)]
                        (if (nil? @currentNode)
                          (println "There is not such key in a list")
                          (if (not= (:key @currentNode) key)
                            (let [key_node (:key @currentNode)]
                              (if (< key key_node)
                                (recur (:left @currentNode))
                                (recur (:right @currentNode))))
                            (println @(:value @currentNode))))))
(dict-get dict 2)

(defn node-remove [node] (let [current_left (:left @node)
                               current_right (:right @node)]
                           (dosync (ref-set node @current_left)
                                   (ref-set (:right @node) @current_right))))

(defn dict-search [dict key] (loop [currentNode (:root dict)]
                               (if (nil? @currentNode)
                                 (println "There is not such key in a list")
                                 (let [key_node (:key @currentNode)]
                                   (if (not= key_node key)
                                     (if (< key key_node)
                                       (recur (:left @currentNode))
                                       (recur (:right @currentNode)))
                                   currentNode)))))

(defn dict-node-leaf? [node] (let [left (:left @node) right (:right @node)]
                               (if (and (nil? @left) (nil? @right))
                                 true
                                 false)))

(defn dict-node-one-child? [node]
  (not= (nil? (:left @node)) (nil? (:right @node))))

(defn dict-node-get-child [node] (if (nil? @(:left @node))
                                   @(:right @node)
                                   @(:left @node)))

(defn leftmost-node [node] (if (nil? node)
                             node
                             (recur (:left @node))))

(defn dict-remove [dict key] (let [node (dict-search dict key)]
                               (if (dict-node-leaf? node)
                                 (dosync (ref-set node nil)
                                         (ref-set (:count dict) (dec @(:count dict))))
                                 (if (dict-node-one-child? node)
                                   (dosync (ref-set node (dict-node-get-child node))
                                           (ref-set (:count dict) (dec @(:count dict))))
                                   (println "What to do")))) dict)
