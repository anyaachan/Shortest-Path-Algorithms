;; Creating a vertex record
(defrecord Vertex [label longtitude latitude connected state distance estimate])
(defn make-vertex [label longtitude latitude]
  (Vertex. label longtitude latitude (ref '()) (ref 0) (ref 0) (ref 0)))

(defrecord Edge [from to label weight]) ;; Creating an edge record
(defn make-edge [from to label weight]
  (Edge. from to label weight))

(defrecord Connected [label weight])

(defrecord Graph [vertices edges])
(defn make-graph []
  (Graph. (ref {}) (ref '())))
(def graph (make-graph))

(defn graph-add-vertex! [graph label longtitude latitude] ;; A function to add a vertex to the graph data structure
  (let [vertex (make-vertex label longtitude latitude)]
    (dosync
     (ref-set (:vertices graph)
              (assoc @(:vertices graph)
                     (:label vertex) vertex)))))

(defn graph-add-edge! [graph from to label weight] ;; A function to add an edge to the graph data structure
  (let [edge (make-edge from to label weight)
        connected1 (:connected (get @(:vertices graph) from))
        connected2 (:connected (get @(:vertices graph) to))]
    (dosync
     (ref-set (:edges graph) (conj @(:edges graph) edge))
     (ref-set connected1 (conj @connected1 (Connected. to weight)))
     (ref-set connected2 (conj @connected2 (Connected. from weight))))) nil)

(load-file "e-roads-2020-full.clj") ;; Uploading the data

(defn graph-clear! [graph] ;; A function to clear the graph
  (doseq [vertex (vals @(:vertices graph))]
    (dosync
     (ref-set (:state vertex) 0)
     (ref-set (:distance vertex) 0)
     (ref-set (:estimate vertex) 0))))

(defn marking-unweighted [graph queue-dijkstra connected-list is-bfs? i] ;; A function to mark the graph with BFS or DFS
  (let [queue-dijkstra (if is-bfs?
                (reverse queue-dijkstra)
                queue-dijkstra)]
    (loop [remaining-connected connected-list
           updated-queue-dijkstra queue-dijkstra]
      (let [current-vertex (get @(:vertices graph) (:label (first remaining-connected)))]
        (if
         (not-empty remaining-connected)
          (if (= @(:state current-vertex) 0)
            (do
              (dosync
               (ref-set (:state current-vertex) 1)
               (ref-set (:distance current-vertex) (inc i)))
              (recur (rest remaining-connected) (conj updated-queue-dijkstra current-vertex)))
            (if
             (= (:state current-vertex) 1)
              (if (> (:distance current-vertex) (+ i 1))
                (do
                  (dosync
                   (ref-set (:distance current-vertex) (inc i)))
                  (recur (rest remaining-connected) (conj updated-queue-dijkstra current-vertex)))
                (recur (rest remaining-connected) (updated-queue-dijkstra)))
              (recur (rest remaining-connected) updated-queue-dijkstra)))
          (if is-bfs? (reverse updated-queue-dijkstra) updated-queue-dijkstra))))))

(defn graph-iter-dijkstra [graph finish bfs?]
  (dosync
   (ref-set (:state (get @(:vertices graph) finish)) 2)
   (ref-set (:distance (get @(:vertices graph) finish)) 0))
  (loop [queue-dijkstra (list (get @(:vertices graph) finish))]
    (when (not-empty queue-dijkstra)
      (recur (marking-unweighted graph (rest queue-dijkstra) @(:connected (first queue-dijkstra)) bfs? @(:distance (first queue-dijkstra)))))))

(defn find-smallest-dijkstra [graph current-connected last-distance] 
  (loop [connected-vertices-dijkstra current-connected
         best-distance ##Inf
         best-label nil]
    (let [current-vertex (get @(:vertices graph) (:label (first connected-vertices-dijkstra)))]
      (if (some? current-vertex)
        (if (and
             (< @(:distance current-vertex) best-distance)
             (= (- last-distance
                   @(:distance current-vertex))
                1))
          (recur
           (rest connected-vertices-dijkstra)
           @(:distance current-vertex)
           (:label current-vertex))
          (recur
           (rest connected-vertices-dijkstra)
           best-distance
           best-label))
        [best-distance best-label]))))

(defn shortest-path-dijkstra [finish start graph]
  (graph-clear! graph)
  (if (some? graph)
    (do
      (graph-clear! graph)
      (graph-iter-dijkstra graph finish true)
      (loop
       [current-connected @(:connected (get @(:vertices graph) start))
        shortest-path (list start)]
        (let
         [best (find-smallest-dijkstra
                graph
                current-connected
                @(:distance (get @(:vertices graph) (first shortest-path))))
          best-label (nth best 1)
          best-distance (nth best 0)]
          (if (not (= best-distance ##Inf))
            (if (> best-distance 0)
              (recur
               @(:connected (get @(:vertices graph) best-label))
               (conj shortest-path best-label))
              (doseq [city (conj shortest-path best-label)]
                (println city)))
            (println "The path does not exist")))))
    (println "The graph you provided is empty")))

(defn degree-to-rad [deg] ;; Converts degrees to radians
  (/ (* Math/PI deg) 180))

(defn great-circle-distance ;; A function to calculate the distance between two points on the Earth using Great Circle Distance
  ([graph label1 label2]
   (great-circle-distance
    (get @(:vertices graph) label1)
    (get @(:vertices graph) label2)))
  ([vertex1 vertex2]
   (let [r 6378
         lon1 (degree-to-rad (:longtitude vertex1))
         lat1 (degree-to-rad (:latitude vertex1))
         lon2 (degree-to-rad (:longtitude vertex2))
         lat2 (degree-to-rad (:latitude vertex2))
         delta-long (abs (- lon2 lon1))]
     (* r
        (Math/acos
         (+
          (*
           (Math/sin lat1)
           (Math/sin lat2))
          (*
           (Math/cos lat1)
           (Math/cos lat2)
           (Math/cos delta-long))))))))

(graph-clear! g)

(defn marking-weighted [graph queue-arg connected-list is-bfs? total-weight start]
  (let [queue (if is-bfs?
                (reverse queue-arg)
                queue-arg)]
    (loop [remaining-connected connected-list
           updated-queue queue]
      (let [current-weight (:weight (first remaining-connected))
            current-vertex (get @(:vertices graph) (:label (first remaining-connected)))]
        (if (not-empty remaining-connected)
          (if (= @(:state current-vertex) 0)
            (do
              (dosync
               (ref-set (:state current-vertex) 1) 
               (ref-set (:distance current-vertex) (+ total-weight current-weight))
               (ref-set (:estimate current-vertex) (great-circle-distance current-vertex (get @(:vertices graph) start)))) 
              (recur (rest remaining-connected) (conj updated-queue current-vertex)))
            (if (= @(:state current-vertex) 1)
              (if (> @(:distance current-vertex) (+ total-weight current-weight))
                (do
                  (dosync
                   (ref-set (:distance current-vertex) (+ total-weight current-weight)))
                  (recur (rest remaining-connected) (conj updated-queue current-vertex)))
                (recur (rest remaining-connected) updated-queue))
              (recur (rest remaining-connected) updated-queue)))
          (if (= (:label (first queue-arg)) start)
            '()
            (if is-bfs? (reverse updated-queue) updated-queue)))))))

(defn graph-iter-a* [graph finish start bfs?]
  (dosync
   (ref-set (:state (get @(:vertices graph) finish)) 2)
   (ref-set (:distance (get @(:vertices graph) finish)) 0))
  (loop [queue (list (get @(:vertices graph) finish))]
    (when (not-empty queue)
      (let [sorted-queue (sort (fn [a b] (compare @(:estimate a) @(:estimate b))) queue)]
        (recur (marking-weighted graph (rest sorted-queue) @(:connected (first sorted-queue)) bfs? @(:distance (first sorted-queue)) start))))))

(defn find-smallest-a* [graph current-connected last-distance]
  (loop [connected-vertices current-connected
         best-distance ##Inf
         best-label nil]
    (let [current-vertex (get @(:vertices graph) (:label (first connected-vertices)))]
      (if (not (nil? current-vertex))
        (if (and (< @(:distance current-vertex) best-distance)
                 (= (- last-distance
                       @(:distance current-vertex))
                    (:weight (first connected-vertices))))
          (do
            (dosync
             (ref-set (:state current-vertex) 2))
            (recur
             (rest connected-vertices)
             @(:distance current-vertex)
             (:label (first connected-vertices))))
          (recur
           (rest connected-vertices)
           best-distance
           best-label))
        [best-distance best-label]))))

(defn shortest-path-a-star [finish start graph]
  (graph-clear! graph)
  (graph-iter-a* graph finish start true)
  (loop
   [current-connected @(:connected (get @(:vertices graph) start))
    shortest-path (list [start @(:distance (get @(:vertices graph) start))])]
    (let
     [best (find-smallest-a*
            graph
            current-connected
            @(:distance (get @(:vertices graph) (first (first shortest-path)))))
      best-label (nth best 1)
      best-distance (nth best 0)]
      (if (not= best-distance ##Inf)
        (if (> best-distance 0)
          (recur
           @(:connected (get @(:vertices graph) best-label))
           (conj shortest-path [best-label best-distance]))
          (doseq [city (conj shortest-path [best-label best-distance])]
            (println city)))
        (println "There is no path")))))

;; In order to find the shortest path between two given cities, evaluate two lines of code below that finds the path to the given citis
(println "Shortest path from Ucharal to Prague in graph without weights: ")
(shortest-path-dijkstra "Ucharal" "Prague" g)

(println " ")

(println "Shortest path from Limerick to Prague in graph without weights: ")
(shortest-path-dijkstra "Limerick" "Prague" g)

(println " ")

(println "Shortest path from Ucharal to Prague in graph with weights: ")
(shortest-path-a-star "Ucharal" "Prague" g)

(println " ")

(println "Shortest path from Limerick to Prague in graph with weights: ")
(shortest-path-a-star "Limerick" "Prague" g)

