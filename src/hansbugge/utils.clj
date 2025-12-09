(ns hansbugge.utils
  (:require
   [babashka.http-client :as http]
   [babashka.fs :as fs]
   [clojure.data.priority-map :refer [priority-map-keyfn]]
   [clojure.string :as str]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce -session-cookie
  (str "session=" (-> (slurp "session.txt") str/trim)))

(defn fetch-input [{:keys [year day]
                    :or {year 2024}}]
  (let [url (format "https://adventofcode.com/%s/day/%s/input" year day)]
    (-> (http/get url {:headers {"Cookie" -session-cookie}})
        :body)))

(comment
  (println (fetch-input {:year 2023 :day 1})))

;;; Problems with lines of integers
(defn numbers [input]
  (->> (str/split-lines input)
       (map #(map parse-long (re-seq #"-?\d+" %)))))

(defn transpose [xss]
  (apply mapv vector xss))

;;; Grid type problems

(defn grid
  ([input] (grid input identity))
  ([input char-parser]
   (into []
         (comp
          (map #(map char-parser %))
          (map vec))
         (str/split-lines input))))

(defn sparse-grid
  ([height width]
   {:height height
    :width width}))

(comment
  (grid "abc\ndef\nghi")
  (grid "012\n345\n678" (comp parse-long str))
  )

(def all-directions #{:n :s :e :w :ne :nw :se :sw})

(defn step [[^long x ^long y] direction]
  (case direction
    :n [(dec x) y]
    :s [(inc x) y]
    :e [x (inc y)]
    :w [x (dec y)]
    :ne [(dec x) (inc y)]
    :nw [(dec x) (dec y)]
    :se [(inc x) (inc y)]
    :sw [(inc x) (dec y)]))

(def opposite
  {:n :s
   :s :n
   :e :w
   :w :e
   :ne :sw
   :nw :se
   :se :nw
   :sw :ne})

(def clockwise
  {:n :e
   :e :s
   :s :w
   :w :n})

(def counter-clockwise
  {:n :w
   :w :s
   :s :e
   :e :n})

(defn points
  "All points in a grid, left-to-right, up-to-down."
  [g]
  (for [x (range (or (:height g) (count g)))
        y (range (or (:width g) (count (first g))))]
    [x y]))

(defn within-grid? [g [x y]]
  (let [h (or (:height g) (count g))
        w (or (:width g) (count (first g)))]
    (and (< -1 x h)
         (< -1 y w))))

(defn manhattan-dist ^long [p1 p2]
  (let [[^long x1 ^long y1] p1
        [^long x2 ^long y2] p2]
    (+ (abs (- x1 x2)) (abs (- y1 y2)))))

(defn flood-fill
  "Stack based flood-fill algorithm

  start-point is the origin of the search, a coordinate pair of longs.

  f : State -> Point -> State is a state function taking the current state,
  the current point deemed 'inside', and returns a new state for the remaining
  calculation.

  begin-state is the state the search begins with.

  inside-pred : State -> Point -> Bool takes the current state and the current
  point and decides whether the point is 'inside' the area to be filled.

  fuel is the number of iterations to perform before giving up.
  "
  ([start-point f begin-state inside-pred]
   (flood-fill start-point f begin-state inside-pred (long 1e6)))
  ([start-point f begin-state inside-pred start-fuel]
   (loop [stack (conj (m/queue) start-point)
          state begin-state
          ^long fuel start-fuel]
     (if (zero? fuel)
       [::out-of-fuel start-fuel stack state]
       (if-some [point (peek stack)]
         (if (inside-pred state point)
           (recur (into (pop stack)
                        (map #(step point %))
                        [:n :e :s :w])
                  (f state point)
                  (dec fuel))
           (recur (pop stack) state (dec fuel)))
         state)))))

(comment
  (flood-fill [0 0]
              (fn [state p] (m/update-existing-in state p (constantly :fill)))
              [[:A :A] [:A :B]]
              (fn [g p] (= :A (get-in g p))))
  ;; => [[:fill :fill]
  ;;     [:fill :B]]
  )

(defn print-grid [g]
  (doseq [row g]
    (doseq [c row]
      (print c))
    (prn)
    (flush)))

;;; Graph

(defn dijkstra
  "Dijkstra's algorithm for connected graphs

  graph is a map Node -> Set (Node x Double)
  i.e. a map that takes a node to its neighbors along with their edge weights

  source is a node
  targets is set of nodes
  fuel is a recursion cap to avoid non-termination, default 1000000

  Returns shortest distance from source node to one of target nodes along with
  all the  corresponding shortest paths."
  ([graph source targets]
   (dijkstra graph source targets 1000000))
  ([graph source targets ^long fuel]
   ;; q : node -> [how many times we've visited it; distance from start node; shortest paths to node]
   (loop [q (priority-map-keyfn (fn [v] [(first v) (second v)]) source [0 0.0 [[]]])
          fuel fuel]
     (let [[u [^long _visited ^double distu paths]] (peek q)]
       (cond
         ;; (not (zero? visited)) ::failed
         (zero? fuel) ::out-of-fuel
         (targets u)
         ;; we have reached the target
         [distu (map #(conj % u) paths)]
         :else
         (let [new-q
               (reduce
                (fn [q [v weight]]
                  (let [maybe-new-distv (+ distu (double weight))]
                    (update q v
                            (fn [[_ ^double distv prev-paths :as org]]
                              (cond
                                (or
                                 ;; we haven't seen the node yet
                                 (nil? distv)
                                 ;; we have seen it, but this distance is shorter
                                 (< maybe-new-distv distv))
                                ;; then use the new distance
                                [0 maybe-new-distv (map #(conj % u) paths)]

                                ;; we have seen it, and the distance is equally short
                                (= maybe-new-distv distv)
                                ;; add all paths together
                                [0 maybe-new-distv (concat prev-paths (map #(conj % u) paths))]

                                ;; otherwise do nothing
                                :else org)))))
                q
                ;; neighbors with weights of node u
                (graph u))
               ;; mark this node as visited, giving it lower priority in the queue,
               new-q (update new-q u update 0 inc)]
           (recur new-q (dec fuel))))))))

(defn dijkstra*
  "Dijkstra's algorithm for connected graphs

  graph is a map Node -> Set (Map {:to Node :label any :weight Double?})
  i.e. a map that takes a node to its neighbors along with their edge weights

  source is a node
  targets is set of nodes
  fuel is a recursion cap to avoid non-termination, default 1000000

  Returns shortest distance from source node to one of target nodes along with
  all the  corresponding shortest paths."
  ([graph source targets]
   (dijkstra* graph source targets 1000000))
  ([graph source targets ^long fuel]
   ;; q : node -> [how many times we've visited it; distance from start node; shortest paths to node]
   (loop [q (priority-map-keyfn (fn [v] [(first v) (second v)]) source [0 0.0 [[]]])
          fuel fuel]
     (let [[u [^long _visited ^double distu paths]] (peek q)]
       (cond
         ;; (not (zero? visited)) ::failed
         (zero? fuel) ::out-of-fuel
         (targets u)
         ;; we have reached the target
         [distu paths]
         :else
         (let [new-q
               (reduce
                (fn [q {:keys [to weight] :as edge}]
                  (let [maybe-new-distv (+ distu (double (or weight 1.0)))]
                    (update q to
                            (fn [[_ ^double distv prev-paths :as org]]
                              (cond
                                (or
                                 ;; we haven't seen the node yet
                                 (nil? distv)
                                 ;; we have seen it, but this distance is shorter
                                 (< maybe-new-distv distv))
                                ;; then use the new distance
                                [0 maybe-new-distv (map #(conj % (assoc edge :from u)) paths)]

                                ;; we have seen it, and the distance is equally short
                                (= maybe-new-distv distv)
                                ;; add all paths together
                                [0 maybe-new-distv (concat prev-paths (map #(conj % (assoc edge :from u)) paths))]

                                ;; otherwise do nothing
                                :else org)))))
                q
                ;; neighbors with weights of node u
                (graph u))
               ;; mark this node as visited, giving it lower priority in the queue,
               new-q (update new-q u update 0 inc)]
           (recur new-q (dec fuel))))))))

(comment
  (let [graph {:A #{[:B 1] [:C 1]}
               :B #{[:D 1]}
               :C #{[:D 1]}
               :D #{[:E 1]}}]
    (dijkstra graph :A #{:E}))

  (let [graph {:A #{{:to :B} {:to :C}}
               :B #{{:to :D}}
               :C #{{:to :D}}
               :D #{{:to :E}}}]
    (dijkstra* graph :A #{:E}))

  (let [graph {:A #{[:B 1] #_[:C 1] [:a1 0.5]}
               :a1 #{[:a2 0.5]}
               :a2 #{[:C 0] [:E 2.0]}
               ;; :B #{[:D 1]}
               :C #{[:D 1]}
               :D #{[:E 1]}}]
    (dijkstra graph :A #{:E}))
  )

(defn unweighted->weighted [unweighted-graph]
  (into {}
        (map (juxt key #(into #{} (map (fn [node] [node 1]) (val %)))))
        unweighted-graph))

(comment
  (unweighted->weighted
   {:A #{:B}
    :B #{:C}})
  ;; => {:A #{[:B 1]}, :B #{[:C 1]}}
  )

;;; Parallelisation helpers

(defn partition-ncpus [coll]
  (let [cnt (count coll)
        ncpus (.availableProcessors (Runtime/getRuntime))
        q (quot cnt ncpus)]
    (partition-all (if (zero? (rem cnt ncpus)) q (inc q)) coll)))

;;; Templating

(defn -day-content [{:keys [year day]
                     :or {year 2024}}]
  (let [template "(ns hansbugge.year%s.day%s
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]
   [medley.core :as m]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defonce input (utils/fetch-input {:year %s :day %s}))
(def test-input \"\")

(defn part-1 [input])

(comment
  (part-1 test-input)
  (part-1 input)
  )

(defn part-2 [input])

(comment
  (part-2 test-input)
  (part-2 input)
  )
"]
    (format template year day year day)))

(defn generate-day [{:keys [year day]
                     :or {year 2024}}]
  (let [path (format "src/hansbugge/year%s/day%s.clj" year day)]
    (when (fs/exists? path)
      (throw (ex-info "Path already exists" {:path path})))
    (fs/create-dirs (fs/parent path))
    (spit path (-day-content {:year year :day day}))))

(comment
  (generate-day {:year 2025 :day 7})
  )

(defn generate-today []
  (let [today (java.time.LocalDate/now)
        m (.getMonth today)
        d (.getDayOfMonth today)
        y (.getYear today)]
    (assert (= java.time.Month/DECEMBER m))
    (assert (or (and (< y 2025) (<= d 25))
                (<= d 12)))
    (generate-day {:year y :day d})))

(comment
  (generate-today)
  )
