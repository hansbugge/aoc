(ns hansbugge.utils
  (:require
   [babashka.http-client :as http]
   [babashka.fs :as fs]
   [clojure.string :as str]))

(set! *warn-on-reflection* true)

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
       (map #(map parse-long (re-seq #"\d+" %)))))

;;; Grid type problems

(defn grid
  ([input] (grid input identity))
  ([input char-parser]
   (into []
         (comp
          (map #(map char-parser %))
          (map vec))
         (str/split-lines input))))

(comment
  (grid "abc\ndef\nghi")
  (grid "012\n345\n678" (comp parse-long str))
  )

(def all-directions #{:n :s :e :w :ne :nw :se :sw})

(defn step [[x y] direction]
  (case direction
    :n [(dec x) y]
    :s [(inc x) y]
    :e [x (inc y)]
    :w [x (dec y)]
    :ne [(dec x) (inc y)]
    :nw [(dec x) (dec y)]
    :se [(inc x) (inc y)]
    :sw [(inc x) (dec y)]))

(defn points
  "All points in a grid, left-to-right, up-to-down."
  [g]
  (for [x (range (count g))
        y (range (count (first g)))]
    [x y]))

(defn within-grid? [g [x y]]
  (let [h (count g)
        w (count (first g))]
    (and (< -1 x h)
         (< -1 y w))))

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
   [hansbugge.utils :as utils]))

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
  (generate-day {:year 2024 :day 2})
  )

(defn generate-today []
  (let [today (java.time.LocalDate/now)
        m (.getMonth today)
        d (.getDayOfMonth today)
        y (.getYear today)]
    (assert (= java.time.Month/DECEMBER m))
    (generate-day {:year y :day d})))

(comment
  (generate-today)
  )
