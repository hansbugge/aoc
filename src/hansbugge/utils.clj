(ns hansbugge.utils
  (:require
   [babashka.http-client :as http]
   [babashka.fs :as fs]
   [clojure.string :as str]))

(defonce -session-cookie
  (str "session=" (-> (slurp "session.txt") str/trim)))

(defn fetch-input [{:keys [year day]
                    :or {year 2024}}]
  (let [url (format "https://adventofcode.com/%s/day/%s/input" year day)]
    (-> (http/get url {:headers {"Cookie" -session-cookie}})
        :body)))

(comment
  (println (fetch-input {:year 2023 :day 1})))


(defn -day-content [{:keys [year day]
                     :or {year 2024}}]
  (let [template "(ns hansbugge.year%s.day%s
  (:require
   [clojure.string :as str]
   [hansbugge.utils :as utils]))

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
