(ns view-markdown.core
  (:require [clojure.string :as str])
  (:use [clojure.test])
  (:use [hiccup.core])
  (:use [hiccup.page-helpers]))

(def sample-doc "# h1
a
b
c

## h2

d

## h2 b
e
f

# h1 b

g

## h2 c

h

### h3

i
j

### h3 b

k
l

# h1 d

m")

(defn create-entries [doc]
  (conj (let [lines (str/split-lines doc)]
          (for [li lines]
            {:text li}))
        {:is-root true}))

(deftest
  test-create-entries
  (is (:is-root (nth (create-entries sample-doc) 0)))
  (is (= (nth (create-entries sample-doc) 4)
         {:text "c"})))

(defn add-line-numbers [entries]
  (map-indexed (fn [i e] (assoc e :num i))
               entries))

(deftest
  test-add-line-numbers
  (let [entry (nth (add-line-numbers (create-entries sample-doc)) 6)]
    (is (= entry
         {:text "## h2" :num 6}))))

(defn add-headers [entries]
  (for [entry entries]
    (assoc entry :header
           (cond (:is-root entry) 0
                 (re-find #"^###### " (:text entry)) 6
                 (re-find #"^##### " (:text entry)) 5
                 (re-find #"^#### " (:text entry)) 4
                 (re-find #"^### " (:text entry)) 3
                 (re-find #"^## " (:text entry)) 2
                 (re-find #"^# " (:text entry)) 1
                 :else nil))))

(deftest
  test-add-headers
  (let [entries (add-headers (create-entries sample-doc))
        header-of (fn [i] (:header (nth entries i)))]
    (is (= (header-of 1)
           1))
    (is (= (header-of 6)
           2))
    (is (= (header-of 2)
           nil))))

(defn headers-only [entries]
  (filter :header entries))

(def make-entries (comp add-headers add-line-numbers create-entries))

(def sample-full-entries
  (make-entries sample-doc))

(deftest
  test-headers-only
  (let [entries sample-full-entries]
    (is (= (:text (nth (headers-only entries) 1))
           "# h1"))
    (is (= (:text (nth (headers-only entries) 5))
           "## h2 c"))))

(defn add-children-texts [headers entries]
  (for [h headers]
    (assoc h :texts (->> entries
                      (drop (inc (:num h)))
                      (take-while (comp not :header))))))

(deftest
  test-add-children-texts
  (let [headers (add-children-texts (headers-only sample-full-entries)
                                    sample-full-entries)]
    (is (= (map :num (:texts (nth headers 1)))
           [2 3 4 5]))
    (is (= (map :num (:texts (nth headers 2)))
           [7 8 9]))
    (is (= (map :num (:texts (nth headers 7)))
           [28 29 30 31]))))

(defn add-children-headers [headers]
  (for [h headers]
    (assoc h :headers (->> headers
                        (drop-while (fn [e] (<= (:num e)
                                                (:num h))))
                        (take-while (fn [e] (> (:header e)
                                               (:header h))))))))

(deftest
  test-add-children-headers
  (let [headers (add-children-headers (headers-only sample-full-entries))]
    (is (= (map :num (:headers (nth headers 1)))
           [6 10]))
    (is (= (map :num (:headers (nth headers 5)))
           [22 27]))
    (is (= (map :num (:headers (nth headers 6)))
           []))))

(defn master-data-struture [doc]
  (let [entries (make-entries doc)
        headers (headers-only entries)]
    (-> headers
      (add-children-texts entries)
      add-children-headers)))

(deftest
  test-master-data-structure
  (let [master (master-data-struture sample-doc)]
    (is (= (count (:texts (second master)))
           4))
    (is (= (count (:headers (second master)))
           2))))

(def custom-js
  "
function showOnlyDiv(i) {
  $('div').hide()
  $('div#' + i).show()
}
$(function() {
  showOnlyDiv(0)
})
  "
  )

(spit "/sandbox/s.html"
      (html5
        (include-js "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js")
        (javascript-tag custom-js)
        (for [h (master-data-struture sample-doc)]
          [:div {:id (:num h)}
           [:h2 {:onclick "showOnlyDiv(0)"} "Back to root"]
           [:h1 (:text h)]
           (for [t (:texts h)] [:p (:text t)])
           (for [s (:headers h)]
             [:h2
              {:onclick (str "showOnlyDiv(" (:num s) ")")}
              (:text s)])])))

