(ns view-markdown.core
  (:require [clojure.string :as str])
  (:use [clojure.test])
  (:use [hiccup.core])
  (:use [hiccup.page-helpers])
  (:gen-class))

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
                                               (:header h))))
                        (filter (fn [e] (= (:header e)
                                           (inc (:header h)))))))))

(deftest
  test-add-children-headers
  (let [headers (add-children-headers (headers-only sample-full-entries))]
    (is (= (map :num (:headers (nth headers 0)))
           [1 14 32]))
    (is (= (map :num (:headers (nth headers 4)))
           [18]))
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

(def custom-js "
var history = []
function forward(i) {
  history.push(i)
  setBackButton()
  showOnlyDiv(i)
}
function back() {
  if(history.length == 1) return
  history.pop()
  var last = history.slice().pop()
  setBackButton()
  showOnlyDiv(last)
}
function showOnlyDiv(i) {
  $('div').hide()
  $('div#' + i).show()
}
function setBackButton() {
  $('#back')[(history.length == 1) ? 'hide' : 'show']()
}
$(function() { forward(0) })
")

(def custom-css "
  body {
    background-color: #d6d6c3;  /* white */
    color: #414050;  /* dark gray */
    padding: 0px;
    margin: 0px;
  }
  h2#back {
    letter-spacing: 1.5px;
    font-size: 18px;
    padding: 0.5em;
    margin: 5px;
    background-color: #d49147;  /* orange */
  }
  h1 {
    font-family: 'Lucida Sans Unicode','Lucida Grande','Lucida Sans',Lucida,sans-serif;
    font-size: 36px;
    padding: 0.4em;
    margin: 5px;
    background-color: #97d9d7;  /* blue */
  }
  h2 {
    font-family: 'Lucida Sans Unicode','Lucida Grande','Lucida Sans',Lucida,sans-serif;
    font-size: 18px;
    padding: 0.5em;
    margin: 2px;
    background-color: #b8a997;  /* light gray */
  }
  pre {
    font-family: monospace;
    font-size: 18px;
    line-height: 1.2em;
    margin: 0;
    padding: 0;
    padding-left: 1em;
  }
")

(defn gen-page [in-text]
  (html5
    [:style {:type "text/css"} custom-css]
    (include-js "https://ajax.googleapis.com/ajax/libs/jquery/1.7.1/jquery.min.js")
    (javascript-tag custom-js)
    (for [h (master-data-struture in-text)]
      [:div {:id (:num h)}
       [:h2#back {:onclick "back()" } "Back"]
       (when (:text h) [:h1 (:text h)])
       (for [t (:texts h)]
         (if (empty? (str/trim (:text t)))
           [:br]
           [:pre (:text t)]))
       (for [s (:headers h)]
         [:h2
          {:onclick (str "forward(" (:num s) ")")}
          (:text s)])])))

(defn help-msg []
  (println "
Program takes 2 args:

- in-path  : a markdown-like file
- out-path : where to write file
"))

(defn main [in-path out-path]
  (spit out-path (gen-page (slurp in-path))))

(defn -main [& args]
  (if (not= 2 (count args))
    (help-msg)
    (main (first args)
          (second args))))
