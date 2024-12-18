(ns main)


(defn move-left 
  ([state n]
   (update state :pos-x #(- % n))) ; Move `n` steps left
  ([state] 
   (move-left state  1)))     ; Default to 1 step left

(defn move-right 
  ([state n]
   (update state :pos-x #(+ % n))) ; Move `n` steps right
  ([state] 
   (move-right state 1)))     ; Default to 1 step right

(defn move-up 
  ([state n]
   (update state :pos-y #(- % n))) ; Move `n` steps up
  ([state]
   (update state :pos-y dec)))     ; Default to 1 step up

(defn move-down 
  ([state n]
   (update state :pos-y #(+ % n))) ; Move `n` steps down
  ([state]
   (update state :pos-y inc)))     ; Default to 1 step down

(defn $ [state]
  (let [pos (dec (count (get (:data state) (state :pos-y))))]
   (assoc state :pos-x pos))) 

(defn beg [state]
   (assoc state :pos-x  0))

(defn get-cur-row [state] 
  (get (:data state) (:pos-y state)))

(defn transform-range
  [coll start end transform-fn]
  (map-indexed
    (fn [idx val]
      (if (and (>= idx start) (< idx end))
        (transform-fn val)
        val))
    coll))

(defn toggle-case [ch]
  (let [intch (int ch)] 
    (if (Character/isAlphabetic (int ch) ) 
      (char (bit-xor intch (bit-shift-left 1 5))) 
      ch)))

(defn eval ;returns [numofstackargsused newstate]
  ([{ :keys [pos-x pos-y] :as state} cmd stack]
     (cond 
      (= \h cmd) [1 (move-left state (peek stack))] 
      (= \l cmd) [1 (move-right state (peek stack))]
      (= \k cmd) [1 (move-up state (peek stack))] 
      (= \j cmd) [1 (move-down state (peek stack))] 
      (= \D cmd) (let [updated-row (vec (take (dec pos-x) (get-in state [:data pos-y])))] 
                   [1  (-> (update state :pos-x dec)   
                        (assoc-in [:data pos-y] updated-row))])
      (= \x cmd) (let [quantifier (peek stack) 
                       cur-row (get (:data state) pos-y)
                       updated-row (vec (concat (subvec cur-row 0 pos-x) 
                                                (subvec cur-row (min (count cur-row) (+ quantifier pos-x)))))] 
                   [1  (assoc-in state [:data pos-y] updated-row)])  
      (= \~ cmd) (let [quantifier (peek stack) 
                        #_ (println pos-x)
                       cur-row (get (:data state) pos-y)
                       updated (vec (transform-range cur-row  pos-x (+ quantifier pos-x) toggle-case)) ] 
                   [1  (-> (assoc-in state [:data pos-y] updated)
                           (update :pos-x  (fn [i] (if (= i (dec (count cur-row))) 
                                                       i 
                                                       (inc i)))))])
      
                   
      #_(= \w cmd) #_[1 (move-word state stack)] 
      
      (= \$ cmd) [1 ($ state)] ;todo add quantifier
      (= \^ cmd) [1 (beg state)]  ;;todo add quantifier
      :else state))
  ([state cmd] 
     (eval state cmd [1])))




(defn process-commands [initial-state commands]
  (loop [{:keys [pos-x pos-y]:as state} initial-state
         [hd & nxt :as cmds] commands
         stack []]
    (if hd 
      (cond 
        (Character/isWhitespace hd) (recur state nxt stack)
        (= \q hd) (let [r (first nxt) #_ (must be a a-z ) 
                        macro (take-while #(not= \q %) (next nxt))
                        tokens-consumed (+ 3 (count macro)) ] 
                    (if (empty? macro)
                      (recur (assoc-in state [:registers r] nil) (drop tokens-consumed cmds) stack)
                      (recur (assoc-in state [:registers r] macro) (drop tokens-consumed cmds) stack))) 

        (= \@ hd) (let [r (first nxt) 
                        quantifier (peek stack)
                        macro (apply concat (repeat (or quantifier 1) (get-in state [:registers r]))) ] 
                    (if (:error state)
                        (recur (assoc state :error nil) (vec (rest nxt)) (if quantifier (pop stack) stack))
                        (recur state (vec (concat macro (rest nxt))) (if quantifier (pop stack) stack) )))

        (= \f hd) (let [counth (or (peek stack) 1) 
                        ch (first nxt)
                        cur-row (get (:data state) pos-y)
                        [counth updated-posx] (reduce (fn [[counth new-pos] cur]
                                                (if (zero? counth)
                                                    (reduced [counth new-pos]) 
                                                    (if (= ch (get cur-row cur))
                                                        [(dec counth) cur] 
                                                        [counth pos-x]))) 
                                            [counth pos-x] (range (inc pos-x) (count cur-row))) ] 

                  (recur (-> (assoc state :pos-x updated-posx)
                             (update :errors  (fn [prev]  
                                (if (= updated-posx pos-x) "f found nothing" prev )))) 
                         (vec  (rest nxt))
                         (if (peek stack) (pop stack) stack) ))

        (<= 49 (int hd) 57) (let [digits (apply str (take-while Character/isDigit cmds)) ] 
                              (recur state (drop (count digits) cmds) (conj stack (parse-long digits))))

        :else (if (empty? stack) 
                (let [[popped state] (eval state hd)] 
                  (recur state nxt stack))
                (let [[popped state] (eval state hd stack) ] 
                  (recur state nxt (subvec stack 0 (- (count stack) popped))))))  
      state)))


(defmacro registers []
  (into {}  (map (juxt char (constantly nil)) (range (int \a)  (inc  (int \z)) ))))




(def macros 
"qa fAj~ @a  q 

@a
")

(def program 
  "qa fA~j^   q 
   @a ")
(def buf 
"AAA
AAA
AAA

")

;; Example usage
(let [state {:data  (mapv vec (clojure.string/split buf #"\r?\n" -1))  
             :registers (registers)
             :pos-x 0
             :pos-y 0
             :error nil
             :mode :normal }
      final-state (process-commands state program) #_ (println final-state) ]
    final-state  
    
    )


