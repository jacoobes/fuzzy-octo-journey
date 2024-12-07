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



(defn eval ;returns [numofstackargsused newstate]
  ([state cmd stack]
     (cond 
      (= \h cmd) [1 (move-left state (peek stack))] 
      (= \l cmd) [1 (move-right state (peek stack))]
      (= \k cmd) [1 (move-up state (peek stack))] 
      (= \j cmd) [1 (move-down state (peek stack))] 
      (= \D cmd) (let [cur-pos (:pos-y state) ;todo add quantifier 
                       updated-row (vec (take (dec (state :pos-x)) (get-in state [:data cur-pos])))] 
                   [1  (-> (update state :pos-x dec)   
                        (assoc-in [:data cur-pos] updated-row))])
      (= \x cmd) (let [cur-pos [(:pos-y state) (:pos-x state)]  
                       quantifier (peek stack) 
                       cur-row (get (:data state) (first cur-pos))
                       updated-row (vec (concat (subvec cur-row 0 (second cur-pos)) 
                                                (subvec cur-row (min (count cur-row) (+ quantifier (second cur-pos))))))] 
                   [1  (assoc-in state [:data (first cur-pos)] updated-row)])  
                   
      #_(= \w cmd) #_[1 (move-word state stack)] 
      
      (= \$ cmd) [1 ($ state)] ;todo add quantifier
      (= \^ cmd) [1 (beg state)]  ;;todo add quantifier
      :else state))
  ([state cmd]
     (eval state cmd [1])))

(defn process-commands [initial-state commands]
  (loop [state initial-state
         [hd & nxt :as cmds] commands
         stack []]
    (if hd 
      (cond 
        (Character/isWhitespace hd) (recur state nxt stack)
        (<= 49 (int hd) 57) (let [digits (apply str (take-while #(Character/isDigit %) cmds)) ] 
                              (recur state (drop (count digits) cmds) (conj stack (parse-long digits)))) 
        :else (if (empty? stack) 
                (let [[popped state] (eval state hd)] 
                  (recur state nxt stack))
                (let [[popped state] (eval state hd stack) ] 
                  (recur state nxt (subvec stack 0 (- (count stack) popped))))))  
      state)))


;; Example usage
(let [state {:data [[\a \b \c \d \e]
                    [\f \g \h \i \j]] 
             :pos-x 0
             :pos-y 0
             :mode :normal }
      final-state (process-commands state [\l \1 \0 \x])
      _ (println final-state) ]
    [(:data final-state) ] 
    
    )


