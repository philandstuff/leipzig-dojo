(ns leipzig-dojo.wtf
  (:use
    leipzig.melody
    leipzig.scale
    leipzig.canon
    leipzig.live)
  (:require [overtone.live :as overtone]
            [overtone.synth.stringed :as strings])) 

(strings/gen-stringed-synth ektara 1 true)

(defn pick [distort amp {midi :pitch, start :time, length :duration}] 
    (let [synth-id (overtone/at start
                     (ektara midi :distort distort :amp amp :gate 1))]
      (overtone/at (+ start length) (overtone/ctl synth-id :gate 0))))

(defmethod play-note :leader [note]
  (pick 0.7 1.0 note))
(defmethod play-note :follower [note]
  (pick 0.3 1.0 note))
(defmethod play-note :bass [note]
  (pick 0.9 0.2 (update-in note [:pitch] #(- % 12))))

(def melody "A simple melody built from durations and pitches."
               ; Row, row, row your boat,
  (->> (phrase [3/3 3/3 3/3 5/6 1/6 3/3 5/6 1/6 6/3]
               [  0   0   0  -2   2   0  -2   2   0])
       (then
        (phrase [3/3 3/3 3/3 5/6 1/6 3/3 5/6 1/6 6/3]
                [  4   4   4   5   2 -1/2 -2   2   0]))
       (then (phrase [3/3 5/6 1/6 3/3 5/6       1/6 1/4       1/4 1/2       1/2 1/2 1         5/6 1/6]
                     [7   0   0   7   (+ 6 1/2) 6   (+ 5 1/2) 5   (+ 5 1/2) (+ 5 1/2) 1/2 (+ 3 1/2) 3   5/2]))
       (then (phrase [1/4 1/4 1/2 1/2 1/2    1  5/6 1/6 1 5/6 1/6 2 1 5/6 1/6 1 5/6       1/6]
                     [  2   1   2   2 -2  -1/2 -2  -1/2 2   0   2 4 7   0   0 7 (+ 6 1/2) 6]))
       (then (phrase [1/4       1/4 1/2       1/2       1/2 1         5/6 1/6]
                     [(+ 5 1/2) 5   (+ 5 1/2) (+ 5 1/2) 1/2 (+ 3 1/2) 3   (+ 2 1/2)]))
       (then (phrase [1/4 1/4 1/2 1/2 1/2 1    5/6 1/6 1 5/6 1/6 2]
                     [2     1   2 2   -2  -1/2 -2   2  0 -2   2  0]))
       (where :part (is :leader))))

(def bass "A bass part to accompany the melody."
  (->> (phrase [1  1 2]
               [0 -3 0])
     (where :part (is :bass))
     (times 4)))

(defn row-row
  "Play the tune 'Row, row, row your boat' as a round."
  [speed key]
  (->> melody
    ;;(with bass)
    #_(times 2)
    #_(canon (comp (simple 4)
                 (partial where :part (is :follower))))
    (where :time speed)
    (where :duration speed)
    (where :pitch key)
    play))

(comment
  (row-row (bpm 220) (comp A sharp minor))
  (row-row (bpm 150) (comp low B flat major))
)
