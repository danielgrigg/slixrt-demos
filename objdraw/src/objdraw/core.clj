(ns objdraw.core
  (:gen-class)
  (:use [slimath core]
        [sliimp core filter sampler film]
        [sligeom core bounding aggregate transform intersect]
        [slitrace core camera shape prim])
  (:import [sliimp.sampler Sample]
           [sligeom.transform Transform]
           [sligeom.intersect Ray]
           [slitrace.camera Camera]
           [java.util.concurrent Executors]))
  
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn- parse-float [^String x] (Float/parseFloat x))
(defn- parse-int [^String x] (Integer/parseInt x))
(defn- parse-triple [xs t] (apply vector-of t (map parse-float xs)))
  
(defn- parse-vertex-index [^String face-index]
  (apply vector-of :int 
         (map parse-int (re-seq #"\d+" face-index))))

(defmulti parse-tokens (fn [tokens] (first tokens)))

(defmethod parse-tokens "#" [params] 
  [:comment (->> (rest params) (interpose \space) (apply str))])

(defmethod parse-tokens "v" [params]
  [:vertices (parse-triple (rest params) :float)])

(defmethod parse-tokens "vt" [params]
  [:uvs (parse-triple (rest params) :float)])

(defmethod parse-tokens "vn" [params]
  [:normals (parse-triple (rest params) :float)])

(defmethod parse-tokens "f" [params]
  [:faces (vec (map parse-vertex-index (rest params)))])

(defmethod parse-tokens "g" [params]
  [:groups (rest params)])

(defmethod parse-tokens "mtllib" [params]
  [:mtllib (rest params)])

(defmethod parse-tokens "usemtl" [params]
  [:usemtl (rest params)])

(defmethod parse-tokens "s" [params]
  [:shading-mode (rest params)])

(defmethod parse-tokens :default [params]
  [:unknown (apply str (interpose \space params))])

(defn parse-line 
  "Parse a single line. If a map m is supplied, accumulate the 
parse into it."
  ([^String line]   (parse-tokens (re-seq #"\S+" line)))
  ([m ^String line]
     (let [[k v] (parse-line line)]
       (update-in m [k] conj v))))

(defn lines [^String s]
    (remove clojure.string/blank? (clojure.string/split-lines s)))

(defn model 
  ([]
     {:vertices [] :faces [] :ignored []})
([name]
   (with-open [rdr (java.io.BufferedReader. 
                    (java.io.FileReader. name))]
     (reduce  parse-line-into (model) (line-seq rdr)))))

(defn model-flatten [{:keys [vertices faces] :as m}]
  (let [de-index (partial nth vertices)
        faces' (for [f faces :let [f' (map dec f)]]
                 (map de-index f'))]
    (assoc-in m [:faces] faces')))


(defn synchronous-render [film f] 
  (let [[x0 y0 x1 y1] (rect-vec (:bounds film))]
      (doseq [y (range y0 y1) x (range x0 x1) 
              :let [^Sampler ss (sampler-film film x y)]]
          (doseq [^Sample s (:samples ss)]        
            (splat! film (f s)))))
  (finish-film! film))

(defn batch-pixels [film ^long n]
  (let [[x0 y0 x1 y1] (rect-vec (:bounds film))]
    (partition-all n (for [y (range y0 y1) x (range x0 x1)] [x y]))))

(defn parallel-render [film f nthreads npixels-task]
  (let [ pool (Executors/newFixedThreadPool nthreads)       
        tasks (map (fn [ps]
                     (fn []
                       (time
                       (doseq [[x y] ps 
                               :let [^Sampler sampler (sampler-film film x y)]]
                              (doseq [^Sample s (:samples sampler) ]
                                (splat! film (f s)))))))
                   (batch-pixels film npixels-task))]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)
    (finish-film! film)))

(defn make-world [t]
  (group (compose (translate 0 0 -5)
                  (rotate [1 0.3 1] t))
        (->> (for [y (range -2 3) 
                   x (range -2 3)
                   z (range -2 3)] 
               (translate x y z))
            (map #(instance % (sphere 0.3))
                 ))))


(def ^:dynamic *world-transform* (compose (translate -0 -0 -6)
                                          ;(rotate [1 1 0] 1.1)
                                          ))
(def ^:dynamic *grid* (grid (bbox (point3 -1 -1 -1) (point3 1 1 1)) 256))

(defn ^Ray world-ray-from-sample [camera ^Sample s]
  (let [^Transform SP (screen-projection-transform camera)
        ^Ray r-camera (camera-ray (:x-film s) (:y-film s) SP)
        ^Ray r-world (transform r-camera (inverse *world-transform*))]
    r-world))

(defn radiance [world ^Camera c ^Sample s] 
  (if-let [[t p [nx ny nz]] (trace world (world-ray-from-sample c s)) ]
    (sample s nx ny nz)
    (sample s 0.0 0.0 0.0)))

(defn grid-trace-depth [^Camera c ^Sample s]
  (let [r-world (world-ray-from-sample c s)
        d (:direction r-world)
        vs (grid-seq *grid* r-world)
        n (count vs)
        n' (double (/ n 512))]
;    (println "s" s "d" n)
    (if vs
      (sample s n' n' n')
      (sample s 0.0 0.0 0.0))))

(defn radiance2 [SP s]
  (sample s 1.0 1.0 0.0))


(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))

  (let [[t w n] (map read-string (take 3 args))
        ^Film F (film :bounds (rect :width w :height w) 
                      :filter (tent)
                      :finished-f #(spit-film! % (str "/tmp/s-" t ".exr"))
                      :sampler-f stratified-seq2
                      :samples-per-pixel 2)
             ^Camera C (perspective-camera 
                        {:width (long (width F)) :height (long (height F))} )]
    (println "objdraw " t (* (width F) (height F)) "pixels")
    (synchronous-render F (partial radiance (make-world t) C))
    ))
;    (parallel-render F C grid-trace-depth nthreads n)))

