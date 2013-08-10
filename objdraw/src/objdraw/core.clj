(ns objdraw.core
  (:gen-class)
  (:use [slimath core]
        [sliimp core filter sampler film]
        [sligeom core bounding aggregate transform [intersect :only [ray]]]
        [slitrace core camera shape prim])
  (:import [sliimp.sampler Sample]
           [sligeom.transform Transform]
           [sligeom.intersect Ray]))
  
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *world-transform* (compose (translate -0 -0 -6)
                                          (rotate [1 1 0] 1.1)))
(def ^:dynamic *projection* (perspective :fov-rads (Math/toRadians 38.) 
                                         :aspect 1.0 
                                         :near 1.0 
                                         :far 100.0))

;(def ^:dynamic *world* (instance (translate 0 0 0)  (sphere 1.0)))

(def ^:dynamic *world* (instance (compose (translate -0 -0 -0)
                                          (rotate [1 1 0] 0.0))
                                            (bbox (point3 -1 -1 -1) (point3 1 1 1))))

(def ^:dynamic *grid* (grid3 (bbox (point3 -1 -1 -1) (point3 1 1 1)) 256))

(defn ^Ray world-ray-from-sample [^Transform SP ^Sample s]
  (let [^Ray r-camera (camera-ray (:x-film s) (:y-film s) SP)
        ^Ray r-world (transform r-camera (inverse *world-transform*))]
    r-world))

(defn radiance [^Transform SP ^Sample s] 
  (if-let [[t p n] (trace *world* (world-ray-from-sample SP s)) ]
    (sample s t t t)
    (sample s 0.0 0.0 0.0)))

(defn grid-trace-depth [^Transform SP ^Sample s]
  (let [r-world (world-ray-from-sample SP s)
        d (:direction r-world)
        vs (grid3-seq *grid* r-world)
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

  (when-let [^Film F (film :bounds (rect :width 1024 :height 1024) 
                      :filter (gaussian)
                      :finished-f #(spit-film! % "/tmp/slixrt.exr")
                      :sampler-f stratified-seq2
                      :samples-per-pixel 2)]
    (println "objdraw\n")
    (let [[x0 y0 x1 y1] (rect-vec (:bounds F))
          sf (partial (:sampler-f F) (:samples-per-pixel F))
          S (screen-transform (width F) (height F))
          P (perspective :fov-rads (Math/toRadians 38.) 
                         :aspect (double (/ (width F) (height F))) 
                         :near 1.0 
                         :far 100.0)
          SP (compose S P)
;          radiance-f (partial radiance SP)
          radiance-f (partial grid-trace-depth SP)]
      (doseq [y (range y0 y1) x (range x0 x1)]
        (let [^Sampler ss (sf x y)]
          (doseq [^Sample s (:samples ss)]        
            (splat! F (radiance-f s))))))
    (finish-film! F)))

