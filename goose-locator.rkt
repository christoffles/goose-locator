#lang racket

(require "uw-api.rkt")


;;;;;;;;;;;;;;;;
;; INTERFACE: ;;
;;;;;;;;;;;;;;;;

;; (nearest-nest building) consumes a building name and produces the 
;;   location of the nearest goose nest if the building's latitude 
;;   and longitude are available
;; nearest-nest: Str -> Str

;; Examples:
;;  (nearest-nest "DP") => "Arts Quad"
;;  (nearest-nest "UW Place Eby Hall") => "On top of Tim Hortons, 
;;    nest maybe on the parking lot infront of Woolwich South"
;;  (nearest-nest "UWP") => "Building latitude or longitude not 
;;    given"

;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION: ;;
;;;;;;;;;;;;;;;;;;;;;

;; (find-value lst key) consumes a dictionary and a key and produces ;; the value corresponding to key
;; find-value: (listof (list Str Any)) -> Any
(define (find-value lst key)
  (cond [(empty? lst) false]
        [(string=? key (first (first lst))) (second (first lst))]
        [else (find-value (rest lst) key)]))
;; this function is from my q4black

;; Useful Constants
(define radius 6371)
(define distance-threshold 1000)

(define (nearest-nest building)
  ;; (coordinates building lob) consumes a building name and a list 
  ;;   of  buildings details and produces the coordinates of the 
  ;;   building a list '(latitude longitude)
  ;; coordinates: String -> (list Num Num)
  (define (coordinates building lob)
    (cond [(empty? lob) (list "null" "null")]
           [(or (string=? building 
                         (find-value (first lob) "building_name"))
               (member building
                         (find-value (first lob) "alternate_names")))
           (list (find-value (first lob) "longitude")
                 (find-value (first lob) "latitude"))]
           [else (coordinates building (rest lob))]))
  
  ;; (distance coord1 coord2) consumes two sets of coordinates and
  ;;   produces the distance between them calculated using the 
  ;;   Haversine formula
  ;; distance: Num Num -> Num
  (define (distance coord1 coord2)
    (define (distance-radians long1 long2 lat1 lat2)
      (define a (+ (sqr (sin (/ (- lat2 lat1) 2)))
                   (* (cos lat1) (cos lat2)
                      (sqr (sin (/ (- long2 long1) 2))))))
      (define c (* 2 (atan (sqrt a) (sqrt (- 1 a)))))
      (* radius c))
    (distance-radians (degrees->radians (first coord1))
                      (degrees->radians (first coord2))
                      (degrees->radians (second coord1))
                      (degrees->radians (second coord2))))
  
  

  
  ;; (nearest-nest-acc building-coord logn nearest) consumes a set of 
  ;;   coordinates and a list of goose nest details and produces
  ;;   the goose nest nearest to the building
  (define (nearest-nest-acc building-coord logn nearest)
    ;; nearest = '(distance location)
    (cond [(empty? logn) (second nearest)]
          [else 
           (define dist
             (distance (list (find-value (first logn) "longitude")
                             (find-value (first logn) "latitude"))
                                          building-coord))
           (cond [(< dist (first nearest))
                   (nearest-nest-acc building-coord (rest logn) 
                               (list dist (find-value (first logn)                                                         "location")))]
                 [else (nearest-nest-acc building-coord (rest logn)
                                         nearest)])]))
  (define build-coord 
    (coordinates building (uw-api "/buildings/list")))
  
  (cond [(or (equal? (first build-coord) "null")
             (equal? (second build-coord) "null"))
             ;; equal? used since it could be "null" or a number
         "Building latitude or longitude not given"]
        [else (nearest-nest-acc build-coord
                                (uw-api "/resources/goosewatch") 
                                (list distance-threshold 
                                      "no goose nests nearby"))]))

