(#%require (only racket/base random void))
;;;import the function `random` and `void`
(#%require (rename racket/base block let))
;;;to break the restriction of r5rs about internal definitions (5.2.2)


;will implement this function to flapmap
(define (accumulate op init lst)
  (if (null? lst)
      init
      (op (car lst) (accumulate op init (cdr lst)))))

(define (flatmap f lst); It is similar to the function map function. However, it flattens the nested list. For example,
        (accumulate append '() (map f lst))); the result of (map (lambda (x) (list x)) '(1 2 3)) is ((1) (2) (3)),
                                           ;and the result of converting map to flat-map is (1 twenty three).

;check if the prediction is valid in the list,
(define (filter predicate sequence);reference SICP page 107 filter function
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;range function, from start point to the end
(define (range start end)
  (if (> start end)
      '()
      (cons start (range (+ start 1) end))))

;initialize the random block to max distance
(define max-distance 99999999)

(define (make-graph n m)
  (define random-picked '());initialize an empty list
  (define (random-pick)
    (define gx (random n))
    (define gy (random m))
    (if (or (and (= gx 0) (= gy 0));can't put random block at start point
            (member (list gx gy) random-picked);can't put random block at same place
            (and (= gx (- n 1)) (= gy (- m 1)))) ;can't put random block at end point
        (random-pick) ; return random-pick function
        (set! random-picked (cons (list gx gy) random-picked))));else save the block to the list
   ;;random pick a block, add to random-picked           
  (for-each (lambda (x)                                      
              (random-pick)) (range 1 n))
  ;;execute n times, get n blocks
  (flatmap (lambda (x)
             (flatmap (lambda (y)
                        (cond ((or (and (= x 0) (= y 0)) (and (= x (- n 1)) (= y (- m 1)))) ;if it is start or end point
                               (list (list (list x y) 0)));initialize the weight to 0
                              ((member (list x y) random-picked)
                               (list (list (list x y) max-distance)));initialize the blocks have max-distance
                              (else
                               (list (list (list x y) (random 20))))));else random weight for other points
                      (range 0 (- m 1))))
           (range 0 (- n 1))))
; make all coordinate have a weight, we set start point and end point have weight 0

;;build a N X M map

;;Dijkstar alogrithm

(define (dijkstra graph n m)
  (block ()
         (define (get-adjacent x y)
           (define (valid? p) ;;check if the point is valid
             (and (<= 0 (car p) (- n 1)) (<= 0 (cadr p) (- m 1))))
           (filter valid? (list (list (- x 1) y)
                                (list (+ x 1) y)
                                (list x (+ y 1))
                                (list x (- y 1)))))
         ;get all points that adjacent to (x y)
        
       (define (get-weight x y)
           (cadr (assoc (list x y) graph))); assoc function: get the point in the graph, and get the weight

         ;;get weight of the point
  
         (define solved '((0 0))); initialize the solved list is start point
         (define unsolved (cdr (flatmap (lambda (x);other points in the graph is unsolved
                                          (flatmap
                                           (lambda (y)
                                             (list (list x y)))
                                           (range 0 (- m 1)))
                                          )
                                        (range 0 (- n 1)))))
         ;;get solved point and unsolved points
         
         (define inital-points  (cons (list 0 0) (get-adjacent 0 0)))
         (define dist (flatmap (lambda (x)           
                                 (flatmap
                                  (lambda (y)
                                    (list (list (if (member (list x y) inital-points)
                                                    (get-weight x y)
                                                    max-distance)
                                                )))
                                  (range 0 (- m 1)))
                                 )
                               (range 0 (- n 1))))
         ;get all the distance of all points
  
         (define (get-dist x y)
           (car (list-ref dist (+ y (* x m)))));make 2D list into 1-dimensional list to calculate distance
  
         ;get the distance from start point to the point we execute now

         (define (set-dist! x y update-dist)
           (set-car! (list-ref dist (+ y (* x m))) update-dist));list-ref is more like check the index
         ;set the distance from start point to the point we execute now

  
         (define (work-done?)
           (member (list (- n 1) (- m 1)) solved));use member function to determine if all the path are solved

         ;;check if we find the shortest path

         (define (pick-min);return the minimal distance of the point
           (accumulate (lambda (x y)
                        (if (< (get-dist (car x)
                                         (cadr x))
                               (get-dist (car y)
                                         (cadr y)));find out the shortest path, compare to their neighbors 
                            x
                            y))
                      (car unsolved)
                      (cdr unsolved)))

         ;;choose the stortest from unsolved list
         
         (define (work)
           (if (work-done?)
               (get-dist (- n 1) (- m 1));if all path are visited, return the distance from begin to the end
               (block () (define curr (pick-min))
                      (define curr-dist (get-dist (car curr) (cadr curr)));get current distance
                      (define need-updaten (get-adjacent (car curr) (cadr curr)));get adjacent points
                      (for-each
                       (lambda (p)
                         (define new-dist (+ curr-dist (get-weight (car p) (cadr p))));get new distance
                         (if (<  new-dist (get-dist (car p) (cadr p))
                                 )
                             (set-dist! (car p) (cadr p) new-dist);update the new-dist if it is minimal cost right now 
                             (void))) need-updaten); else return void (return nothing)
                      (set! solved (cons curr solved))
                      (set! unsolved (filter (lambda (x)
                                               (not (equal? x curr))) unsolved))
                      (work))))
         ;;work through the map,check the new distance is always the stortest
         ;;put the point we get through to solved list
         (define walked '()) ;record the place we already get through, prevent dead lock
         
         (define (get-path x y)
           (if (and (= x 0) (= y 0))
               '((0 0)) ;already arrive
               (begin
                 (set! walked (cons (list x y) walked))
                 (cons (list x y) (let ((p (car (filter
                                                 (lambda (p)
                                                   (and
                                                    (= (get-dist (car p) (cadr p))
                                                       (- (get-dist x y)
                                                          (get-weight x y)))
                                                    (not (member p walked))))
                                                 (get-adjacent x y)))))
                                    (get-path (car p) (cadr p)))))))
           (work)
           (if (= (get-dist (- n 1) (- m 1)) max-distance)
               '()   ;;indicates no path
               (reverse (get-path (- n 1) (- m 1))))))
    ; back trace the point we get through.

(define (pretty-print graph path n m)
  (for-each (lambda (x)
              (for-each (lambda (y)
                          (display (cond ((member (list x y) path) "*")
                                         ((= (cadr (assoc (list x y) graph))
                                              max-distance) "X")
                                         (else "."))))
              (range 0 (- m 1)))
              (newline)
              )
            (range 0 (- n 1))))
  (define g (make-graph 20 20))
  (define p (dijkstra g 20 20))
  (pretty-print g p 20 20)






  