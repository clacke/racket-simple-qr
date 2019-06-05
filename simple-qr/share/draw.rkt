#lang racket

(require racket/draw)

(require simple-svg)

(provide (contract-out
          [locate-brick (-> natural? pair? pair?)]
          [draw-background (-> natural? natural? void?)]
          [draw-points (-> natural? hash? hash? void?)]
          [*output_handle* parameter?]
          [*output_type* parameter?]
          ))

(define *output_handle* (make-parameter #f))

(define *output_type* (make-parameter 'png))

(define (locate-brick module_width place_pair)
  (cons (* (sub1 (cdr place_pair)) module_width)
        (* (sub1 (car place_pair)) module_width)))

(define (draw-points module_width points_map color_map)
  (hash-for-each
   points_map
   (lambda (point_pair val)
     (let ([new_point_pair (cons (+ (cdr point_pair) 4) (+ (car point_pair) 4))])
       (draw-module (*output_handle*)
                    (hash-ref color_map point_pair (if (string=? (~a val) "1") "black" "white"))
                    (locate-brick module_width new_point_pair)
                    module_width)))))

(define (draw-background modules module_width)
  (let loop-row ([row 1])
    (when (<= row modules)
          (let loop-col ([col 1])
            (when (<= col modules)
                  (draw-module (*output_handle*) "white" (locate-brick module_width (cons row col)) module_width)
                  (loop-col (add1 col))))
          (loop-row (add1 row)))))
