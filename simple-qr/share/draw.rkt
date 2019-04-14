#lang racket

(require racket/draw)

(provide (contract-out
          [locate-brick (-> natural? pair? pair?)]
          [draw-module (-> (is-a?/c bitmap-dc%) (or/c (is-a?/c color%) string?) pair? natural? void?)]
          [draw-background (-> (is-a?/c bitmap-dc%) natural? natural? void?)]
          [draw-points (-> (is-a?/c bitmap-dc%) natural? hash? hash? void?)]
          [draw (-> natural? natural? hash? hash? path-string? void?)]
          ))

(define (locate-brick module_width place_pair)
  (cons (* (sub1 (cdr place_pair)) module_width)
        (* (sub1 (car place_pair)) module_width)))

(define (draw-module dc color place_pair module_width)
  (when (not (string=? color "transparent"))
        (send dc set-pen color 1 'solid)
        (send dc set-brush color 'solid)

        (send dc draw-rectangle (cdr place_pair) (car place_pair) module_width module_width)))

(define (draw-points dc module_width points_map color_map)
  (hash-for-each
   points_map
   (lambda (point_pair val)
     (let ([new_point_pair (cons (+ (cdr point_pair) 4) (+ (car point_pair) 4))])
       (draw-module dc 
                    (hash-ref color_map point_pair (if (string=? (~a val) "1") "black" "white")) 
                    (locate-brick module_width new_point_pair) 
                    module_width)))))

(define (draw-background dc modules module_width)
  (let loop-row ([row 1])
    (when (<= row modules)
          (let loop-col ([col 1])
            (when (<= col modules)
                  (draw-module dc "white" (locate-brick module_width (cons row col)) module_width)
                  (loop-col (add1 col))))
          (loop-row (add1 row)))))

(define (draw modules module_width points_map color_map file_name)
  (let* ([canvas_width (* (+ modules 8) module_width)]
         [target (make-bitmap canvas_width canvas_width)]
         [dc (new bitmap-dc% [bitmap target])])

    (draw-background dc (+ modules 8) module_width)

    (draw-points dc module_width points_map color_map)

    (send target save-file file_name 'png)
    
    (void)))
