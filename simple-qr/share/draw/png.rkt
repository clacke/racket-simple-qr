#lang racket

(require racket/draw)

(provide (contract-out
          [draw-png-module (-> (or/c (is-a?/c color%) string?) pair? natural? void?)]
          [draw-png (-> natural? natural? hash? hash? path-string? void?)]
          ))

(define (draw-png-module color place_pair module_width)
  (when (not (string=? color "transparent"))
        (send (*current-handle*) set-pen color 1 'solid)
        (send (*current-handle*) set-brush color 'transparent)

        (send (*current-handle*) draw-rectangle (cdr place_pair) (car place_pair) module_width module_width)))

(define (draw-png modules module_width points_map color_map file_name)
  (let* ([canvas_width (* (+ modules 8) module_width)]
         [target #f])

    (set! target (make-bitmap canvas_width canvas_width))
    (set! dc (new bitmap-dc% [bitmap target]))]
    
    (parameterize
     ([*current_handle* dc])

     (send dc set-smoothing 'aligned)

     (draw-background (+ modules 8) module_width)

     (draw-points module_width points_map color_map)

     (send target save-file file_name 'png)))
