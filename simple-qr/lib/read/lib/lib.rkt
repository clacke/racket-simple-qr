#lang racket

(provide (contract-out
          [pic->points (-> path-string? list?)]
          [points->pic (-> (listof list?) path-string? hash? any)]
          [find-threshold (-> list? exact-nonnegative-integer?)]
          [points->bw (-> list? exact-nonnegative-integer? list?)]
          [print-points (-> list? void?)]
          [guess-first-dark-width (-> list? exact-nonnegative-integer?)]
          [guess-module-width (-> (or/c #f exact-nonnegative-integer?) list? (or/c boolean? list?))]
          [squash-points (-> list? exact-nonnegative-integer? list?)]
          [*trace_level* parameter?]
          [trace (-> string? exact-nonnegative-integer? void?)]
          [qr-read (-> path-string? (or/c string? boolean?))]
          [squash-matrix (-> (listof list?) exact-nonnegative-integer? (listof list?))]
          [point-distance (-> pair? pair? number?)]
          [find-pattern-center-points (-> (listof list?) (or/c boolean? pair?))]
          [check-center-points-valid (-> hash? boolean?)]
          [get-center-points (-> hash? list?)]
          [calculate-rotate-ratio (-> pair? pair? exact-nonnegative-integer? number?)]
          [align-matrix (-> (listof list?) any/c (listof list?))]
          [trim-matrix (-> (listof list?) (listof list?))]
          ))

(require racket/draw)

(require "../matrix-rotate/lib.rkt")
(require "../../share/format-information.rkt")
(require "../../share/finder-pattern.rkt")
(require "../../share/separator.rkt")
(require "../../share/timing-pattern.rkt")
(require "../../share/alignment-pattern.rkt")
(require "../../share/version-information.rkt")
(require "../../share/dark-module.rkt")
(require "../../share/fill-data.rkt")

(define *trace_level* (make-parameter 0))

(define (trace data trace_level)
  (when (>= (*trace_level*) trace_level)
        (printf "t[~a]=~a\n" trace_level data)))

(define (print-matrix matrix)
  (for-each
   (lambda (row)
     (for-each
      (lambda (col)
        (printf "~a" (~a #:width 1 #:align 'right #:pad-string "0" col)))
      row)
     (printf "\n"))
   matrix)
  (printf "\n"))

(define (pic->points pic_path)
  (let* ([img (make-object bitmap% pic_path)]
         [width (send img get-width)]
         [height (send img get-height)]
         [bits_count (* width height 4)])

    (let ([bits_bytes (make-bytes bits_count)])
      (send img get-argb-pixels 0 0 width height bits_bytes)
      
      (let loop ([loop_list (bytes->list bits_bytes)]
                 [rows '()]
                 [cols '()])
        (if (= (length rows) height)
            (reverse rows)
            (if (= (length cols) width)
                (loop loop_list (cons (reverse cols) rows) '())
                (loop (cdr (cdr (cdr (cdr loop_list)))) 
                      rows
                      (cons (+ (list-ref loop_list 1) (list-ref loop_list 2) (list-ref loop_list 3)) cols))))))))

(define (find-threshold points_list)
  (let ([max_value 0]
        [min_value 765])
    (let row-loop ([loop_row_list points_list])
      (if (not (null? loop_row_list))
          (begin
            (let col-loop ([loop_col_list (car loop_row_list)])
              (when (not (null? loop_col_list))
                    (cond
                     [(> (car loop_col_list) max_value)
                      (set! max_value (car loop_col_list))]
                     [(< (car loop_col_list) min_value)
                      (set! min_value (car loop_col_list))]
                     )
                    (col-loop (cdr loop_col_list))))
            (row-loop (cdr loop_row_list)))
          (floor (/ (- max_value min_value) 2))))))

(define (points->bw points_list threshold)
  (map
   (lambda (row)
     (map
      (lambda (col)
        (if (> col threshold) 0 1))
      row))
   points_list))

(define (print-points points_list)
  (let row-loop ([loop_row_list points_list])
    (when (not (null? loop_row_list))
        (let col-loop ([loop_col_list (car loop_row_list)])
          (if (not (null? loop_col_list))
              (begin
                (printf "~a" (car loop_col_list))
                (col-loop (cdr loop_col_list)))
              (printf "\n")))
        (row-loop (cdr loop_row_list)))))

(define (points->pic points_list pic_path pixel_map)
  (let* ([width (length (car points_list))]
         [height (length points_list)]
         [points_pic (make-object bitmap% width height)])
    (send points_pic set-argb-pixels 0 0 width height 
          (let loop ([rows points_list]
                     [row_index 0]
                     [bytes_list '()])
            (if (not (null? rows))
                (loop
                 (cdr rows)
                 (add1 row_index)
                 (cons
                  (let col-loop ([cols (car rows)]
                                 [col_index 0]
                                 [col_bytes_list '()])
                    (if (not (null? cols))
                        (if (hash-has-key? pixel_map (cons row_index col_index))
                            (col-loop (cdr cols) (add1 col_index) `(,@(hash-ref pixel_map (cons row_index col_index)) ,@col_bytes_list))
                            (if (= (car cols) 0)
                                (col-loop (cdr cols) (add1 col_index) (cons 255 (cons 255 (cons 255 (cons 255 col_bytes_list)))))
                                (col-loop (cdr cols) (add1 col_index) (cons 0 (cons 0 (cons 0 (cons 255 col_bytes_list)))))))
                        (reverse col_bytes_list)))
                  bytes_list))
                (list->bytes (foldr (lambda (a b) (append a b)) '() (reverse bytes_list))))))
    (send points_pic save-file pic_path 'png)))

(define (guess-first-dark-width points)
  (let loop ([points_loop points]
             [dark_length 0])
    (if (not (null? points_loop))
        (if (= (car points_loop) 0)
            (if (> dark_length 0)
                dark_length
                (loop (cdr points_loop) dark_length))
            (loop (cdr points_loop) (add1 dark_length)))
        dark_length)))

(define (squash-points points width)
  (let ([min_width (ceiling (* width 0.5))])
    (let loop ([loop_points points]
               [last_value -1]
               [same_count 0]
               [result_list '()])

      (if (not (null? loop_points))
          (if (= same_count width)
              (loop (cdr loop_points) (car loop_points) 1 (cons last_value result_list))
              (if (= (car loop_points) last_value)
                  (loop (cdr loop_points) last_value (add1 same_count) result_list)
                  (if (= last_value -1)
                      (loop (cdr loop_points) (car loop_points) (add1 same_count) result_list)
                      (if (>= same_count min_width)
                          (loop (cdr loop_points) (car loop_points) 1 (cons last_value result_list))
                          (loop (cdr loop_points) (car loop_points) 1 result_list)))))
          (if (and (> same_count 0) (>= same_count min_width))
              (reverse (cons last_value result_list))
              (reverse result_list))))))

(define (guess-module-width guess_module_width points_row)
  (let ([max_module_width (floor (/ (length points_row) 14))])
    (let loop ([points points_row])
      (if (not (null? points))
          (if (= (car points) 1)
              (begin
                (when (not guess_module_width)
                      (set! guess_module_width (guess-first-dark-width points)))

                (let* ([squashed_line (squash-points points_row guess_module_width)]
                       [squashed_str 
                        (foldr (lambda (a b) (string-append a b)) "" (map (lambda (b) (number->string b)) squashed_line))])

                  (if (regexp-match #rx"010111010" squashed_str)
                      (begin
                        (trace (format "~a" squashed_str) 1)
                        (cons
                         guess_module_width
                         (map
                          (lambda (item)
                            (* guess_module_width (add1 (car item))))
                          (regexp-match-positions* #rx"010111010" squashed_str))))
                      (if (> (length points) guess_module_width)
                          (loop (list-tail points guess_module_width))
                          #f))))
              (loop (cdr points)))
          #f))))

(define (guess-matrix matrix)
  (let loop ([rows matrix]
             [row_index 0]
             [result_list '()]
             [guess_module_width #f])
    (if (not (null? rows))
        (let ([guess_result (guess-module-width guess_module_width (car rows))])
          (when guess_result 
                (trace (format "row:~a" row_index) 1)
                (trace (format "guess_result:~a" guess_result) 1))

          (if guess_result
              (loop 
               (cdr rows) 
               (add1 row_index) 
               (cons `(,row_index ,@(cdr guess_result)) result_list)
               (car guess_result))
              (loop 
               (cdr rows) 
               (add1 row_index) 
               result_list
               guess_module_width)))
        (cons guess_module_width (reverse result_list)))))

(define (align-matrix matrix fill)
  (let ([max_length 
         (apply max (map (lambda (row) (length row)) matrix))])
    (map
     (lambda (row)
       (if (< (length row) max_length)
           (append row (build-list (- max_length (length row)) (lambda (x) fill)))
           row))
     matrix)))

(define (trim-blank-lines matrix)
  (let loop ([loop_list matrix]
             [start #t]
             [result_list '()])
    (if (not (null? loop_list))
        (if start
            (if (andmap
                 (lambda (item)
                   (= item 0))
                 (car loop_list))
                (loop (cdr loop_list) #t result_list)
                (loop (cdr loop_list) #f (cons (car loop_list) result_list)))
            (loop (cdr loop_list) #f (cons (car loop_list) result_list)))
        (reverse result_list))))

(define (trim-matrix matrix)
  ;; trim four direction
  (matrix-row->col
   (trim-blank-lines
    (matrix-row->col
     (trim-blank-lines
      (matrix-row->col
       (trim-blank-lines
        (matrix-row->col 
         (trim-blank-lines 
          matrix)))))))))

(define (trim-tail matrix)
  (let loop ([loop_list matrix]
             [result_list '()]
             [pattern_count 0])
    (if (not (null? loop_list))
        (if (= pattern_count 6)
            (reverse (cons (cadr loop_list) (cons (car loop_list) result_list)))
            (if (equal? (take (car loop_list) 7) '(1 0 1 1 1 0 1))
                (loop (cdr loop_list) (cons (car loop_list) result_list) (add1 pattern_count))
                (loop (cdr loop_list) (cons (car loop_list) result_list) pattern_count)))
        (reverse result_list))))

(define (squash-matrix matrix module_width)
  (let ([squash_matrix_x
         (map
          (lambda (row)
            (squash-points row module_width))
          matrix)])
    
;    (print-matrix matrix)

;    (print-matrix squash_matrix_x)

    (let ([rotate_matrix (matrix-col->row (align-matrix squash_matrix_x 0))]
          [squash_matrix_y #f])

;      (print-matrix rotate_matrix)

      (set! squash_matrix_y
            (map
             (lambda (row)
               (squash-points row module_width))
             rotate_matrix))

;      (print-matrix squash_matrix_y)

      (matrix-row->col (align-matrix squash_matrix_y 0)))))

(define (find-pattern-center guess_results)
  (let ([group_map (make-hash)])
    (let loop ([guesses guess_results]
               [group_start_x -1]
               [group_end_x -1])
      (when (not (null? guesses))
            (let ([guess_result (car guesses)])
              (let ([point_x (first guess_result)]
                    [point_y_list (cdr guess_result)])

                (if (not (= point_x group_end_x))
                    (begin
                      (set! group_start_x point_x)
                      (set! group_end_x (add1 point_x)))
                    (set! group_end_x (add1 group_end_x)))
                
                (for-each
                 (lambda (point_y)
                   (let ([start_point (cons group_start_x point_y)])
                     (if (hash-has-key? group_map start_point)
                         (hash-set! group_map start_point `(,@(hash-ref group_map start_point) ,(cons point_x point_y)))
                         (hash-set! group_map start_point `(,(cons point_x point_y))))))
                 point_y_list)))
            (loop (cdr guesses) group_start_x group_end_x)))
    group_map))

(define (check-center-points-valid points_distance_map)
  (if (not (= (hash-count points_distance_map) 6))
      #f
      (if (not 
           (andmap
            (lambda (item)
              (let* ([point_pair (car item)]
                     [distance (cdr item)]
                     [point_a (car point_pair)]
                     [point_b (cdr point_pair)]
                     [opposite_pair (cons point_b point_a)])
                (and
                 (hash-has-key? points_distance_map opposite_pair)
                 (= distance (hash-ref points_distance_map opposite_pair)))))
            (hash->list points_distance_map)))
          #f
          #t)))

(define (get-center-points points_distance_map)
  (let ([points_hash (make-hash)]
        [max_distance 0]
        [point_a #f]
        [point_maybe_b #f]
        [point_maybe_c #f]
        [point_b #f]
        [point_c #f])

    (hash-for-each
     points_distance_map
     (lambda (points_pair distance)
       (when (> distance max_distance)
             (set! max_distance distance))
       (hash-set! points_hash (car points_pair) "")))

    (let ([center_points (hash-keys points_hash)])
      (if (= (hash-ref points_distance_map (cons (list-ref center_points 0) (list-ref center_points 1))) max_distance)
          (begin
            (set! point_a (str->point (list-ref center_points 2)))
            (set! point_maybe_b (str->point (list-ref center_points 0)))
            (set! point_maybe_c (str->point (list-ref center_points 1))))
          (if (= (hash-ref points_distance_map (cons (list-ref center_points 0) (list-ref center_points 2))) max_distance)
              (begin
                (set! point_a (str->point (list-ref center_points 1)))
                (set! point_maybe_b (str->point (list-ref center_points 0)))
                (set! point_maybe_c (str->point (list-ref center_points 2))))
              (begin
                (set! point_a (str->point (list-ref center_points 0)))
                (set! point_maybe_b (str->point (list-ref center_points 1)))
                (set! point_maybe_c (str->point (list-ref center_points 2)))))))

    (let ([point_a_x (car point_a)]
          [point_a_y (cdr point_a)])
      (cond
       [(and
         (>= (cdr point_maybe_b) point_a_y)
         (>= (cdr point_maybe_c) point_a_y))
        (if (< (car point_maybe_b) (car point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]
       [(and
         (<= (cdr point_maybe_b) point_a_y)
         (<= (cdr point_maybe_c) point_a_y))
        (if (> (car point_maybe_b) (car point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]
       [(and
         (>= (car point_maybe_b) point_a_x)
         (>= (car point_maybe_c) point_a_x))
        (if (> (cdr point_maybe_b) (cdr point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]
       [(and
         (<= (car point_maybe_b) point_a_x)
         (<= (car point_maybe_c) point_a_x))
        (if (< (cdr point_maybe_b) (cdr point_maybe_c))
            (begin
              (set! point_b point_maybe_b)
              (set! point_c point_maybe_c))
            (begin
              (set! point_b point_maybe_c)
              (set! point_c point_maybe_b)))]))

      (list point_a point_b point_c)))

(define (point-distance point_x point_y)
  (inexact->exact
   (floor
    (sqrt (+ 
           (expt (- (car point_x) (car point_y)) 2)
           (expt (- (cdr point_x) (cdr point_y)) 2))))))

(define (point->str point)
  (string-append (number->string (car point)) "-" (number->string (cdr point))))

(define (str->point str)
  (let ([items (regexp-split #rx"-" str)])
    (cons (string->number (first items)) (string->number (second items)))))

(define (find-pattern-center-points matrix)
  (let* ([guess_results (guess-matrix matrix)]
         [module_width (car guess_results)]
         [group_map (find-pattern-center (cdr guess_results))]
         [group_list #f]
         [all_center_points #f]
         [points_distance_map (make-hash)]
         [center_points #f]
         )
    
    (if (< (hash-count group_map) 3)
        #f
        (begin
          (set! group_list (take (sort (hash-values group_map) (lambda (c d) (> (length c) (length d)))) 3))

          (set! all_center_points
                (map
                 (lambda (group_list)
                   (let* ([center_point (list-ref group_list (floor (/ (length group_list) 2)))]
                          [point_x (car center_point)]
                          [point_y (+ (cdr center_point) (* 3 module_width) (floor (/ module_width 2)))])
                     (cons point_x point_y)))
                 group_list))

          (trace (format "step4 guess_results:~a" guess_results) 1)
          (trace (format "step4 group_map:~a" group_map) 1)
          (trace (format "step4 group_map first 3 group:~a" group_list) 1)
          (trace (format "step4 all_center_points:~a" all_center_points) 1)
    
          (let outer-loop ([points all_center_points])
            (when (not (null? points))
                  (let inner-loop ([inner_points all_center_points])
                    (when (not (null? inner_points))
                          (when (not (equal? (car points) (car inner_points)))
                                (hash-set! points_distance_map 
                                           (cons (point->str (car points)) (point->str (car inner_points)))
                                           (point-distance (car points) (car inner_points))))
                          (inner-loop (cdr inner_points))))
                  (outer-loop (cdr points))))

          (trace (format "step4 points_distance_map:~a" points_distance_map) 1)

          (if (check-center-points-valid points_distance_map)
              (cons module_width (get-center-points points_distance_map))
              #f)))))

(define (calculate-rotate-ratio point_a point_b radius)
  (let ([point_a_x (car point_a)]
        [point_a_y (cdr point_a)]
        [point_b_x (car point_b)]
        [point_b_y (cdr point_b)]
        [matrix_count (* radius 2 4)]
        [move_count #f])
    
    (trace (format "calculate rotate ratio:~a,~a,~a" point_a point_b radius) 1)
    
    (cond
     [(and
       (= point_b_x point_a_x)
       (> point_b_y point_a_y))
      (set! move_count 0)]
     [(and
       (> point_b_x point_a_x)
       (>= point_b_y (+ point_a_y radius)))
      (set! move_count (- point_b_x point_a_x))]
     [(and
       (>= point_b_x (+ point_a_x radius))
       (>= point_b_y point_a_y))
      (set! move_count (+ radius (- radius (- point_b_y point_a_y))))]
     [(and
       (>= point_b_x (+ point_a_x radius))
       (< point_b_y point_a_y))
      (set! move_count (+ (- point_a_y point_b_y) (* radius 2)))]
     [(and
       (<= point_b_y (- point_a_y radius))
       (>= point_b_x point_a_x))
      (set! move_count (+ (- radius (- point_b_x point_a_x)) (* radius 3)))]
     [(and
       (<= point_b_y (- point_a_y radius))
       (< point_b_x point_a_x))
      (set! move_count (* -1 (+ (- radius (- point_a_x point_b_x)) (* radius 3))))]
     [(and
       (<= point_b_x (- point_a_x radius))
       (<= point_b_y point_a_y))
      (set! move_count (* -1 (+ (- point_a_y point_b_y) (* radius 2))))]
     [(and
       (<= point_b_x (- point_a_x radius))
       (> point_b_y point_a_y))
      (set! move_count (* -1 (+ (- radius (- point_b_y point_a_y)) radius)))]
     [(and
       (>= point_b_y (+ point_a_y radius))
       (< point_b_x point_a_x))
      (set! move_count (* -1 (- point_a_x point_b_x)))]
     )
    
    (/ move_count matrix_count)))

(define (rotate-and-cut-bmp bmp_file ratio point_a distance_ab module_width output_file)
  (let* ([origin_bmp (make-object bitmap% bmp_file 'png)]
         [dest_width (+ distance_ab (* 12 module_width))]
         [dest_bmp (make-object bitmap% dest_width dest_width)]
         [dc (send dest_bmp make-dc)])
    (send dc translate 0 0)
    (send dc rotate (* (* pi 2) ratio))
    (send dc draw-bitmap origin_bmp (+ (* -1 (cdr point_a)) (* 6 module_width)) (+ (* -1 (car point_a)) (* 6 module_width)))
    (send dest_bmp save-file output_file 'png)))

(define (transform-points-list points_list start_point_pair)
  (map
   (lambda (point)
     (cons (+ (car start_point_pair) (sub1 (car point))) (+ (cdr start_point_pair) (sub1 (cdr point)))))
   points_list))

(define (exclude-finder-pattern width exclude_points_map)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (first (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (second (get-finder-pattern)) start_point))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (third (get-finder-pattern)) start_point)))
   (list
    (cons 0 0)
    (cons 0 (- width 7))
    (cons (- width 7) 0))))

(define (exclude-separator width exclude_points_map)
  (for-each
   (lambda (start_point)
     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
     (transform-points-list (first (get-separator)) '(0 . 0)))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (second (get-separator)) (cons (- width 8) 0)))

     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (transform-points-list (third (get-separator)) (cons 0 (- width 8)))))
   (list
    (cons 0 0)
    (cons 0 (- width 7))
    (cons (- width 7) 0))))

(define (exclude-timing-pattern width exclude_points_map timing_points_map)
  (for-each
   (lambda (timing_points)
     (for-each
      (lambda (point_pair)
        (hash-set! timing_points_map (cons (sub1 (car point_pair)) (sub1 (cdr point_pair))) "timing")
        (hash-set! exclude_points_map (cons (sub1 (car point_pair)) (sub1 (cdr point_pair))) '(0 0 255 255)))
      timing_points))
   (get-timing-pattern-points width)))

(define (exclude-format-information width exclude_points_map)
  (for-each
   (lambda (point_pair)
     (hash-set! exclude_points_map point_pair '(0 0 255 255)))
   (transform-points-list (first (get-format-information)) '(0 . 0)))

  (for-each
   (lambda (point_pair)
     (hash-set! exclude_points_map point_pair '(0 0 255 255)))
   (transform-points-list (second (get-format-information)) (cons 0 (- width 8))))

  (for-each
   (lambda (point_pair)
     (hash-set! exclude_points_map point_pair '(0 0 255 255)))
   (transform-points-list (third (get-format-information)) (cons (- width 8) 0))))

(define (exclude-alignment-pattern version exclude_points_map timing_points_map)
  (for-each
   (lambda (center_point)
     (for-each
      (lambda (point_pair)
        (hash-set! exclude_points_map point_pair '(0 0 255 255)))
      (foldr (lambda (a b) (quasiquote ((unquote-splicing a) (unquote-splicing b)))) '() 
             (fill-alignment-pattern-points (cons (sub1 (car center_point)) (sub1 (cdr center_point)))))))
   (get-alignment-pattern-center-points version exclude_points_map timing_points_map)))

(define (exclude-version version width exclude_points_map)
  (when (>= version 7)
        (for-each
         (lambda (point_pair)
           (hash-set! exclude_points_map point_pair '(0 0 255 255)))
         (transform-points-list (first (get-version-points)) (cons 0 (- width 11))))

          (for-each
           (lambda (point_pair)
             (hash-set! exclude_points_map point_pair '(0 0 255 255)))
           (transform-points-list (second (get-version-points)) (cons (- width 11) 0)))))

(define (exclude-dark-module version exclude_points_map)
  (let ([dark_point (get-dark-point version)])
    (hash-set! exclude_points_map (cons (sub1 (car dark_point)) (sub1 (cdr dark_point))) '(0 0 255 255))))

(define (qr-read pic_path)
  (let* ([step1_points_list #f]
         [original_height #f]
         [original_width #f]
         [step2_threshold #f]
         [step3_bw_points #f]
         [step4_pattern_center_points #f]
         [step5_rotate_ratio #f]
         [step6_rotated_points #f]
         [step7_trimed_points #f]
         [step8_squashed_points #f]
         [step9_end_points #f]
         )

    (set! step1_points_list (pic->points pic_path))
    (set! original_width (length step1_points_list))
    (set! original_height (length (car step1_points_list)))
    (trace (format "step1:convert pic file to pixel points[~aX~a]" original_width original_height) 1)
    
    (set! step2_threshold (find-threshold step1_points_list))
    (trace (format "step2:find threshold is ~a" step2_threshold) 1)

    (set! step3_bw_points (points->bw step1_points_list step2_threshold))
    (trace (format "step3:use threshold convert pixel to points 0 or 1") 1)
    (points->pic step3_bw_points "step3_bw.png" (make-hash))

    (set! step4_pattern_center_points (find-pattern-center-points step3_bw_points))
    (trace (format "step4 pattern center points:~a" step4_pattern_center_points) 1)
    (when step4_pattern_center_points
          (let ([pixel_map (make-hash)]
                [module_width (car step4_pattern_center_points)]
                [center_points (cdr step4_pattern_center_points)])
            (hash-set! pixel_map (first center_points) '(0 0 255 255))
            (hash-set! pixel_map (second center_points) '(0 255 0 255))
            (hash-set! pixel_map (third center_points) '(255 0 0 255))

            (points->pic step3_bw_points "step4_pattern_center.png" pixel_map)
    
            (set! step5_rotate_ratio (calculate-rotate-ratio 
                                      (first center_points) 
                                      (second center_points) 
                                      (point-distance (first center_points) (second center_points))))
            (trace (format "step5 rotate ratio:~a" step5_rotate_ratio) 1)
            
            (if (= step5_rotate_ratio 0)
                (set! step6_rotated_points step3_bw_points)
                (begin
                  (rotate-and-cut-bmp "step3_bw.png" 
                                      step5_rotate_ratio 
                                      (first center_points) 
                                      (point-distance (first center_points) (second center_points)) 
                                      module_width
                                      "step6_rotated.png")
                  (set! step6_rotated_points (points->bw (pic->points "step6_rotated.png") step2_threshold))))
            
            (set! step7_trimed_points (trim-matrix step6_rotated_points))
            (points->pic step7_trimed_points "step7_trimed.png" (make-hash))
            
            (set! step8_squashed_points (squash-matrix step7_trimed_points module_width))
            (points->pic step8_squashed_points "step8_squashed.png" (make-hash))
            (print-matrix step8_squashed_points)

            (set! step9_end_points (trim-matrix (trim-tail step8_squashed_points)))
            (points->pic step9_end_points "step9_end.png" (make-hash))
            (print-matrix step9_end_points)
            
            (let* ([init_matrix step9_end_points]
                   [width (length (car init_matrix))]
                   [version #f]
                   [format_information #f]
                   [error_level #f]
                   [exclude_points_map (make-hash)]
                   [timing_points_map (make-hash)])

                   (set! version (add1 (/ (- (length (car init_matrix)) 21) 4)))

                   (printf "width:~a, version:~a\n" width version)

                   (exclude-finder-pattern width exclude_points_map)
                   (points->pic init_matrix "step91_exclude_finder_pattern.png" exclude_points_map)
                   (exclude-separator width exclude_points_map)
                   (points->pic init_matrix "step92_exclude_separator.png" exclude_points_map)
                   (exclude-timing-pattern width exclude_points_map timing_points_map)
                   (points->pic init_matrix "step93_exclude_timing_pattern.png" exclude_points_map)
                   (exclude-alignment-pattern version exclude_points_map timing_points_map)
                   (points->pic init_matrix "step94_exclude_alignment_pattern.png" exclude_points_map)
                   (exclude-format-information width exclude_points_map)
                   (points->pic init_matrix "step95_exclude_format_information.png" exclude_points_map)
                   (exclude-version version width exclude_points_map)
                   (points->pic init_matrix "step96_exclude_version.png" exclude_points_map)
                   (exclude-dark-module version exclude_points_map)
                   (points->pic init_matrix "step97_exclude_dark_module.png" exclude_points_map)

                   (let ([trace_list (get-data-socket-list modules #:skip_points_hash exclude_points_map)])

                   (if (or (not (exact-nonnegative-integer? version)) (> version 40))
                       ""
                       (void))
                   )
            ))
    )
  "")