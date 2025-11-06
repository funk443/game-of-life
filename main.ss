;; Copyright © 2025 CToID <funk443@icloud.com>
;;
;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See the COPYING file
;; for more details.

(import (scheme))

;; Raylib FFI

(load-shared-object "./raylib-5.5_win64_msvc16/lib/raylib.dll")

(define-ftype color
  (struct
    (r unsigned-8)
    (g unsigned-8)
    (b unsigned-8)
    (a unsigned-8)))

(define-ftype vector2
  (struct
    (x float)
    (y float)))

(define-syntax the
  (syntax-rules (pointer of type freed get set! to)
    ((_ pointer of type ftype-name)
     (make-ftype-pointer ftype-name (foreign-alloc (ftype-sizeof ftype-name))))
    ((_ ptr freed)
     (foreign-free (ftype-pointer-address ptr)))
    ((_ ptr of type ftype-name get slot)
     (ftype-ref ftype-name (slot) ptr))
    ((_ ptr of type ftype-name set! slot to val)
     (ftype-set! ftype-name (slot) ptr val))))

(define init-window
  (foreign-procedure "InitWindow" (int int string) void))

(define close-window
  (foreign-procedure "CloseWindow" () void))

(define window-should-close?
  (foreign-procedure "WindowShouldClose" () boolean))

(define set-target-fps
  (foreign-procedure "SetTargetFPS" (int) void))

(define begin-drawing
  (foreign-procedure "BeginDrawing" () void))

(define end-drawing
  (foreign-procedure "EndDrawing" () void))

(define (get-color hex)
  (define return-value (the pointer of type color))
  ((foreign-procedure "GetColor" (unsigned-int) (& color)) return-value hex)
  return-value)

(define clear-background
  (foreign-procedure "ClearBackground" ((& color)) void))

(define draw-rectangle-v
  (foreign-procedure "DrawRectangleV" ((& vector2) (& vector2) (& color)) void))

(define draw-line-v
  (foreign-procedure "DrawLineV" ((& vector2) (& vector2) (& color)) void))

(define mouse-button-left 0)

(define mouse-button-pressed?
  (foreign-procedure "IsMouseButtonPressed" (int) boolean))

(define get-mouse-position
  (foreign-procedure "GetMousePosition" () (& vector2)))

(define key-space 32)

(define key-pressed?
  (foreign-procedure "IsKeyPressed" (int) boolean))

(define draw-text
  (foreign-procedure "DrawText" (string int int int (& color)) void))

;; Helpers

(define make-array
  (case-lambda
    ((width height)
     (make-array width height #f))
    ((width height obj)
     (list (cons 'width width)
           (cons 'height height)
           (cons 'content (make-vector (* width height) obj))))))

(define (array-ref array x y)
  (define width (cdr (assq 'width array)))
  (define height (cdr (assq 'height array)))
  (define content (cdr (assq 'content array)))
  (unless (< x width)
    (assertion-violation #f "Index x out of range." x width))
  (unless (< y height)
    (assertion-violation #f "Index y out of range." y height))
  (vector-ref content (+ x (* y width))))

(define (array-set! array x y val)
  (define width (cdr (assq 'width array)))
  (define height (cdr (assq 'height array)))
  (define content (cdr (assq 'content array)))
  (unless (< x width)
    (assertion-violation #f "Index x out of range." x width))
  (unless (< y height)
    (assertion-violation #f "Index y out of range." y height))
  (vector-set! content (+ x (* y width)) val))

(define (cell-live? ground x y)
  (define width (cdr (assq 'width ground)))
  (define height (cdr (assq 'height ground)))
  (define content (cdr (assq 'content ground)))
  (define self (array-ref ground x y))
  (define neighbor-count
    (let loop ((nx (sub1 x))
               (ny (sub1 y))
               (count 0))
      (cond
        ((> ny (add1 y))
         count)
        ((and (= nx x) (= ny y))
         (loop (add1 nx) ny count))
        (else
         (loop (if (>= nx (add1 x)) (sub1 x) (add1 nx))
               (if (>= nx (add1 x)) (add1 ny) ny)
               (if (and (<= 0 nx (sub1 width))
                        (<= 0 ny (sub1 height))
                        (array-ref ground nx ny))
                 (add1 count)
                 count))))))
  (if self
    (if (or (< neighbor-count 2) (> neighbor-count 3)) #f #t)
    (if (= neighbor-count 3) #t #f)))

(define (tick ground)
  (define width (cdr (assq 'width ground)))
  (define height (cdr (assq 'height ground)))
  (let loop ((x 0)
             (y 0)
             (changes '()))
    (cond
      ((>= y height)
       (reverse changes))
      ((>= x width)
       (loop 0 (add1 y) changes))
      ((eq? (array-ref ground x y) (cell-live? ground x y))
       (loop (add1 x) y changes))
      (else
       (loop (add1 x) y (cons (list x y) changes))))))

;; Main codes

(define ground-width 100)
(define ground-height 100)
(define cell-size 10)
(define ground (make-array ground-width ground-height))
(define paused? (make-parameter #t))

(init-window (* ground-width cell-size) (* ground-height cell-size) "Game of Life")
(set-target-fps 30)
(set! cell-size (inexact cell-size))

(define background-color (get-color #x000000ff))

(define grid-line-color (get-color #x202020ff))
(define grid-line-start (the pointer of type vector2))
(define grid-line-end (the pointer of type vector2))
(the grid-line-start of type vector2 set! x to #i0)
(the grid-line-start of type vector2 set! y to #i0)
(the grid-line-end of type vector2 set! x to #i0)
(the grid-line-end of type vector2 set! y to #i0)

(define box-color (get-color #xff0000ff))
(define box-location (the pointer of type vector2))
(define box-size (the pointer of type vector2))
(the box-location of type vector2 set! x to #i0)
(the box-location of type vector2 set! y to #i0)
(the box-size of type vector2 set! x to cell-size)
(the box-size of type vector2 set! y to cell-size)

(define mouse-position (the pointer of type vector2))
(the mouse-position of type vector2 set! x to #i0)
(the mouse-position of type vector2 set! y to #i0)

(define text-color (get-color #x00ff00ff))

(let loop ()
  (when (not (window-should-close?))
    (when (mouse-button-pressed? mouse-button-left)
      (get-mouse-position mouse-position)
      (let ((x (exact (floor (/ (the mouse-position of type vector2 get x)
                                cell-size))))
            (y (exact (floor (/ (the mouse-position of type vector2 get y)
                                cell-size)))))
        (when (and (<= 0 x ground-width) (<= 0 y ground-height))
          (array-set! ground x y (not (array-ref ground x y))))))
    (when (key-pressed? key-space)
      (paused? (not (paused?))))
    (unless (paused?)
      (for-each (lambda (change-position)
                  (define x (car change-position))
                  (define y (cadr change-position))
                  (array-set! ground x y (not (array-ref ground x y))))
                (tick ground)))
    (begin-drawing)
    (clear-background background-color)
    (let draw-grid-lines ((x 1)
                          (y 1))
      (cond
        ((>= y ground-height)
         #f)
        ((>= x ground-width)
         (the grid-line-start of type vector2 set! x to #i0)
         (the grid-line-start of type vector2 set! y to (* y cell-size))
         (the grid-line-end of type vector2 set! x to (* ground-width cell-size))
         (the grid-line-end of type vector2 set! y to (* y cell-size))
         (draw-line-v grid-line-start grid-line-end grid-line-color)
         (draw-grid-lines x (add1 y)))
        (else
         (the grid-line-start of type vector2 set! x to (* x cell-size))
         (the grid-line-start of type vector2 set! y to #i0)
         (the grid-line-end of type vector2 set! x to (* x cell-size))
         (the grid-line-end of type vector2 set! y to (* ground-height cell-size))
         (draw-line-v grid-line-start grid-line-end grid-line-color)
         (draw-grid-lines (add1 x) y))))
    (let draw-live-cells ((x 0)
                          (y 0))
      (cond
        ((>= y ground-height)
         #f)
        ((>= x ground-width)
         (draw-live-cells 0 (add1 y)))
        ((array-ref ground x y)
         (the box-location of type vector2 set! x to (inexact (* cell-size x)))
         (the box-location of type vector2 set! y to (inexact (* cell-size y)))
         (draw-rectangle-v box-location box-size box-color)
         (draw-live-cells (add1 x) y))
        (else
         (draw-live-cells (add1 x) y))))
    (when (paused?)
      (draw-text "PAUSED" 5 5 10 text-color))
    (end-drawing)
    (loop)))

(the background-color freed)
(the grid-line-color freed)
(the grid-line-start freed)
(the grid-line-end freed)
(the box-color freed)
(the box-location freed)
(the box-size freed)
(the mouse-position freed)
(the text-color freed)

(close-window)
