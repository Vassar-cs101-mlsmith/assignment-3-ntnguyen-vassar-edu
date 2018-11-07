;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname assign3) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; CMPU-101 
; Fall 2018
; Assign 3
; <Nhan Nguyen> 
;
; Description: Uses a list of bouncing balls to animate many balls
; of different sizes and colors, all moving in the same scene at 
; different speeds.

(require 2htdp/image) 
(require 2htdp/universe)

(define RADIUS 25)

; Scene dimensions
(define WIDTH 500)
(define HEIGHT 300)

; Create the background scene image
(define BACKGROUND
  (place-image (rectangle WIDTH HEIGHT "solid" "lightgray")
               (/ WIDTH 2) (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))

; Data Definitions 
(define-struct ball (im x y dx dy)) 
; A ball is a (make-ball im p dx dy) where
; im is an image (of the ball), 
; x and y are numbers representing the ball's position, and
; dx and dy are numbers representing the ball's horizontal and 
;   vertical velocity

; Data Definition for a list-of-balls:
; A list-of-balls is either:
; 1. '(), or
; 2. (cons b lob), where b is a ball
;    and lob is a list-of-balls

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define four (4) example ball CONSTANTS:
;   one touching each edge of the scene (top, bottom, left, right)
;   These will help you test bounce conditions.

; here's one of my ball CONSTANTS, which you may use or modify
; if you like to define the rest.
(define BALL-AT-LEFT 
  (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             (+ RADIUS 100) (/ HEIGHT 2) -4 4)) 

(define BALL-AT-RIGHT  
  (make-ball (circle (+ RADIUS 8) "solid" "orange")
             (- WIDTH 50 ) (/ HEIGHT 4) -6 6))

 
(define BALL-ON-TOP  
  (make-ball (circle (+ RADIUS 6) "solid" "purple")
             (/ WIDTH 2) (- HEIGHT 30) -5 5))


(define BALL-AT-BOTTOM
  (make-ball (circle (+ RADIUS 2) "solid" "yellow")
             (/ WIDTH 2) 250 -3 3))


; Define INIT-LOB to be a list-of-balls:
; You will use this to be the initial state of the world.
; I've defined it to be the empty list, but you should define it
; to contain the four example ball CONSTANTS you just defined. 
(define INIT-LOB
  (list BALL-AT-LEFT BALL-AT-RIGHT BALL-ON-TOP BALL-AT-BOTTOM))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Templates for a ball and a list-of-balls.
; Use these to help you get started with the functions below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> ???
; Template for a function that consumes a ball
(define (fun-for-ball b) 
  (...(ball-im b)...
   ...(ball-x b)...(ball-y b)...
   ...(ball-dx b)...(ball-dy b)...))

; list-of-balls -> ???
; Template for a function that consumes a list-of-balls
(define (fun-for-list-of-balls lob) 
  (cond
    [(empty? lob)...] 
    [else (...(fun-for-ball (first lob))...
           ...(fun-for-lob (rest lob))...)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Design the functions below, in order. I've supplied the
; signature, purpose statement, and header for each function.
; 
; You provide the check-expect examples, and using the appropriate
; template, complete the function bodies.
;
; I recommend you proceed in order, and complete each function,
; with passing tests, before going on to the next.
;
; The reason for completing the functions in the order they appear
; is earlier functions can be used as helper functions for the
; later functions.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ball -> number
; computes the radius of given ball
(define (ball-radius b)
  (/ (image-width (ball-im b)) 2))

(check-expect (ball-radius BALL-AT-LEFT) 29)  
(check-expect (ball-radius BALL-AT-RIGHT) 33)
(check-expect (ball-radius BALL-ON-TOP) 31)
(check-expect (ball-radius BALL-AT-BOTTOM) 27)

; ball -> boolean
; determines whether the ball reached the top edge of scene
(define (top-edge? b)
  (<= (ball-y b) (ball-radius b)))

(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             (+ RADIUS 4) (/ HEIGHT 2) -4 4)) #false)
(check-expect (top-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             (+ RADIUS 4) 10 -4 4)) #true)

; ball -> boolean
; determines whether the ball reached the bottom edge of scene
(define (bottom-edge? b)
  (>= (ball-y b) (- HEIGHT (ball-radius b)))) 
 
(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             (+ RADIUS 4) 280 -4 4)) #true)
(check-expect (bottom-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             (+ RADIUS 4) 10 -4 4)) #false)

; ball -> boolean
; determines whether the ball reached the left edge of scene
(define (left-edge? b)
  (<= (ball-x b) (ball-radius b)))

(check-expect (left-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             30 280 -4 4)) #false)
(check-expect (left-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             10 288 -8 8)) #true)

; ball -> boolean
; determines whether the ball reached the right edge of scene
(define (right-edge? b) 
  (>= (ball-x b) (- WIDTH (ball-radius b))))

(check-expect (right-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 -4 4)) #true)
(check-expect (right-edge? (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             50 288 -8 8)) #false)

; ball -> ball
; reverse ball's up-down direction   
(define (reverse-up-down b)
  (make-ball (ball-im b) (ball-x b) (ball-y b) (ball-dx b) (-(ball-dy b))))

(check-expect (reverse-up-down (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 4 4)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 4 -4)) 
(check-expect (reverse-up-down (make-ball (circle (+ RADIUS 4) "solid" "cyan")
            480 500 -25 25)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 500 -25 -25))

; ball -> ball
; reverse ball's left-right direction   
(define (reverse-left-right b) 
  (make-ball (ball-im b) (ball-x b) (ball-y b) (-(ball-dx b)) (ball-dy b)))

(check-expect (reverse-left-right (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 -4 4)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 4 4))  
(check-expect (reverse-left-right (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             60 280 -50 50)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             60 280 50 50))

; ball -> ball 
; changes direction of given ball if it hit the top or bottom edge
(define (bounce-up-down b)
  (cond
    [(or (top-edge? b) (bottom-edge? b)) (reverse-up-down b)]
    [else b]))
  
(check-expect (bounce-up-down (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 -4 4)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 -4 -4))
(check-expect (bounce-up-down (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 100 48 4)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 100 48 4))
  
; ball -> ball  
; changes direction of given ball if it hit the left or right edge
(define (bounce-left-right b) 
  (cond
    [(or (left-edge? b) (right-edge? b)) (reverse-left-right b)]
    [else b]))

(check-expect (bounce-left-right (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 50 4)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             480 280 -50 4))
(check-expect (bounce-left-right (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             200 280 9 4)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             200 280 9 4))
   
; ball -> ball 
; moves the given ball by its dx and dy amounts
(define (move-ball b)
  (make-ball (ball-im b) (+ (ball-x b) (ball-dx b)) (+ (ball-y b) (ball-dy b))
             (ball-dx b) (ball-dy b))) 

(check-expect (move-ball (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             400 280 50 4)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             450 284 50 4))
   
(check-expect (move-ball (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             100 350 68 8)) (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             168 358 68 8))

; list-of-balls -> list-of-balls
; moves (and possibly bounces) each ball in given list
(define (move-list-of-balls lob)
  (cond  
    [(empty? lob) lob]
    [(cons? lob)
     (cons (move-ball (bounce-up-down (bounce-left-right (first lob)))) (move-list-of-balls (rest lob)))])) 
 
 
; ball image -> image 
; renders given ball b on given background bg
(define (render-ball b bg)
  (place-image (ball-im b) (ball-x b) (ball-y b) bg))

(check-expect (render-ball (make-ball (circle (+ RADIUS 4) "solid" "cyan")
             400 280 50 4) BACKGROUND) (place-image (circle (+ RADIUS 4) "solid" "cyan") 400 280 BACKGROUND))

(check-expect (render-ball (make-ball (circle (+ RADIUS 90) "outline" "red")
             60 280 -50 50) BACKGROUND) (place-image (circle (+ RADIUS 90) "outline" "red") 60 280 BACKGROUND))

; list-of-balls -> image 
; produces image of each ball at each given current position on
; background. 
; (Yes, I provided this function for you! You shouldn't have to
;  touch it if you've correctly implemented the functions above.)
(define (render-balls lob)  
  (cond [(empty? lob) BACKGROUND]
        [else (render-ball (first lob)
                           (render-balls (rest lob)))])) 

; Here's the main function with the big-bang expression!
; Once you've implemented move-list-of-balls, uncomment on-tick below.
(define (main w) 
  (big-bang w
            (on-tick move-list-of-balls 1/28) 
            (to-draw render-balls))) 

; Run program automatically, or type this in Interactions Pane:
; Use INIT-LOB as the initial state of the world...
(main INIT-LOB)