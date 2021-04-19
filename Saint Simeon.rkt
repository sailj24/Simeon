;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Saint Simeon|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

#| Saint Simeon's Style
Monk leaps from ledge to ledge to climb the tower. You win when Monk reach the TOP, where Saint Simeon is

|#

;Universals
(define START-TIME (current-seconds))
(define (TIME) (- (current-seconds) START-TIME))
(define HEIGHT 400)
(define WIDTH (/ HEIGHT 2.5))
(define RADIUS (/ HEIGHT 20))
(define BASESPACE (/ HEIGHT 20)) 
(define JUMP (/ HEIGHT 80))
(define HAPPYHEIGHT (* HEIGHT .75))
(define SPACESCALE (* 2.3 BASESPACE)) 
(define STARTHEIGHT 2000)
(define LOWEST (- HEIGHT RADIUS BASESPACE))
(define SIDEWAYS (/ HEIGHT 40))
(define MONK-IMG (add-curve (overlay (beside (overlay (circle (/ RADIUS 10) 'solid 'black) (circle (/ RADIUS 5) 'solid 'green))
                                             (circle (/ RADIUS 10) 'solid 'peru)
                                             (overlay (circle (/ RADIUS 10) 'solid 'black) (circle (/ RADIUS 5) 'solid 'green)))
                                     (circle RADIUS 'solid 'peru)) 8 28 -60 8/9 33 28 -90 0 'maroon))
(define SIMEON-IMG (add-curve (overlay (beside (overlay (circle (/ RADIUS 10) 'solid 'black) (circle (/ RADIUS 5) 'solid 'lightblue))
                                               (circle (/ RADIUS 10) 'solid 'peru)
                                               (overlay (circle (/ RADIUS 10) 'solid 'black) (circle (/ RADIUS 5) 'solid 'lightblue)))
                                       (circle RADIUS 'solid 'peru)) 8 28 -60 8/9 33 28 -90 0 'maroon))
(define BIRD-IMG (triangle (* RADIUS 1.5) 'solid 'red))
(define BOOK-IMG (rectangle 25 35 'solid 'brown))
(define NEST (ellipse (* 1.8 RADIUS) 14 'solid 'brown))
(define TOP (rectangle (* .65 HEIGHT) (/ HEIGHT 4) 'solid 'yellow))
(define BACKGROUND (square HEIGHT 'solid 'blue))
(define GROUND (rectangle HEIGHT 56 'solid 'green))


;ledge struct
;l is number, length, difficulty to leap to it
;n is boolean, presence of a nest or not (affects when birds at tack)
(define-struct Ledge (l x y n))

;Monk struct
;x and y are position, dx is 0, -1, or 1, dy is horizontal velocity, s is status (string)
(define-struct Monk (x y dx dy s))

;Obstacle Struct
;t is type string ("book" or "bird")
;p is posn, location
;s is status, string ("stationary" "flying-right" "flying-left" "falling")
(define-struct Obstacle (t p s))

;Worldstate Struct
;loo is list of Obs structs, which will be moving down or sideways at random or semi-random times
;lol is list of Ledge structs, whick will be moving down with each spacebar
;h is number of the height of the tower
;p is the posn of the center of the top
;m is a Monk struct
(define-struct WS (loo lol h p m g))


;->number, placement along width of tower
(define (rndx) (random (* HEIGHT .37) (* HEIGHT .6)))

;String->number
(define (rnds-checker word) (if (equal? "flying-right" word) 0 HEIGHT))

;->number, placement either on left or right of screen
(define (rndS) (list-ref (list "flying-right" "flying-left") (random 2)))


;->number, ledge length
(define (rndL) (list-ref (list (* 2 RADIUS) (* 2.5 RADIUS) (+ 10 (* 2 RADIUS))) (random 3)))

;->Boolean, semi-randomized
(define (rndN) (list-ref (list #f #t #f #f #f #t) (random 6)))


;list->list
;removes all ledges with y smaller than (- HEIGHT STARTHEIGHT)
(define (check-higher l) (filter (λ (ledge) (> (Ledge-y ledge) (- HEIGHT STARTHEIGHT))) l))

;number -> ledge
;algorithm for a ledge
(define (firstLedges n) (make-Ledge (rndL)
                                    (rndx)
                                    (- LOWEST (* (* BASESPACE n) (exp (/ n SPACESCALE))))
                                    (rndN)))

; ->List
(define (build-lol) (check-higher (build-list (round (* 1.5 STARTHEIGHT)) firstLedges)))
(list? (build-lol))
(empty? (build-lol))



;Obstacle, Img-> Img
(define (make-visible obs img) (cond [(equal? (Obstacle-t obs) "bird") (place-image BIRD-IMG  (posn-x (Obstacle-p obs)) (posn-y (Obstacle-p obs)) img)]
                                     [(equal? (Obstacle-t obs) "book") (place-image BOOK-IMG (posn-x (Obstacle-p obs)) (posn-y (Obstacle-p obs)) img)]
                                     [else img]))
;list, img->img
(define (place-helper ws img) (foldl make-visible img (WS-loo ws)))
;WS, IMG->img
(define (place-obstacles ws img) (if (empty? (WS-loo ws))
                                     img
                                     (place-helper ws img)))
(check-expect (place-obstacles (make-WS (list (make-Obstacle "bird" (make-posn 25 45) "flying-right"))
                                        '()
                                        500
                                        (make-posn 200 -100)
                                        (make-Monk 250 300 1 5 "standing")
                                        #f)
                               (square HEIGHT 'solid 'green))
              (place-image BIRD-IMG 25 45 (square HEIGHT 'solid 'green)))
(check-expect (place-obstacles (make-WS '()
                                        '()
                                        500
                                        (make-posn 200 -100)
                                        (make-Monk 250 300 1 5 "standing")
                                        #f)
                               (square HEIGHT 'solid 'green))
              (square HEIGHT 'solid 'green))
              
                                  
;Ledge, Image ->Image
(define (place-l ledge img) (place-image (if (false? (Ledge-n ledge))
                                             (line (Ledge-l ledge) 0 'black)
                                             (overlay NEST (line (Ledge-l ledge) 0 'black)))
                                         (Ledge-x ledge)
                                         (Ledge-y ledge)
                                         img))

(check-expect (place-l (make-Ledge 20 150 69 #f)
                       (square 200 'solid 'black)) (place-image (line 20 0 'black)
                                                                150
                                                                69
                                                                (square 200 'solid 'black)))
; List, Image-> Image
(define (place-ledges n l) (foldl place-l (overlay/align "center"
                                                         "bottom"
                                                         (rectangle WIDTH (if (< n HEIGHT) n HEIGHT) 'solid 'gray)
                                                         BACKGROUND)
                                  l))

;monk, img-> img
;places MONK on everything else
(define (place-monk ws img) (place-image MONK-IMG
                                         (Monk-x (WS-m ws))
                                         (Monk-y (WS-m ws))
                                         (if (false? (WS-g ws))
                                             img
                                             (overlay/align "center" "bottom" GROUND img))))

(check-expect (place-monk (make-WS (list empty) (list empty) 200 (make-posn 200 224) (make-Monk 200 135 0 0 "standing") #t)
                          (square 400 'solid 'green))
              (place-image MONK-IMG 200 135 (overlay/align "center" "bottom" GROUND (square 400 'solid 'green))))



;WS, IMG-> IMG
;places TOP on everything else
(define (make-top ws img) (place-image TOP
                                       (posn-x (WS-p ws))
                                       (posn-y (WS-p ws))
                                       img))

;render function
;WS->Image
(define (render ws)
  (place-obstacles ws (place-monk ws (make-top ws (place-ledges (WS-h ws) (WS-lol ws))))))


;list->list
;removes all ledges with y coordinates bigger than HEIGHT
(define (check-lower l) (filter (λ (ledge) (< (Ledge-y ledge) HEIGHT)) l))

;ledge->ledge
;moves one ledge down
(define (ledge-down ledge) (make-Ledge (Ledge-l ledge)
                                       (Ledge-x ledge)
                                       (+ (* 2 JUMP) (Ledge-y ledge))
                                       (Ledge-n ledge)))
(check-expect (ledge-down (make-Ledge 30 195 370 #f)) (make-Ledge 30 195 (+ (* 2 JUMP) 370) #f))







;helping functions for key-handler; change dx and dy and move everything down
;WS->WS
(define (move-up-monk ws) (make-WS (WS-loo ws)
                                   (check-lower (map ledge-down (WS-lol ws)))
                                   (- (WS-h ws) (* 2 JUMP))
                                   (make-posn 200 (+ (* 2 JUMP) (posn-y (WS-p ws))))
                                   (make-Monk (Monk-x (WS-m ws))
                                              (Monk-y (WS-m ws)) 
                                              (Monk-dx (WS-m ws))
                                              (- JUMP)
                                              "jumping")
                                   #f))
;WS->WS
(define (move-left-monk ws) (make-WS (WS-loo ws)
                                     (WS-lol ws)
                                     (WS-h ws)
                                     (WS-p ws)
                                     (make-Monk (Monk-x (WS-m ws))
                                                (Monk-y (WS-m ws))
                                                -1
                                                (Monk-dy (WS-m ws))
                                                (Monk-s (WS-m ws)))
                                     (WS-g ws)))
;WS->WS 
(define (move-right-monk ws) (make-WS (WS-loo ws)
                                      (WS-lol ws)
                                      (WS-h ws)
                                      (WS-p ws)
                                      (make-Monk (Monk-x (WS-m ws))
                                                 (Monk-y (WS-m ws))
                                                 1
                                                 (Monk-dy (WS-m ws))
                                                 (Monk-s (WS-m ws)))
                                      (WS-g ws))) 

;key-handler function
;WS->WS
(define (key-handler ws key) (cond [(equal? key " ")
                                    (if (equal? "standing" (Monk-s (WS-m ws))) (move-up-monk ws) ws)] 
                                                         
                                   [(equal? key "left") (move-left-monk ws)]
                                   [(equal? key "right") (move-right-monk ws)]
                                   [else ws]))
 

;helping functions for tock; animation right, left, and gravity
;WS->monk
(define (move-right ws) (make-Monk (+ (Monk-x (WS-m ws)) 2)
                                   (+ (Monk-y (WS-m ws)) (Monk-dy (WS-m ws)))
                                   (Monk-dx (WS-m ws))
                                   (+ (Monk-dy (WS-m ws)) .2)
                                   (Monk-s (WS-m ws))))
;WS-> monk
(define (move-left ws) (make-Monk (- (Monk-x (WS-m ws)) 2)
                                  (+ (Monk-y (WS-m ws)) (Monk-dy (WS-m ws)))
                                  (Monk-dx (WS-m ws))
                                  (+ (Monk-dy (WS-m ws)) .2)
                                  (Monk-s (WS-m ws))))
;WS->monk
(define (dont-move-x ws) (if (encounter? ws)
                             (make-Monk (+ 1 (Monk-x (WS-m ws)))
                                        (Monk-y (WS-m ws))
                                        1
                                        (Monk-dy (WS-m ws))
                                        "knocked")
                             (make-Monk (Monk-x (WS-m ws))
                                        (+ (Monk-y (WS-m ws)) (Monk-dy (WS-m ws)))
                                        (Monk-dx (WS-m ws))
                                        (if (equal? "standing" (Monk-s (WS-m ws))) 0
                                            (+ (Monk-dy (WS-m ws)) .2))
                                        (Monk-s (WS-m ws)))))



;number, number, number, number-> number
;finds distance btwn 2 x y points
(define (distance x y x2 y2) (sqrt (+ (sqr (- x2 x))
                                      (sqr (- y2 y)))))

;ledge, monk-> monk
;checks whether distance is less than RADIUS and if y is increasing
;returns the monk of the present or if he has landed the landing coordinates incorporated into the monk
(define (landing? ledge m) (if (and (< (distance (Monk-x m) (Monk-y m) (Ledge-x ledge) (Ledge-y ledge)) (* RADIUS .8))
                                    (positive? (Monk-dy m)))
                               (make-Monk (Ledge-x ledge)
                                          (- (Ledge-y ledge) RADIUS)
                                          0
                                          0
                                          "standing")
                               m))
                                          
;WS, monk-> monk
;figures out what the height should be
(define (monkHeight ws m) (foldr landing? m (WS-lol ws)))

;WS-> boolean
;makes sure monk is above HAPPYHEIGHT
(define (alive-monk ws) (< (Monk-y (WS-m ws)) HAPPYHEIGHT))

;WS-> WS
;finds weighted average of difference between HAPPYHEIGHT and monk-y in local, then applies it to WS-h, WS-lol, and WS-p and Monk-y of WS-m

(define (scroll-all ws) (local [(define scroll-height (+ (* .1 HAPPYHEIGHT) (* .9 (Monk-y (WS-m ws)))))
                                (define height-change (abs (- (Monk-y (WS-m ws))
                                                              (+ (* .1 HAPPYHEIGHT) (* .9 (Monk-y (WS-m ws)))))))]
                          (make-WS (map (λ (obs) (make-Obstacle (Obstacle-t obs)
                                                                (make-posn (posn-x (Obstacle-p obs)) (+ height-change (posn-y (Obstacle-p obs))))
                                                                (Obstacle-s obs))) 
                                        (WS-loo ws))
                                   (map (λ (ledge) (make-Ledge (Ledge-l ledge)
                                                               (Ledge-x ledge)
                                                               (+ height-change (Ledge-y ledge))
                                                               (Ledge-n ledge)))
                                        (WS-lol ws))
                                   (- (WS-h ws) height-change)
                                   (make-posn (posn-x (WS-p ws)) (+ height-change (posn-y (WS-p ws))))
                                   (make-Monk (Monk-x (WS-m ws))
                                              scroll-height
                                              (Monk-dx (WS-m ws))
                                              (Monk-dy (WS-m ws))
                                              (Monk-s (WS-m ws)))
                                   (WS-g ws))))
#|(check-expect (scroll-all (make-WS (list empty) (build-lol) STARTHEIGHT (make-posn 200 (- HEIGHT STARTHEIGHT)) (make-Monk WIDTH 235 0 0 "standing") #f))
              (make-WS (list empty)
                       (map (λ (ledge) (make-Ledge (Ledge-l ledge)
                                                   (Ledge-x ledge)
                                                   (+ 6.5 (Ledge-y ledge))
                                                   (Ledge-n ledge))) (build-lol))
                       (- STARTHEIGHT 6.5)
                       (make-posn 200 (+ (- HEIGHT STARTHEIGHT) 6.5))
                       (make-Monk WIDTH 241.5 0 0 "standing")
                       #f)) |#




;Obstacle->Obstacle
(define (moveRightbird obs)
  (make-Obstacle (Obstacle-t obs)
                 (make-posn (+ (posn-x (Obstacle-p obs)) SIDEWAYS) (posn-y (Obstacle-p obs)))
                 (Obstacle-s obs)))
;Obstacle->Obstacle
(define (moveLeftbird obs)
  (make-Obstacle (Obstacle-t obs)
                 (make-posn (- (posn-x (Obstacle-p obs)) SIDEWAYS) (posn-y (Obstacle-p obs)))
                 (Obstacle-s obs)))
  

;Obstacle->Obstacle
;moves birds left and right
(define (move-bird obs) (cond [(equal? (Obstacle-s obs) "flying-right")
                               (moveRightbird obs)]
                              [(equal? (Obstacle-s obs) "flying-left")
                               (moveLeftbird obs)]
                              [else obs]))
                               

;Obstacle->Obstacle
;moves books left and right
(define (move-book obs) (make-Obstacle (Obstacle-t obs)
                                       (make-posn (+ 3 (posn-x (Obstacle-p obs))) 
                                                  (+ (* .08 (posn-x (Obstacle-p obs)))
                                                     (posn-y (Obstacle-p obs))))
                                       (Obstacle-s obs)))

;list->list
;filters out birds that are offscreen once they are existent
(define (offscreenBird? list) (filter (λ (obs) (and (< (posn-x (Obstacle-p obs)) HEIGHT)
                                                    (> (posn-x (Obstacle-p obs)) 0))) list))

;list, ws-> list
;moves birds right or left and books down
;(and will include checks that they stop existing once they're off screen)
;will include algorithm for loop-de-loops!!
(define (existing-obstacle ws list) (offscreenBird? (map (λ (obs) (cond [(equal? "bird" (Obstacle-t obs)) (move-bird obs)]
                                                                        [(equal? "book" (Obstacle-t obs)) (move-book obs)]
                                                                        [else obs])) list)))

;monk, list of ledges-> list of 1 ledge OR empty list
;produce the ledge monk is on
(define (nearest-ledge monk list) (filter (λ (ledge) (and (Ledge-n ledge)
                                                          (= (+ (Monk-y monk) RADIUS) (Ledge-y ledge)))) list))
                           

;list of obstacles-> number
(define (count-helper l) (foldl (λ (obs num) (if (equal? (Obstacle-t obs) "bird")
                                                 (+ 1 num) num)) 0 l))
                                  
(check-expect (count-helper (list (make-Obstacle "bird"
                                                 (make-posn 2 250)
                                                 "flying-right")))
              1)
;list->boolean
(define (bird-spacer list) (foldr (λ (obs tv) (and (> RADIUS (posn-y (Obstacle-p obs))) tv)) #t list)) 
(check-expect (bird-spacer
               (list (make-Obstacle "bird" (make-posn 10 250) "flying-right") (make-Obstacle "bird" (make-posn 25 250) "flying-left")))
              #false)
                                                    
;WS, list of obstacles->list of Obstacles
;triggers bird if standing on nest
(define (birdie ws list) (local [(define specific-ledge (nearest-ledge (WS-m ws) (WS-lol ws)))
                                 (define bird-count (count-helper list))
                                 (define side (rndS))] 
                           (cond [(and (equal? "standing" (Monk-s (WS-m ws)))
                                       (empty? specific-ledge))
                                  (WS-loo ws)]
                                 [(and (equal? "standing" (Monk-s (WS-m ws)))
                                       (not (empty? specific-ledge))
                                       (<= bird-count 1)
                                       (bird-spacer list)) 
                                  (cons (make-Obstacle "bird"
                                                       (make-posn (rnds-checker side) (Monk-y (WS-m ws)))
                                                       side) (WS-loo ws))]
                                 [else (WS-loo ws)])))
                                                    
                                 
                             
;WS->WS
;essentially the tock function but without the scrolling or the book 
(define (make-moved ws) (make-WS (if (empty? (WS-loo ws))
                                     (birdie ws (WS-loo ws))
                                     (provoked? ws (existing-obstacle ws (birdie ws (WS-loo ws)))))
                                 (WS-lol ws)
                                 (WS-h ws)
                                 (WS-p ws)
                                 (monkHeight ws (cond [(WS-g ws) (WS-m ws)]
                                                      [(< (Monk-dx (WS-m ws)) 0) (move-left ws)]
                                                      [(> (Monk-dx (WS-m ws)) 0) (move-right ws)]
                                                      [(= 0 (Monk-dx (WS-m ws))) (dont-move-x ws)]))
                                 (WS-g ws)))



;WS->boolean
;checks whether any obstacle gets in the viscinity of Monk
;foldl
(define (encounter? ws)
  (local [(define y (Monk-y (WS-m ws)))
          (define x (Monk-x (WS-m ws)))]
    (foldl (λ (obs tv) (or (< (distance x y (posn-x (Obstacle-p obs)) (posn-y (Obstacle-p obs)))
                              RADIUS) tv)) #f (WS-loo ws))))
(check-expect (encounter? (make-WS (list (make-Obstacle "bird" (make-posn 250 250) "flying-right")
                                         (make-Obstacle "book" (make-posn 300 10) "falling"))
                                   '()
                                   500
                                   (make-posn 250 500)
                                   (make-Monk 250 250 0 0 "standing")
                                   #f)) #t)

(check-expect (encounter? (make-WS (list (make-Obstacle "bird" (make-posn 30 250) "flying-right")
                                         (make-Obstacle "book" (make-posn 300 10) "falling"))
                                   '()
                                   500
                                   (make-posn 250 500)
                                   (make-Monk 250 250 0 0 "standing")
                                   #f)) #f)

;WS-> Obstacle
;returns whichever obstacle encountered monk
;filters out whichever obstacles didn't hit monk
(define (obstacleHit ws) (first (filter (λ (obs) (> RADIUS (distance (Monk-x (WS-m ws))
                                                                     (Monk-y (WS-m ws))
                                                                     (posn-x (Obstacle-p obs))
                                                                     (posn-y (Obstacle-p obs)))))
                                        (WS-loo ws))))
(check-expect (obstacleHit (make-WS (list (make-Obstacle "bird" (make-posn 250 250) "flying-right")
                                          (make-Obstacle "book" (make-posn 300 10) "falling"))
                                    '()
                                    500
                                    (make-posn 250 500)
                                    (make-Monk 250 250 0 0 "standing")
                                    #f))
              (make-Obstacle "bird" (make-posn 250 250) "flying-right"))

; WS, list-> list
;LIST of all the other obstacles
(define (obstacleNothit ws) (filter (λ (obs) (< RADIUS (distance (Monk-x (WS-m ws))
                                                                 (Monk-y (WS-m ws))
                                                                 (posn-x (Obstacle-p obs))
                                                                 (posn-y (Obstacle-p obs)))))
                                    (WS-loo ws)))


;WS, List->List
;changes obstacle's status within the list
(define (changeStatus ws list) (local [(define obs (obstacleHit ws))
                                       (define obsList (obstacleNothit ws))]
                                 (cond [(equal? (Obstacle-t obs) "book")
                                        (list* (make-Obstacle "book"
                                                              (make-posn (posn-x (Obstacle-p obs))
                                                                         (- 10 (posn-y (Obstacle-p obs))))
                                                              "bouncing")
                                               obsList)]
                                       [(equal? (Obstacle-t obs) "bird")
                                        (list* obs obsList)]
                                       [else list])))

;WS, list->list of Obstacles
;changes status of birds to "attacking", books to "bouncing"
(define (provoked? ws list) (if (encounter? ws)
                                (changeStatus ws list)
                                list))
                                   


;List of obstacles-> list
(define (book-filter list)
  (filter (λ (obs) (> HEIGHT (posn-y (Obstacle-p obs)))) list))
;->boolean
(define (theTimeisRight ws)                                 
  (and (>= (WS-h ws) HEIGHT)
       (or (= 5 (TIME))
           (= 15 (TIME))
           (= 50 (TIME))
           (= 90 (TIME))
           (= 100 (TIME))
           (= 150 (TIME))
           (= 165 (TIME))
           (= 167 (TIME))
           (= 170 (TIME)))))
;list->boolean
(define (book-checker list)
  (empty? (filter (λ (obs) (equal? "book" (Obstacle-t obs))) list)))
    
;WS->WS
;adds Book obstacle if the time is right
(define (bookie ws) (make-WS (if (and (theTimeisRight ws)
                                      (book-checker (book-filter (WS-loo ws))))
                                 (cons (make-Obstacle "book" (make-posn (rndx) 0) "falling") (book-filter (WS-loo ws)))
                                 (book-filter (WS-loo ws)))
                             (WS-lol ws)
                             (WS-h ws)
                             (WS-p ws)
                             (WS-m ws)
                             (WS-g ws)))


;tock function
; WS -> WS
(define (tock ws) (if (alive-monk ws)
                      (bookie (scroll-all (make-moved ws)))
                      (bookie (make-moved ws))))

;WS->Image
(define (renderSimeon ws) (place-image SIMEON-IMG
                                       (- (Monk-x (WS-m ws)) 50)
                                       (Monk-y (WS-m ws))
                                       (render ws)))

;WS->Image
;You won!or You Died
(define (final-image ws) (cond [(equal? "won" (stopDecision ws))
                                (overlay (above (text "You Won!" 25  'black)
                                                (text "Saint Simeon says thanks!" 25  'black))
                                         (renderSimeon ws))]
                               [(equal? "lost" (stopDecision ws))
                                (above (overlay (text "You Died. Sad Day" 25  'darkslateblue)
                                                (rectangle (/ HEIGHT 1.5) (/ HEIGHT 20) 'solid 'pink))
                                       (add-curve (overlay (beside (circle (/ RADIUS 5) 'solid 'black)
                                                                   (circle (/ RADIUS 10) 'solid 'peru)
                                                                   (circle (/ RADIUS 5) 'solid 'black)) (circle RADIUS 'solid 'peru)) 11 32 45 9/12 30 32 90 0 'maroon))]
                               [else (empty-scene HEIGHT HEIGHT)]))


;WS-> string
;decides whether you won or lost
(define (stopDecision ws)
  (cond [(or (> (Monk-y (WS-m ws)) HEIGHT)
             (equal? "dead" (Monk-s (WS-m ws))))
         "lost"]
        [(<= (Monk-y (WS-m ws)) (+ 30 (posn-y (WS-p ws)))) "won"]
        [else "stillKicking"]))

;WS->Boolean
;how it stops, either winning or losing
(define (win ws) (if (or (equal? "won" (stopDecision ws))
                         (equal? "lost" (stopDecision ws))) #t #f))


; ->WS 
(define initial-WS (make-WS '() (build-lol) STARTHEIGHT (make-posn 200 (- HEIGHT STARTHEIGHT)) (make-Monk WIDTH LOWEST 0 0 "standing") #t))

(big-bang initial-WS (to-draw render) (on-key key-handler) (on-tick tock .03) (stop-when win final-image))