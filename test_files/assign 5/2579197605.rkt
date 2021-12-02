

#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Lab5_part1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))













(define-struct widget (name quantity price))






(define W1 (make-widget "W1" 1 1))
(define Z1 (make-widget "Z1" 51 16))
(define A1 (make-widget "A1" 2 3))
(define D1 (make-widget "D1" 5 5))
(define E5 (make-widget "E5" 7 20))


(define-struct bst (widget left right))




(define BST-SINGLE-NODE (make-bst D1 false false))
(define BST-MAIN (make-bst A1
                           false
                           (make-bst W1
                                     (make-bst D1 false false)
                                     (make-bst Z1 false false))))










(check-expect (build-tree (list D1 Z1 W1 A1)) BST-MAIN)


(check-expect (build-tree empty) false)


(check-expect (build-tree (list D1)) BST-SINGLE-NODE)

(define (build-tree low)
  (foldr insert-name false low))







(check-expect (smaller? "E5" BST-SINGLE-NODE) false)


(check-expect (smaller? "D0" BST-SINGLE-NODE) true)


(check-expect (smaller? "" BST-SINGLE-NODE) true)

(define (smaller? k b)
  (string<? k (widget-name (bst-widget b))))







(check-expect (same? "A1" BST-MAIN) true)


(check-expect (same? "D2" BST-SINGLE-NODE) false)


(check-expect (same? "" BST-SINGLE-NODE) false)

(define (same? k b)
  (string=? k (widget-name (bst-widget b))))






(check-expect (find-name "Z1" BST-MAIN) Z1)


(check-expect (find-name "A1" BST-MAIN) A1)


(check-expect (find-name "Santa" BST-MAIN) false)

(define (find-name k b)
  (cond
    [(false? b) false]
    [(same? k b) (bst-widget b)]
    [(smaller? k b) (find-name k (bst-left b))]
    [else
     (find-name k (bst-right b))]))







(check-expect (insert-name A1 BST-SINGLE-NODE)
              (make-bst D1
                        (make-bst A1 false false)
                        false))


(check-expect (insert-name Z1 BST-SINGLE-NODE)
              (make-bst D1
                        false
                        (make-bst Z1 false false)))


(check-expect (insert-name E5 BST-MAIN)
              (make-bst A1 false (make-bst W1
                                           (make-bst D1
                                                     false
                                                     (make-bst E5 false false))
                                           (make-bst Z1 false false))))


(check-expect (insert-name D1 false) BST-SINGLE-NODE)

(define (insert-name v b)
  (cond
    [(false? b) (make-bst v false false)]
    [(smaller? (widget-name v) b) (make-bst (bst-widget b)
                                            (insert-name v (bst-left b))
                                            (bst-right b))]
    [else
     (make-bst (bst-widget b)
               (bst-left b)
               (insert-name v (bst-right b)))]))