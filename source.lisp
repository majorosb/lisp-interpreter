(defun simpleRec [i] 
  (if (= i 10) 
    (print i) (simpleRec (+ i 1))))

; This is a comment
; Fibonacci sequence

(defun fib [i] 
  (if (= i 0) 1
     (if (= i 1) 2
         (+ (fib (- i 1)) (fib (- i 2))))))

(defun areaSquare [n] 
 (let (set area (* n n))
 (print area)))

(defun areaRectangle [n m]
  (let (set area (* n m))
  (print area)
  ))

; Demo of while loop

(defun simpleWhile [n] 
  (while (< n 10)
         (set n (+ n 1))
         (print n)))


;(simpleRec 0)
;(print (fib 5))
;(areaSquare 5)
;(set calcRectFour (areaRectangle 4 5))
;(set func areaSquare)
;(func 4)
(simpleWhile 0)
(print area)
