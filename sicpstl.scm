(define nil '())



(define (smallest-divisor n)
  (find-divisor n 2))


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n)  test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))


(define (divides? test-divisor n)
  (= (remainder n test-divisor) 0))


(define (prime? n)
  (= (smallest-divisor n) n))


(define (fib n)
  (fib-iter 1 0 n))


(define (fib-iter a b count)
  (if (= 0 count)
      b
      (fib-iter (+ a b) a (- count 1))))


(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))


(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	         (accumulate op initial (cdr sequence)))))


;; a higher dimension version of "accumulate"
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (car-n seqs))
      (accumulate-n op init (cdr-n seqs)))))


;;generate a sequence of integers in a given range
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))


(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))
      
;; a higher dimension version of car & cdr
;; used in writing accumulate-n


(define (car-n seqs)
  (if (null? seqs)
      nil
      (cons (car (car seqs))
	    (car-n (cdr seqs)))))


(define (cdr-n seqs)
  (if (null? seqs)
      nil
      (cons (cdr (car seqs))
	    (cdr-n (cdr seqs)))))


;;section 2.2.3 nested mappings
(define (unique-pairs n)
  (flatmap (lambda (i) 
  				(map (lambda (j) (list i j)) 
  					(enumerate-interval 1 (- i 1))))
			(enumerate-interval 1 n)))


(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))


(define (remove item sequence)
  (filter (lambda (x) (not (= x item))) sequence))


(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
		 (map (lambda (p) (cons x p))
		      (permutations (remove x s))))
	       s)))


(define (sum-list sequence)
  (accumulate + 0 sequence))


(define (copy-list seq m n)
  (cond ((or (< n 0) (< n m)) nil)
        ((> n (length seq)) seq)
        (else (flatmap  
                  (lambda (i) (list (list-ref seq i)))
                (enumerate-interval m n)))))
;note that append can only apply with list object


(define (in-list? seq item)
  (not (null? (filter (lambda (x) (= x item)) seq))))
;note that this procedure can only apply with non-nested
;list, need list version of equity?, which exactly is to
;determine whether two trees are identical


(define (pretty-print x)
  (newline)
  (display "=>\t")
  (display x))

(define (display-line x)
   (newline)
   (display x))

(define (pretty-list seq)
  (map pretty-print seq))  


(define (integers-starting-from n)
   (cons-stream n (integers-starting-from (+ n 1))))


(define ones (cons-stream 1 ones))
(define negative-ones (cons-stream -1 
                              negative-ones))

(define (add-streams s1 s2)
   (stream-map + s1 s2))

(define (div-streams s1 s2)(stream-map / s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define integers (cons-stream 1 
                      (add-streams ones integers)))

(define double (cons-stream 1 (scale-stream double 2)))

(define fibs 
   (cons-stream 0
      (cons-stream  1
                    (add-streams (stream-cdr fibs)
                                 fibs))))

(define  (mul-streams s1 s2)
   (stream-map  *  s1  s2))


(define factorials
    (cons-stream 1 (mul-streams factorials
                               (stream-cdr integers))))

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams  (partial-sums s)
                             (stream-cdr s))))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
    (else
        (let ((s1car (stream-car s1))
             (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                  (cons-stream 
                       s1car 
                       (merge (stream-cdr s1) 
                              s2)))
                  ((> s1car s2car)
                    (cons-stream 
                       s2car 
                       (merge s1 
                              (stream-cdr s2))))
                  (else
                    (cons-stream 
                       s1car
                  (merge 
                    (stream-cdr s1)
                    (stream-cdr s2)))))))))


(define (memo-proc proc)
   (let ((already-run? false)(result false))
        (lambda ()
           (if (not already-run?)
               (begin (set! result (proc))
                      (set! already-run?  true)
                      result)
               result))))

(define the-mepty-stream '())

(define (stream-enumerate-interval low high)
   (if (> low high)
      the-mepty-stream
      (cons-stream 
         low
         (stream-enumerate-interval  (+ low 1) high))))


(define (stream-for-each proc s)
   (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

 (define (display-stream s)
    (stream-for-each display-line s))


 (define (sieve stream)
    (cons-stream  
       (stream-car  stream)
       (sieve (stream-filter
                (lambda (x)
                   (not (divisible?
                        x  (stream-car stream))))
                 (stream-cdr  stream)))))


(define (divisible? x y)(= (remainder x y) 0))

(define (show x) (display-line x)  x)

(define (expand num den radix)
   (cons-stream
      (quotient (* num radix) den)
      (expand (remainder  (* num radix) den) 
                          den radix)))

(define natural-sequence 
     (div-streams ones 
                   (integers-starting-from 1)))

(define (integrate-series s)
    (mul-streams s  natural-sequence))


(define exp-series
   (cons-stream 
      1  (integrate-series  exp-series)))


(define cosine-series
   (cons-stream 1 
          (mul-streams
               negative-ones
              (integrate-series sine-serires))))

(define sine-series
   (cons-stream  0 
                (integrate-series  cosine-series)))

(define (pi-summands n)
   (cons-stream
      (/ 1.0 n)
      (stream-map  -  (pi-summands (+ n 2)))))


(define (euler-transform s)
   (let ((s0 (stream-ref s 0))
         (s1 (stream-ref s 1))
         (s2 (stream-ref s 2)))
      (cons-stream
        (- s2 (/ (square (- s2 s1))
                 (+ s0 (* -2 s1) s2)))
        (euler-transform (stream-cdr s)))))


(define (make-tableau transform s)
   (cons-stream
      s
      (make-tableau 
         transform
           (transform s))))


(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau  transform s)))


(define (ln-2-summands n)
   (cons-stream 
      (/ 1.0 n)
      (stream-map -  (ln-2-summands (+ n 1)))))

(define ln2-stream (partial-sums (ln-2-summands 1)))


(define (interleave s1  s2)
   (if (stream-null?  s1)
        s2 
       (cons-stream
          (stream-car s1)
          (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
   (cons-stream
      (list (stream-car s) (stream-car t))
      (interleave
         (stream-map (lambda (x)
                        (list (stream-car s) x))
                     (stream-cdr t))
         (pairs (stream-cdr s) (stream-cdr t)))))


(define (grids s t)
  (cons-stream
     (list (stream-car s) (stream-car t))
      (interleave
         (interleave
            (stream-map (lambda (x)
                           (list (stream-car s) x))
                         (stream-cdr t))
            (stream-map (lambda (x)
                           (list x (stream-car t)))
                          (stream-cdr s)))
          (grids (stream-cdr s) (stream-cdr t)))))


(define (triples s t u)
  (cons-stream
      (list (stream-car s)
            (stream-car t)
            (stream-car u))
      (interleave
         (stream-map (lambda (x) 
                        (cons (stream-car s) x))
                     (stream-cdr (pairs t u)))
         (triples (stream-cdr s)
                  (stream-cdr t)
                  (stream-cdr u)))))


(define (pythagorean? triple)
   (if (= (square (caddr triple))
          (+ (square (car triple)) 
             (square (cadr triple))))
       #t
       #f))

(define pythagorean-triple 
   (stream-filter pythagorean?
       (triples integers integers integers)))


(define (merge-weighted s1  s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
    (else
        (let ((s1car (stream-car s1))
             (s2car (stream-car s2)))
            (cond ((< (weight s1car) (weight s2car))
                  (cons-stream 
                       s1car 
                       (merge-weighted 
                              (stream-cdr s1) 
                              s2
                              weight)))
                  ((> (weight s1car) (weight s2car))
                    (cons-stream 
                       s2car 
                       (merge-weighted 
                               s1 
                              (stream-cdr s2)
                              weight)))
                  (else
                    (cons-stream 
                       s1car
                  (merge-weighted 
                     (stream-cdr s1)
                     (stream-cdr s2)
                     weight))))))))


(define (weighted-pairs s1 s2)
   (cons-stream 
       (list (stream-car s1) (stream-car s2))
       (merge-weighted 
          (stream-map 
             (lambda (x)
                     (list (stream-car s1) x))
             (stream-cdr s2))
          (weighted-pairs 
              (stream-cdr s1)
              (stream-cdr s2))
          weight)))


(define (biased-weighted-pairs s1 s2)
   (stream-filter 
       (lambda (x)
               (and (not (divides? 2 (car x)))
                    (not (divides? 3 (car x)))
                    (not (divides? 5 (car x)))
                    (not (divides? 2 (cadr x)))
                    (not (divides? 3 (cadr x)))
                    (not (divides? 5 (cadr x)))))
         (weighted-pairs s1 s2)))


(define (weight pair)
   (+ (* 2 (car pair)) 
      (* 3 (cadr pair))
      (* 5 (car pair) (cadr pair))))


(define (implicit-integral integrand initial-value dt)
   (define int 
      (cons-stream
         initial-value
           (add-streams (scale-stream integrand dt)
                        int)))
   int)


(define (RC R C Current dt)
   (define initial-voltage 0)
   (define voltage 
       (cons-stream 
           initial-voltage
           (add-streams
              (add-streams
                 (scale-stream Current R)
                 (scale-stream
                   (scale-stream Current dt) (/ 1 C)))
            voltage)))
 voltage)


(define (delay-implicit-integral
         delayed-integrand initial-value dt)
   (define int 
      (cons-stream
          initial-value
          (let ((integrand 
                   (force delayed-integrand)))
            (add-streams
               (scale-stream integrand dt)
               int))))
   int)


(define (explicit-integral
         integrand initial-value dt)
  (cons-stream 
    initial-value
    (if (stream-null? integrand)
        the-empty-stream
        (integral 
          (stream-cdr integrand)
          (+ (* dt (stream-car integrand)) 
             initial-value)
          dt))))


(define (delay-explicit-integral
           delayed-integrand  initial-value dt)
   (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
         (if (stream-null? integrand)
             the-empty-stream
             (delay-explicit-integral
                (delay (stream-cdr integrand))
                (+ (* dt (stream-car integrand))
                   initial-value)
                dt)))))
