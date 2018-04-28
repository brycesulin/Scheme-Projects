(define (in? u lst)    ;#1
  (not (equal? (member u lst) #f)))

(define (removeDups ls)    ;#2
  (let loop ((ls ls) (seen '()))
     (cond
       ((null? ls) '())
       ((memq (car ls) seen) (loop (cdr ls) seen))
       (else (cons (car ls) (loop (cdr ls) (cons (car ls) seen)))))))

(define (sumList elemList)    ;auxiliary function
  (if
    (null? elemList)
    0
    (+ (car elemList) (sumList (cdr elemList)))
  ))

(define (powerset set)        ;auxiliary function
  (if (null? set)
      '(())
      (let ((rest (powerset (cdr set))))
        (append (map (lambda (element) (cons (car set) element))
                     rest)
                rest))))

(define (contains n set)       ;auxiliary function
    (if (null? set)
        #f
        (if (= (car set) n)
            #t
            (contains n (cdr set)))))

(define (groupSum set n)       ;#3
    (contains n (map sumList (powerset set))))

(define (postfix lis)          ;#4
  (cond
    ((not (list? lis))
     lis)
    ((list? lis)
     (if (equal? (length lis) 3)
       (cond
         ((equal? (cddr lis) '(+))
          (+ (postfix (car lis)) (postfix (cadr lis))))
         ((equal? (cddr lis) '(-))
          (- (postfix (car lis)) (postfix (cadr lis))))
         ((equal? (cddr lis) '(*))
          (* (postfix (car lis)) (postfix (cadr lis))))
         ((equal? (cddr lis) '(/))
          (/ (postfix (car lis)) (postfix (cadr lis)))))))))