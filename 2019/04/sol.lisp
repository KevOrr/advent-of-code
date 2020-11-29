(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("screamer" "trivia") :silent t))
(in-package :screamer)

(defconstant +RANGE+ '(248345 746315))
(defconstant +USAGE+ "Usage: sbcl --noinform --non-interactive --load sol.lisp [LOWER UPPER [NDIGITS]]")

(defun some-digits (&optional (n 6))
  (loop :for i :from 1 :to n
        :collect (an-integer-betweenv 0 9 (format nil "digit ~D" i))))

(defun assert-num-between! (digits lower upper)
  (let ((num (reduce #'+v
                     (loop :for i :upfrom 0
                           :for d :in (reverse digits)
                           :collect (*v d (expt 10 i))))))
    (assert! (andv (<=v lower num)
                   (<=v num upper)))))

(defun assert-increasing! (digits)
  (loop :for a :in digits
        :for b :in (cdr digits)
        :do (assert! (<=v a b))))

(defun assert-exists-twin! (digits)
  (assert!
   (reduce
    #'orv
    (loop :for a :in digits
          :for b :in (cdr digits)
          :collect (=v a b)))))

(defun assert-exists-lone-twin! (digits)
  (assert!
   (reduce
    #'orv
    ;; it must be true that for some pair of neighbors `b', `c', as well as the
    ;; preceeding digit `a' (or `nil' if none), and next digit `d' (or `nil' if none), ...
    (loop :for a :in (cons nil digits)
          :for b :in digits
          :for c :in (cdr digits)
          :for d :in (append (cddr digits) '(nil))
          ;; :do (format t "~a~%~a~%~a~%~a~%~%" a b c d)
          :collect (andv
                    ;; `b' must equal `c'
                    (=v b c)
                    ;; if there is a preceding digit, then it must not equal `b'
                    (if (eq nil a) t (/=v a b))
                    ;; if there is a following digit, then it must not equal `c'
                    (if (eq nil d) t (/=v c d)))))))

(defun main (&optional (ndigits 6) (lower (car +RANGE+)) (upper (cadr +RANGE+)))
  (let ((num (some-digits ndigits)))
    (assert-num-between! num lower upper)
    (assert-increasing! num)
    (all-values
      (either
        (progn
          (assert-exists-twin! num)
          (format t "Challenge 1: ~a~%"
                  (length (all-values (solution num (static-ordering #'linear-force)))))
          (fail))
        (progn
          (assert-exists-lone-twin! num)
          (format t "Challenge 2: ~a~%"
                  (length (all-values (solution num (static-ordering #'linear-force)))))))))
  nil)

(eval-when (:execute)
  (let ((args (cdr sb-ext:*posix-argv*))
        lower
        upper
        ndigits)
    (trivia:match args
      ((list lower upper rest)
       (setf lower (parse-integer lower))
       (setf upper (parse-integer upper))
       (trivia:match rest
         ((list ndigits)
          (setf ndigits (parse-integer ndigits))))))
    (apply #'main (append (if ndigits (list ndigits))
                          (if lower (list lower))
                          (if upper (list upper))))))
