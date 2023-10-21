(defpackage mini-scheme-compiler
  (:use :cl))
(in-package :mini-scheme-compiler)

(defun comp (expr env)
  "Compile the expression expr into a list of instructions"
  (cond
    ((symbolp expr) (gen-var expr env))
    ((atom expr) (gen 'CONST expr))
    ((scheme-macro (car expr)) (comp (scheme-macro-expand expr) env))
    ((case (car expr)
       (quote (gen 'CONST (cadr expr)))
       (begin (comp-begin (cdr expr) env))
       (set! (seq
              (comp (caddr expr) nil)
              (gen-set (cadr expr) env)))
       (if (let ((pred (cadr expr))
                 (then (caddr expr))
                 (else (cadddr expr)))
             (comp-if pred then else env)))
       (lambda (gen 'FN (comp-lambda (cadr expr) (cddr expr) env)))
       ;; apply function
       (t (let ((args (cdr expr)))
            (seq
             ;;D# 引数を順にコンパイル
             (mappend #'(lambda (expr2) (comp expr2 env)) args)
            ;; 関数呼び出しのコンパイル
             (comp (car expr) env)
             (gen 'call (length args)))))))))

;; マクロかどうか？だけではなく、マクロ自身の関数を返す。
(defun scheme-macro (symbol)
  (and (symbolp symbol)
       ;; シンボルのプロパティにせず別管理にしたほうがよい？
       (get symbol 'scheme-macro)))

(defun scheme-macro-expand (expr)
  "Macro expand this Scheme expression."
  (if (and (listp expr) (scheme-macro (car expr)))
      (scheme-macro-expand
       (apply (scheme-macro (car expr)) (rest expr)))
      expr))

;; eval-whenは必要？
;; シンボルのshcheme-marco属性に関数を格納する。
(defmacro def-scheme-macro (name paramlist &body body)
  "Define a Scheme macro."
  `(setf (get ',name 'scheme-macro)
         #'(lambda ,paramlist ,@body)))

(defun comp-begin (exprs env)
  "Compile a sequence of expression, poping all but the last."
  (cond
    ((null exprs) (gen 'CONST nil))
    ((length=1 exprs) (comp (car exprs) env))
    (t (seq (comp (car exprs) nil)
            (gen 'POP)
            ;; seqではappendしているので最後がリストになればよい。
            (comp-begin (cdr exprs) nil)))))

(defun comp-if (pred then else env)
  "Compile a conditional expression."
  (let ((label1 (gen-label))
        (label2 (gen-label)))
    (seq (comp pred env) (gen 'FJUMP label1)
         (comp then env) (gen 'JUMP label2)
         (list label1) (comp else env)
         (list label2))))

(defun comp-lambda (args body env)
  "Compile a lambda form into a closure with compiled code."
  (assert (and (listp args) (every #'symbolp args)) ()
          "Lambda arglist must be a list of symbols, not ~a" args)
  (format t "comp-lambda: env=~a~%" (cons args env))
  (make-fn
   :env env
   :args args
   :code (seq (gen 'ARGS (length args))
              ;; argsはシンボルのリストになっているのに
              ;; (cons args env) で期待するenvの形式になるのだろうか？
              (comp-begin body (cons args env))
              (gen 'RETURN ))))

(defstruct (fn (:print-function print-fn))
  code (env nil) (name nil) (args nil))

(defun print-fn (fn &optional (stream *standard-output*) depth)
  (declare (ignore depth))
  (format stream "FN{~a} code=~a env=~a" (or (fn-name fn) '??) (fn-code fn) (fn-env fn)))

(defun seq (&rest code)
  "Return a sequence of instructions"
  (apply #'append code))

(defun gen-var (var env)
  "Generate an instruction to reference a variable's value."
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LVAR (car p) (cadr p) ";" var)
        (gen 'GVAR var))))

(defun gen-set (var env)
  "Generate an instruction to set a varibale to top-of-stack"
  (let ((p (in-env-p var env)))
    (if p
        (gen 'LSET (car p) (cadr p) ";" var)
        (gen 'GSET var))))

(defun in-env-p (symbol env)
  "If symbol is in the environment, return its index numbers."
  (let ((frame (find symbol env :test #'find)))
    (if frame
        ;; (フレームインデックス フレーム内のインデックス)となる。
        (list (position frame env) (position symbol frame)))))

(defun gen (opcode &rest args)
  "Return a one-element list of the specified instruction."
  (list (cons opcode args)))

(defvar *label-num* 0)

(defun gen-label (&optional (label 'L))
  "Generate label(a symbol of the form Lnnn)."
  (intern (format nil "~a~d" label (incf *label-num*))))

(defun length=1 (list)
  "Is list a list of length 1?"
  (and (consp list) (null (cdr list))))

(defun mappend (function list)
  (apply #'append (mapcar function list)))

;;;
;;; macro
;;;
(def-scheme-macro let (bindings &rest body)
  `((lambda ,(mapcar #'car bindings) ,@body)
    ,@(mapcar #'cadr bindings)))

(def-scheme-macro let* (bindings &rest body)
  (if (null bindings)
      `(begin ,@body)
      `(let (,(car bindings))
         (let* ,(cdr bindings)
           ,@body))))

(def-scheme-macro and (&rest args)
  (cond ((null args) 't)
        ((length=1 args) (car args))
        (t `(if ,(car args)
                (and ,@(cdr args))))))

(def-scheme-macro or (&rest args)
  (cond ((null args) 'nil)
        ((length=1 args) (car args))
        ;; orは成立したときの値を返す。
        (t (let ((var (gensym)))
             `(let ((,var ,(car args)))
                (if ,var ,var
                    (or ,@(cdr args))))))))

;; (cond
;;   ((= a 10) :a10)
;;   ((= a 20) :a20)
;;   (t :otherwise))
;; Schemeの場合else
(def-scheme-macro cond (&rest clauses)
  (cond ((null clauses) nil)
        ((length=1 (car clauses))
         `(or ,(car clauses) (cond ,@(cdr clauses))))
        ((starts-with (car clauses) 'else)
         `(begin ,@(cdr (car clauses))))
        (t `(if ,(car (car clauses))
                (begin ,@(cdr (car clauses)))
                (cond ,@(cdr clauses))))))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (car list) x)))

(def-scheme-macro case (key &rest clauses)
  (let ((key-val (gensym "KEY")))
    `(let ((,key-val ,key))
      (cond ,(mapcar
              #'(lambda (clause)
                  (if (starts-with clause 'else)
                      clause
                      ;; eqv?だと思ったが、clauseは ((a b c) :a_b_c) のようにリストを書くようだ。
                      ;; Common Lispだと ((a) :a) → (a :a) と書ける。
                      ;; mklistを挟むと良いのかもしれない。
                      `((member ,key-val ',(car clause))
                        ,@(cdr clause))))
              clauses)))))

(def-scheme-macro define (name &rest body)
  ;; symbolpではない？
  (if (atom name)
      ;; var定義の場合
      ;; name!を呼び出して関数名フィールドを設定する。
      `(name! (set! ,name ,@body) ',name)
      ;; 関数定義の場合
      ;; scheme-macro-expandしなくて良いのでは？
      `(define ,(car name)
           (lambda ,(cdr name) ,@body))))

(defun name! (fn name)
  "Set the name field of fn, if it is an-un-named fn"
  (when (and (fn-p fn) (null (fn-name fn)))
    (setf (fn-name fn) name))
  name)

(def-scheme-macro delay (computation)
  `(lambda () ,computation))

(def-scheme-macro letrec (bindings &rest body)
  `(let ,(mapcar #'(lambda (v) (list (car v) nil)) bindings)
     ,@(mapcar #'(lambda (v) `(set! ,@v)) bindings)
     ,@body))

;;
;;
;;
(defvar *global-env* nil)

(defun get-global-var (var)
  (let ((pair (assoc var *global-env*)))
    (if (null pair)
        (error "Unbound scheme variable: ~a" var)
        (cdr pair))))

(defun set-global-var! (var val)
  (if (assoc var *global-env*)
      (setf (cdr (assoc var *global-env*)) val)
      (setf *global-env* (acons var val *global-env*)))
  val)

(defparameter *scheme-procs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri)))

(defun init-scheme-proc (f)
  "Define a Scheme procedure as a corresponding CL functions."
  (if (listp f)
      (set-global-var! (car f) (symbol-function (cadr f)))
      (set-global-var! f (symbol-function f))))

(defun init-scheme-interp ()
  "Initialize the scheme interpreter with some global variables."
  (dolist (f *scheme-procs*)
    (init-scheme-proc f))
  (set-global-var! t t)
  (set-global-var! nil nil)
  (set-global-var! 'name! #'name!))

;;
;; utility
;;
;; compoileは定義済みの関数なので衝突する。
(defun compiler (expr)
  "Compile an expression as if it were in a parameterless lambda"
  (setf *label-num* 0)
  (comp-lambda '() (list expr) nil))

(defun comp-show (expr)
  "Compile an expression and show the resulting code"
  (show-fn (compiler expr))
  (values))

(defun show-fn (fn &optional (stream *standard-output*) (depth 0))
  "Print all the instructions in a function.
If the argument is not function, just printc it.
but in a column at least 8 spaces wide."
  (if (not (fn-p fn))
      (format stream "~8a" fn)
      (progn
        (fresh-line)
        (incf depth 8)
        ;; instr stand for instruction
        (dolist (instr (fn-code fn))
          (if (label-p instr)
              (format stream "~a:" instr)
              (progn
                ;; 改行するほうが分かりやすくなる？
                (format stream "~vt" depth)
                ;; ここでは instrは (JUMP L8) のようになっている。→argではいまいち。
                (dolist (arg instr)
                  (show-fn arg stream depth))
                (fresh-line)))))))

(defun label-p (instr)
  "Is instr(uction) a label?"
  (atom instr))

;;
;; for test
;;
(defun test-1 ()
  (let ((env (copy-list '((a 10 b 20) (c 30 d 40 e 50) (f 60)))))
    ;; (in-env-p 'e env)
    (gen-var 'e env)
    ))

;;
;; ver1.0
;; 
;; MINI-SCHEME-COMPILER> (comp-show '(f (g x)))
;; comp-lambda: env=(NIL)
;;         ARGS    0       
;;         GVAR    X       
;;         GVAR    G       
;;         CALL    1       
;;         GVAR    F       
;;         CALL    1       
;;         RETURN  
;;
;; MINI-SCHEME-COMPILER> (comp-show '(f (g (h x) (h y))))
;; comp-lambda: env=(NIL)
;;         ARGS    0       
;;         GVAR    X       
;;         GVAR    H       
;;         CALL    1       
;;         GVAR    Y       
;;         GVAR    H       
;;         CALL    1       
;;         GVAR    G       
;;         CALL    2       
;;         GVAR    F       
;;         CALL    1       
;;         RETURN  
;;
;; MINI-SCHEME-COMPILER> (comp-show '(begin "doc" x (f x) y))
;; comp-lambda: env=(NIL)
;;         ARGS    0       
;;         CONST   doc     
;;         POP     
;;         GVAR    X       
;;         POP     
;;         GVAR    X       
;;         GVAR    F       
;;         CALL    1       
;;         POP     
;;         GVAR    Y       
;;         RETURN  
