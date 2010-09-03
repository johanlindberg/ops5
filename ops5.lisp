;Common Lisp Support Functions: 
;These functions are not defined in vanilla Common Lisp, but are used
;in the OPSMODS.l code and in OPS5.


(defun putprop(name val att)
   (setf (get name att) val))

(defun memq(obj lis)
    (member obj lis :test #'eq))

(defun fix(num)
    (round num))
    

(defun assq(item alist)
     (assoc item alist :test #'eq))

(defun ncons(x) (cons x nil))

(defun neq(x y) (not (eq x y)))

(defun delq(obj list)
   (delete obj list :test #'eq))

(defmacro comment(&optional &rest x) nil) ;comment is a noop

(defun plus(x y)
   (+ x y))

(defun quotient(x y)
   (/ x y))

(defun flatc(x)
   (length (princ-to-string x)))

; File OPS5.common.1.lsp: part 1 of OPS5 in Common Lisp
; ----------


;	VPS2 -- Interpreter for OPS5
;
;	Copyright (C) 1979, 1980, 1981
;	Charles L. Forgy,  Pittsburgh, Pennsylvania



; Users of this interpreter are requested to contact

;
;	Charles Forgy
;	Computer Science Department
;	Carnegie-Mellon University
;	Pittsburgh, PA  15213
; or
;	Forgy@CMUA
; 
; so that they can be added to the mailing list for OPS5.  The mailing list
; is needed when new versions of the interpreter or manual are released.



;;; Definitions

;#+ vax (defun putprop(name val att)
;   (setf (get name att) val))



(proclaim '(special *matrix* *feature-count* *pcount* *vars* *cur-vars*
          *curcond* *subnum* *last-node* *last-branch* *first-node*
          *sendtocall* *flag-part* *alpha-flag-part* *data-part*
          *alpha-data-part* *ce-vars* *virtual-cnt* *real-cnt*
          *current-token* *c1* *c2* *c3* *c4* *c5* *c6* *c7* *c8* *c9*
          *c10* *c11* *c12* *c13* *c14* *c15* *c16* *c17* *c18* *c19*
          *c20* *c21* *c22* *c23* *c24* *c25* *c26* *c27* *c28* *c29*
          *c30* *c31* *c32* *c33* *c34* *c35* *c36* *c37* *c38* *c39*
          *c40* *c41* *c42* *c43* *c44* *c45* *c46* *c47* *c48* *c49*
          *c50* *c51* *c52* *c53* *c54* *c55* *c56* *c57* *c58* *c59*
          *c60* *c61* *c62* *c63* *c64* *record-array* *result-array* 
          *max-cs* *total-cs* *limit-cs* *cr-temp* *side*
          *conflict-set* *halt-flag* *phase* *critical*
          *cycle-count* *total-token* *max-token* *refracts* 
          *limit-token* *total-wm* *current-wm* *max-wm*
          *action-count* *wmpart-list* *wm* *data-matched* *p-name*
          *variable-memory* *ce-variable-memory* 
          *max-index* ; number of right-most field in wm element 
          *next-index* *size-result-array* *rest* *build-trace* *last*
          *ptrace* *wtrace* *in-rhs* *recording* *accept-file* *trace-file* 
          *mtrace* *madeby* ; used to trace and record makers of elements
          *write-file* *record-index* *max-record-index* *old-wm*
          *record* *filters* *break-flag* *strategy* *remaining-cycles*
	  *wm-filter* *rhs-bound-vars* *rhs-bound-ce-vars* *ppline* 
	  *ce-count* *brkpts* *class-list* *buckets* *action-type*
          *literals*   ;stores literal definitions
          *pnames*     ;stores production names
	  *externals*  ;tracks external declarations 
          *vector-attributes*  ;list of vector-attributes
	  ))

;(declare (localf ce-gelm gelm peek-sublex sublex
;          eval-nodelist sendto and-left and-right not-left not-right
;          top-levels-eq add-token real-add-token remove-old
;          remove-old-num remove-old-no-num removecs insertcs dsort
;          best-of best-of* conflict-set-compare =alg ))


;;; Functions that were revised so that they would compile efficiently


;* The function == is machine dependent\!
;* This function compares small integers for equality.  It uses EQ
;* so that it will be fast, and it will consequently not work on all
;* Lisps.  It works in Franz Lisp for integers in [-128, 127]


;(defun == (&rest z) (= (cadr z) (caddr z)))
(defun == (x y) (= x y))

; =ALG returns T if A and B are algebraicly equal.

(defun =alg (a b) (= a b))

(defmacro fast-symeval (&rest z)
	 `(cond ((eq ,(car z) '*c1*) *c1*)
		((eq ,(car z) '*c2*) *c2*)
		((eq ,(car z) '*c3*) *c3*)
		((eq ,(car z) '*c4*) *c4*)
		((eq ,(car z) '*c5*) *c5*)
		((eq ,(car z) '*c6*) *c6*)
		((eq ,(car z) '*c7*) *c7*)
		(t (eval ,(car z)))  ))

; getvector and putvector are fast routines for using one-dimensional
; arrays.  these routines do no checking; they assume
;	1. the array is a vector with 0 being the index of the first
;	   element
;	2. the vector holds arbitrary list values
;defun versions are useful for tracing

; Example call: (putvector array index value)

(defmacro putvector (array_ref ind var)
      `(setf (aref ,array_ref ,ind) ,var))

;(defun putvector (array_ref ind var)
;      (setf (aref array_ref ind) var))

; Example call: (getvector name index)

;(defmacro getvector(&rest z)
;     (list 'cxr (caddr z) (cadr z)))

(defmacro getvector(array_ref ind)
      `(aref ,array_ref ,ind))

;(defun getvector (array_ref ind)
 ;       (aref array_ref ind))

(defun ce-gelm (x k)
  (prog nil
   loop (and (== k 1.) (return (car x)))
        (setq k (1- k))
        (setq x (cdr x))
        (go loop))) 

; The loops in gelm were unwound so that fewer calls on DIFFERENCE
; would be needed

(defun gelm (x k)
  (prog (ce sub)
        (setq ce  (floor (/ k 10000)))
        (setq sub (- k (* ce 10000)))
 celoop (and (== ce 0) (go ph2))
        (setq x (cdr x))
        (and (== ce 1) (go ph2))
        (setq x (cdr x))
        (and (== ce 2) (go ph2))
        (setq x (cdr x))
        (and (== ce 3) (go ph2))
        (setq x (cdr x))
        (and (== ce 4) (go ph2))
        (setq ce (- ce 4))
        (go celoop)
   ph2  (setq x (car x))
   subloop (and (== sub 0) (go finis))
        (setq x (cdr x))
        (and (== sub 1) (go finis))
        (setq x (cdr x))
        (and (== sub 2) (go finis))
        (setq x (cdr x))
        (and (== sub 3) (go finis))
        (setq x (cdr x))
        (and (== sub 4) (go finis))
        (setq x (cdr x))
        (and (== sub 5) (go finis))
        (setq x (cdr x))
        (and (== sub 6) (go finis))
        (setq x (cdr x))
        (and (== sub 7) (go finis))
        (setq x (cdr x))
        (and (== sub 8) (go finis))
        (setq sub (- sub 8))
        (go subloop)
   finis (return (car x)))) 


;;; Utility functions



(defun printline (x) (mapc (function printline*) x)) 

(defun printline* (y) (princ '| |) (print y)) 

(defun printlinec (x) (mapc (function printlinec*) x)) 

(defun printlinec* (y) (princ '| |) (princ y)) 

; intersect two lists using eq for the equality test

(defun interq (x y)
  (intersection x y :test #'eq))

(defun enter (x ll)
   (and (not (member x ll :test #'equal))
       (push x ll)))


;Hack read-macro tables to accept single characters -- right out of CL book.
(defun single-macro-character (stream char)
   (declare (ignore stream))
   (character char))

(defun i-g-v nil
 (prog (x)
        (set-macro-character #\{ #'single-macro-character )
        (set-macro-character #\} #'single-macro-character )
        (set-macro-character #\^ #'single-macro-character )
;	(setsyntax '\{ 66.) ;These are already normal characters in CL
;	(setsyntax '\} 66.)
;	(setsyntax '^ 66.)
	(setq *buckets* 64.)		; OPS5 allows 64 named slots
	(setq *accept-file* nil)
	(setq *write-file* nil)
	(setq *trace-file* nil)
        (and (boundp '*class-list*)
          (mapc #'(lambda(class) (putprop class nil 'att-list)) *class-list*))
	(setq *class-list* nil)
	(setq *brkpts* nil)
	(setq *strategy* 'lex)
  	(setq *in-rhs* nil)
  	(setq *ptrace* t)
  	(setq *wtrace* nil)
	(setq *mtrace* t)            ; turn on made-by tracing
	(setq *madeby* nil)          ; record makers of wm elements
  	(setq *recording* nil)
        (setq *refracts* nil)
	(setq *real-cnt* (setq *virtual-cnt* 0.))
	(setq *max-cs* (setq *total-cs* 0.))
  	(setq *limit-token* 1000000.)
	(setq *limit-cs* 1000000.)
	(setq *critical* nil)
	(setq *build-trace* nil)
	(setq *wmpart-list* nil)
        (setq *pnames* nil)
        (setq *literals* nil) ; records literal definitions
	(setq *externals* nil) ; records external definitions
	(setq *vector-attributes* nil) ;records vector attributes
	(setq *size-result-array* 127.)
	(setq *result-array* (make-array 128))
	(setq *record-array* (make-array 128))
	(setq x 0)
        (setq *pnames* nil)     ; list of production names
  loop	(putvector *result-array* x nil)
	(setq x (1+ x))
	(and (not (> x *size-result-array*)) (go loop))
	(make-bottom-node)
	(setq *pcount* 0.)
	(initialize-record)
	(setq *cycle-count* (setq *action-count* 0.))
	(setq *total-token*
	       (setq *max-token* (setq *current-token* 0.)))
	(setq *total-cs* (setq *max-cs* 0.))
	(setq *total-wm* (setq *max-wm* (setq *current-wm* 0.)))
	(setq *conflict-set* nil)
	(setq *wmpart-list* nil)
	(setq *p-name* nil)
	(setq *remaining-cycles* 1000000)
))

; if the size of result-array changes, change the line in i-g-v which
; sets the value of *size-result-array*

(defun %warn (what where)
  (prog nil
    (terpri)
    (princ '?)
    (and *p-name* (princ *p-name*))
    (princ '|..|)
    (princ where)
    (princ '|..|)
    (princ what)
    (return where))) 

(defun %error (what where)
    (%warn what where)
    (throw '!error! nil)) 


(defun top-levels-eq (la lb)
  (prog nil
   lx   (cond ((eq la lb) (return t))
              ((null la) (return nil))
              ((null lb) (return nil))
              ((not (eq (car la) (car lb))) (return nil)))
        (setq la (cdr la))
        (setq lb (cdr lb))
        (go lx))) 


;;; LITERAL and LITERALIZE

(defmacro literal (&rest z)
  `(prog (atm val old args)
        (setq args ',z)
   top  (and (atom args) (return 'bound))
        (or (eq (cadr args) '=) (return (%warn '|wrong format| args)))
        (setq atm (car args))
        (setq val (caddr args))
        (setq args (cdddr args))
        (cond ((not (numberp val))
               (%warn '|can bind only to numbers| val))
              ((or (not (symbolp atm)) (variablep atm))
                (%warn '|can bind only constant atoms| atm))
              ((and (setq old (literal-binding-of atm)) (not (equal old val)))
               (%warn '|attempt to rebind attribute| atm))
              (t (putprop atm val 'ops-bind )))
        (go top))) 

(defmacro literalize (&rest l)
  `(prog (class-name atts)
    (setq class-name (car ',l))
    (cond ((have-compiled-production)
           (%warn '|literalize called after p| class-name)
           (return nil))
          ((get class-name 'att-list)
           (%warn '|attempt to redefine class| class-name)
	   (return nil)))
    (setq *class-list* (cons class-name *class-list*))
    (setq atts (remove-duplicates (cdr ',l)))
    (test-attribute-names atts)
    (mark-conflicts atts atts)
    (putprop class-name  atts 'att-list))) 

(defmacro vector-attribute  (&rest l)
  `(cond ((have-compiled-production)
         (%warn '|vector-attribute called after p| ',l))
        (t 
         (test-attribute-names ',l)
	 (mapc (function vector-attribute2) ',l)))) 

(defun vector-attribute2 (att) (putprop att t 'vector-attribute)
			       (setq  *vector-attributes* 
				   (enter att *vector-attributes*)))

(defun is-vector-attribute (att) (get att 'vector-attribute))

(defun test-attribute-names (l)
  (mapc (function test-attribute-names2) l)) 

(defun test-attribute-names2 (atm)
  (cond ((or (not (symbolp atm)) (variablep atm))
         (%warn '|can bind only constant atoms| atm)))) 

(defun finish-literalize nil
  (cond ((not (null *class-list*))
         (mapc (function note-user-assigns) *class-list*)
         (mapc (function assign-scalars) *class-list*)
         (mapc (function assign-vectors) *class-list*)
         (mapc (function put-ppdat) *class-list*)
         (mapc (function erase-literal-info) *class-list*)
         (setq *class-list* nil)
         (setq *buckets* nil)))) 

(defun have-compiled-production nil (not (zerop *pcount*))) 

(defun put-ppdat (class)
  (prog (al att ppdat)
        (setq ppdat nil)
        (setq al (get class 'att-list))
   top  (cond ((not (atom al))
               (setq att (car al))
               (setq al (cdr al))
               (setq ppdat
                     (cons (cons (literal-binding-of att) att)
                           ppdat))
               (go top)))
        (putprop class ppdat 'ppdat))) 

; note-user-assigns and note-user-vector-assigns are needed only when
; literal and literalize are both used in a program.  They make sure that
; the assignments that are made explicitly with literal do not cause problems
; for the literalized classes.

(defun note-user-assigns (class)
  (mapc (function note-user-assigns2) (get class 'att-list)))

(defun note-user-assigns2 (att)
  (prog (num conf buck clash)
        (setq num (literal-binding-of att))
	(and (null num) (return nil))
	(setq conf (get att 'conflicts))
	(setq buck (store-binding att num))
	(setq clash (find-common-atom buck conf))
	(and clash
	     (%warn '|attributes in a class assigned the same number|
	            (cons att clash)))
        (return nil)))

(defun note-user-vector-assigns (att given needed)
  (and (> needed given)
       (%warn '|vector attribute assigned too small a value in literal| att)))

(defun assign-scalars (class)
  (mapc (function assign-scalars2) (get class 'att-list))) 

(defun assign-scalars2 (att)
  (prog (tlist num bucket conf)
        (and (literal-binding-of att) (return nil))
        (and (is-vector-attribute att) (return nil))
        (setq tlist (buckets))
        (setq conf (get att 'conflicts))
   top  (cond ((atom tlist)
               (%warn '|could not generate a binding| att)
               (store-binding att -1.)
               (return nil)))
        (setq num (caar tlist))
        (setq bucket (cdar tlist))
        (setq tlist (cdr tlist))
        (cond ((disjoint bucket conf) (store-binding att num))
        (t (go top))))) 

(defun assign-vectors (class)
  (mapc (function assign-vectors2) (get class 'att-list))) 

(defun assign-vectors2 (att)
  (prog (big conf new old need)
        (and (not (is-vector-attribute att)) (return nil))
        (setq big 1.)
        (setq conf (get att 'conflicts))
   top  (cond ((not (atom conf))
               (setq new (car conf))
               (setq conf (cdr conf))
               (cond ((is-vector-attribute new)
                      (%warn '|class has two vector attributes|
		              (list att new)))
                     (t (setq big (max (literal-binding-of new) big))))
               (go top)))
        (setq need (1+ big))
	(setq old (literal-binding-of att))
	(cond (old (note-user-vector-assigns att old need))
	      (t (store-binding att need)))
        (return nil)))

(defun disjoint (la lb) (not (find-common-atom la lb))) 

(defun find-common-atom (la lb)
  (prog nil
   top  (cond ((null la) (return nil))
              ((member (car la) lb :test #'eq) (return (car la)))
              (t (setq la (cdr la)) (go top))))) 

(defun mark-conflicts (rem all)
  (cond ((not (null rem))
         (mark-conflicts2 (car rem) all)
         (mark-conflicts (cdr rem) all)))) 

(defun mark-conflicts2 (atm lst)
  (prog (l)
        (setq l lst)
   top  (and (atom l) (return nil))
        (conflict atm (car l))
        (setq l (cdr l))
        (go top))) 

(defun conflict (a b)
  (prog (old)
    (setq old (get a 'conflicts))
    (and (not (eq a b))
         (not (member b old :test #'eq))
         (putprop a (cons b old) 'conflicts )))) 

;(defun remove-duplicates (lst)
;  (cond ((atom lst) nil)
;        ((member (car lst) (cdr lst) :test #'eq) (remove-duplicates (cdr lst)))
;        (t (cons (car lst) (remove-duplicates (cdr lst)))))) 

(defun literal-binding-of (name) (get name 'ops-bind)) 

(defun store-binding (name lit)
  (putprop name lit 'ops-bind)
  (add-bucket name lit)) 

(defun add-bucket (name num)
  (prog (buc)
    (setq buc (assoc num (buckets)))
    (and (not (member name buc :test #'eq))
         (rplacd buc (cons name (cdr buc))))
    (return buc))) 

(defun buckets nil
  (and (atom *buckets*) (setq *buckets* (make-nums *buckets*)))
  *buckets*) 

(defun make-nums (k)
  (prog (nums)
        (setq nums nil)
   l    (and (< k 2.) (return nums))
        (setq nums (cons (cons k nil) nums))
        (setq k (1- k))
        (go l))) 

;(defun erase-literal-info (class)
;  (mapc (function erase-literal-info2) (get class 'att-list))
;  (remprop class 'att-list)) 

; modified to record literal info in the variable *literals*
(defun erase-literal-info (class)
      (setq *literals*
            (cons (cons class (get class 'att-list)) *literals*))
      (mapc (function erase-literal-info2) (get class 'att-list))
      (remprop class 'att-list))


(defun erase-literal-info2 (att) (remprop att 'conflicts)) 


;;; LHS Compiler

(defmacro p (&rest z) 
 `(progn 
   (finish-literalize)
   (princ '*) 
  ;(drain);drain probably drains a line feed
   (compile-production (car ',z) (cdr ',z)))) 

(defun compile-production (name matrix)
  (prog (erm)
        (setq *p-name* name)
        (setq erm (catch '!error! (cmp-p name matrix) ))
	; following line is modified to save production name on *pnames*
        (and (null erm) (setq *pnames* (enter name *pnames*)))
	(setq *p-name* nil)
	(return erm)))

(defun peek-lex nil (car *matrix*)) 

(defun lex nil
  (prog2 nil (car *matrix*) (setq *matrix* (cdr *matrix*)))) 

(defun end-of-p nil (atom *matrix*)) 

(defun rest-of-p nil *matrix*) 

(defun prepare-lex (prod) (setq *matrix* prod)) 


(defun peek-sublex nil (car *curcond*)) 

(defun sublex nil
  (prog2 nil (car *curcond*) (setq *curcond* (cdr *curcond*)))) 

(defun end-of-ce nil (atom *curcond*)) 

(defun rest-of-ce nil *curcond*) 

(defun prepare-sublex (ce) (setq *curcond* ce)) 

(defun make-bottom-node nil (setq *first-node* (list '&bus nil))) 

(defun cmp-p (name matrix)
  (prog (m bakptrs)
        (cond ((or (null name) (listp name))
               (%error '|illegal production name| name))
              ((equal (get name 'production) matrix)
	       (return nil)))
        (prepare-lex matrix)
        (excise-p name)
        (setq bakptrs nil)
        (setq *pcount* (1+ *pcount*))
        (setq *feature-count* 0.)
	(setq *ce-count* 0)
        (setq *vars* nil)
        (setq *ce-vars* nil)
	(setq *rhs-bound-vars* nil)
	(setq *rhs-bound-ce-vars* nil)
        (setq *last-branch* nil)
        (setq m (rest-of-p))
   l1   (and (end-of-p) (%error '|no '-->' in production| m))
        (cmp-prin)
        (setq bakptrs (cons *last-branch* bakptrs))
        (or (eq '--> (peek-lex)) (go l1))
        (lex)
	(check-rhs (rest-of-p))
        (link-new-node (list '&p
                             *feature-count*
                             name
                             (encode-dope)
                             (encode-ce-dope)
                             (cons 'progn (rest-of-p))))
        (putprop name (cdr (nreverse bakptrs)) 'backpointers )
	(putprop name matrix 'production)
        (putprop name *last-node* 'topnode))) 

(defun rating-part (pnode) (cadr pnode)) 

(defun var-part (pnode) (car (cdddr pnode))) 

(defun ce-var-part (pnode) (cadr (cdddr pnode))) 

(defun rhs-part (pnode) (caddr (cdddr pnode))) 

(defun excise-p (name)
  (cond ((and (symbolp name) (get name 'topnode))
	 (printline (list name 'is 'excised))
         (setq *pcount* (1- *pcount*))
         (remove-from-conflict-set name)
         (kill-node (get name 'topnode))
         (setq *pnames* (delete name *pnames* :test #'eq))
	 (remprop name 'production)
	 (remprop name 'backpointers)
         (remprop name 'topnode)))) 

(defun kill-node (node)
  (prog nil
   top  (and (atom node) (return nil))
        (rplaca node '&old)
        (setq node (cdr node))
        (go top))) 

(defun cmp-prin nil
  (prog nil
        (setq *last-node* *first-node*)
        (cond ((null *last-branch*) (cmp-posce) (cmp-nobeta))
              ((eq (peek-lex) '-) (cmp-negce) (cmp-not))
              (t (cmp-posce) (cmp-and))))) 

(defun cmp-negce nil (lex) (cmp-ce)) 

(defun cmp-posce nil
  (setq *ce-count* (1+ *ce-count*))
  (cond ((eq (peek-lex) #\{) (cmp-ce+cevar))
        (t (cmp-ce)))) 

(defun cmp-ce+cevar nil
  (prog (z)
        (lex)
        (cond ((atom (peek-lex)) (cmp-cevar) (cmp-ce))
              (t (cmp-ce) (cmp-cevar)))
        (setq z (lex))
        (or (eq z #\}) (%error '|missing '}'| z)))) 

(defun new-subnum (k)
  (or (numberp k) (%error '|tab must be a number| k))
  (setq *subnum* (round k))) 

(defun incr-subnum nil (setq *subnum* (1+ *subnum*))) 

(defun cmp-ce nil
  (prog (z)
        (new-subnum 0.)
        (setq *cur-vars* nil)
        (setq z (lex))
        (and (atom z)
             (%error '|atomic conditions are not allowed| z))
        (prepare-sublex z)
   la   (and (end-of-ce) (return nil))
        (incr-subnum)
        (cmp-element)
        (go la))) 

(defun cmp-element nil
        (and (eq (peek-sublex) #\^) (cmp-tab))
        (cond ((eq (peek-sublex) '#\{) (cmp-product))
              (t (cmp-atomic-or-any))))

(defun cmp-atomic-or-any nil
        (cond ((eq (peek-sublex) '<<) (cmp-any))
              (t (cmp-atomic))))

(defun cmp-any nil
  (prog (a z)
        (sublex)
        (setq z nil)
   la   (cond ((end-of-ce) (%error '|missing '>>'| a)))
        (setq a (sublex))
        (cond ((not (eq '>> a)) (setq z (cons a z)) (go la)))
        (link-new-node (list '&any nil (current-field) z)))) 


(defun cmp-tab nil
  (prog (r)
        (sublex)
        (setq r (sublex))
        (setq r ($litbind r))
        (new-subnum r))) 

(defun $litbind (x)
  (prog (r)
        (cond ((and (symbolp x) (setq r (literal-binding-of x)))
               (return r))
              (t (return x))))) 

(defun get-bind (x)
  (prog (r)
        (cond ((and (symbolp x) (setq r (literal-binding-of x)))
               (return r))
              (t (return nil))))) 

(defun cmp-atomic nil
  (prog (test x)
        (setq x (peek-sublex))
        (cond ((eq x '=) (setq test 'eq) (sublex))
              ((eq x '<>) (setq test 'ne) (sublex))
              ((eq x '<) (setq test 'lt) (sublex))
              ((eq x '<=) (setq test 'le) (sublex))
              ((eq x '>) (setq test 'gt) (sublex))
              ((eq x '>=) (setq test 'ge) (sublex))
              ((eq x '<=>) (setq test 'xx) (sublex))
              (t (setq test 'eq)))
        (cmp-symbol test))) 

(defun cmp-product nil
  (prog (save)
        (setq save (rest-of-ce))
        (sublex)
   la   (cond ((end-of-ce)
               (cond ((member #\} save) 
		      (%error '|wrong contex for '}'| save))
		     (t (%error '|missing '}'| save))))
              ((eq (peek-sublex) #\}) (sublex) (return nil)))
        (cmp-atomic-or-any)
        (go la))) 

(defun variablep (x) (and (symbolp x) (char-equal (char (symbol-name x) 0) #\<))) 

(defun cmp-symbol (test)
  (prog (flag)
        (setq flag t)
        (cond ((eq (peek-sublex) '//) (sublex) (setq flag nil)))
        (cond ((and flag (variablep (peek-sublex)))
               (cmp-var test))
              ((numberp (peek-sublex)) (cmp-number test))
              ((symbolp (peek-sublex)) (cmp-constant test))
              (t (%error '|unrecognized symbol| (sublex)))))) 

(defun concat3(x y z)
   (intern (format nil "~s~s~s" x y z)))

(defun cmp-constant (test)
  (or (member test '(eq ne xx) )
      (%error '|non-numeric constant after numeric predicate| (sublex)))
  (link-new-node (list (concat3 't test 'a)
                       nil
                       (current-field)
                       (sublex)))) 


(defun cmp-number (test)
  (link-new-node (list (concat3 't test 'n)
                       nil
                       (current-field)
                       (sublex)))) 

(defun current-field nil (field-name *subnum*)) 

(defun field-name (num)
  (cond ((= num 1.) '*c1*)
        ((= num 2.) '*c2*)
        ((= num 3.) '*c3*)
        ((= num 4.) '*c4*)
        ((= num 5.) '*c5*)
        ((= num 6.) '*c6*)
        ((= num 7.) '*c7*)
        ((= num 8.) '*c8*)
        ((= num 9.) '*c9*)
        ((= num 10.) '*c10*)
        ((= num 11.) '*c11*)
        ((= num 12.) '*c12*)
        ((= num 13.) '*c13*)
        ((= num 14.) '*c14*)
        ((= num 15.) '*c15*)
        ((= num 16.) '*c16*)
        ((= num 17.) '*c17*)
        ((= num 18.) '*c18*)
        ((= num 19.) '*c19*)
        ((= num 20.) '*c20*)
        ((= num 21.) '*c21*)
        ((= num 22.) '*c22*)
        ((= num 23.) '*c23*)
        ((= num 24.) '*c24*)
        ((= num 25.) '*c25*)
        ((= num 26.) '*c26*)
        ((= num 27.) '*c27*)
        ((= num 28.) '*c28*)
        ((= num 29.) '*c29*)
        ((= num 30.) '*c30*)
        ((= num 31.) '*c31*)
        ((= num 32.) '*c32*)
        ((= num 33.) '*c33*)
        ((= num 34.) '*c34*)
        ((= num 35.) '*c35*)
        ((= num 36.) '*c36*)
        ((= num 37.) '*c37*)
        ((= num 38.) '*c38*)
        ((= num 39.) '*c39*)
        ((= num 40.) '*c40*)
        ((= num 41.) '*c41*)
        ((= num 42.) '*c42*)
        ((= num 43.) '*c43*)
        ((= num 44.) '*c44*)
        ((= num 45.) '*c45*)
        ((= num 46.) '*c46*)
        ((= num 47.) '*c47*)
        ((= num 48.) '*c48*)
        ((= num 49.) '*c49*)
        ((= num 50.) '*c50*)
        ((= num 51.) '*c51*)
        ((= num 52.) '*c52*)
        ((= num 53.) '*c53*)
        ((= num 54.) '*c54*)
        ((= num 55.) '*c55*)
        ((= num 56.) '*c56*)
        ((= num 57.) '*c57*)
        ((= num 58.) '*c58*)
        ((= num 59.) '*c59*)
        ((= num 60.) '*c60*)
        ((= num 61.) '*c61*)
        ((= num 62.) '*c62*)
        ((= num 63.) '*c63*)
        ((= num 64.) '*c64*)
        (t (%error '|condition is too long| (rest-of-ce))))) 


;;; Compiling variables
;
;
;
; *cur-vars* are the variables in the condition element currently 
; being compiled.  *vars* are the variables in the earlier condition
; elements.  *ce-vars* are the condition element variables.  note
; that the interpreter will not confuse condition element and regular
; variables even if they have the same name.
;
; *cur-vars* is a list of triples: (name predicate subelement-number)
; eg:		( (<x> eq 3)
;		  (<y> ne 1)
;		  . . . )
;
; *vars* is a list of triples: (name ce-number subelement-number)
; eg:		( (<x> 3 3)
;		  (<y> 1 1)
;		  . . . )
;
; *ce-vars* is a list of pairs: (name ce-number)
; eg:		( (ce1 1)
;		  (<c3> 3)
;		  . . . )

(defun var-dope (var) (assoc var *vars* :test #'eq))

(defun ce-var-dope (var) (assoc var *ce-vars* :test #'eq))

(defun cmp-var (test)
  (prog (old name)
        (setq name (sublex))
        (setq old (assoc name *cur-vars* :test #'eq))
        (cond ((and old (eq (cadr old) 'eq))
               (cmp-old-eq-var test old))
              ((and old (eq test 'eq)) (cmp-new-eq-var name old))
              (t (cmp-new-var name test))))) 

(defun cmp-new-var (name test)
  (setq *cur-vars* (cons (list name test *subnum*) *cur-vars*))) 

(defun cmp-old-eq-var (test old)
  (link-new-node (list (concat3 't test 's)
                       nil
                       (current-field)
                       (field-name (caddr old))))) 

(defun cmp-new-eq-var (name old)
  (prog (pred next)
        (setq *cur-vars* (delete old *cur-vars* :test #'eq))
        (setq next (assoc name *cur-vars* :test #'eq))
        (cond (next (cmp-new-eq-var name next))
              (t (cmp-new-var name 'eq)))
        (setq pred (cadr old))
        (link-new-node (list (concat3 't pred 's)
                             nil
                             (field-name (caddr old))
                             (current-field))))) 

(defun cmp-cevar nil
  (prog (name old)
        (setq name (lex))
        (setq old (assoc name *ce-vars* :test #'eq))
        (and old
             (%error '|condition element variable used twice| name))
        (setq *ce-vars* (cons (list name 0.) *ce-vars*)))) 

(defun cmp-not nil (cmp-beta '&not)) 

(defun cmp-nobeta nil (cmp-beta nil)) 

(defun cmp-and nil (cmp-beta '&and)) 

(defun cmp-beta (kind)
  (prog (tlist vdope vname vpred vpos old)
        (setq tlist nil)
   la   (and (atom *cur-vars*) (go lb))
        (setq vdope (car *cur-vars*))
        (setq *cur-vars* (cdr *cur-vars*))
        (setq vname (car vdope))
        (setq vpred (cadr vdope))
        (setq vpos (caddr vdope))
        (setq old (assoc vname *vars* :test #'eq))
        (cond (old (setq tlist (add-test tlist vdope old)))
              ((not (eq kind '&not)) (promote-var vdope)))
        (go la)
   lb   (and kind (build-beta kind tlist))
        (or (eq kind '&not) (fudge))
        (setq *last-branch* *last-node*))) 

(defun add-test (list new old)
  (prog (ttype lloc rloc)
	(setq *feature-count* (1+ *feature-count*))
        (setq ttype (concat3 't (cadr new) 'b))
        (setq rloc (encode-singleton (caddr new)))
        (setq lloc (encode-pair (cadr old) (caddr old)))
        (return (cons ttype (cons lloc (cons rloc list)))))) 


; the following two functions encode indices so that gelm can
; decode them as fast as possible

(defun encode-pair (a b) (+ (* 10000. (1- a)) (1- b))) 

(defun encode-singleton (a) (1- a)) 

(defun promote-var (dope)
  (prog (vname vpred vpos new)
        (setq vname (car dope))
        (setq vpred (cadr dope))
        (setq vpos (caddr dope))
        (or (eq 'eq vpred)
            (%error '|illegal predicate for first occurrence|
                   (list vname vpred)))
        (setq new (list vname 0. vpos))
        (setq *vars* (cons new *vars*)))) 

(defun fudge nil
  (mapc (function fudge*) *vars*)
  (mapc (function fudge*) *ce-vars*)) 

(defun fudge* (z)
  (prog (a) (setq a (cdr z)) (rplaca a (1+ (car a))))) 

(defun build-beta (type tests)
  (prog (rpred lpred lnode lef)
        (link-new-node (list '&mem nil nil (protomem)))
        (setq rpred *last-node*)
        (cond ((eq type '&and)
               (setq lnode (list '&mem nil nil (protomem))))
              (t (setq lnode (list '&two nil nil))))
        (setq lpred (link-to-branch lnode))
        (cond ((eq type '&and) (setq lef lpred))
              (t (setq lef (protomem))))
        (link-new-beta-node (list type nil lef rpred tests)))) 

(defun protomem nil (list nil)) 

(defun memory-part (mem-node) (car (cadddr mem-node))) 

(defun encode-dope nil
  (prog (r all z k)
        (setq r nil)
        (setq all *vars*)
   la   (and (atom all) (return r))
        (setq z (car all))
        (setq all (cdr all))
        (setq k (encode-pair (cadr z) (caddr z)))
        (setq r (cons (car z) (cons k r)))
        (go la))) 

(defun encode-ce-dope nil
  (prog (r all z k)
        (setq r nil)
        (setq all *ce-vars*)
   la   (and (atom all) (return r))
        (setq z (car all))
        (setq all (cdr all))
        (setq k (cadr z))
        (setq r (cons (car z) (cons k r)))
        (go la))) 



;;; Linking the nodes

(defun link-new-node (r)
  (cond ((not (member (car r) '(&p &mem &two &and &not)))
	 (setq *feature-count* (1+ *feature-count*))))
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-node* (link-left *last-node* r))) 

(defun link-to-branch (r)
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-branch* (link-left *last-branch* r))) 

(defun link-new-beta-node (r)
  (setq *virtual-cnt* (1+ *virtual-cnt*))
  (setq *last-node* (link-both *last-branch* *last-node* r))
  (setq *last-branch* *last-node*)) 

(defun link-left (pred succ)
  (prog (a r)
        (setq a (left-outs pred))
        (setq r (find-equiv-node succ a))
        (and r (return r))
        (setq *real-cnt* (1+ *real-cnt*))
        (attach-left pred succ)
        (return succ))) 

(defun link-both (left right succ)
  (prog (a r)
        (setq a (interq (left-outs left) (right-outs right)))
        (setq r (find-equiv-beta-node succ a))
        (and r (return r))
        (setq *real-cnt* (1+ *real-cnt*))
        (attach-left left succ)
        (attach-right right succ)
        (return succ))) 

(defun attach-right (old new)
  (rplaca (cddr old) (cons new (caddr old)))) 

(defun attach-left (old new)
  (rplaca (cdr old) (cons new (cadr old)))) 

(defun right-outs (node) (caddr node)) 

(defun left-outs (node) (cadr node)) 

(defun find-equiv-node (node list)
  (prog (a)
        (setq a list)
   l1   (cond ((atom a) (return nil))
              ((equiv node (car a)) (return (car a))))
        (setq a (cdr a))
        (go l1))) 

(defun find-equiv-beta-node (node list)
  (prog (a)
        (setq a list)
   l1   (cond ((atom a) (return nil))
              ((beta-equiv node (car a)) (return (car a))))
        (setq a (cdr a))
        (go l1))) 

; do not look at the predecessor fields of beta nodes; they have to be
; identical because of the way the candidate nodes were found

(defun equiv (a b)
  (and (eq (car a) (car b))
       (or (eq (car a) '&mem)
           (eq (car a) '&two)
           (equal (caddr a) (caddr b)))
       (equal (cdddr a) (cdddr b)))) 

(defun beta-equiv (a b)
  (and (eq (car a) (car b))
       (equal (cddddr a) (cddddr b))
       (or (eq (car a) '&and) (equal (caddr a) (caddr b))))) 

; the equivalence tests are set up to consider the contents of
; node memories, so they are ready for the build action

;;; Network interpreter

(defun match (flag wme)
  (sendto flag (list wme) 'left (list *first-node*)))

; note that eval-nodelist is not set up to handle building
; productions.  would have to add something like ops4's build-flag

(defun eval-nodelist (nl)
  (prog nil
   top  (and (not nl) (return nil))
        (setq *sendtocall* nil)
	(setq *last-node* (car nl))
        (apply (caar nl) (cdar nl))
        (setq nl (cdr nl))
        (go top))) 

(defun sendto (flag data side nl)
  (prog nil
   top  (and (not nl) (return nil))
        (setq *side* side)
        (setq *flag-part* flag)
        (setq *data-part* data)
        (setq *sendtocall* t)
	(setq *last-node* (car nl))
        (apply (caar nl) (cdar nl))
        (setq nl (cdr nl))
        (go top))) 

; &bus sets up the registers for the one-input nodes.  note that this
(defun &bus (outs)
  (prog (dp)
        (setq *alpha-flag-part* *flag-part*)
        (setq *alpha-data-part* *data-part*)
        (setq dp (car *data-part*))
        (setq *c1* (car dp))
        (setq dp (cdr dp))
        (setq *c2* (car dp))
        (setq dp (cdr dp))
        (setq *c3* (car dp))
        (setq dp (cdr dp))
        (setq *c4* (car dp))
        (setq dp (cdr dp))
        (setq *c5* (car dp))
        (setq dp (cdr dp))
        (setq *c6* (car dp))
        (setq dp (cdr dp))
        (setq *c7* (car dp))
        (setq dp (cdr dp))
        (setq *c8* (car dp))
        (setq dp (cdr dp))
        (setq *c9* (car dp))
        (setq dp (cdr dp))
        (setq *c10* (car dp))
        (setq dp (cdr dp))
        (setq *c11* (car dp))
        (setq dp (cdr dp))
        (setq *c12* (car dp))
        (setq dp (cdr dp))
        (setq *c13* (car dp))
        (setq dp (cdr dp))
        (setq *c14* (car dp))
        (setq dp (cdr dp))
        (setq *c15* (car dp))
        (setq dp (cdr dp))
        (setq *c16* (car dp))
        (setq dp (cdr dp))
        (setq *c17* (car dp))
        (setq dp (cdr dp))
        (setq *c18* (car dp))
        (setq dp (cdr dp))
        (setq *c19* (car dp))
        (setq dp (cdr dp))
        (setq *c20* (car dp))
        (setq dp (cdr dp))
        (setq *c21* (car dp))
        (setq dp (cdr dp))
        (setq *c22* (car dp))
        (setq dp (cdr dp))
        (setq *c23* (car dp))
        (setq dp (cdr dp))
        (setq *c24* (car dp))
        (setq dp (cdr dp))
        (setq *c25* (car dp))
        (setq dp (cdr dp))
        (setq *c26* (car dp))
        (setq dp (cdr dp))
        (setq *c27* (car dp))
        (setq dp (cdr dp))
        (setq *c28* (car dp))
        (setq dp (cdr dp))
        (setq *c29* (car dp))
        (setq dp (cdr dp))
        (setq *c30* (car dp))
        (setq dp (cdr dp))
        (setq *c31* (car dp))
        (setq dp (cdr dp))
        (setq *c32* (car dp))
        (setq dp (cdr dp))
        (setq *c33* (car dp))
        (setq dp (cdr dp))
        (setq *c34* (car dp))
        (setq dp (cdr dp))
        (setq *c35* (car dp))
        (setq dp (cdr dp))
        (setq *c36* (car dp))
        (setq dp (cdr dp))
        (setq *c37* (car dp))
        (setq dp (cdr dp))
        (setq *c38* (car dp))
        (setq dp (cdr dp))
        (setq *c39* (car dp))
        (setq dp (cdr dp))
        (setq *c40* (car dp))
        (setq dp (cdr dp))
        (setq *c41* (car dp))
        (setq dp (cdr dp))
        (setq *c42* (car dp))
        (setq dp (cdr dp))
        (setq *c43* (car dp))
        (setq dp (cdr dp))
        (setq *c44* (car dp))
        (setq dp (cdr dp))
        (setq *c45* (car dp))
        (setq dp (cdr dp))
        (setq *c46* (car dp))
        (setq dp (cdr dp))
        (setq *c47* (car dp))
        (setq dp (cdr dp))
        (setq *c48* (car dp))
        (setq dp (cdr dp))
        (setq *c49* (car dp))
        (setq dp (cdr dp))
        (setq *c50* (car dp))
        (setq dp (cdr dp))
        (setq *c51* (car dp))
        (setq dp (cdr dp))
        (setq *c52* (car dp))
        (setq dp (cdr dp))
        (setq *c53* (car dp))
        (setq dp (cdr dp))
        (setq *c54* (car dp))
        (setq dp (cdr dp))
        (setq *c55* (car dp))
        (setq dp (cdr dp))
        (setq *c56* (car dp))
        (setq dp (cdr dp))
        (setq *c57* (car dp))
        (setq dp (cdr dp))
        (setq *c58* (car dp))
        (setq dp (cdr dp))
        (setq *c59* (car dp))
        (setq dp (cdr dp))
        (setq *c60* (car dp))
        (setq dp (cdr dp))
        (setq *c61* (car dp))
        (setq dp (cdr dp))
        (setq *c62* (car dp))
        (setq dp (cdr dp))
        (setq *c63* (car dp))
        (setq dp (cdr dp))
        (setq *c64* (car dp))
        (eval-nodelist outs))) 

(defun &any (outs register const-list)
  (prog (z c)
        (setq z (fast-symeval register))
        (cond ((numberp z) (go number)))
   symbol (cond ((null const-list) (return nil))
                ((eq (car const-list) z) (go ok))
                (t (setq const-list (cdr const-list)) (go symbol)))
   number (cond ((null const-list) (return nil))
                ((and (numberp (setq c (car const-list)))
                      (=alg c z))
                 (go ok))
                (t (setq const-list (cdr const-list)) (go number)))
   ok   (eval-nodelist outs))) 

(defun teqa (outs register constant)
  (and (eq (fast-symeval register) constant) (eval-nodelist outs))) 

(defun tnea (outs register constant)
  (and (not (eq (fast-symeval register) constant)) (eval-nodelist outs))) 

(defun txxa (outs register constant)
  (and (symbolp (fast-symeval register)) (eval-nodelist outs))) 

(defun teqn (outs register constant)
  (prog (z)
        (setq z (fast-symeval register))
        (and (numberp z)
             (=alg z constant)
             (eval-nodelist outs)))) 

(defun tnen (outs register constant)
  (prog (z)
        (setq z (fast-symeval register))
        (and (or (not (numberp z))
                 (not (=alg z constant)))
             (eval-nodelist outs)))) 

(defun txxn (outs register constant)
  (prog (z)
        (setq z (fast-symeval register))
        (and (numberp z) (eval-nodelist outs)))) 

(defun tltn (outs register constant)
  (prog (z)
        (setq z (fast-symeval register))
        (and (numberp z)
             (greaterp constant z)
             (eval-nodelist outs)))) 

(defun tgtn (outs register constant)
  (prog (z)
        (setq z (fast-symeval register))
        (and (numberp z)
             (greaterp z constant)
             (eval-nodelist outs)))) 

(defun tgen (outs register constant)
  (prog (z)
        (setq z (fast-symeval register))
        (and (numberp z)
             (not (greaterp constant z))
             (eval-nodelist outs)))) 

(defun tlen (outs register constant)
  (prog (z)
        (setq z (fast-symeval register))
        (and (numberp z)
             (not (greaterp z constant))
             (eval-nodelist outs)))) 

(defun teqs (outs vara varb)
  (prog (a b)
        (setq a (fast-symeval vara))
        (setq b (fast-symeval varb))
        (cond ((eq a b) (eval-nodelist outs))
              ((and (numberp a)
                    (numberp b)
                    (=alg a b))
               (eval-nodelist outs))))) 

(defun tnes (outs vara varb)
  (prog (a b)
        (setq a (fast-symeval vara))
        (setq b (fast-symeval varb))
        (cond ((eq a b) (return nil))
              ((and (numberp a)
                    (numberp b)
                    (=alg a b))
               (return nil))
              (t (eval-nodelist outs))))) 

(defun txxs (outs vara varb)
  (prog (a b)
        (setq a (fast-symeval vara))
        (setq b (fast-symeval varb))
        (cond ((and (numberp a) (numberp b)) (eval-nodelist outs))
              ((and (not (numberp a)) (not (numberp b)))
               (eval-nodelist outs))))) 

(defun tlts (outs vara varb)
  (prog (a b)
        (setq a (fast-symeval vara))
        (setq b (fast-symeval varb))
        (and (numberp a)
             (numberp b)
             (greaterp b a)
             (eval-nodelist outs)))) 

(defun tgts (outs vara varb)
  (prog (a b)
        (setq a (fast-symeval vara))
        (setq b (fast-symeval varb))
        (and (numberp a)
             (numberp b)
             (greaterp a b)
             (eval-nodelist outs)))) 

(defun tges (outs vara varb)
  (prog (a b)
        (setq a (fast-symeval vara))
        (setq b (fast-symeval varb))
        (and (numberp a)
             (numberp b)
             (not (greaterp b a))
             (eval-nodelist outs)))) 

(defun tles (outs vara varb)
  (prog (a b)
        (setq a (fast-symeval vara))
        (setq b (fast-symeval varb))
        (and (numberp a)
             (numberp b)
             (not (greaterp a b))
             (eval-nodelist outs)))) 

(defun &two (left-outs right-outs)
  (prog (fp dp)
        (cond (*sendtocall*
               (setq fp *flag-part*)
               (setq dp *data-part*))
              (t
               (setq fp *alpha-flag-part*)
               (setq dp *alpha-data-part*)))
        (sendto fp dp 'left left-outs)
        (sendto fp dp 'right right-outs))) 

(defun &mem (left-outs right-outs memory-list)
  (prog (fp dp)
        (cond (*sendtocall*
               (setq fp *flag-part*)
               (setq dp *data-part*))
              (t
               (setq fp *alpha-flag-part*)
               (setq dp *alpha-data-part*)))
        (sendto fp dp 'left left-outs)
        (add-token memory-list fp dp nil)
        (sendto fp dp 'right right-outs))) 

(defun &and (outs lpred rpred tests)
  (prog (mem)
        (cond ((eq *side* 'right) (setq mem (memory-part lpred)))
              (t (setq mem (memory-part rpred))))
        (cond ((not mem) (return nil))
              ((eq *side* 'right) (and-right outs mem tests))
              (t (and-left outs mem tests))))) 

(defun and-left (outs mem tests)
  (prog (fp dp memdp tlist tst lind rind res)
        (setq fp *flag-part*)
        (setq dp *data-part*)
   fail (and (null mem) (return nil))
        (setq memdp (car mem))
        (setq mem (cdr mem))
        (setq tlist tests)
   tloop (and (null tlist) (go succ))
        (setq tst (car tlist))
        (setq tlist (cdr tlist))
        (setq lind (car tlist))
        (setq tlist (cdr tlist))
        (setq rind (car tlist))
        (setq tlist (cdr tlist))
        ;the next line differs in and-left & -right
        (setq res (funcall tst (gelm memdp rind) (gelm dp lind)))
        (cond (res (go tloop))
              (t (go fail)))
   succ ;the next line differs in and-left & -right
        (sendto fp (cons (car memdp) dp) 'left outs)
        (go fail))) 

(defun and-right (outs mem tests)
  (prog (fp dp memdp tlist tst lind rind res)
        (setq fp *flag-part*)
        (setq dp *data-part*)
   fail (and (null mem) (return nil))
        (setq memdp (car mem))
        (setq mem (cdr mem))
        (setq tlist tests)
   tloop (and (null tlist) (go succ))
        (setq tst (car tlist))
        (setq tlist (cdr tlist))
        (setq lind (car tlist))
        (setq tlist (cdr tlist))
        (setq rind (car tlist))
        (setq tlist (cdr tlist))
        ;the next line differs in and-left & -right
        (setq res (funcall tst (gelm dp rind) (gelm memdp lind)))
        (cond (res (go tloop))
              (t (go fail)))
   succ ;the next line differs in and-left & -right
        (sendto fp (cons (car dp) memdp) 'right outs)
        (go fail))) 


(defun teqb (new eqvar)
  (cond ((eq new eqvar) t)
        ((not (numberp new)) nil)
        ((not (numberp eqvar)) nil)
        ((=alg new eqvar) t)
        (t nil))) 

(defun tneb (new eqvar)
  (cond ((eq new eqvar) nil)
        ((not (numberp new)) t)
        ((not (numberp eqvar)) t)
        ((=alg new eqvar) nil)
        (t t))) 

(defun tltb (new eqvar)
  (cond ((not (numberp new)) nil)
        ((not (numberp eqvar)) nil)
        ((greaterp eqvar new) t)
        (t nil))) 

(defun tgtb (new eqvar)
  (cond ((not (numberp new)) nil)
        ((not (numberp eqvar)) nil)
        ((greaterp new eqvar) t)
        (t nil))) 

(defun tgeb (new eqvar)
  (cond ((not (numberp new)) nil)
        ((not (numberp eqvar)) nil)
        ((not (greaterp eqvar new)) t)
        (t nil))) 

(defun tleb (new eqvar)
  (cond ((not (numberp new)) nil)
        ((not (numberp eqvar)) nil)
        ((not (greaterp new eqvar)) t)
        (t nil))) 

(defun txxb (new eqvar)
  (cond ((numberp new)
         (cond ((numberp eqvar) t)
               (t nil)))
        (t
         (cond ((numberp eqvar) nil)
               (t t))))) 


(defun &p (rating name var-dope ce-var-dope rhs)
  (prog (fp dp)
        (cond (*sendtocall*
               (setq fp *flag-part*)
               (setq dp *data-part*))
              (t
               (setq fp *alpha-flag-part*)
               (setq dp *alpha-data-part*)))
        (and (member fp '(nil old)) (removecs name dp))
        (and fp (insertcs name dp rating)))) 

(defun &old (a b c d e) nil) ;a null function used for deleting node

(defun &not (outs lmem rpred tests)
  (cond ((and (eq *side* 'right) (eq *flag-part* 'old)) nil)
        ((eq *side* 'right) (not-right outs (car lmem) tests))
        (t (not-left outs (memory-part rpred) tests lmem)))) 

(defun not-left (outs mem tests own-mem)
  (prog (fp dp memdp tlist tst lind rind res c)
        (setq fp *flag-part*)
        (setq dp *data-part*)
        (setq c 0.)
   fail (and (null mem) (go fin))
        (setq memdp (car mem))
        (setq mem (cdr mem))
        (setq tlist tests)
   tloop (and (null tlist) (setq c (1+ c)) (go fail))
        (setq tst (car tlist))
        (setq tlist (cdr tlist))
        (setq lind (car tlist))
        (setq tlist (cdr tlist))
        (setq rind (car tlist))
        (setq tlist (cdr tlist))
        ;the next line differs in not-left & -right
        (setq res (funcall tst (gelm memdp rind) (gelm dp lind)))
        (cond (res (go tloop))
              (t (go fail)))
   fin  (add-token own-mem fp dp c)
        (and (== c 0.) (sendto fp dp 'left outs)))) 

(defun not-right (outs mem tests)
  (prog (fp dp memdp tlist tst lind rind res newfp inc newc)
        (setq fp *flag-part*)
        (setq dp *data-part*)
        (cond ((not fp) (setq inc -1.) (setq newfp 'new))
              ((eq fp 'new) (setq inc 1.) (setq newfp nil))
              (t (return nil)))
   fail (and (null mem) (return nil))
        (setq memdp (car mem))
        (setq newc (cadr mem))
        (setq tlist tests)
   tloop (and (null tlist) (go succ))
        (setq tst (car tlist))
        (setq tlist (cdr tlist))
        (setq lind (car tlist))
        (setq tlist (cdr tlist))
        (setq rind (car tlist))
        (setq tlist (cdr tlist))
        ;the next line differs in not-left & -right
        (setq res (funcall tst (gelm dp rind) (gelm memdp lind)))
        (cond (res (go tloop))
              (t (setq mem (cddr mem)) (go fail)))
   succ (setq newc (+ inc newc))
        (rplaca (cdr mem) newc)
        (cond ((or (and (== inc -1.) (== newc 0.))
                   (and (== inc 1.) (== newc 1.)))
               (sendto newfp memdp 'right outs)))
        (setq mem (cddr mem))
        (go fail))) 



;;; Node memories


(defun add-token (memlis flag data-part num)
  (prog (was-present)
        (cond ((eq flag 'new)
               (setq was-present nil)
               (real-add-token memlis data-part num))
              ((not flag) 
	       (setq was-present (remove-old memlis data-part num)))
              ((eq flag 'old) (setq was-present t)))
        (return was-present))) 

(defun real-add-token (lis data-part num)
  (setq *current-token* (1+ *current-token*))
  (cond (num (rplaca lis (cons num (car lis)))))
  (rplaca lis (cons data-part (car lis)))) 

(defun remove-old (lis data num)
  (cond (num (remove-old-num lis data))
        (t (remove-old-no-num lis data)))) 

(defun remove-old-num (lis data)
  (prog (m next last)
        (setq m (car lis))
        (cond ((atom m) (return nil))
              ((top-levels-eq data (car m))
               (setq *current-token* (1- *current-token*))
               (rplaca lis (cddr m))
               (return (car m))))
        (setq next m)
   loop (setq last next)
        (setq next (cddr next))
        (cond ((atom next) (return nil))
              ((top-levels-eq data (car next))
               (rplacd (cdr last) (cddr next))
               (setq *current-token* (1- *current-token*))
               (return (car next)))
              (t (go loop))))) 

(defun remove-old-no-num (lis data)
  (prog (m next last)
        (setq m (car lis))
        (cond ((atom m) (return nil))
              ((top-levels-eq data (car m))
               (setq *current-token* (1- *current-token*))
               (rplaca lis (cdr m))
               (return (car m))))
        (setq next m)
   loop (setq last next)
        (setq next (cdr next))
        (cond ((atom next) (return nil))
              ((top-levels-eq data (car next))
               (rplacd last (cdr next))
               (setq *current-token* (1- *current-token*))
               (return (car next)))
              (t (go loop))))) 



;;; Conflict Resolution
;
;
; each conflict set element is a list of the following form:
; ((p-name . data-part) (sorted wm-recency) special-case-number)

(defun removecs (name data)
  (prog (cr-data inst cs)
        (setq cr-data (cons name data))
	(setq cs *conflict-set*)
 loop1	(cond ((null cs) 
               (record-refract name data)
               (return nil)))
	(setq inst (car cs))
	(setq cs (cdr cs))
	(and (not (top-levels-eq (car inst) cr-data)) (go loop1))
        (setq *conflict-set* (delete inst *conflict-set* :test #'eq))))

(defun insertcs (name data rating)
  (prog (instan)
    (and (refracted name data) (return nil))
    (setq instan (list (cons name data) (order-tags data) rating))
    (and (atom *conflict-set*) (setq *conflict-set* nil))
    (return (setq *conflict-set* (cons instan *conflict-set*))))) 

(defun order-tags (dat)
  (prog (tags)
        (setq tags nil)
   l1  (and (atom dat) (go l2))
        (setq tags (cons (creation-time (car dat)) tags))
        (setq dat (cdr dat))
        (go l1)
   l2  (cond ((eq *strategy* 'mea)
               (return (cons (car tags) (dsort (cdr tags)))))
              (t (return (dsort tags)))))) 

; destructively sort x into descending order

(defun dsort (x)
  (prog (sorted cur next cval nval)
        (and (atom (cdr x)) (return x))
   loop (setq sorted t)
        (setq cur x)
        (setq next (cdr x))
   chek (setq cval (car cur))
        (setq nval (car next))
        (cond ((> nval cval)
               (setq sorted nil)
               (rplaca cur nval)
               (rplaca next cval)))
        (setq cur next)
        (setq next (cdr cur))
        (cond ((not (null next)) (go chek))
              (sorted (return x))
              (t (go loop))))) 

(defun conflict-resolution nil
  (prog (best len)
        (setq len (length *conflict-set*))
        (cond ((> len *max-cs*) (setq *max-cs* len)))
        (setq *total-cs* (+ *total-cs* len))
        (cond (*conflict-set*
               (setq best (best-of *conflict-set*))
               (setq *conflict-set* (delete best *conflict-set* :test #'eq))
               (return (pname-instantiation best)))
              (t (return nil))))) 

(defun best-of (set) (best-of* (car set) (cdr set))) 

(defun best-of* (best rem)
  (cond ((not rem) best)
        ((conflict-set-compare best (car rem))
         (best-of* best (cdr rem)))
        (t (best-of* (car rem) (cdr rem))))) 

(defun remove-from-conflict-set (name)
  (prog (cs entry)
   l1   (setq cs *conflict-set*)
   l2   (cond ((atom cs) (return nil)))
        (setq entry (car cs))
        (setq cs (cdr cs))
        (cond ((eq name (caar entry))
               (setq *conflict-set* (delete entry *conflict-set* :test #'eq))
               (go l1))
              (t (go l2))))) 

(defun pname-instantiation (conflict-elem) (car conflict-elem)) 

(defun order-part (conflict-elem) (cdr conflict-elem)) 

(defun instantiation (conflict-elem)
  (cdr (pname-instantiation conflict-elem))) 


(defun conflict-set-compare (x y)
  (prog (x-order y-order xl yl xv yv)
        (setq x-order (order-part x))
        (setq y-order (order-part y))
        (setq xl (car x-order))
        (setq yl (car y-order))
   data (cond ((and (null xl) (null yl)) (go ps))
              ((null yl) (return t))
              ((null xl) (return nil)))
        (setq xv (car xl))
        (setq yv (car yl))
        (cond ((> xv yv) (return t))
              ((> yv xv) (return nil)))
        (setq xl (cdr xl))
        (setq yl (cdr yl))
        (go data)
   ps   (setq xl (cdr x-order))
        (setq yl (cdr y-order))
   psl  (cond ((null xl) (return t)))
        (setq xv (car xl))
        (setq yv (car yl))
        (cond ((> xv yv) (return t))
              ((> yv xv) (return nil)))
        (setq xl (cdr xl))
        (setq yl (cdr yl))
        (go psl))) 


(defun conflict-set nil
  (prog (cnts cs p z best)
        (setq cnts nil)
        (setq cs *conflict-set*)
   l1  (and (atom cs) (go l2))
        (setq p (caaar cs))
        (setq cs (cdr cs))
        (setq z (assoc p cnts :test #'eq))
        (cond ((null z) (setq cnts (cons (cons p 1.) cnts)))
              (t (rplacd z (1+ (cdr z)))))
        (go l1)
   l2  (cond ((atom cnts)
               (setq best (best-of *conflict-set*))
               (terpri)
               (return (list (caar best) 'dominates))))
        (terpri)
        (princ (caar cnts))
        (cond ((> (cdar cnts) 1.)
               (princ '|	(|)
               (princ (cdar cnts))
               (princ '| occurrences)|)))
        (setq cnts (cdr cnts))
        (go l2))) 


;;; WM maintaining functions
;
; The order of operations in the following two functions is critical.
; add-to-wm order: (1) change wm (2) record change (3) match 
; remove-from-wm order: (1) record change (2) match (3) change wm
; (back will not restore state properly unless wm changes are recorded
; before the cs changes that they cause)  (match will give errors if 
; the thing matched is not in wm at the time)


(defun add-to-wm (wme override)
  (prog (fa z part timetag port)
    (setq *critical* t)
    (setq *current-wm* (1+ *current-wm*))
    (and (> *current-wm* *max-wm*) (setq *max-wm* *current-wm*))
    (setq *action-count* (1+ *action-count*))
    (setq fa (wm-hash wme))
    (or (member fa *wmpart-list* :test #'eq)
        (setq *wmpart-list* (cons fa *wmpart-list*)))
    (setq part (get fa 'wmpart*))
    (cond (override (setq timetag override))
          (t (setq timetag *action-count*)))
    (setq z (cons wme timetag))
    (putprop fa (cons z part) 'wmpart*)
    (record-change '=>wm *action-count* wme)
    (match 'new wme)
    (setq *critical* nil)
    (cond ((and *in-rhs* *wtrace*)
           (setq port (trace-file))
           (terpri port)
           (princ '|=>wm: | port)
           (ppelm wme port)))
    (and *in-rhs* *mtrace* (setq *madeby* 
                                 (cons (cons wme *p-name*) *madeby*))))) 

; remove-from-wm uses eq, not equal to determine if wme is present

(defun remove-from-wm (wme)
  (prog (fa z part timetag port)
    (setq fa (wm-hash wme))
    (setq part (get fa 'wmpart*))
    (setq z (assoc wme part :test #'eq))
    (or z (return nil))
    (setq timetag (cdr z))
    (cond ((and *wtrace* *in-rhs*)
           (setq port (trace-file))
           (terpri port)
           (princ '|<=wm: | port)
           (ppelm wme port)))
    (setq *action-count* (1+ *action-count*))
    (setq *critical* t)
    (setq *current-wm* (1- *current-wm*))
    (record-change '<=wm timetag wme)
    (match nil wme)
    (putprop fa (delete z part :test #'eq) 'wmpart* )
    (setq *critical* nil))) 

; mapwm maps down the elements of wm, applying fn to each element
; each element is of form (datum . creation-time)

(defun mapwm (fn)
  (prog (wmpl part)
        (setq wmpl *wmpart-list*)
   lab1 (cond ((atom wmpl) (return nil)))
        (setq part (get (car wmpl) 'wmpart*))
        (setq wmpl (cdr wmpl))
        (mapc fn part)
        (go lab1))) 

(defmacro wm (&rest a) 
  `(progn
   (mapc (function (lambda (z) (terpri) (ppelm z t))) 
	(get-wm ',a))
  nil) )

(defun get-wm (z)
  (setq *wm-filter* z)
  (setq *wm* nil)
  (mapwm (function get-wm2))
  (prog2 nil *wm* (setq *wm* nil))) 

(defun get-wm2 (elem) 
 (cond ((or (null *wm-filter*) (member (cdr elem) *wm-filter*))
	(setq *wm* (cons (car elem) *wm*)))))

(defun wm-hash (x)
  (cond ((not x) '<default>)
        ((not (car x)) (wm-hash (cdr x)))
        ((symbolp (car x)) (car x))
        (t (wm-hash (cdr x))))) 

(defun creation-time (wme)
  (cdr (assoc wme (get (wm-hash wme) 'wmpart*) :test #'eq))) 

(defun rehearse nil
  (prog nil
    (setq *old-wm* nil)
    (mapwm (function refresh-collect))
    (mapc (function refresh-del) *old-wm*)
    (mapc (function refresh-add) *old-wm*)
    (setq *old-wm* nil))) 

(defun refresh-collect (x) (setq *old-wm* (cons x *old-wm*))) 

(defun refresh-del (x) (remove-from-wm (car x))) 

(defun refresh-add (x) (add-to-wm (car x) (cdr x))) 

(defun trace-file ()
  (prog (port)
        (setq port t)
	(cond (*trace-file*
	       (setq port ($ofile *trace-file*))
	       (cond ((null port)
	              (%warn '|trace: file has been closed| *trace-file*)
		      (setq port t)))))
        (return port)))


;;; Basic functions for RHS evaluation

(defun eval-rhs (pname data)
  (prog (node port)
    (cond (*ptrace*
           (setq port (trace-file))
           (terpri port)
           (princ *cycle-count* port)
           (princ '|. | port)
           (princ pname port)
           (time-tag-print data port)))
    (setq *data-matched* data)
    (setq *p-name* pname)
    (setq *last* nil)
    (setq node (get pname 'topnode))
    (init-var-mem (var-part node))
    (init-ce-var-mem (ce-var-part node))
    (begin-record pname data)
    (setq *in-rhs* t)
    (eval (rhs-part node))
    (setq *in-rhs* nil)
    (end-record))) 

(defun time-tag-print (data port)
  (cond ((not (null data))
         (time-tag-print (cdr data) port)
         (princ '| | port)
         (princ (creation-time (car data)) port))))

(defun init-var-mem (vlist)
  (prog (v ind r)
        (setq *variable-memory* nil)
   top  (and (atom vlist) (return nil))
        (setq v (car vlist))
        (setq ind (cadr vlist))
        (setq vlist (cddr vlist))
        (setq r (gelm *data-matched* ind))
        (setq *variable-memory* (cons (cons v r) *variable-memory*))
        (go top))) 

(defun init-ce-var-mem (vlist)
  (prog (v ind r)
        (setq *ce-variable-memory* nil)
   top  (and (atom vlist) (return nil))
        (setq v (car vlist))
        (setq ind (cadr vlist))
        (setq vlist (cddr vlist))
        (setq r (ce-gelm *data-matched* ind))
        (setq *ce-variable-memory*
              (cons (cons v r) *ce-variable-memory*))
        (go top))) 

(defun make-ce-var-bind (var elem)
  (setq *ce-variable-memory*
        (cons (cons var elem) *ce-variable-memory*))) 

(defun make-var-bind (var elem)
  (setq *variable-memory* (cons (cons var elem) *variable-memory*))) 

(defun $varbind (x)
  (prog (r)
	(and (not *in-rhs*) (return x))
        (setq r (assoc x *variable-memory* :test #'eq))
        (cond (r (return (cdr r)))
              (t (return x))))) 

(defun get-ce-var-bind (x)
  (prog (r)
        (cond ((numberp x) (return (get-num-ce x))))
        (setq r (assoc x *ce-variable-memory* :test #'eq))
        (cond (r (return (cdr r)))
              (t (return nil))))) 

(defun get-num-ce (x)
  (prog (r l d)
        (setq r *data-matched*)
        (setq l (length r))
        (setq d (- l x))
        (and (> 0. d) (return nil))
   la   (cond ((null r) (return nil))
              ((> 1. d) (return (car r))))
        (setq d (1- d))
        (setq r (cdr r))
        (go la))) 


(defun build-collect (z)
  (prog (r)
   la   (and (atom z) (return nil))
        (setq r (car z))
        (setq z (cdr z))
        (cond ((and r (listp r))
               ($value '\()
               (build-collect r)
               ($value '\)))
              ((eq r '\\) ($change (car z)) (setq z (cdr z)))
              (t ($value r)))
        (go la))) 

(defun unflat (x) (setq *rest* x) (unflat*)) 

(defun unflat* nil
  (prog (c)
        (cond ((atom *rest*) (return nil)))
        (setq c (car *rest*))
        (setq *rest* (cdr *rest*))
        (cond ((eq c '\() (return (cons (unflat*) (unflat*))))
              ((eq c '\)) (return nil))
              (t (return (cons c (unflat*))))))) 


(defun $change (x)
  (prog nil
        (cond ((and x (listp x)) (eval-function x)) ;modified to check for nil
              (t ($value ($varbind x)))))) 

(defun eval-args (z)
  (prog (r)
        (rhs-tab 1.)
   la   (and (atom z) (return nil))
        (setq r (car z))
        (setq z (cdr z))
        (cond ((eq r #\^)
               (rhs-tab (car z))
               (setq r (cadr z))
               (setq z (cddr z))))
        (cond ((eq r '//) ($value (car z)) (setq z (cdr z)))
              (t ($change r)))
        (go la))) 


(defun eval-function (form)
  (cond ((not *in-rhs*)
	 (%warn '|functions cannot be used at top level| (car form)))
	(t (eval form))))


;;; Functions to manipulate the result array


(defun $reset nil
  (setq *max-index* 0)
  (setq *next-index* 1)) 

; rhs-tab implements the tab ('^') function in the rhs.  it has
; four responsibilities:
;	- to move the array pointers
;	- to watch for tabbing off the left end of the array
;	  (ie, to watch for pointers less than 1)
;	- to watch for tabbing off the right end of the array
;	- to write nil in all the slots that are skipped
; the last is necessary if the result array is not to be cleared
; after each use; if rhs-tab did not do this, $reset
; would be much slower.

(defun rhs-tab (z) ($tab ($varbind z)))

(defun $tab (z)
  (prog (edge next)
        (setq next ($litbind z))
        (and (floatp next) (setq next (round next)))
        (cond ((or (not (numberp next)) 
		   (> next *size-result-array*)
		   (> 1. next))
               (%warn '|illegal index after ^| next)
               (return *next-index*)))
        (setq edge (- next 1.))
        (cond ((> *max-index* edge) (go ok)))
   clear (cond ((== *max-index* edge) (go ok)))
        (putvector *result-array* edge nil)
        (setq edge (1- edge))
        (go clear)
   ok   (setq *next-index* next)
        (return next))) 

(defun $value (v)
  (cond ((> *next-index* *size-result-array*)
         (%warn '|index too large| *next-index*))
        (t
         (and (> *next-index* *max-index*)
              (setq *max-index* *next-index*))
         (putvector *result-array* *next-index* v)
         (setq *next-index* (1+ *next-index*))))) 

(defun use-result-array nil
  (prog (k r)
        (setq k *max-index*)
        (setq r nil)
   top  (and (== k 0.) (return r))
        (setq r (cons (getvector *result-array* k) r))
        (setq k (1- k))
        (go top))) 

(defun $assert nil
  (setq *last* (use-result-array))
  (add-to-wm *last* nil))

(defun $parametercount nil *max-index*)

(defun $parameter (k)
  (cond ((or (not (numberp k)) (> k *size-result-array*) (< k 1.))
	 (%warn '|illegal parameter number | k)
         nil)
        ((> k *max-index*) nil)
	(t (getvector *result-array* k))))


;;; RHS actions


(defmacro make(&rest z)
  `(prog nil
        ($reset)
        (eval-args ',z)
        ($assert))) 

(defmacro modify (&rest z)
  `(prog (old args)
        (setq args ',z)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'modify)
	       (return nil)))
        (setq old (get-ce-var-bind (car args)))
        (cond ((null old)
               (%warn '|modify: first argument must be an element variable|
                        (car args))
               (return nil)))
        (remove-from-wm old)
        (setq args (cdr args))
        ($reset)
   copy (and (atom old) (go fin))
        ($change (car old))
        (setq old (cdr old))
        (go copy)
   fin  (eval-args args)
        ($assert))) 

(defmacro bind (&rest z)
  `(prog (val)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'bind)
	       (return nil)))
    (cond ((< (length z) 1.)
           (%warn '|bind: wrong number of arguments to| ',z)
           (return nil))
          ((not (symbolp (car ',z)))
           (%warn '|bind: illegal argument| (car ',z))
           (return nil))
          ((= (length ',z) 1.) (setq val (gensym)))
          (t ($reset)
             (eval-args (cdr ',z))
             (setq val ($parameter 1.))))
    (make-var-bind (car ',z) val))) 

(defmacro cbind (&rest z)
  `(cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'cbind))
	((not (= (length ',z) 1.))
	 (%warn '|cbind: wrong number of arguments| ',z))
	((not (symbolp (car ',z)))
	 (%warn '|cbind: illegal argument| (car ',z)))
	((null *last*)
	 (%warn '|cbind: nothing added yet| (car ',z)))
	(t (make-ce-var-bind (car ',z) *last*)))) 

(defmacro oremove (&rest z)
  `(prog (old args)
        (setq args ',z)
	(and (not *in-rhs*)(return (top-level-remove args)))
   top  (and (atom args) (return nil))
        (setq old (get-ce-var-bind (car args)))
        (cond ((null old)
               (%warn '|remove: argument not an element variable| (car args))
               (return nil)))
        (remove-from-wm old)
        (setq args (cdr args))
        (go top))) 

(defmacro ocall (&rest z)
  `(prog (f)
	(setq f (car ',z))
        ($reset)
        (eval-args (cdr ',z))
        (funcall f))) 

(defmacro owrite (&rest z)
 `(prog (port max k x needspace)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'write)
	       (return nil)))
	($reset)
	(eval-args ',z)
	(setq k 1.)
	(setq max ($parametercount))
	(cond ((< max 1.)
	       (%warn '|write: nothing to print| ',z)
	       (return nil)))
	(setq port (default-write-file))
	(setq x ($parameter 1.))
	(cond ((and (symbolp x) ($ofile x)) 
	       (setq port ($ofile x))
	       (setq k 2.)))
        (setq needspace t)
   la   (and (> k max) (return nil))
	(setq x ($parameter k))
	(cond ((eq x '|=== C R L F ===|)
	       (setq needspace nil)
               (terpri port))
              ((eq x '|=== R J U S T ===|)
	       (setq k (+ 2 k))
	       (do-rjust ($parameter (1- k)) ($parameter k) port))
	      ((eq x '|=== T A B T O ===|)
	       (setq needspace nil)
	       (setq k (1+ k))
	       (do-tabto ($parameter k) port))
	      (t 
	       (and needspace (princ '| | port))
	       (setq needspace t)
	       (princ x port)))
	(setq k (1+ k))
	(go la))) 
	
(defun default-write-file ()
  (prog (port)
	(setq port t)
	(cond (*write-file*
	       (setq port ($ofile *write-file*))
	       (cond ((null port) 
		      (%warn '|write: file has been closed| *write-file*)
		      (setq port t)))))
        (return port)))

                                                                                                                                                                                                         
(defun do-rjust (width value port)
  (prog (size)
	(cond ((eq value '|=== T A B T O ===|)
	       (%warn '|rjust cannot precede this function| 'tabto)
               (return nil))
	      ((eq value '|=== C R L F ===|)
	       (%warn '|rjust cannot precede this function| 'crlf)
               (return nil))
	      ((eq value '|=== R J U S T ===|)
	       (%warn '|rjust cannot precede this function| 'rjust)
               (return nil)))
        (setq size (length (princ-to-string value )))
	(cond ((> size width)
	       (princ '| | port)
	       (princ value port)
	       (return nil)))
        (do k (- width size) (1- k) (not (> k 0)) (princ '| | port)) ; ERR ill-formed do
	(princ value port)))

(defun do-tabto (col port)
  (eval `(format ,port (concatenate 'string "~" (princ-to-string ,col) "T"))))

;  (prog (pos)
;	(setq pos (1+ (nwritn port)))
;	(cond ((> pos col)
;	       (terpri port)
;	       (setq pos 1)))
;	(do k (- col pos) (1- k) (not (> k 0)) (princ '| | port))
;	(return nil)))


(defun halt nil 
  (cond ((not *in-rhs*)
	 (%warn '|cannot be called at top level| 'halt))
	(t (setq *halt-flag* t)))) 

(defmacro build (&rest z)
  `(prog (r)
	(cond ((not *in-rhs*)
	       (%warn '|cannot be called at top level| 'build)
	       (return nil)))
        ($reset)
        (build-collect ',z)
        (setq r (unflat (use-result-array)))
        (and *build-trace* (funcall *build-trace* r))
        (compile-production (car r) (cdr r)))) 

(defun infile(file)
   (open file :direction :input))

(defun outfile(file)
   (open file :direction :output))

(defmacro openfile (&rest z)
  `(prog (file mode id)
	($reset)
	(eval-args ',z)
	(cond ((not (equal ($parametercount) 3.))
	       (%warn '|openfile: wrong number of arguments| ',z)
	       (return nil)))
	(setq id ($parameter 1))
	(setq file ($parameter 2))
	(setq mode ($parameter 3))
	(cond ((not (symbolp id))
	       (%warn '|openfile: file id must be a symbolic atom| id)
	       (return nil))
              ((null id)
               (%warn '|openfile: 'nil' is reserved for the terminal| nil)
               (return nil))
	      ((or ($ifile id)($ofile id))
	       (%warn '|openfile: name already in use| id)
	       (return nil)))
	(cond ((eq mode 'in) (putprop id  (infile file) 'inputfile))
	      ((eq mode 'out) (putprop id  (outfile file) 'outputfile))
	      (t (%warn '|openfile: illegal mode| mode)
		 (return nil)))
	(return nil)))

(defun $ifile (x) 
  (cond ((and x (symbolp x)) (get x 'inputfile))
        (t *standard-input*)))
 
(defun $ofile (x) 
  (cond ((and x (symbolp x)) (get x 'outputfile))
        (t *standard-output*)))


(defmacro closefile (&rest z)
  `(progn 
    ($reset)
    (eval-args ',z)
    (mapc (function closefile2) (use-result-array))))

(defun closefile2 (file)
  (prog (port)
	(cond ((not (symbolp file))
	       (%warn '|closefile: illegal file identifier| file))
	      ((setq port ($ifile file))
	       (close port)
	       (remprop file 'inputfile))
	      ((setq port ($ofile file))
	       (close port)
	       (remprop file 'outputfile)))
	(return nil)))

(defmacro default (&rest z)
  `(prog (file use)
	($reset)
	(eval-args ',z)
	(cond ((not (equal ($parametercount) 2.))
	       (%warn '|default: wrong number of arguments| ',z)
	       (return nil)))
	(setq file ($parameter 1))
	(setq use ($parameter 2))
	(cond ((not (symbolp file))
	       (%warn '|default: illegal file identifier| file)
	       (return nil))
	      ((not (member use '(write accept trace)))
	       (%warn '|default: illegal use for a file| use)
	       (return nil))
	      ((and (member use '(write trace)) 
		    (not (null file))
		    (not ($ofile file)))
	       (%warn '|default: file has not been opened for output| file)
	       (return nil))
	      ((and (eq use 'accept) 
		    (not (null file))
		    (not ($ifile file)))
	       (%warn '|default: file has not been opened for input| file)
	       (return nil))
	      ((eq use 'write) (setq *write-file* file))
	      ((eq use 'accept) (setq *accept-file* file))
	      ((eq use 'trace) (setq *trace-file* file)))
	(return nil)))



;;; RHS Functions

(defmacro accept (&rest z)
  `(prog (port arg)
	(cond ((> (length ',z) 1.)
	       (%warn '|accept: wrong number of arguments| ',z)
	       (return nil)))
	(setq port t)
	(cond (*accept-file*
	       (setq port ($ifile *accept-file*))
	       (cond ((null port) 
		      (%warn '|accept: file has been closed| *accept-file*)
		      (return nil)))))
	(cond ((= (length ',z) 1)
	       (setq arg ($varbind (car ',z)))
	       (cond ((not (symbolp arg))
	              (%warn '|accept: illegal file name| arg)
		      (return nil)))
	       (setq port ($ifile arg))
	       (cond ((null port) 
		      (%warn '|accept: file not open for input| arg)
		      (return nil)))))
        (cond ((= (tyipeek port) -1.)
	       ($value 'end-of-file)
	       (return nil)))
	(flat-value (read port)))) 

(defun flat-value (x)
  (cond ((atom x) ($value x))
        (t (mapc (function flat-value) x)))) 

(defun span-chars (x prt)
  (do ((ch (tyipeek prt) (tyipeek prt))) ((not (member ch x #'char-equal))) (read-char prt)))

(defmacro acceptline (&rest z)
  `(prog ( def arg port)
	(setq port t)
	(setq def ',z)
	(cond (*accept-file*
	       (setq port ($ifile *accept-file*))
	       (cond ((null port) 
		      (%warn '|acceptline: file has been closed| 
		             *accept-file*)
		      (return nil)))))
	(cond ((> (length def) 0)
	       (setq arg ($varbind (car def)))
	       (cond ((and (symbolp arg) ($ifile arg))
	              (setq port ($ifile arg))
		      (setq def (cdr def))))))
        (span-chars '(9. 41.) port)
	(cond ((member (tyipeek port) '(-1. 10.))
	       (mapc (function $change) def)
	       (return nil)))
   lp1	(flat-value (read port))
        (span-chars '(9. 41.) port)
	(cond ((not (member (tyipeek port) '(-1. 10.))) (go lp1)))))

(defmacro substr (&rest l)
  `(prog (k elm start end)
        (cond ((not (= (length ',l) 3.))
               (%warn '|substr: wrong number of arguments| ',l)
               (return nil)))
        (setq elm (get-ce-var-bind (car ',l)))
        (cond ((null elm)
               (%warn '|first argument to substr must be a ce var|
                        ',l)
               (return nil)))
        (setq start ($varbind (cadr ',l)))
	(setq start ($litbind start))
        (cond ((not (numberp start))
               (%warn '|second argument to substr must be a number|
                        ',l)
               (return nil)))
	;if a variable is bound to INF, the following
	;will get the binding and treat it as INF is
	;always treated.  that may not be good
        (setq end ($varbind (caddr ',l)))
        (cond ((eq end 'inf) (setq end (length elm))))
	(setq end ($litbind end))
        (cond ((not (numberp end))
               (%warn '|third argument to substr must be a number|
                        ',l)
               (return nil)))
        ;this loop does not check for the end of elm
        ;instead it relies on cdr of nil being nil
        ;this may not work in all versions of lisp
        (setq k 1.)
   la   (cond ((> k end) (return nil))
              ((not (< k start)) ($value (car elm))))
        (setq elm (cdr elm))
        (setq k (1+ k))
        (go la))) 


(defmacro compute (&rest z) `($value (ari ',z))) 

; arith is the obsolete form of compute
(defmacro arith (&rest z) `($value (ari ',z))) 

(defun ari (x)
  (cond ((atom x)
         (%warn '|bad syntax in arithmetic expression | x)
	 0.)
        ((atom (cdr x)) (ari-unit (car x)))
        ((eq (cadr x) '+)
         (+ (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '-)
         (difference (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '*)
         (times (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '//)
         (/ (ari-unit (car x)) (ari (cddr x))))
        ((eq (cadr x) '\\)
         (mod (round (ari-unit (car x))) (round (ari (cddr x)))))
        (t (%warn '|bad syntax in arithmetic expression | x) 0.))) 

(defun ari-unit (a)
  (prog (r)
        (cond ((listp a) (setq r (ari a)))
              (t (setq r ($varbind a))))
        (cond ((not (numberp r))
               (%warn '|bad value in arithmetic expression| a)
               (return 0.))
              (t (return r))))) 

(defun genatom nil ($value (gensym))) 

(defmacro litval (&rest z)
  `(prog (r)
	(cond ((not (= (length ',z) 1.))
	       (%warn '|litval: wrong number of arguments| ',z)
	       ($value 0) 
	       (return nil))
	      ((numberp (car ',z)) ($value (car ',z)) (return nil)))
	(setq r ($litbind ($varbind (car ',z))))
	(cond ((numberp r) ($value r) (return nil)))
	(%warn '|litval: argument has no literal binding| (car ',z))
	($value 0)))


(defmacro rjust (&rest z)
  `(prog (val)
        (cond ((not (= (length ',z) 1.))
	       (%warn '|rjust: wrong number of arguments| ',z)
               (return nil)))
        (setq val ($varbind (car ',z)))
	(cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	       (%warn '|rjust: illegal value for field width| val)
	       (return nil)))
        ($value '|=== R J U S T ===|)
	($value val)))


(defmacro crlf()
     ($value '|=== C R L F ===|))

(defmacro tabto (&rest z)
  `(prog (val)
        (cond ((not (= (length ',z) 1.))
	       (%warn '|tabto: wrong number of arguments| ',z)
	       (return nil)))
        (setq val ($varbind (car ',z)))
	(cond ((or (not (numberp val)) (< val 1.) (> val 127.))
	       (%warn '|tabto: illegal column number| ',z)
	       (return nil)))
        ($value '|=== T A B T O ===|)
	($value val)))



;;; Printing WM

(defmacro ppwm (&rest z)
  `(prog (next a avlist)
        (setq avlist ',z)
        (setq *filters* nil)
        (setq next 1.)
   l   (and (atom avlist) (go print))
        (setq a (car avlist))
        (setq avlist (cdr avlist))
        (cond ((eq a #\^)
               (setq next (car avlist))
               (setq avlist (cdr avlist))
               (setq next ($litbind next))
               (and (floatp next) (setq next (round next)))
               (cond ((or (not (numberp next))
                          (> next *size-result-array*)
                          (> 1. next))
                      (%warn '|illegal index after ^| next)
                      (return nil))))
              ((variablep a)
               (%warn '|ppwm does not take variables| a)
               (return nil))
              (t (setq *filters* (cons next (cons a *filters*)))
                 (setq next (1+ next))))
        (go l)
   print (mapwm (function ppwm2))
        (terpri)
        (return nil))) 

(defun ppwm2 (elm-tag)
  (cond ((filter (car elm-tag)) (terpri) (ppelm (car elm-tag) t)))) 

(defun filter (elm)
  (prog (fl indx val)
        (setq fl *filters*)
   top  (and (atom fl) (return t))
        (setq indx (car fl))
        (setq val (cadr fl))
        (setq fl (cddr fl))
        (and (ident (nth (1- indx) elm) val) (go top))
        (return nil))) 

(defun ident (x y)
  (cond ((eq x y) t)
        ((not (numberp x)) nil)
        ((not (numberp y)) nil)
        ((=alg x y) t)
        (t nil))) 

; the new ppelm is designed especially to handle literalize format
; however, it will do as well as the old ppelm on other formats

(defun ppelm (elm port)
  (prog (ppdat sep val att mode lastpos)
	(princ (creation-time elm) port)
	(princ '|:  | port)
        (setq mode 'vector)
	(setq ppdat (get (car elm) 'ppdat))
	(and ppdat (setq mode 'a-v))
	(setq sep '|(|)
        (setq lastpos 0)
	(do
	 ((curpos 1 (1+ curpos)) (vlist elm (cdr vlist)))
	 ((atom vlist) nil)
	 (setq val (car vlist))
	 (setq att (assoc curpos ppdat))
	 (cond (att (setq att (cdr att)))
	       (t (setq att curpos)))
         (and (symbolp att) (is-vector-attribute att) (setq mode 'vector))
	 (cond ((or (not (null val)) (eq mode 'vector))
		(princ sep port)
		(ppval val att lastpos port)
		(setq sep '|    |)
		(setq lastpos curpos))))
	(princ '|)| port)))

(defun ppval (val att lastpos port)
  (cond ((not (equal att (1+ lastpos)))
         (princ '^ port)
         (princ att port)
         (princ '| | port)))
  (princ val port))

;;; printing production memory

(defmacro pm (&rest z)  `(progn (mapc #'pprule ',z) (terpri) nil)) 

;Major modification here, because Common Lisp doesn't have a standard method
;for determining the column position of the cursor.  So we have to keep count.
;So colprinc records the current column number and prints the symbol.

(proclaim '(special *current-col*))
(setq *current-col* 0)

(defun nflatc(x)
   (length (princ-to-string x)))

(defun colprinc(x)
    (setq *current-col* (+ *current-col* (nflatc x)))
    (princ x))

(defun pprule (name)
  (prog (matrix next lab)
        (terpri)
        (setq *current-col* 0)
        (and (not (symbolp name)) (return nil))
        (setq matrix (get name 'production))
	(and (null matrix) (return nil))
	(terpri)
	(colprinc '|(p |)
	(colprinc name)
   top	(and (atom matrix) (go fin))
        (setq next (car matrix))
	(setq matrix (cdr matrix))
	(setq lab nil)
	(terpri)
	(cond ((eq next '-)
	       (colprinc '|  - |)
	       (setq next (car matrix))
	       (setq matrix (cdr matrix)))
	      ((eq next '-->)
	       (colprinc '|  |))
	      ((and (eq next '{) (atom (car matrix)))
	       (colprinc '|   {|)
	       (setq lab (car matrix))
	       (setq next (cadr matrix))
	       (setq matrix (cdddr matrix)))
	      ((eq next '{)
	       (colprinc '|   {|)
	       (setq lab (cadr matrix))
	       (setq next (car matrix))
	       (setq matrix (cdddr matrix)))
	      (t (colprinc '|    |)))
        (ppline next)
	(cond (lab (colprinc '| |) (colprinc lab) (colprinc '})))
	(go top)
    fin	(colprinc '|)|)))

(defun ppline (line)
  (prog ()
	(cond ((atom line) (colprinc line))
              ((equalp (symbol-name (car line)) "DISPLACED") ;don't print expanded macros
               (ppline (cadr line)))
	      (t
	       (colprinc '|(|)
	       (setq *ppline* line)
	       (ppline2)
	       (colprinc '|)|)))
        (return nil)))

(defun ppline2 ()
  (prog (needspace)
        (setq needspace nil)
   top  (and (atom *ppline*) (return nil))
        (and needspace (colprinc '| |))
        (cond ((eq (car *ppline*) #\^) (ppattval))
	      (t (pponlyval)))
        (setq needspace t)
        (go top)))

;NWRITN, sort of. 
(defun nwritn(&optional port)
   (- 76 *current-col*))

(defun ppattval ()
  (prog (att val)
        (setq att (cadr *ppline*))
	(setq *ppline* (cddr *ppline*))
	(setq val (getval))
	(cond ((> (+ (nwritn) (nflatc att) (nflatc val)) 76.)
	       (terpri)
	       (colprinc '|        |)))
        (colprinc '^)
	(colprinc att)
	(mapc (function (lambda (z) (colprinc '| |) (colprinc z))) val)))

(defun pponlyval ()
  (prog (val needspace)
	(setq val (getval))
	(setq needspace nil)
	(cond ((> (+ (nwritn) (nflatc val)) 76.)
	       (setq needspace nil)
	       (terpri)
	       (colprinc '|        |)))
    top	(and (atom val) (return nil))
        (and needspace (colprinc '| |))
	(setq needspace t)
	(colprinc (car val))
	(setq val (cdr val))
	(go top)))

(defun getval ()
  (prog (res v1)
        (setq v1 (car *ppline*))
	(setq *ppline* (cdr *ppline*))
	(cond ((member v1 '(= <> < <= => > <=>) :test #'eq)
	       (setq res (cons v1 (getval))))
	      ((eq v1 '{)
	       (setq res (cons v1 (getupto '}))))
	      ((eq v1 '<<)
	       (setq res (cons v1 (getupto '>>))))
	      ((eq v1 '//)
	       (setq res (list v1 (car *ppline*)))
	       (setq *ppline* (cdr *ppline*)))
	      (t (setq res (list v1))))
        (return res)))

(defun getupto (end)
  (prog (v)
        (and (atom *ppline*) (return nil))
	(setq v (car *ppline*))
	(setq *ppline* (cdr *ppline*))
	(cond ((eq v end) (return (list v)))
	      (t (return (cons v (getupto end))))))) 






;;; backing up



(defun record-index-plus (k)
  (setq *record-index* (+ k *record-index*))
  (cond ((< *record-index* 0.)
         (setq *record-index* *max-record-index*))
        ((> *record-index* *max-record-index*)
         (setq *record-index* 0.)))) 

; the following routine initializes the record.  putting nil in the
; first slot indicates that that the record does not go back further
; than that.  (when the system backs up, it writes nil over the used
; records so that it will recognize which records it has used.  thus
; the system is set up anyway never to back over a nil.)

(defun initialize-record nil
  (setq *record-index* 0.)
  (setq *recording* nil)
  (setq *max-record-index* 31.)
  (putvector *record-array* 0. nil)) 

; *max-record-index* holds the maximum legal index for record-array
; so it and the following must be changed at the same time

(defun begin-record (p data)
  (setq *recording* t)
  (setq *record* (list '=>refract p data))) 

(defun end-record nil
  (cond (*recording*
         (setq *record*
               (cons *cycle-count* (cons *p-name* *record*)))
         (record-index-plus 1.)
         (putvector *record-array* *record-index* *record*)
         (setq *record* nil)
         (setq *recording* nil)))) 

(defun record-change (direct time elm)
  (cond (*recording*
         (setq *record*
               (cons direct (cons time (cons elm *record*))))))) 

; to maintain refraction information, need keep only one piece of information:
; need to record all unsuccessful attempts to delete things from the conflict
; set.  unsuccessful deletes are caused by attempting to delete refracted
; instantiations.  when backing up, have to avoid putting things back into the
; conflict set if they were not deleted when running forward

(defun record-refract (rule data)
  (and *recording*
       (setq *record* (cons '<=refract (cons rule (cons data *record*))))))

(defun refracted (rule data)
  (prog (z)
        (and (null *refracts*) (return nil))
	(setq z (cons rule data))
	(return (member z *refracts*))))

(defun back (k)
  (prog (r)
   l   (and (< k 1.) (return nil))
        (setq r (getvector *record-array* *record-index*))
        (and (null r) (return '|nothing more stored|))
        (putvector *record-array* *record-index* nil)
        (record-index-plus -1.)
        (undo-record r)
        (setq k (1- k))
        (go l))) 

(defun undo-record (r)
  (prog (save act a b rate)
        ;*recording* must be off during back up
        (setq save *recording*)
        (setq *refracts* nil)
        (setq *recording* nil)
        (and *ptrace* (back-print (list 'undo (car r) (cadr r))))
        (setq r (cddr r))
   top  (and (atom r) (go fin))
        (setq act (car r))
        (setq a (cadr r))
        (setq b (caddr r))
        (setq r (cdddr r))
        (and *wtrace* (back-print (list 'undo act a)))
        (cond ((eq act '<=wm) (add-to-wm b a))
              ((eq act '=>wm) (remove-from-wm b))
              ((eq act '<=refract)
               (setq *refracts* (cons (cons a b) *refracts*)))
              ((and (eq act '=>refract) (still-present b))
	       (setq *refracts* (delete (cons a b) *refracts*))
               (setq rate (rating-part (get a 'topnode)))
               (removecs a b)
               (insertcs a b rate))
              (t (%warn '|back: cannot undo action| (list act a))))
        (go top)
   fin  (setq *recording* save)
        (setq *refracts* nil)
        (return nil))) 

; still-present makes sure that the user has not deleted something
; from wm which occurs in the instantiation about to be restored; it
; makes the check by determining whether each wme still has a time tag.

(defun still-present (data)
  (prog nil
   l   (cond ((atom data) (return t))
              ((creation-time (car data))
               (setq data (cdr data))
               (go l))
              (t (return nil))))) 


(defun back-print (x) 
  (prog (port)
        (setq port (trace-file))
        (terpri port)
	(print x port)))




;;; Functions to show how close rules are to firing

(defmacro matches (&rest rule-list)
  `(progn 
    (mapc (function matches2) ',rule-list)
    (terpri)) )

(defun matches2 (p)
  (cond ((atom p)
         (terpri)
         (terpri)
         (princ p)
         (matches3 (get p 'backpointers) 2. (cons 1. nil))))) 

(defun matches3 (nodes ce part)
  (cond ((not (null nodes))
         (terpri)
         (princ '| ** matches for |)
         (princ part)
         (princ '| ** |)
         (mapc (function write-elms) (find-left-mem (car nodes)))
         (terpri)
         (princ '| ** matches for |)
         (princ (cons ce nil))
         (princ '| ** |)
         (mapc (function write-elms) (find-right-mem (car nodes)))
         (matches3 (cdr nodes) (1+ ce) (cons ce part))))) 

(defun write-elms (wme-or-count)
  (cond ((listp wme-or-count)
	 (terpri)
	 (mapc (function write-elms2) wme-or-count)))) 

(defun write-elms2 (x)
  (princ '|  |)
  (princ (creation-time x)))

(defun find-left-mem (node)
  (cond ((eq (car node) '&and) (memory-part (caddr node)))
        (t (car (caddr node))))) 

(defun find-right-mem (node) (memory-part (cadddr node))) 


;;; Check the RHSs of productions 


(defun check-rhs (rhs) (mapc (function check-action) rhs))

(defun check-action (x)
  (prog (a)
    (cond ((atom x)
           (%warn '|atomic action| x)
	   (return nil)))
    (setq a  (car x))
    (cond ((eq a 'bind) (check-bind x))
          ((eq a 'cbind) (check-cbind x))
          ((eq a 'make) (check-make x))
          ((eq a 'modify) (check-modify x))
          ((eq a 'oremove) (check-remove x))
          ((eq a 'owrite) (check-write x))
          ((eq a 'ocall) (check-call x))
          ((eq a 'halt) (check-halt x))
          ((eq a 'openfile) (check-openfile x))
          ((eq a 'closefile) (check-closefile x))
          ((eq a 'default) (check-default x))
          ((eq a 'build) (check-build x))
          ;;the following section is responsible for replacing standard ops RHS actions
          ;;with actions which don't conflict with existing CL functions.  The RPLACA function
          ;;is used so that the change will be reflected in the production body.
          ((eq a 'remove) (rplaca x 'oremove) 
                          (check-remove x))
          ((eq a 'write)   (rplaca x 'owrite)
                          (check-write x)) 
          ((eq a 'call)   (rplaca x 'ocall)
                          (check-call x))
          (t (%warn '|undefined rhs action| a))))) 

(defun check-build (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-build-collect (cdr z)))

(defun check-build-collect (args)
  (prog (r)
    top	(and (null args) (return nil))
	(setq r (car args))
	(setq args (cdr args))
	(cond ((listp r) (check-build-collect r))
	      ((eq r '\\)
	       (and (null args) (%warn '|nothing to evaluate| r))
	       (check-rhs-value (car args))
	       (setq args (cdr args))))
	(go top)))

(defun check-remove (z) 
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (mapc (function check-rhs-ce-var) (cdr z))) 

(defun check-make (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-openfile (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-closefile (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-default (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-modify (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-rhs-ce-var (cadr z))
  (and (null (cddr z)) (%warn '|no changes to make| z))
  (check-change& (cddr z))) 

(defun check-write (z)
  (and (null (cdr z)) (%warn '|needs arguments| z))
  (check-change& (cdr z))) 

(defun check-call (z)
  (prog (f)
    (and (null (cdr z)) (%warn '|needs arguments| z))
    (setq f (cadr z))
    (and (variablep f)
         (%warn '|function name must be a constant| z))
    (or (symbolp f)
        (%warn '|function name must be a symbolic atom| f))
    (or (externalp f)
        (%warn '|function name not declared external| f))
    (check-change& (cddr z)))) 

(defun check-halt (z)
  (or (null (cdr z)) (%warn '|does not take arguments| z))) 

(defun check-cbind (z)
  (prog (v)
    (or (= (length z) 2.) (%warn '|takes only one argument| z))
    (setq v (cadr z))
    (or (variablep v) (%warn '|takes variable as argument| z))
    (note-ce-variable v))) 

(defun check-bind (z)
  (prog (v)
    (or (> (length z) 1.) (%warn '|needs arguments| z))
    (setq v (cadr z))
    (or (variablep v) (%warn '|takes variable as argument| z))
    (note-variable v)
    (check-change& (cddr z)))) 


(defun check-change& (z)
  (prog (r tab-flag)
        (setq tab-flag nil)
   la   (and (atom z) (return nil))
        (setq r (car z))
        (setq z (cdr z))
        (cond ((eq r #\^)
               (and tab-flag
                    (%warn '|no value before this tab| (car z)))
               (setq tab-flag t)
               (check-tab-index (car z))
               (setq z (cdr z)))
              ((eq r '//) (setq tab-flag nil) (setq z (cdr z)))
              (t (setq tab-flag nil) (check-rhs-value r)))
        (go la))) 

(defun check-rhs-ce-var (v)
  (cond ((and (not (numberp v)) (not (ce-bound? v)))
         (%warn '|unbound element variable| v))
        ((and (numberp v) (or (< v 1.) (> v *ce-count*)))
         (%warn '|numeric element designator out of bounds| v)))) 

(defun check-rhs-value (x)
  (cond ((and x (listp x)) (check-rhs-function x))
        (t (check-rhs-atomic x)))) 

(defun check-rhs-atomic (x)
  (and (variablep x) 
       (not (bound? x)) 
       (%warn '|unbound variable| x)))

(defun check-rhs-function (x)
  (prog (a)
    (setq a (car x))
    (cond ((eq a 'compute) (check-compute x))
          ((eq a 'arith) (check-compute x))
          ((eq a 'substr) (check-substr x))
          ((eq a 'accept) (check-accept x))
          ((eq a 'acceptline) (check-acceptline x))
          ((eq a 'crlf) (check-crlf x))
          ((eq a 'genatom) (check-genatom x))
	  ((eq a 'litval) (check-litval x))
          ((eq a 'tabto) (check-tabto x))
	  ((eq a 'rjust) (check-rjust x))
	  ((not (externalp a))
	   (%warn '"rhs function not declared external" a)))))

(defun check-litval (x) 
  (or (= (length x) 2) (%warn '|wrong number of arguments| x))
  (check-rhs-atomic (cadr x)))

(defun check-accept (x)
  (cond ((= (length x) 1) nil)
        ((= (length x) 2) (check-rhs-atomic (cadr x)))
	(t (%warn '|too many arguments| x))))

(defun check-acceptline (x)
  (mapc (function check-rhs-atomic) (cdr x)))

(defun check-crlf (x) 
  (check-0-args x)) 

(defun check-genatom (x) (check-0-args x)) 

(defun check-tabto (x)
  (or (= (length x) 2) (%warn '|wrong number of arguments| x))
  (check-print-control (cadr x)))

(defun check-rjust (x)
  (or (= (length x) 2) (%warn '|wrong number of arguments| x))
  (check-print-control (cadr x)))

(defun check-0-args (x)
  (or (= (length x) 1.) (%warn '|should not have arguments| x))) 

(defun check-substr (x)
  (or (= (length x) 4.) (%warn '|wrong number of arguments| x))
  (check-rhs-ce-var (cadr x))
  (check-substr-index (caddr x))
  (check-last-substr-index (cadddr x))) 

(defun check-compute (x) (check-arithmetic (cdr x))) 

(defun check-arithmetic (l)
  (cond ((atom l)
         (%warn '|syntax error in arithmetic expression| l))
        ((atom (cdr l)) (check-term (car l)))
        ((not (member (cadr l) '(+ - * // \\) :test #'eq))
         (%warn '|unknown operator| l))
        (t (check-term (car l)) (check-arithmetic (cddr l))))) 

(defun check-term (x)
  (cond ((listp x) (check-arithmetic x))
        (t (check-rhs-atomic x)))) 

(defun check-last-substr-index (x)
  (or (eq x 'inf) (check-substr-index x))) 

(defun check-substr-index (x)
  (prog (v)
    (cond ((bound? x) (return x)))
    (setq v ($litbind x))
    (cond ((not (numberp v))
           (%warn '|unbound symbol used as index in substr| x))
          ((or (< v 1.) (> v 127.))
           (%warn '|index out of bounds in tab| x))))) 

(defun check-print-control (x)
  (prog ()
    (cond ((bound? x) (return x)))
    (cond ((or (not (numberp x)) (< x 1.) (> x 127.))
           (%warn '|illegal value for printer control| x))))) 

(defun check-tab-index (x)
  (prog (v)
    (cond ((bound? x) (return x)))
    (setq v ($litbind x))
    (cond ((not (numberp v))
           (%warn '|unbound symbol occurs after ^| x))
          ((or (< v 1.) (> v 127.))
           (%warn '|index out of bounds after ^| x))))) 

(defun note-variable (var)
  (setq *rhs-bound-vars* (cons var *rhs-bound-vars*)))

(defun bound? (var)
  (or (member var *rhs-bound-vars* :test #'eq)
      (var-dope var)))

(defun note-ce-variable (ce-var)
  (setq *rhs-bound-ce-vars* (cons ce-var *rhs-bound-ce-vars*)))

(defun ce-bound? (ce-var)
  (or (member ce-var *rhs-bound-ce-vars* :test #'eq)
      (ce-var-dope ce-var)))

;;; Top level routines

(defun process-changes (adds dels)
  (prog (x)
   process-deletes (and (atom dels) (go process-adds))
        (setq x (car dels))
        (setq dels (cdr dels))
        (remove-from-wm x)
        (go process-deletes)
   process-adds (and (atom adds) (return nil))
        (setq x (car adds))
        (setq adds (cdr adds))
        (add-to-wm x nil)
        (go process-adds))) 

(defun main nil
  (prog (instance r)
        (setq *halt-flag* nil)
        (setq *break-flag* nil)
        (setq instance nil)
   dil  (setq *phase* 'conflict-resolution)
        (cond (*halt-flag*
               (setq r '|end -- explicit halt|)
               (go finis))
	      ((zerop *remaining-cycles*)
	       (setq r '***break***)
	       (setq *break-flag* t)
	       (go finis))
              (*break-flag* (setq r '***break***) (go finis)))
	(setq *remaining-cycles* (1- *remaining-cycles*))
        (setq instance (conflict-resolution))
        (cond ((not instance)
               (setq r '|end -- no production true|)
               (go finis)))
        (setq *phase* (car instance))
        (accum-stats)
        (eval-rhs (car instance) (cdr instance))
        (check-limits)
	(and (broken (car instance)) (setq *break-flag* t))
        (go dil)
  finis (setq *p-name* nil)
        (return r))) 

(defun do-continue (wmi)
    (cond (*critical*
           (terpri)
           (princ '|warning: network may be inconsistent|)))
    (process-changes wmi nil)
    (print-times (main))) 

(defun accum-stats nil
  (setq *cycle-count* (1+ *cycle-count*))
  (setq *total-token* (+ *total-token* *current-token*))
  (cond ((> *current-token* *max-token*)
         (setq *max-token* *current-token*)))
  (setq *total-wm* (+ *total-wm* *current-wm*))
  (cond ((> *current-wm* *max-wm*) (setq *max-wm* *current-wm*)))) 


(defun print-times (mess)
  (prog (cc ac)
    	(cond (*break-flag* (terpri) (return mess)))
        (setq cc (+ (float *cycle-count*) 1.0e-20))
        (setq ac (+ (float *action-count*) 1.0e-20))
        (terpri)
        (princ mess)
        (pm-size)
        (printlinec (list *cycle-count*
                          'firings
                          (list *action-count* 'rhs 'actions)))
        (terpri)
        (printlinec (list (round (/ (float *total-wm*) cc))
                          'mean 'working 'memory 'size
                          (list *max-wm* 'maximum)))
        (terpri)
        (printlinec (list (round (/ (float *total-cs*) cc))
                          'mean 'conflict 'set 'size
                          (list *max-cs* 'maximum)))
        (terpri)
        (printlinec (list (round (/ (float *total-token*) cc))
                          'mean 'token 'memory 'size
                          (list *max-token* 'maximum)))
        (terpri))) 

(defun pm-size nil
  (terpri)
  (printlinec (list *pcount*
                    'productions
                    (list *real-cnt* '// *virtual-cnt* 'nodes)))
  (terpri)) 

(defun check-limits nil
  (cond ((> (length *conflict-set*) *limit-cs*)
         (terpri)
         (terpri)
         (printlinec (list '|conflict set size exceeded the limit of|
                           *limit-cs*
                           '|after|
                           *p-name*))
         (setq *halt-flag* t)))
  (cond ((> *current-token* *limit-token*)
         (terpri)
         (terpri)
         (printlinec (list '|token memory size exceeded the limit of|
                           *limit-token*
                           '|after|
                           *p-name*))
         (setq *halt-flag* t)))) 


(defun top-level-remove (z)
  (cond ((equal z '(*)) (process-changes nil (get-wm nil)))
        (t (process-changes nil (get-wm z))))) 

(defmacro excise (&rest z) `(mapc (function excise-p) ',z))

(defmacro run (&rest z)
  `(cond ((null ',z) (setq *remaining-cycles* 1000000.) (do-continue nil))
        ((and (atom (cdr ',z)) (numberp (car ',z)) (> (car ',z) 0.))
         (setq *remaining-cycles* (car ',z))
         (do-continue nil))
        (t 'what\?))) 

(defmacro strategy (&rest z)
  `(cond ((atom ',z) *strategy*)
        ((equal ',z '(lex)) (setq *strategy* 'lex))
        ((equal ',z '(mea)) (setq *strategy* 'mea))
        (t 'what\?))) 

(defmacro cs (&optional z)
  `(cond ((null ',z) (conflict-set))
        (t 'what?))) 

(defmacro watch (&rest z)
  `(cond ((equal ',z '(0.))
         (setq *wtrace* nil)
         (setq *ptrace* nil)
         0.)
        ((equal ',z '(1.)) (setq *wtrace* nil) (setq *ptrace* t) 1.)
        ((equal ',z '(2.)) (setq *wtrace* t) (setq *ptrace* t) 2.)
        ((equal ',z '(3.))
         (setq *wtrace* t)
         (setq *ptrace* t)
         '(2. -- conflict set trace not supported))
        ((and (atom ',z) (null *ptrace*)) 0.)
        ((and (atom ',z) (null *wtrace*)) 1.)
        ((atom ',z) 2.)
        (t 'what\?))) 

(defmacro external  (&rest z) `(catch (external2 ',z) '!error!))

(defun external2 (z) (mapc (function external3) z))

(defun external3 (x) 
  (cond ((symbolp x) (putprop x t 'external-routine)
		     (setq *externals* (enter x *externals*)))
	(t (%error '|not a legal function name| x))))

(defun externalp (x)
  (cond ((symbolp x) (get x 'external-routine))
	(t (%warn '|not a legal function name| x) nil)))

(defmacro pbreak (&rest z)
  `(cond ((atom ',z) (terpri) *brkpts*)
	(t (mapc (function pbreak2) ',z) nil)))

(defun pbreak2 (rule)
  (cond ((not (symbolp rule)) (%warn '|illegal name| rule))
	((not (get rule 'topnode)) (%warn '|not a production| rule))
	((member rule *brkpts* :test #'eq) (setq *brkpts* (rematm rule *brkpts*)))
	(t (setq *brkpts* (cons rule *brkpts*)))))

(defun rematm (atm list)
  (cond ((atom list) list)
	((eq atm (car list)) (rematm atm (cdr list)))
	(t (cons (car list) (rematm atm (cdr list))))))

(defun broken (rule) (member rule *brkpts* :test #'eq))
