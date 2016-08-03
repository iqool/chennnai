
(defparameter *demon* t
  "Pseudo-Figur, 'Blockade' für den Feldrand")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (loop for column across "12345678" as c from 20 to 90 by 10 do
       (loop for row across "ABCDEFGH" as r in '(1 2 3 4 5 6 7 8) do
            (eval `(defparameter ,(intern (format nil "~A~A" row column)) (+ ,c ,r))))))

(defparameter *small-center* (list d4 e4 d5 e5))
(defparameter *big-center* (list c3 d3 e3 f3 c4 d4 e4 f4 c5 d5 e5 f5 c6 d6 e6 f6))

(defun  make-empty-board-fields ()
  "erstellt leeres Brett"
  (let ((f (make-array 120 :initial-element nil)))
    (loop for i from 0 to 20
       do (setf (aref f i) *demon*))
    (loop for i from 99 to 119
       do (setf (aref f i) *demon*))
    (loop for i from 20 to 90 by 10
       do (setf (aref f i) *demon*
                (aref f (+ 9 i)) *demon*))
    f)
)

(defclass board()
  ((fields     :accessor fields 
               :initform (make-empty-board-fields)
               :type (simple-array t))
   (stones     :accessor stones 
               :initform (list 'white () 'black ()))
   (material   :accessor material
               :initform 0
               :type fixnum)
   (positional :accessor positional
               :initform 0
               :type fixnum)
   (movability :accessor movability
               :initform 0 
               :type fixnum)
   (attack     :accessor attack
               :initform 0 
               :type fixnum)
))

(defmethod ms:class-persistant-slots ((b board)) 
  '(fields stones))

(defclass stone ()
  ((at      :initarg :at     :accessor at      :type (integer 0 120))
   (value   :initarg :value  :accessor value   :type fixnum)
   (dirs    :initarg :dirs   :accessor dirs    :type (simple-array fixnum 8))
   (letter  :initarg :letter :accessor letter)
   (n-moves :initform 0      :accessor n-moves :type fixnum)))

(defgeneric opponent (stone))
(defgeneric direction (stone))
(defgeneric color (stone))
(defgeneric sign (stone))

(defclass black (stone) ())
(defclass white (stone) ())

(defmethod ms:class-persistant-slots ((s stone)) '(at))

(defmethod color ((s white)) 
  'white)

(defmethod direction ((s white)) 
  10)

(defmethod sign ((s white)) 
  1)
(defmethod sign ((s black)) 
  -1)

(defmethod color ((s black)) 'black)
(defmethod direction ((s black)) -10)

(defun field-name (i)
  (format nil "~A~A" 
               (subseq "ABCDEFGH" (1- (mod i 10)) (mod i 10)) (truncate (1- (/ i 10 )))))

(defmethod print-object ((s stone) stream)
  (format stream "~A~A"
          (letter s)
          (field-name (at s))))

(defclass long-step (stone)  ())
(defclass short-step (stone)  ())

(defclass king (short-step) ()
  (:default-initargs :value 1500    :dirs '(-11 -10 -9 -1 1 9 10 11)))

(defclass queen  (long-step) ()
  (:default-initargs :value 850     :dirs '(-11 -10 -9 -1 1 9 10 11)))

(defclass rook (long-step) ()
  (:default-initargs :value 450     :dirs '(-10 -1 1 10)))

(defclass bishop (long-step) ()
  (:default-initargs :value 300     :dirs '(-11 -9 9 11)))

(defclass knight (short-step) ()
  (:default-initargs :value 300     :dirs '(-21 -19 -12 -8 8 12 19 21)))

(defclass pawn (short-step) ()
  (:default-initargs :value 100     :dirs nil)) ;;Bauernbewegungen
                                                ;;sind im Code direkt
                                                ;;abgelegt

(defclass white-king   (white king)   () (:default-initargs :letter (if *ascii* #\K #\WHITE_CHESS_KING)))
(defclass white-queen  (white queen)  () (:default-initargs :letter (if *ascii* #\Q #\WHITE_CHESS_QUEEN)))
(defclass white-rook   (white rook)   () (:default-initargs :letter (if *ascii* #\R #\WHITE_CHESS_ROOK)))
(defclass white-bishop (white bishop) () (:default-initargs :letter (if *ascii* #\B #\WHITE_CHESS_BISHOP)))
(defclass white-knight (white knight) () (:default-initargs :letter (if *ascii* #\N #\WHITE_CHESS_KNIGHT)))
(defclass white-pawn   (white pawn)   () (:default-initargs :letter (if *ascii* #\P #\WHITE_CHESS_PAWN)))
(defclass black-king   (black king)   () (:default-initargs :letter (if *ascii* #\k #\BLACK_CHESS_KING)))
(defclass black-queen  (black queen)  () (:default-initargs :letter (if *ascii* #\q #\BLACK_CHESS_QUEEN)))
(defclass black-rook   (black rook)   () (:default-initargs :letter (if *ascii* #\r #\BLACK_CHESS_ROOK)))
(defclass black-bishop (black bishop) () (:default-initargs :letter (if *ascii* #\b #\BLACK_CHESS_BISHOP)))
(defclass black-knight (black knight) () (:default-initargs :letter (if *ascii* #\n #\BLACK_CHESS_KNIGHT)))
(defclass black-pawn   (black pawn)   () (:default-initargs :letter (if *ascii* #\p #\BLACK_CHESS_PAWN)))

(defclass abstract-move () 
  ((prevalue :initform 0   :initarg :prevalue :accessor prevalue)))

(defgeneric move (abstract-move board)) 
(defgeneric unmove (abstract-move board)) 

(defclass castling (abstract-move)
  ((to :initform nil :accessor to)))

(defclass short-castling (castling)
  ((color :initarg :color :accessor color)))

(defclass long-castling (castling)
  ((color :initarg :color :accessor color)))

(defclass move (abstract-move)
  ((from :initarg  :from  :accessor from)
   (to   :initarg  :to    :accessor to)
   (capture  :initform nil :initarg :capture  :accessor capture)))

(defclass promotion-move (move)
  ((promotion  :initarg :promotion  :accessor promotion)
   (was        :initform nil        :accessor was)))

(defmethod opponent ((s white)) 'black)
(defmethod opponent ((s black)) 'white)
(defmethod opponent ((c (eql 'white))) 'black)
(defmethod opponent ((c (eql 'black))) 'white)

(defgeneric opponent-p (stone t))

(defmethod opponent-p ((me white) (you t))
  (typep you 'black))

(defmethod opponent-p ((me black) (you t))
  (typep you 'white))

(defgeneric board-at (board  fixnum))
(defmethod board-at ((b board) i)
  (declare (optimize (safety 0) (speed 3)))
  "Hole Feldinhalt aufgrund Feldindex"
  (aref (the (simple-array t) (slot-value b 'fields))  i))

(defgeneric empty (board integer))
(defmethod empty ((b board) (i integer))
  "Stellt fest ob Brett an Position i leer ist"
  (null (board-at b i)))

(defgeneric possible-moves (stone board))
(defmethod possible-moves ((s short-step) (b board))
  "Ermittle Zugmöglichkeiten einer einschrittigen Figur"
  (let ((f (at s))
        (opp (opponent (color s))))
    (loop for d in (dirs s) 
       when (or (empty b (+ f d)) 
                (eq opp (color (board-at b (+ d f)))))
       collect (make-instance 'move :from f :to (+ d f) :capture (board-at b (+ d f))))))

(defmethod possible-moves ((s long-step) (b board))
  "Ermittle Zugmöglichkeiten einer mehrschrittigen Figur"
  (let ((f (at s)))
    (loop for d in (dirs s) append 
         (loop for dest = (+ f d) then (+ dest d) 
            while (empty b dest)
            collect (make-instance 'move :from f :to dest) into lst
            finally (return (if (opponent-p s (board-at b dest))
                                (cons (make-instance 'move 
                                                     :from f 
                                                     :to dest 
                                                     :capture (board-at b dest)) lst)
                                lst))))))

(defmacro define-possible-pawn-moves (pawn-class 
                                      queen-class 
                                      opp-color forward startline promotionline)
  `(defmethod possible-moves ((p ,pawn-class) (b board))
     (let* ((moves) 
            (src (at p))
            (dest (+ src ,forward)))
       ;;Abschnitt 1: nicht schlagende Züge
       ;;normaler Einzelschritt
       (when (null (board-at b dest)) 
         (push (if (eql ,promotionline (truncate dest 10)) 
                   ;;Verwandlung (Dame)
                   (make-instance 
                    'promotion-move 
                    :from src :to dest 
                    :promotion (make-instance ',queen-class))
                   ;; normaler Schritt nach vorne
                   (make-instance 'move :from src :to dest :prevalue 10)) moves)
         ;; Doppelschritt, falls Einzelschritt möglich auch war und
         ;; der Bauer in der Grundreihe 'startline' steht
         (when (eql ,startline (truncate src 10)) 
           (incf dest ,forward)
           (when (null (board-at b dest))
             (push (make-instance 'move :from src :to dest :prevalue 20) moves))))
       
       ;; Abschnitt 2: Schlagzug links und rechts
       (loop 
          for side in '(-1 1) 
          as dest = (+ src ,forward side)
          when (typep (board-at b dest) ',opp-color)
          do (push (if (eql ,promotionline (truncate dest 10))
                       ;; Schlagzug  mit Verwandlung
                       (make-instance 
                        'promotion-move 
                        :from src :to dest 
                        :capture (board-at b dest)
                        :promotion (make-instance ',queen-class))
                       ;; Schlagzug ohne Verwandlung
                       (make-instance 
                        'move 
                        :from src :to dest :prevalue 10
                        :capture (board-at b dest))) moves))
       moves)))

(define-possible-pawn-moves white-pawn white-queen black  10 3 9)
(define-possible-pawn-moves black-pawn black-queen white -10 8 2)

(defun translate (column row)
  "Übersetzt Feldkoordinaten in FeldIndex"
  (+ (* 10 (1+ row)) 
     (position column '(nil A B C D E F G H))))

(defun pick (b column row)
  "Hole Feldinhalt aufgrund Feldkoordinaten"
  (aref (fields b) (translate column row)))

(defmethod legal-moves ((c symbol) (b board))
  (remove-if #'(lambda (m)
                 (or (typep m 'castling) ; Rochaden werden vom
                                         ; Zuggenenerator schon so
                                         ; validiert das dadurch kein
                                         ; Eigenschach entsteht.
                     (typep (board-at b (to m)) 'king)
                     (progn
                       (move m b) 
                       (let ((ck (check c b)))
                         (unmove m b)
                         ck)))) 
             (possible-moves c b)))

(defun game-state (b c)
  (if (not (king-p c b)) 
      'pissed
      (if (null (legal-moves c b))
          (if (check c b) 'checkmate 'stalemate))))
  
(defmethod print-object ((b board) stream)
  "Stellt Brett in ASCII dar"
  (when *verbose* 
    (format stream "~&WHITE ~A" (getf (stones b) 'white))
    (format stream "~&BLACK ~A" (getf (stones b) 'black)))
  (format stream "~&~%")
  (loop for row from 8 downto 1 do
       (loop for col in '(* a b c d e f g h) do
            (if (eq col '*) 
                (format stream "~A " row)
                (let ((s (pick b col row)))
                  (if s
                      (format stream "~A " (letter s))
                      (format stream "~A " (if *ascii* #\- #\u25ef))))))
       (format stream "~%"))
  (if *ascii*
      (format stream "  a b c d e f g h")
      (format stream "  ~A~A~A~A~A~A~A~A"  #\u24d0 #\u24d1 #\u24d2 #\u24d3 #\u24d4 #\u24d5 #\u24d6 #\u24d7))
      ;;  (format stream "  ~A~A~A~A~A~A~A~A" #\u249c #\u249d #\u249e #\u249f #\u24a0 #\u24a1 #\u24a2 #\u24a3)
  (let ((state-w (game-state b 'white)) (state-b (game-state b 'black)))
    (when state-w (format stream "~&WHITE IS ~A" state-w))
    (when state-b (format stream "~&BLACK IS ~A" state-b)))
    
  (when *verbose*
    (format stream "~&WHITE MOVES ~A" (legal-moves 'white b))
    (format stream "~&BLACK MOVES ~A" (legal-moves 'black b))))

(defun field-string (i)
  "Feldbezeichnung ('E1','A8', etc) aufgrund Index erstellen"
  (let ((r (- (floor i 10) 2))
        (c (1- (rem i 10))))
    (format nil "~a~a"  (code-char (+ c (char-code #\A)))  (code-char (+ r (char-code #\1))))))

(defmethod print-object ((m move) s)
  "Zug ausgeben"
  (if (capture m) 
      (format s "~Ax~A" (field-string (from m)) (capture m))
      (format s "~A-~A" (field-string (from m)) (field-string (to m)))))

(defmethod print-object :after ((p promotion-move) s)
  (format s "/~A" (promotion p)))

(defmethod print-object ((m short-castling) s) (format s "O-O"))
(defmethod print-object ((m long-castling) s)  (format s "O-O-O"))

(defmethod initialize-instance :after ((m move) &key)
  (incf (prevalue m)
        (let ((c (capture m)))
          (if c
              (value c)
              0))))

(defmethod ms:class-persistant-slots ((m move)) '(from to capture))

(defgeneric svalue (stone))

(defmethod svalue ((s white))
  (value s))

(defmethod svalue ((s black))
  (- (value s)))

(defgeneric place (stone board integer))

(defmethod place ((s stone) (b board) (i integer))
  "Plaziere figur auf Brett an index i"
  (setf (aref (fields b) i) s)
  (setf (at s) i)
  (pushnew s (getf (stones b) (color s)))
  (incf (material b) (svalue s)))

(defgeneric unplace (board integer))

(defmethod unplace ((b board) (i integer))
  "Entferne Figur von Index i"
  (let* ((f (board-at b i))
         (color (color f)))
    (setf (aref (fields b) i) nil)
    (setf (getf (stones b) color) 
          (remove-if #'(lambda (s) (eql (at s) i)) 
                     (getf (stones b) color)))
    (decf (material b) (svalue f))
    f))

(defmethod color ((x (eql t))) nil)

(defun attacks-from (who field dir b)
  (loop for test = (+ dir field) then (+ dir test)
  while (null (board-at b test))
  finally (return  (member (type-of (board-at b test)) who))))

(defgeneric attacks (symbol integer board))

(defmethod attacks ((color (eql 'black)) (i integer) (b board))
  (or (loop for dir in '(10 1 -1 -10)
         thereis (attacks-from '(black-queen black-rook) i dir b))
      (loop for dir in '(11 9 -9 -11)
         thereis (attacks-from '(black-queen black-bishop) i dir b))
      (loop for dir in '(19 21 8 12 -8 -21 -19 -12) 
         thereis (eq 'black-knight (type-of (board-at b (+ i dir)))))
      (eq (type-of (board-at b (+ i 11))) 'black-pawn)
      (eq (type-of (board-at b (+ i 9))) 'black-pawn)
      (loop for dir in '(11 9  10 1 -9 -11 -1 -10)
         thereis (eq 'black-king (type-of (board-at b (+ i dir)))))))

(defmethod attacks ((color (eql 'white)) (i integer) (b board))
  (or (loop for dir in '(-10 -1 1 10)
         thereis (attacks-from '(white-queen white-rook) i dir b))
      (loop for dir in '(-11 -9 9 11)
         thereis (attacks-from '(white-queen white-bishop) i dir b))
      (loop for dir in '(-21 -19 -12 -8 8 12 19 21)
         thereis (eq 'white-knight (type-of (board-at b (+ i dir)))))
      (eq (type-of (board-at b (- i 11))) 'white-pawn)
      (eq (type-of (board-at b (- i 9))) 'white-pawn)
      (loop for dir in '(-11 -10 -9 -1 1 9 10 11)
         thereis (eq 'white-king (type-of (board-at b (+ i dir)))))))

(defmethod kills-a-king ((c castling))
  nil)

(defmethod kills-a-king ((m move))
  (typep (capture m) 'king))

(defgeneric count-movability (stone board)
  (:documentation 
   "Untersucht Beweglichkeit und Summe der Angriffswerte einer
   Figur"))

(defmethod count-movability ((s short-step) (b board))
  "Ermittle Bewglichkeit einer einschrittigen Figur"
  (let ((f (at s))
        (opp (opponent (color s))))
    (loop
       for d in (dirs s)
       for piece = (board-at b (+ d f))
;;       do (print (list (+ d f) piece))
       when (null piece) sum 1 into fields
       else if (eq opp (color piece))
       sum (value (board-at b (+ d f))) into attack
       finally (return (list fields attack)))))

(defmethod count-movability ((s long-step) (b board))
  (let ((f (at s))
        (mblty 0)
        (attck 0))
    (loop for d in (dirs s) do
         (multiple-value-bind (mov att)
             (loop for dest = (+ f d) then (+ dest d) 
                while (empty b dest)
                sum 1 into cnt
;;              do (prin1 (field-string dest))
                finally (return (if (opponent-p s (board-at b dest))
                                    (values cnt (value (board-at b dest)))
                                    (values cnt 0))))
           (incf mblty mov)
           (incf attck att)))
    (list mblty attck)))

(defun L+ (a b) (mapcar #'+ a b))

(defun cnt-movability (b)
  (mapcar #'-
          (loop 
             with white-all = (list 0 0)
             for s in (getf (stones b) 'white) 
             as white =  (count-movability s b)
             do (setf white-all (L+ white-all white))
             finally (return white-all))
          (loop 
             with black-all = (list 0 0)
             for s in (getf (stones b) 'black) 
             as black =  (count-movability s b)
             do (setf black-all (L+ black-all black))
             finally (return black-all))))

(defun fen (data)
  "forsyth-edwards-notation"
  (let ((board (make-instance 'board)))
    (destructuring-bind (pieces &optional 
                                (to-move 'w)
                                (castling "KQkq")
                                (ep "-")
                                (move-count 0) 
                                (move-number 1))
        (cl-ppcre:split " " data)
      (declare (ignore to-move castling ep move-count move-number))
      (loop 
         for line in (cl-ppcre:split "/" pieces)
         as index from 90 downto 20 by 10 do
           (let ((column 1))
             (loop for char across line do
                  (assert (>= 8 column 1) () "Indexfehler in ~A" line) 
                  (if (char>= #\8 char #\1)
                      (setf column (+ column (parse-integer (string char))))
                      (progn 
                        (place (make-stone char) board (+ index column)) 
                        (incf column)
                        )))))
      board)))

(defun start-position ()
  (fen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

(defvar *board* (make-instance 'board))

(defun check (color b)
  (let ((kingpos (loop 
                    for i in (getf (stones b) color) 
                    when (typep i 'king) 
                    do (return (at i))) ))
    (and kingpos (attacks (opponent color) kingpos b))))

(defun heuristic (b)
  (+ (material b) 
     (car (movability b))
     (round (cadr (movability b)) *attack-ratio*)
     (positional b)))

(defmethod possible-moves ((color symbol) (b board))
  (sort 
   (loop for s in (getf (stones b) color) 
      append (possible-moves (board-at b (at s)) b))
   #'> :key #'prevalue))

(defun king-p (color b)
  (car (member-if (lambda (s) (typep s 'king)) (getf (stones b) color))))

(defun king (color b)
  (let ((king (king-p color b)))
    (if king king (error "~A King has vanished" color))))


(defparameter *move-count* 0)

(defmethod move ((m move) (b board))
  (incf *move-count*)
  (let ((to (to m))
        (from (from m)))
    (assert (board-at b from))
    (when (not (empty b to)) 
      (unplace b to))
    (let ((s (unplace b from))) 
      (place s b to)
      (incf (n-moves s)))))

(defmethod move ((m promotion-move) (b board))
  (incf *move-count*)
  (let ((to (to m))
        (from (from m)))
    (assert (board-at b from))
    (when (not (empty b to)) 
      (unplace b to))
    (let ((s (unplace b from))) 
      (place (promotion m) b to)
      (incf (n-moves s))
      (setf (was m) s))))

(defmethod unmove ((m move) (b board))
  (let ((to (to m))
        (from (from m)))
    (assert (null (board-at b from)))
    (let ((s (unplace b to))) 
      (place s b from)
      (decf (n-moves s)))
    (when (capture m)
      (place (capture m) b to))))

(defmethod unmove ((m promotion-move) (b board))
  (let ((to (to m))
        (from (from m)))
    (assert (null (board-at b from)))
    (let ((s (unplace b to))) 
      (place (was m) b from)
      (decf (n-moves s)))
    (when (capture m)
      (place (capture m) b to))))

(defmacro define-castling-moves (color king-class rook-class opp-color a b c d e f g h)
  `(defmethod possible-moves ((k ,king-class) (bo board))
     (let ((moves (call-next-method)))
       (when (eql 0 (n-moves k)) 
         (when (and (eq (at k) ,e) 
                    (null (board-at bo ,f)) 
                    (null (board-at bo ,g))
                    (typep (board-at bo ,h) ',rook-class)
                    (eql 0 (n-moves (board-at bo ,h)))
                    (not (attacks ',opp-color ,e bo))
                    (not (attacks ',opp-color ,f bo))
                    (not (attacks ',opp-color ,g bo)))
           (push (make-instance 'short-castling :color ',color) moves))
         (when (and (eq (at k) ,e)
                    (null (board-at bo ,d)) 
                    (null (board-at bo ,c))
                    (null (board-at bo ,b))
                    (typep (board-at bo ,a) ',rook-class)
                    (eql 0 (n-moves (board-at bo ,a)))
                    (not (attacks ',opp-color ,e bo))
                    (not (attacks ',opp-color ,d bo))
                    (not (attacks ',opp-color ,c bo)))
           (push (make-instance 'long-castling :color ',color) moves)))
       moves)))

(define-castling-moves white white-king white-rook black a1 b1 c1 d1 e1 f1 g1 h1)
(define-castling-moves black black-king black-rook white a8 b8 c8 d8 e8 f8 g8 h8)

    ;; Ke1-g1 und Th1-f1 (kurze weiße Rochade)
(defparameter e1g1 (make-instance 'move :from e1 :to g1))
(defparameter h1f1 (make-instance 'move :from h1 :to f1))

    ;; Ke8-g8 und Th8-f8 (kurze schwarze Rochade)
(defparameter e8g8 (make-instance 'move :from e8 :to g8))
(defparameter h8f8 (make-instance 'move :from h8 :to f8))

    ;; Ke1-c1 und Ta1-d1 (lange weiße Rochade)
(defparameter e1c1 (make-instance 'move :from e1 :to c1))
(defparameter a1d1 (make-instance 'move :from a1 :to d1))

    ;; Ke8-c8 und Ta8-d8 (lange schwarze Rochade)
(defparameter e8c8 (make-instance 'move :from e8 :to c8))
(defparameter a8d8 (make-instance 'move :from a8 :to d8))


(defmethod initialize-instance :after ((c short-castling) &key)
  (incf (prevalue c) *short-castling-points*))

(defmethod initialize-instance :after ((c long-castling) &key)
  (incf (prevalue c) *long-castling-points*))

(defmethod initialize-instance :after ((p promotion-move) &key)
  (incf (prevalue p) (- (value (promotion p)) 100)))

(defmethod move ((m short-castling) (b board))
  (if (eq (color m) 'white)
      (progn (move e1g1 b) (move h1f1 b) 
             (incf (positional b) *short-castling-points*))
      (progn (move e8g8 b) (move h8f8 b)
             (decf (positional b) *short-castling-points*))))

(defmethod unmove ((m short-castling) (b board))
  (if (eq (color m) 'white)
      (progn (unmove e1g1 b) (unmove h1f1 b)
             (decf (positional b) *short-castling-points*))
      (progn (unmove e8g8 b) (unmove h8f8 b)
             (incf (positional b) *short-castling-points*))))

(defmethod move ((m long-castling) (b board))
  (if (eq (color m) 'white)
      (progn (move e1c1 b) (move a1d1 b)
             (incf (positional b) *long-castling-points*))
      (progn (move e8c8 b) (move a8d8 b)
             (decf (positional b) *long-castling-points*))))

(defmethod unmove ((m long-castling) (b board))
  (if (eq (color m) 'white)
      (progn (unmove e1c1 b) (unmove a1d1 b)
             (decf (positional b) *long-castling-points*))
      (progn (unmove e8c8 b) (unmove a8d8 b)
             (incf (positional b) *long-castling-points*))))

(defun negamax% (b depth alpha beta color sign)
  (if (eq depth 0)
      (values (* sign (heuristic b)) nil)
      (progn 
        (when (eq depth 1)
          (setf (movability b) (cnt-movability b)))
        (let ((best most-negative-fixnum)
              (mainvar)
              (bmove)
              (moves (possible-moves color b)))
          (loop for m in moves do
               (if (kills-a-king m) 
                   (progn (setq best 2000000)
                          (setq alpha best)
                          (setq bmove m)
                          (setq mainvar (list '++)))
                   (progn (move m b)
                          (multiple-value-bind (minus-val var) 
                              (negamax% b (1- depth) (- beta) (- alpha) (opponent color) (- sign))
                            (let ((val (- minus-val)))
                              (when (> val best)
                                (setq best val)
                                (setq mainvar var)
                                (setq bmove m))
                              (setq alpha (max alpha val))))
                          (unmove m b))
                   )
             until (or (> alpha 500000) (>= alpha beta)))
          (when (and (> best 500000) (not (check (opponent color) b))) 
            (setq best *stalemate-value*))
          (values alpha (cons bmove mainvar))))))

(defun negamax (b depth alpha beta color sign)
  (if (eq depth 0)
      (values (* sign (heuristic b)) nil)
      (progn 
        (when (eq depth 1) (setf (movability b) (cnt-movability b)))
        (let ((main-variant) (best-move)
              (moves (possible-moves color b)))
          (loop for m in moves do
               (when (kills-a-king m)
                 (return-from negamax (values 2000000 (list m 'king-killed))))
               (move m b)
               (multiple-value-bind (-val variant) (negamax b (1- depth) (- beta) (- alpha) 
                                                            (opponent color) (- sign))
                 (let ((val (- -val)))
                   (when (> val alpha)
                     (setq alpha val)
                     (setq main-variant variant)
                     (setq best-move m))))
               (unmove m b)
             until (>= alpha beta))
          (if (and (< alpha -200000) (not (check color b))) 
              (values (- *stalemate-value*) (list 'stalemate))
          (values alpha (cons best-move main-variant)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar  *user-functions* nil))

(defmacro define-user-function (name args doc &rest body)
  (pushnew name *user-functions*)
  `(defun ,name ,args ,doc ,@body))

(define-user-function verbose ()
    " - Laber-Modus ein/aus"
    (setq *verbose* (not *verbose*)))

(define-user-function i (&optional (b '*board*))
  ") - Startposition setzen"
  (set b (start-position)))

(defun make-stone (c)
  (case c 
    (#\K (make-instance 'white-king))
    (#\Q (make-instance 'white-queen))
    (#\R (make-instance 'white-rook))
    (#\B (make-instance 'white-bishop))
    (#\N (make-instance 'white-knight))
    (#\P (make-instance 'white-pawn))
    (#\k (make-instance 'black-king))
    (#\q (make-instance 'black-queen))
    (#\r (make-instance 'black-rook))
    (#\b (make-instance 'black-bishop))
    (#\n (make-instance 'black-knight))
    (#\p (make-instance 'black-pawn))
    (otherwise (error "wrong character ~A" c))))
  
(defun user-position (lines)
  (let ((b (make-instance 'board)))
    (loop 
       for l in lines
       as li from 90 downto 20 by 10
       do (loop 
             for c across l
             as ri from 1 to 8
             when (not (eq c #\space))
             do (place (make-stone c) b (+ li ri))))
    b))

(define-user-function s (list &optional (b '*board*))
  " '(<string> * 8)) Stellung eingeben"
    (set b (user-position list))
    (d))
    
(define-user-function e (&optional (b '*board*))
  ") - Brett leeren"
  (set b (make-instance 'board))
;  (place (make-instance 'white-king) (symbol-value b) e1)
;  (place (make-instance 'black-king) (symbol-value b) e8)
)

(define-user-function m (from to &optional (b *board*))
  " <from> <to>) - Zug ausführen"
  (move (make-instance 'move :from from :to to :capture (board-at b to)) b)
  (d))

(define-user-function d (&optional (b *board*))
  ") - Stellung anzeigen"
  (print b) t)

(define-user-function save (&optional (f #P"board.chess") (b *board*))
  " <filename>) - Stellung unter <filename> speichern"
  (with-open-file (stream f 
                          :direction :output 
                          :if-exists :overwrite 
                          :if-does-not-exist :create) 
      (print (ms:marshal b) stream))
  f) 

(define-user-function restore (&optional (f #P"board.chess") (b '*board*))
  " <filename>) - Stellung aus <filename> laden"
  (with-open-file (stream f :direction :input) 
    (set b (ms:unmarshal (read stream))))
  (d))
                   
(define-user-function w (d &optional (b *board*))
  " <depth>) - Analyse für Weiss"
  (setf *move-count* 0)
  (negamax b d most-negative-fixnum most-positive-fixnum 'white 1))

(define-user-function w% (d &optional (b *board*))
  " <depth>) - Analyse für Weiss (alt)"
  (setf *move-count* 0)
  (negamax% b d most-negative-fixnum most-positive-fixnum 'white 1))

(define-user-function b (d &optional (b *board*)) 
  " <depth>) - Analyse für Schwarz"
  (setf *move-count* 0)
  (negamax b d most-negative-fixnum most-positive-fixnum 'black -1))

(define-user-function b% (d &optional (b *board*)) 
  " <depth>) - Analyse für Schwarz (alt)"
  (setf *move-count* 0)
  (negamax% b d most-negative-fixnum most-positive-fixnum 'black -1))

(define-symbol-macro wk (make-instance 'white-king))
(define-symbol-macro wq (make-instance 'white-queen))
(define-symbol-macro wr (make-instance 'white-rook))
(define-symbol-macro wb (make-instance 'white-bishop))
(define-symbol-macro wn (make-instance 'white-knight))
(define-symbol-macro wp (make-instance 'white-pawn))

(define-symbol-macro bk (make-instance 'black-king))
(define-symbol-macro bq (make-instance 'black-queen))
(define-symbol-macro br (make-instance 'black-rook))
(define-symbol-macro bb (make-instance 'black-bishop))
(define-symbol-macro bn (make-instance 'black-knight))
(define-symbol-macro bp (make-instance 'black-pawn))

(define-user-function p (stone field &key (board *board*))
  " <stone> <field>) - Figur aufstellen [\"place\"]"
  (when (board-at board field) 
    (unplace board field))
  (place stone board field))

(define-user-function u (field &optional (b *board*))
  " <field>) - Figur entfernen [\"unplace\"]"
  (if (board-at b field) 
      (unplace b field)
      (error "field is empty")))

(defparameter *game-states* '(check checkmate draw stalemate))

(defvar *i-play* 'black)
(defvar *depth* 6)

(defclass player ()
   ((color :accessor color)))

(defclass human (player) ())

(defclass computer (player) ())

(defmethod turn ((me computer) (b board))
  (multiple-value-bind (value mainvar)
      (negamax b *depth* 
               most-negative-fixnum 
               most-positive-fixnum 
               (color me) 
               (if (eq 'white (color me)) 1 -1))
    (declare (ignore value))
    (move (car mainvar) b)
    (car mainvar)))

(defmethod read-user-move ((me human) (b board))
  (let ((from (read)))
    (case from 
      (0-0   (make-instance 'short-castling :color (color me)))
      (0-0-0 (make-instance 'long-castling  :color (color me)))
      (otherwise
       (let ((to (read)))
         (make-instance 'move 
                        :from (symbol-value from) 
                        :to (symbol-value to)))))))

(defmethod user-move-is-legal ((m abstract-move) (me human) (b board))
  (member-if #'(lambda (mi)
                 (or (and (typep mi 'short-castling) (typep m 'short-castling))
                     (and (typep mi 'short-castling) (typep m 'short-castling))
                     (and (eql (from mi) (from m)) (eql (to mi) (to m)))))
             (legal-moves (color me) b)))

(defmethod turn ((me human) (b board))
  (move 
   (loop
      for my-move = (read-user-move me b)
      for legal = (user-move-is-legal my-move me b)
      when (not legal) do (format t "Illegal is not allowed")
      until legal
      finally (return my-move)) 
   b))

(defclass game ()
   ((white    :initarg :white :accessor white)
    (black    :initarg :black :accessor black)
    (board    :initarg :board :accessor board)
    (protocol :initform nil   :accessor protocol)
    (50-moves-counter :initform 0 :accessor 50-moves-counter)))

(defmethod initialize-instance :after ((g game) &key)
  (setf (color (white g)) 'white)
  (setf (color (black g)) 'black))

(defmethod play ((g game))
;;  (i)
  (loop for turn-nr from 1
     do
       (d)
       (format t "~&White Move #~A~%" turn-nr)
       (format t "~A" (turn (white g) (board g)))
       (d)
       (format t "~&Black Move #~A~%" turn-nr)
       (format t "~A" (turn (black g) (board g)))
       ))

(define-user-function game-hc ()
  ") - Spiel Mensch:Computer"
  (play (make-instance 'game 
                       :white (make-instance 'human) 
                       :black (make-instance 'computer)
                       :board *board*)))

(define-user-function game-ch ()
  ") - Spiel Computer:Mensch"
  (play (make-instance 'game 
                       :white (make-instance 'computer) 
                       :black (make-instance 'human) 
                       :board *board*)))

(define-user-function game-cc ()
  ") - Spiel Computer:Computer"
  (play (make-instance 'game 
                       :white (make-instance 'computer) 
                       :black (make-instance 'computer)
                       :board *board*)))

(define-user-function h ()
  ") - Hilfe bekommen"
  (format t "~&CHENNAI ~A -- Hilfe" *chennai-version*)
  (format t "~&--------------------")
  (let ((uf (copy-list *user-functions*)))
    (loop 
       for u in (sort uf (lambda (a b) (string< (symbol-name a) (symbol-name b)))) 
       do (format t "~&(~A~A" u (documentation u 'function)))))

(format t "~%~%~%~%~%~%~%~%~%~%~%~%~%~%CHENNAI ~A -- Common Lisp Chess - Patrick Krusenotto" *chennai-version*)
(format t "~%~%(~A::h) für Hilfe~%" (package-name *package*))
