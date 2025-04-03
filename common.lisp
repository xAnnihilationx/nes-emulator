(in-package :nes-emulator)

(defmacro o1 (x) `(the (simple-array (unsigned-byte 8) (32))  ,x))
(defmacro o2 (x) `(the (simple-array (unsigned-byte 2) (256))  ,x))
(defmacro o3 (x) `(the (simple-array (unsigned-byte 8) (256))  ,x))
(defmacro o4 (x) `(the (simple-array (unsigned-byte 8) (#x10000))  ,x))
(defmacro o5 (x) `(the (simple-array (unsigned-byte 8) (#x4000))  ,x))
(defmacro of (x) `(the fixnum  ,x))

(defmacro decw (x)
  `(if (eq  ,x 0)
      (setf ,x #xff)
      (decf ,x)))

(defmacro incw (x)
  `(if (eq  ,x #xff)
      (setf ,x 0)
      (incf ,x)))

(defmacro decw16 (x &optional amount)
  (if amount
      `(if (<  ,x ,amount)
	   (setf ,x (+ #xffff  (- ,x ,amount)))
	   (decf ,x ,amount))
      `(if (eq  ,x 0)
	   (setf ,x #xffff)
	   (decf ,x))))

(defmacro incw16 (x &optional amount)
 (if amount
      `(if (>  (+ ,x ,amount) #xffff )
	   (setf ,x (- (+ ,x ,amount) #xffff))
	   (incf ,x ,amount))
      `(if (eq  ,x #xffff)
	   (setf ,x 0)
	   (incf ,x))))

(defmacro decw14 (x &optional amount)
  (if amount
      `(if (<  ,x ,amount)
	   (setf ,x (+ #x3fff  (- ,x ,amount)))
	   (decf ,x ,amount))
      `(if (eq  ,x 0)
	   (setf ,x #x3fff)
	   (decf ,x))))

(defmacro incw14 (x &optional amount)
 (if amount
      `(if (>  (+ ,x ,amount) #x3fff )
	   (setf ,x (- (+ ,x ,amount) #x3fff))
	   (incf ,x ,amount))
      `(if (eq  ,x #x3fff)
	   (setf ,x 0)
	   (incf ,x))))

(defparameter NMI-occured t)
