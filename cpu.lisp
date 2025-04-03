(in-package :nes-emulator)

(defparameter ppu-status-register #x2002)
(defparameter ppu-control-register #x2000)
(defparameter do-nestest nil) 
(defparameter upper-bank-rom #xC000 )
(defparameter lower-bank-rom #x8000 )
(defparameter cpu-memory (make-array #x10000 :element-type '(unsigned-byte 8)  :initial-element #x00))
(defparameter PC lower-bank-rom "The cpus program counter" )
(defparameter stack-bottom #x0100 "The stacks bottom ") 
(defparameter SP #x00 "The stack pointer for the cpu")
(defparameter AC #x00 "The cpus accumulator")
(defparameter RX 0 "The cpus X register")
(defparameter RY 0 "The cpus Y register")

(defparameter carry-flag nil
  "Carry flag: this holds the carry out of the most significant
   bit in any arithmetic operation. In subtraction operations however, this
   flag is cleared - set to 0 - if a borrow is required, set to 1 - if no
   borrow is required. The carry flag is also used in shift and rotate
   logical operations.")

(defparameter zero-flag  nil
  "Zero flag: this is set to 1 when any arithmetic or logical
   operation produces a zero result, and is set to 0 if the result is
   non-zero.")

(defparameter interrupt-flag t
  "This is an interrupt enable/disable flag. If it is set,
   interrupts are disabled. If it is cleared, interrupts are enabled.
  NES boots with this flag enabled ")

(defparameter decimal-mode-flag nil
  "This is the decimal mode status flag. When set, and an Add with
   Carry or Subtract with Carry instruction is executed, the source values are
   treated as valid BCD (Binary Coded Decimal, eg. 0x00-0x99 = 0-99) numbers.
   The result generated is also a BCD number.")

(defparameter brk-flag nil
  "This is set when a software interrupt (BRK instruction) is
   executed.")

(defparameter overflow-flag nil
  "Overflow flag: when an arithmetic operation produces a result
   too large to be represented in a byte, V is set.")

(defparameter negative-flag nil
  "Sign flag: this is set if the result of an operation is
   negative, cleared if positive.")

(defparameter ppu-memory (make-array #x4000 :element-type '(unsigned-byte 8)  :initial-element #x00))
(defparameter spr-memory (make-array #x100 :element-type '(unsigned-byte 8)  :initial-element #x00)
  "The 256 Bytes of sprite ram (OAM)the PPU has. ")

(defvar *rom-directory* (merge-pathnames  #P"projects/nes-emulator/roms/" (user-homedir-pathname)))
(defparameter program-rom nil "The program op codes and data")
(defparameter character-rom nil "The programs pattern tables if any")

(defparameter horizontal-mirroring nil )
(defparameter vertical-mirroring nil )
(defparameter battery-backed-ram nil )
(defparameter  debug-output nil)
(defparameter %debug-table% (make-hash-table))

;; ----------------------- PPU Register IO Stuff ------------------------------------------------------

(defparameter rest-ppu-status nil "Read has occured to the ppu status so rest the vblank flag" )
(defparameter ppu-control-write nil)
(defparameter restore-ppu-status nil "A write has occured to the ppu status so restore its previous value ")
(defparameter previous-ppu-byte #x00)

(defparameter oam-addr-write  nil)
(defparameter oam-data-write  nil)
(defparameter oam-data-read  nil)

(defparameter ppu-scroll-write nil)
(defparameter ppu-scroll-write-count 0)
(defparameter ppu-scroll-h 0)
(defparameter ppu-scroll-v 0)

(defparameter ppu-adder-write nil)
(defparameter ppu-adder-write-count 0)
(defparameter ppu-adder-set #x0000)
(defparameter ppu-adder #x0000)

(defparameter ppu-data-write nil)
(defparameter ppu-data-read nil)
(defparameter ppu-data-buffer #x42)

(defparameter oam-dma-write nil)
(defparameter sprite-0-hit nil)
(defparameter sprite-0-hit-x 0)
(defparameter sprite-hit-cycle 0)

(defmacro ppuctrl () `(aref (o4 cpu-memory) ppu-control-register))
(defmacro ppumask () `(aref (o4 cpu-memory) #x2001))
(defmacro ppu-status () `(aref (o4 cpu-memory) ppu-status-register))
(defmacro oam-address () `(aref (o4 cpu-memory) #x2003))
(defmacro oam-data () `(aref (o4 cpu-memory) #x2004))
(defmacro ppu-scroll () `(aref (o4 cpu-memory) #x2005))
(defmacro ppu-address () `(aref (o4 cpu-memory) #x2006))
(defmacro ppu-data () `(aref (o4 cpu-memory) #x2007))
(defmacro oam-dma () `(aref (o4 cpu-memory) #x4014))
(defmacro controller-1 () `(aref (o4 cpu-memory) #x4016))


(defmacro do-immediate(code debug cycles)
  `(let ((operand  '(aref (o4 cpu-memory) (of PC))))     
     `(progn
	(incf (of PC))	
	,@(sublis `((!op . ,operand)) ',code)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A #~X ~%" ,,debug  ,operand))
	(incf (of PC))
	)))


(defmacro do-zero-page(code debug cycles)
   `(let* ((operand  '(aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC)))))     
     `(progn
	(incf (of PC))	
	;,@(parser-thing ',code '!op operand)
	,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A $~X ~%" ,,debug  ,operand))
	(incf (of PC))
	)))

(defmacro do-zero-page-x(code debug cycles)
   `(let* ((operand  '(aref (o4 cpu-memory) (ldb (byte 8 0) (+ RX (aref (o4 cpu-memory) (of PC)))))))     
     `(progn
	(incf (of PC))		
	,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A $~X,X ~%" ,,debug  ,operand))
	(incf (of PC))
	)))

(defmacro do-zero-page-y(code debug cycles)
   `(let* ((operand  '(aref (o4 cpu-memory) (ldb (byte 8 0) (+ RY (aref (o4 cpu-memory) (of PC)))))))     
     `(progn
	(incf (of PC))	
					; ,@(parser-thing ',code '!op operand)
	,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A $~X,Y ~%" ,,debug  ,operand))
	(incf (of PC))
	)))

(defmacro do-absolute(code debug cycles read)
  (let ((mirror-adjust (if read 'cpu-mirror-read 'cpu-mirror-write)))
   `(let* ((operand  '(aref (o4 cpu-memory) (,mirror-adjust address))))     
      `(progn
	 (let ((address #x0000))
	   (incf (of PC))
	   (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	   (incf (of PC))
	   (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
					;   ,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (incf cpu-cycles ,,cycles)
	   (when debug-output (format t "~A $~X ~%" ,,debug  address))
	   (incf (of PC))
	)))))

(defmacro do-absolute-x(code debug cycles read do-pb)
  (let ((mirror-adjust (if read 'cpu-mirror-read 'cpu-mirror-write))
	(pb-shit (if do-pb
		     `(if (equal (ldb (byte 8 8) (of PC)) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
			  (incf cpu-cycles ,cycles)
			  (incf cpu-cycles (1+ ,cycles)))
		     `(incf cpu-cycles ,cycles)))

	)
    `(let* ((operand  '(aref (o4 cpu-memory) (,mirror-adjust (+ address RX))))
	    (pb-cross ',pb-shit))     
      `(progn
	 (let ((address #x0000))
	   (incf (of PC))
	   (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	   (incf (of PC))
	   (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (when debug-output (format t "~A $~X,X ~%" ,,debug  address))
	   (incf (of PC))
	   ,pb-cross
	)))))

(defmacro do-absolute-y(code debug cycles read do-pb)
  (let ((mirror-adjust (if read 'cpu-mirror-read 'cpu-mirror-write))
	(pb-shit (if do-pb
		     `(if (equal (ldb (byte 8 8) (of PC)) (ldb (byte 8 8) (+ address RY)))
		       (incf cpu-cycles ,cycles)
		       (incf cpu-cycles (1+ ,cycles)))
		     `(incf cpu-cycles ,cycles))))
    `(let* ((operand  '(aref (o4 cpu-memory) (,mirror-adjust (+ address RY))))
	    (pb-cross ',pb-shit))     
      `(progn
	 (let ((address #x0000))
	   (incf (of PC))
	   (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	   (incf (of PC))
	   (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (when debug-output (format t "~A $~X,Y ~%" ,,debug  address))
	   (incf (of PC))
	   ,pb-cross
	)))))


(defmacro do-index-indirect(code debug cycles read)
  (let ((mirror-adjust (if read 'cpu-mirror-read 'cpu-mirror-write)))
   `(let* ((operand  '(aref (o4 cpu-memory) (,mirror-adjust address ))))     
      `(progn
	 (let ((address #x0000))
	   (incf (of PC))
	   (let ((zero-page  (logand #x00FF(+ RX (aref (o4 cpu-memory) (of PC) )))))			   
	     (setf (ldb (byte 8 0) address) (aref (o4 cpu-memory) zero-page))
	     (setf (ldb (byte 8 8) address) (aref (o4 cpu-memory)  (logand #x00FF(+ 1 zero-page))))
	     (when debug-output (format t "~A ($~X,X) ~%" ,,debug (aref (o4 cpu-memory) (of PC) )))
	   )
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (incf (of PC))
	   (incf cpu-cycles ,,cycles)
	   )))))

(defmacro do-indirect-index(code debug cycles read do-pb)
  (let ((mirror-adjust (if read 'cpu-mirror-read 'cpu-mirror-write))
	(pb-shit (if do-pb
		     `(if (equal (ldb (byte 8 8) (of PC)) (ldb (byte 8 8) (+ address RY)))
		       (incf cpu-cycles ,cycles)
		       (incf cpu-cycles (1+ ,cycles)))
		     `(incf cpu-cycles ,cycles))))
    `(let* ((operand  '(aref (o4 cpu-memory) (,mirror-adjust (+ address RY))))
	    (pb-cross ',pb-shit)
	    )     
      `(progn
	 (let ((address #x0000))
	   (incf (of PC))
	   (let ((zero-page (aref (o4 cpu-memory) (of PC) ) ))			   
	     (setf (ldb (byte 8 0) address) (aref (o4 cpu-memory) zero-page))
	     (setf (ldb (byte 8 8) address) (aref (o4 cpu-memory)  (logand #x00FF(+ 1 zero-page))))
	     (when debug-output (format t "~A ($~X),Y ~%" ,,debug zero-page))
	   )
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (incf (of PC))
	   ,pb-cross
	   )))))


(defmacro op-code (name code mnemonic read  &key (imm nil) imm-cy
					      zp zp-cy
					      zpx zpx-cy
					      ab ab-cy
					      abx abx-cy
					      aby aby-cy
					      ixid ixid-cy
					      idix idix-cy
					      (idix+ nil)
					      (abx+ nil)
					      (aby+ nil)
					      )
  `(defmacro ,name  (address-mode)
     (case address-mode
       (:immediate ,(if imm `(do-immediate ,code ,mnemonic ,imm-cy) '(error "Adress mode not supported") ))
       (:zero-page ,(if zp `(do-zero-page ,code ,mnemonic ,zp-cy) '(error "Adress mode not supported") ))
       (:zero-page-x ,(if zpx `(do-zero-page-x ,code ,mnemonic ,zpx-cy) '(error "Adress mode not supported") ))
       (:absolute ,(if ab `(do-absolute ,code ,mnemonic ,ab-cy ,read) '(error "Adress mode not supported") ))
       (:absolute-x ,(if abx `(do-absolute-x ,code ,mnemonic ,abx-cy ,read ,abx+) '(error "Adress mode not supported") ))
       (:absolute-y ,(if aby `(do-absolute-y ,code ,mnemonic ,aby-cy ,read ,aby+ ) '(error "Adress mode not supported") ))
       (:index-indirect ,(if ixid `(do-index-indirect ,code ,mnemonic ,ixid-cy ,read) '(error "Adress mode not supported") ))
       (:indirect-index ,(if idix `(do-indirect-index ,code ,mnemonic ,idix-cy ,read ,idix+) '(error "Adress mode not supported") ))
       (otherwise (error "Dumbass!")))))



#|
7   bit	   0
----	----
VPHB	SINN
||||	||||
||||	||++- Base nametable address
||||	|| (0 = $2000;1 = $2400; 2	= $2800; 3 = $2C00)
||||	|+--- VRAM address increment per CPU read/write	of PPUDATA
||||	| (0: add 1, going across; 1: add 32, going down)
||||	+---- Sprite pattern table address for 8x8 sprites
||||	(0: $0000; 1: $1000; ignored in	8x16 mode)
|||+------ Background pattern table address (0:	$0000; 1: $1000)
||+------- Sprite size (0: 8x8;	1: 8x16)
|+-------- PPU	master/slave	select
|  (0: read backdrop from EXT pins; 1: output color on EXT pins)
+--------- Generate an NMI at the start	of the vertical	blanking interval (0: off; 1:on)
|#
(defun ppuctrlp (&key nmi-enable ppu-master sprite-height
		      bg-tile-select sprite-tile-select inc-mode nt-select)
  (if nmi-enable
    (logbitp  7 (ppuctrl))
    (if ppu-master
	(logbitp  6 (ppuctrl))
	(if sprite-height
	    (logbitp  5 (ppuctrl))
	    (if bg-tile-select
		(if (logbitp  4 (ppuctrl))
		    #x1000
		    #x0000)
		(if sprite-tile-select
		    (if (logbitp  3 (ppuctrl))
			#x1000
			#x0000)			
		    (if inc-mode
			(logbitp  2 (ppuctrl))
			(if nt-select
			    (case (logand (ppuctrl) #x3)
			      (0 #x2000)
			      (1 #x2400)
			      (2 #x2800)
			      (3 #x2C00))
			    (error "nothing selected")
			    ))))))))

(defmacro ppuctrl-bit (&key nmi-enable ppu-master sprite-height
		      bg-tile-select sprite-tile-select inc-mode nt-select)
  (if nmi-enable
     '(ldb (byte 1 7) (ppuctrl))
    (if ppu-master
	'(ldb (byte 1 6) (ppuctrl))
	(if sprite-height
	    '(ldb (byte 1 5) (ppuctrl))
	    (if bg-tile-select
		'(ldb (byte 1 4) (ppuctrl))
		(if sprite-tile-select
		    '(ldb (byte 1 3) (ppuctrl))
		    (if inc-mode
			'(ldb (byte 1 2) (ppuctrl))
			(if nt-select
			    '(ldb (byte 2 0) (ppuctrl))
			    (error "nothing selected")
			    ))))))))

(defmacro ppustatus-bit (&key vblank sprite-0-hit sprite-overflow)
  (if vblank 
      `(ldb (byte 1 7) (ppu-status))
      (if sprite-0-hit
	  `(ldb (byte 1 6) (ppu-status))
	  (if sprite-overflow
	      `(ldb (byte 1 5) (ppu-status))
	      (error "nothing selected")))))

(defmacro ppustatusp (&key vblank sprite-0-hit sprite-overflow)
  (if vblank 
      `(logbitp  7 (ppu-status))
      (if sprite-0-hit
	  `(logbitp  6 (ppu-status))
	  (if sprite-overflow
	      `(logbitp  5 (ppu-status))
	      (error "nothing selected")))))
(defmacro ppumask-bit (&key emp-blue emp-green emp-red enable-spr enable-bg clp-spr clp-bg grayscale)
  (if emp-blue
      `(ldb (byte 1 7) (ppumask))
      (if emp-green
	  `(ldb (byte 1 6) (ppumask))
	  (if emp-red
	      `(ldb (byte 1 5) (ppumask))
	      (if enable-spr
		  `(ldb (byte 1 4) (ppumask))
		  (if enable-bg
		      `(ldb (byte 1 3) (ppumask))
		      (if clp-spr
			  `(ldb (byte 1 2) (ppumask))
			  (if clp-bg
			      `(ldb (byte 1 1) (ppumask))
			      (if grayscale
				  `(ldb (byte 1 0) (ppumask))
				  (error "No"))))))))))
(defmacro ppumaskp (&key emp-blue emp-green emp-red enable-spr enable-bg clp-spr clp-bg grayscale)
  (if emp-blue
      `(logbitp 7 (ppumask))
      (if emp-green
	  `(logbitp 6 (ppumask))
	  (if emp-red
	      `(logbitp 5 (ppumask))
	      (if enable-spr
		  `(logbitp 4 (ppumask))
		  (if enable-bg
		      `(logbitp 3 (ppumask))
		      (if clp-spr
			  `(logbitp 2 (ppumask))
			  (if clp-bg
			      `(logbitp 1 (ppumask))
			      (if grayscale
				  `(logbitp 0 (ppumask))
				  (error "No"))))))))))

(defun initalize-memory-and-registers ()
  (dotimes (x (length (o4 cpu-memory)))
    (setf (aref (o4 cpu-memory) x) #x00))
  (dotimes (x (length (o5 ppu-memory)))
    (setf (aref (o5 ppu-memory) x) #x00))
  (dotimes (x (length (o3 spr-memory)))
    (setf (aref (o3 spr-memory) x) #x00))
  (setf AC 0)
  (setf RX 0)
  (setf RY 0)
  (setf SP #xFF)
  (setf (of PC) #x8000)
  (set-status-register #x04)
  (setf %debug-table% (make-hash-table))
  )

(defun untwo (number)
  (if (logbitp 7 number)
      (- 0 (+ 1 (logxor  #xFF number)))
      number))


(defun cpu-mirror-read (address)
  "Catch reads to memory maped io"
  (let ((real-address (cpu-mirror-adjust address)))
    (case real-address 
      (#x2002
       (setf rest-ppu-status t ); reset ppu status flag

     #|  (when (not debug-output)
       (setf debug-output t)
       (if (gethash PC %debug-table%)
	   (setf debug-output nil)
	   (setf (gethash PC %debug-table%) t)))
|#
       )
      (#x2004
       (setf oam-data-read t)
      ; (if (equal (mod  (oam-address) 4 ) 2)       ;; if this is the third sprite byte then and $e3
;	   (setf (oam-data)  (logand #xe3 (aref (o3 spr-memory) (oam-address)))) ; this will execute before the read occurrs      
	   (setf (oam-data)  (aref (o3 spr-memory) (oam-address)))) ; this will execute before the read occurrs            
      (#x2007
       (setf ppu-data-read t)

       ;; if the ppu address is 0 - #x3EFF update the vram buffer and set the ppu-data to whatever was on it before 
       (if (< (of ppu-adder)  #x3F00)
	   (progn
	     (setf (ppu-data) ppu-data-buffer) ; dump what ever was on the read buffer 	     
	     (setf ppu-data-buffer (aref (o5 ppu-memory) (ppu-mirror-adjust ppu-adder)))) ; update the read buffer contents
	   (progn 
	     (setf (ppu-data) (aref (o5 ppu-memory) (ppu-mirror-adjust ppu-adder))) ; else if gets whatever was there
	     (let ((mirror (ppu-mirror-adjust ppu-adder)))
	       (case ppu-adder
		 ((#x3f10 #x3f14  #x3f18 #x3f1C)
		  (setf ppu-data-buffer (aref (o5 ppu-memory) (- ppu-adder  #x1000))))
		 (otherwise
		  (setf ppu-data-buffer (aref (o5 ppu-memory) (- mirror  #x1000)))))))) ;buffer gets shadow vram

       (if (ppuctrlp :inc-mode t)
	   (incw14 ppu-adder 32)
	   (incw14 ppu-adder))
       )
      (#x4016
       (case controler-read-count
	 (0 (setf (ldb (byte 1 0)(controller-1)) (if A-press  1 0)))
	 (1 (setf (ldb (byte 1 0)(controller-1)) (if B-press  1 0)))
	 (2 (setf (ldb (byte 1 0)(controller-1)) (if Select-press  1 0)))
	 (3 (setf (ldb (byte 1 0)(controller-1)) (if Start-press  1 0)))
	 (4 (setf (ldb (byte 1 0)(controller-1)) (if Up-press  1 0)))
	 (5 (setf (ldb (byte 1 0)(controller-1)) (if Down-press  1 0)))
	 (6 (setf (ldb (byte 1 0)(controller-1)) (if Left-press  1 0)))
	 (7  (setf (ldb (byte 1 0)(controller-1)) (if Right-press  1 0))))
       (incf controler-read-count)
       (when (equal controler-read-count 8 )
	 (setf controler-read-count 0))
       )
      
      )
    real-address
    ))

(defun cpu-mirror-write (address)
  "Catch writes to memory maped io"
  (let ((real-address (cpu-mirror-adjust address)))
    (case real-address
      (#x2000
       (setf ppu-control-write t))
      (#x2002
       (setf previous-ppu-byte (ppu-status)) ;ignore writes to this 
       (setf restore-ppu-status t ))
      (#x2003
       (setf oam-addr-write t))
      (#x2004
       (setf oam-data-write t))
      (#x2005
       (setf ppu-scroll-write t)
       (incf (of ppu-scroll-write-count)))
      (#x2006
       (setf ppu-adder-write t)
       (incf (of ppu-adder-write-count)))
      (#x2007
       (setf ppu-data-write t))
      (#x4014
       (setf oam-dma-write t))
      (#x4016
       (setf controler-write t))
      )
    
    real-address
    ))

;;CPU Memory locations $0000-$07FF are mirrored three times at $0800-$1FFF
;;CPU registers $2000-$2007 are also mirror at $2008-$3FFF
;; e.g. data written to  $0000 will also be written to $0800, $1000 and $1800
(defun cpu-mirror-adjust (address)
  "If the location is a mirror it adjusts it back to a non-mirror location also if the address exceeds 16bits it wraps it back around
and it also catches any read and writes to special memory mapped regions"
  (cond 
    ((and (>= (of address) #x0800 ) (<= (of address) #x1FFF )) (mod (of address) #x0800))
    ((and (>= (of address) #x2008 ) (<= (of address) #x3FFF ))  (+ #x2000 (mod (of address) #x8)))
    ((> (of address) #xFFFF ) (ldb (byte 16 0) (of address) )) ;;(mod (of address) #xFFFF)
    (t (of address))))

(defun ppu-mirror-adjust (address)
(cond 
    ((and (>= (of address) #x3000 ) (<= (of address) #x3EFF )) (- (of address) #x1000))
    ((and (>= (of address)  #x3F20 ) (<= (of address) #x3FFF )) (+ #x3F00 (mod (of address) #x20)))  ;;	$3F00-$3F1F
    ((eq address #x3F10) #x3f00)
    ((eq address #x3F14) #x3f04)
    ((eq address #x3F18) #x3f08)
    ((eq address #x3F1C) #x3f0C)
    ((> address #x3FFF) (mod (of address) #x3FFF))
    (t address)))


(defun status-register ()
  "returns the process status byte"
  (let ((register #xFF))
    (setf (ldb (byte 1 0) register)  (if carry-flag 1 0))
    (setf (ldb (byte 1 1) register)  (if zero-flag 1 0))
    (setf (ldb (byte 1 2) register)  (if interrupt-flag 1 0))
    (setf (ldb (byte 1 3) register)  (if decimal-mode-flag 1 0))
    (setf (ldb (byte 1 4) register)  (if brk-flag 1 0))
    (setf (ldb (byte 1 6) register)  (if overflow-flag 1 0))
    (setf (ldb (byte 1 7) register)  (if negative-flag 1 0))
    register
    )
  )

(defun set-status-register (register-byte)
  "sets the process status byte"
  (setf carry-flag  (logbitp 0 register-byte))
  (setf zero-flag  (logbitp 1 register-byte))
  (setf interrupt-flag  (logbitp 2 register-byte))
  (setf decimal-mode-flag  (logbitp 3 register-byte))
  (setf brk-flag (logbitp 4 register-byte))
  (setf overflow-flag (logbitp 6 register-byte))
  (setf negative-flag (logbitp 7 register-byte))
  )

(defmacro process-cpu ()
  `(case (aref (o4 cpu-memory) (of PC))
     ;;SEI Set interrupt disable status 1 byte 2 cycles
     (#x78  (setf interrupt-flag t)
	    (incf cpu-cycles 2)
	    (incf (of PC))
	    (when debug-output (format t "SEI~%"))
	    )

     ;;CLD Clear decimal mode 1 byte 2 cycles
     (#xD8  (setf decimal-mode-flag nil)
	    (incf cpu-cycles 2)
	    (incf (of PC))
	    (when debug-output (format t "CDL~%"))
	    )
     
     ;;SED - Set Decimal Flag
     (#xF8  (setf decimal-mode-flag t)
	    (incf cpu-cycles 2)
	    (incf (of PC))
	    (when debug-output (format t "SED~%"))
	    )
     
     ;;CLC - Clear Carry Flag
     (#x18  (setf carry-flag nil)
	    (incf cpu-cycles 2)
	    (incf (of PC))
	    (when debug-output (format t "CLC~%"))
	    )
     
     ;;SEC - Set Carry Flag
     (#x38  (setf carry-flag t)
	    (incf cpu-cycles 2)
	    (incf (of PC))
	    (when debug-output (format t "SEC~%"))
	    )
     
     ;;CLI - Clear Interrupt Disable
     (#x58 (setf interrupt-flag nil)
	   (incf cpu-cycles 2)
	   (incf (of PC))
	   (when debug-output (format t "CLI~%"))
	   )
     
     ;;CLV - Clear Overflow Flag
     (#xB8 (setf overflow-flag nil)
	   (incf cpu-cycles 2)
	   (incf (of PC))
	   (when debug-output (format t "CLV~%"))
	   )
     
     ;;DEC - Decrement Memory		 
     (#xC6 (dec :zero-page))
     (#xD6 (dec :zero-page-x))
     (#xCE (dec :absolute))
     (#xDE (dec :absolute-x))

     ;;INC - Increment Memory		 
     (#xE6 (inc :zero-page))
     (#xF6 (inc :zero-page-x))
     (#xEE (inc :absolute))
     (#xFE (inc :absolute-x))
     
     ;; LDA - Load Accumulator
     ;; Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate.
     (#xA9 (load-accumulator :immediate))
     (#xA5 (load-accumulator :zero-page))
     (#xB5 (load-accumulator :zero-page-x))
     (#xAD (load-accumulator :absolute))
     (#xBD (load-accumulator :absolute-x))
     (#xB9 (load-accumulator :absolute-y))
     (#xA1 (load-accumulator :index-indirect))
     (#xB1 (load-accumulator :indirect-index))

     ;; LDX Loads a byte of memory into the X register setting the zero and negative flags as appropriate.
     (#xA2 (load-register RX :immediate "LDX"))
     (#xA6 (load-register RX :zero-page "LDX"))
     (#xB6 (load-register RX :zero-page-y "LDX"))
     (#xAE (load-register RX :absolute "LDX"))
     (#xBE (load-register RX :absolute-y "LDX"))

     ;; LDY Loads a byte of memory into the Y register setting the zero and negative flags as appropriate.
     (#xA0 (load-register RY :immediate "LDY"))
     (#xA4 (load-register RY :zero-page "LDY"))
     (#xB4 (load-register RY :zero-page-x "LDY"))
     (#xAC (load-register RY :absolute "LDY"))
     (#xBC (load-register RY :absolute-x "LDY"))

     ;; STA - Stores the contents of the accumulator into memory.
     (#x85 (store-accumulator :zero-page))
     (#x95 (store-accumulator :zero-page-x))
     (#x8D (store-accumulator :absolute))
     (#x9D (store-accumulator :absolute-x))
     (#x99 (store-accumulator :absolute-y))
     (#x81 (store-accumulator :index-indirect))
     (#x91 (store-accumulator :indirect-index))
     
     ;; TXS - Copies the current contents of the X register into the stack register.
     ;; Mode: Implied, Bytes Used: 1, CPU Cycles: 2 
     (#x9A (incf (of PC))
	   (setf SP RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TXS ~%"))
	   )
     ;;TSX - Transfer Stack Pointer to X
     (#xBA (incf (of PC))
	   (setf RX SP)
	   (incf cpu-cycles 2)
	   (set-nz-flags RX)
	   (when debug-output (format t "TSX ~%"))
	   )

     ;; TAX - Transfer Accumulator to X
     (#xAA (incf (of PC))
	   (setf RX AC)
	   (set-nz-flags RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TAX ~%" ))		       
	   )
     ;;TXA - Transfer X to Accumulator
     (#x8A (incf (of PC))
	   (setf AC RX)
	   (set-nz-flags AC)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TXA ~%" ))		       
	   )
     ;; TYA - Transfer Y to Accumulator
     (#x98 (incf (of PC))
	   (setf  AC RY)
	   (set-nz-flags AC)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TYA ~%" ))
	   )

     ;; TAY - Transfer Accumulator to Y
     (#xA8 (incf (of PC))
	   (setf RY AC)
	   (set-nz-flags RY)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TAY ~%" ))
	   )
     
     (#x30 (branch-if-flag  negative-flag "BMI"))       ;BMI - Branch if Minus	     
     (#x10 (branch-if-flag (not negative-flag) "BPL"))  ;BPL - Branch if Positive
     (#xB0 (branch-if-flag carry-flag "BCS"))           ;BCS - Branch if Carry Set
     (#x90 (branch-if-flag (not carry-flag) "BCC"))     ;BCC - Branch if Carry Clear
     (#xD0 (branch-if-flag (not zero-flag) "BNE"))      ;BNE - Branch if Not Equal
     (#xF0 (branch-if-flag  zero-flag "BEQ"))           ;BEQ - Branch if equal 
     (#x70 (branch-if-flag  overflow-flag "BVS"))       ;BVS - Branch if Overflow Set 
     (#x50 (branch-if-flag  (not overflow-flag) "BVC")) ;BVC - Branch if Overflow clear 
     
     ;;CMP Compare memory and accumulator 
     (#xC9 (compare :immediate))  
     (#xC5 (compare :zero-page))  
     (#xD5 (compare :zero-page-x))  
     (#xCD (compare :absolute))  
     (#xDD (compare :absolute-x))  
     (#xD9 (compare :absolute-y))  
     (#xC1 (compare :index-indirect))  
     (#xD1 (compare :indirect-index))  

     ;;CPX - Compare X Register Mode Immediate Bytes 2  Cycles 2
     (#xE0 (compare-register RX :immediate "CPX" ))
     (#xE4 (compare-register RX :zero-page "CPX"))
     (#xEC (compare-register RX :absolute "CPX"))

     ;;CPY - Compare Y Register Mode Immediate Bytes 2  Cycles 2
     (#xC0 (compare-register RY :immediate "CPY"))
     (#xC4 (compare-register RY :zero-page "CPY"))
     (#xCC (compare-register RY :absolute "CPY"))
     
     ;;DEX - Decrement X Register Mode Implied, Bytes 1 , CPU Cycles 2 
     ;;Subtracts one from the X register setting the zero and negative flags as appropriate.
     (#xCA (incf (of PC))
	   (if (zerop RX)
	       (setf RX #xFF)
	       (decf RX))
	   (set-nz-flags RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "DEX ~%" ))
	   )
     ;;DEY - Decrement Y Register Mode Implied, Bytes 1 , CPU Cycles 2 
     ;;Subtracts one from the Y register setting the zero and negative flags as appropriate.
     (#x88 (incf (of PC))
	   (if (zerop RY)
	       (setf RY #xFF)
	       (decf RY))
	   (set-nz-flags RY)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "DEY ~%" ))
	   )

     ;;INX - Increment X Register
     ;;X,Z,N = X+1
     ;;Adds one to the X register setting the zero and negative flags as appropriate.
     (#xE8 (incf (of PC))
	   (setf RX (ldb (byte 8 0) (1+ RX)))
	   (set-nz-flags RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "INX ~%" ))
	   )
     ;;INY - Increment Y Register
     ;;Y,Z,N = Y+1
     ;;Adds one to the Y register setting the zero and negative flags as appropriate.
     (#xC8 (incf (of PC))
	   (setf RY (ldb (byte 8 0) (1+ RY)))
	   (set-nz-flags RY)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "INY ~%" ))
	   )
     ;;JSR - Jump to Subroutine Mode Absolute Bytes 3 Cycles 6
     ;; The JSR instruction pushes the address (minus one) of the return point on to the stack and
     ;; then sets the program counter to the target memory address.
     (#x20 (let ((address #x0000))
	     (incf (of PC))
	     (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	     (incf (of PC))
	     (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	     ;; store the (of PC) on the stack High then low byte
	     (when debug-output (format t "JSR ~X ~%" address ))
	     (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 8) (of PC)))
	     (decw SP)
	     (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 0) (of PC)))
	     (decw SP)
	     (setf (of PC) address)
	     (incf cpu-cycles 6)))

     ;;BRK - Force Interrupt
     (#x00 (brk))
     
     ;;PHA - Push Accumulator
     (#x48 (incf (of PC))
	   (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 0) AC))
	   (decw SP)
	   (incf cpu-cycles 3)
	   (when debug-output (format t "PHA ~%"))
	   )
     ;;PLA - Pull Accumulator
     (#x68 (incf (of PC))
	   (incw SP)
	   (setf AC (aref (o4 cpu-memory) (+ stack-bottom SP)))
	   (incf cpu-cycles 4)
	   (set-nz-flags AC)
	   (when debug-output (format t "PLA ~%"))
	   )		 
     ;;PHP - Push Processor Status
     (#x08 (incf (of PC))
	   (setf brk-flag t) ; PHP sets the break flag when pushed to the stack
	   (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 0) (status-register)))
	   (setf brk-flag nil)

	   (decw SP)
	   (incf cpu-cycles 3)
	   (when debug-output (format t "PHP ~%"))
	   )
     ;;PLP - Pull Processor Status
     (#x28 (let ((register #x00))
	     (incf (of PC))
	     (incw SP)
	     (setf register (aref (o4 cpu-memory) (+ stack-bottom SP)))
	     (setf (ldb (byte 1 4) register) 0) ; ignore bit 4 
	     (set-status-register register)

	     (incf cpu-cycles 4)
	     (when debug-output (format t "PLP ~%"))
	     ))
     
     
     ;;RTS - Return from Subroutine Mode Implied Bytes 1  Cycles 6 
     ;;The RTS instruction is used at the end of a subroutine to return to the calling routine.
     ;;It pulls the program counter (minus one) from the stack.
     (#x60 (let ((address #x0000))
	     (incf (of PC))
	     (incw SP)
	     (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (+ stack-bottom SP)))
	     (incw SP)			 
	     (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (+ stack-bottom SP)))
	     (setf (of PC) (+ 1 address))
	     (when debug-output (format t "RTS -> ~X ~%" address ))
	     (incf cpu-cycles 6)))

     ;;RTI - Return from Interrupt
     (#x40 (let ((address #x0000)
		 (register #x00))
	     (incf (of PC))
	     (incw SP)
	     (setf register (aref (o4 cpu-memory) (+ stack-bottom SP)))
	     (setf (ldb (byte 1 4) register) 0) ; ignore bit 4 
	     (incw SP)
	     (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (+ stack-bottom SP)))
	     (incw SP)			 
	     (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (+ stack-bottom SP)))
	     (setf (of PC)  address)
	     (set-status-register register) 
	     (when debug-output (format t "RTI -> ~X ~%" address ))
	     (incf cpu-cycles 6)))

     
     ;;STX - Store X Register
     (#x86 (store-register RX :zero-page "STX"))
     (#x96 (store-register RX :zero-page-y "STX"))
     (#x8E (store-register RX :absolute "STX"))

     ;;STY - Store Y Register
     (#x84 (store-register RY :zero-page "STY"))
     (#x94 (store-register RY :zero-page-x "STY"))
     (#x8C (store-register RY :absolute "STY"))

     ;; ADC - Add with Carry
     (#x69 (add-with-carry :immediate))
     (#x65 (add-with-carry :zero-page))
     (#x75 (add-with-carry :zero-page-x))
     (#x6D (add-with-carry :absolute))
     (#x7D (add-with-carry :absolute-x))
     (#x79 (add-with-carry :absolute-y))
     (#x61 (add-with-carry :index-indirect))
     (#x71 (add-with-carry :indirect-index))

     ;; SBC - Subtract with Carry
     ((#xE9 #xEB) (subtract-with-carry :immediate))
     (#xE5 (subtract-with-carry :zero-page))
     (#xF5 (subtract-with-carry :zero-page-x))
     (#xED (subtract-with-carry :absolute))
     (#xFD (subtract-with-carry :absolute-x))
     (#xF9 (subtract-with-carry :absolute-y))
     (#xE1 (subtract-with-carry :index-indirect))
     (#xF1 (subtract-with-carry :indirect-index))
     
     ;;AND - Logical AND
     (#x29 (logical-and :immediate))
     (#x25 (logical-and :zero-page))
     (#x35 (logical-and :zero-page-x))
     (#x2D (logical-and :absolute))
     (#x3D (logical-and :absolute-x))
     (#x39 (logical-and :absolute-y))
     (#x21 (logical-and :index-indirect))
     (#x31 (logical-and :indirect-index))

     ;;EOR - Logical XOR
     (#x49 (logical-xor :immediate))
     (#x45 (logical-xor :zero-page))
     (#x55 (logical-xor :zero-page-x))
     (#x4D (logical-xor :absolute))
     (#x5D (logical-xor :absolute-x))
     (#x59 (logical-xor :absolute-y))
     (#x41 (logical-xor :index-indirect))
     (#x51 (logical-xor :indirect-index))

     ;;ORA - Logical OR
     (#x09 (logical-or :immediate))
     (#x05 (logical-or :zero-page))
     (#x15 (logical-or :zero-page-x))
     (#x0D (logical-or :absolute))
     (#x1D (logical-or :absolute-x))
     (#x19 (logical-or :absolute-y))
     (#x01 (logical-or :index-indirect))
     (#x11 (logical-or :indirect-index))

     ;;ASL - Arithmetic Shift Left
     (#x0A (asl :accumulator))
     (#x06 (asl :zero-page))
     (#x16 (asl :zero-page-x))
     (#x0E (asl :absolute))
     (#x1E (asl :absolute-x))

     ;;LSR - Logical Shift Right
     (#x4A (lsr :accumulator))
     (#x46 (lsr :zero-page))
     (#x56 (lsr :zero-page-x))
     (#x4E (lsr :absolute))
     (#x5E (lsr :absolute-x))

     ;;ROL - Rotate Left
     (#x2A (rol :accumulator))
     (#x26 (rol :zero-page))
     (#x36 (rol :zero-page-x))
     (#x2E (rol :absolute))
     (#x3E (rol :absolute-x))

     ;;ROR - Rotate Right
     (#x6A (ror :accumulator))
     (#x66 (ror :zero-page))
     (#x76 (ror :zero-page-x))
     (#x6E (ror :absolute))
     (#x7E (ror :absolute-x))
     
     ;;BIT - Bit Test		 
     (#x24 (bit-test :zero-page))
     (#x2C (bit-test :absolute))

     ;;JMP
     (#x4C (jmp :absolute))
     (#x6C (jmp :indirect))

     ;;NOP
     (#xEA
      (incf (of PC))
      (incf cpu-cycles 2)
      (when debug-output (format t "NOP ~%"))
      )

     ;; Undocumented op codes for 6502
     ;; double NOP
     ((#x04 #x14 #x34 #x44 #x54 #x64 #x74 #x80 #x82 #x89 #xC2 #xD4 #xE2 #xF4)
      (incf (of PC) 2)
      (incf cpu-cycles 2)
      (when debug-output (format t "DOP ~%"))
      )

     ;; Triple NOP 
     ((#x0C #x1C #x3C #x5C #x7C #xDC #xFC)
      (incf (of PC) 3)
      (incf cpu-cycles 4)
      (when debug-output (format t "TOP ~%"))
      )
     ((#x1A #x3A #x5A #x7A #xDA #xFA)
      (incf (of PC))
      (incf cpu-cycles 2)
      (when debug-output (format t "*NOP ~%"))
      )

     ;;DCP
     (#xC7 (dcp :zero-page))
     (#xD7 (dcp :zero-page-x))
     (#xCF (dcp :absolute))
     (#xDF (dcp :absolute-x))
     (#xDB (dcp :absolute-y))
     (#xC3 (dcp :index-indirect))
     (#xD3 (dcp :indirect-index))
     
     ;;ISB
     (#xE7 (isb :zero-page))
     (#xF7 (isb :zero-page-x))
     (#xEF (isb :absolute))
     (#xFF (isb :absolute-x))
     (#xFB (isb :absolute-y))
     (#xE3 (isb :index-indirect))
     (#xF3 (isb :indirect-index))
     
     ;;LAX		 
     (#xA7 (lax :zero-page))
     (#xB7 (lax :zero-page-y))
     (#xAF (lax :absolute))
     (#xBF (lax :absolute-y))    
     (#xA3 (lax :index-indirect))
     (#xB3 (lax :indirect-index))

     ;;SAX
     (#x87 (sax :zero-page))
     (#x97 (sax :zero-page-y))
     (#x83 (sax :index-indirect))
     (#x8F (sax :absolute))

     ;;SLO
     (#x07 (slo :zero-page))
     (#x17 (slo :zero-page-x))
     (#x0F (slo :absolute))
     (#x1F (slo :absolute-x))
     (#x1B (slo :absolute-y))
     (#x03 (slo :index-indirect))
     (#x13 (slo :indirect-index))

     ;;SRE    
     (#x47 (sre :zero-page))
     (#x57 (sre :zero-page-x))
     (#x4f (sre :absolute))
     (#x5f (sre :absolute-x))
     (#x5b (sre :absolute-y))
     (#x43 (sre :index-indirect))
     (#x53 (sre :indirect-index))

     ;;RLA    
     (#x27 (rla :zero-page))
     (#x37 (rla :zero-page-x))
     (#x2F (rla :absolute))
     (#x3F (rla :absolute-x))
     (#x3B (rla :absolute-y))
     (#x23 (rla :index-indirect))
     (#x33 (rla :indirect-index))    

     ;;RRA
     (#x67 (rra :zero-page))
     (#x77 (rra :zero-page-x)) 
     (#x6F (rra :absolute)) 
     (#x7F (rra :absolute-x)) 
     (#x7B (rra :absolute-y)) 
     (#x63 (rra :index-indirect)) 
     (#x73 (rra :indirect-index)) 

     ;;ANC
     ((#x0B #x2B) (anc :immediate))
     
     (otherwise (error (format nil "Unknown Instruction ~X~%" (aref (o4 cpu-memory) (of PC)))))
     )
 
  )

(defmacro interrupt (maskable address-location-low address-location-high )
  (if maskable    
      `(let ((address #x0000))
	 (when (not interrupt-flag)

	   (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) ,address-location-low))
	   (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) ,address-location-high))     

	   (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 8) (of PC)))
	   (decw SP)
	   (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 0) (of PC)))
	   (decw SP)
	   (setf (aref (o4 cpu-memory) (+ stack-bottom SP))(status-register))
	   (decw SP)
	   (setf interrupt-flag t)
	   (when debug-output (format t "IRQ -> ~X~% " address))
	   (setf (of PC) address)
	   (incf cpu-cycles 7)))

      `(let ((address #x0000))    
	 (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) ,address-location-low))
	 (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) ,address-location-high))     
	 (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 8) (of PC)))
	 (decw SP)
	 (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 0) (of PC)))
	 (decw SP)
	 (setf (aref (o4 cpu-memory) (+ stack-bottom SP))(status-register))
	 (decw SP)
	 (setf interrupt-flag t)
	 (when debug-output (format t "NMI -> ~X~% " address))
	 (setf (of PC) address)
	 (incf cpu-cycles 7))      
      ))


(defmacro brk () 
  `(let ((address #x0000))
     (incf (of PC))
     (incf (of PC));; control always returns to the second byte past the BRK opcode.
     (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) #xFFFE))
     (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) #xFFFF))
     ;; store the (of PC) on the stack High then low byte
     (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 8) (of PC)))
     (decw SP)
     (setf (aref (o4 cpu-memory) (+ stack-bottom SP)) (ldb (byte 8 0) (of PC)))
     (decw SP)
     (setf brk-flag t)   
     (setf (aref (o4 cpu-memory) (+ stack-bottom SP))(status-register))
     (decw SP)
     (setf brk-flag nil)
     (setf interrupt-flag t)    
     (setf (of PC) address)
     (incf cpu-cycles 7)
     (when debug-output (format t "BRK  ~%"))
     ))

(defmacro compare-register (register address-mode debug-string)  
  (case address-mode
    (:immediate
     `(progn (incf (of PC))
	     (let ((result (- ,register (aref (o4 cpu-memory) (of PC)))))
	       (set-nz-flags result)
	       (setf carry-flag (>= result 0)))
	     (incf (of PC))
	     (incf cpu-cycles 2)
	     (when debug-output (format t "~A #~X ~%" ,debug-string (aref (o4 cpu-memory) (- (of PC) 1))))
	     ))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn (incf (of PC))
	     (let ((result (- ,register (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))))))
	       (set-nz-flags result)
	       (setf carry-flag (>= result 0)))
	     (incf (of PC))
	     (incf cpu-cycles 3)
	     (when debug-output (format t "~A $~X ~%" ,debug-string (aref (o4 cpu-memory) (- (of PC) 1))))
	     ))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf (of PC))
	  (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	  (let ((result (- ,register (aref (o4 cpu-memory) (cpu-mirror-read address)))))
	    (set-nz-flags result)
	    (setf carry-flag (>=  result 0)))
	  (incf (of PC))
	  (incf cpu-cycles 4)
	  (when debug-output (format t "~A $(~X) ~%" ,debug-string address))
	  )))
    (otherwise (error "Dumbass!"))
    ))

;;JMP - Jump
;;Sets the program counter to the address specified by the operand.
(defmacro jmp (address-mode )
  (case address-mode
    (:indirect
     `(progn (incf (of PC))
	     (let ((address #x0000)
		   (actual #x0000))
	       (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	       (incf (of PC))
	       (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	       (incf (of PC))
	       (setf address (cpu-mirror-adjust address))

	       ;; An original 6502 has does not correctly fetch the target address if the
	       ;;indirect vector falls on a page boundary (e.g. $xxFF where xx is and value from $00 to $FF).
	       ;;In this case fetches the LSB from $xxFF as expected but takes the MSB from $xx00.
	       (if (eq (ldb (byte 8 0) address) #xFF)
		   (progn
		     (setf (ldb (byte 8 0) actual )(aref (o4 cpu-memory) address))
		     (setf (ldb (byte 8 8) actual )(aref (o4 cpu-memory) (logand #xFF00 address)))
		     )
		   (progn
		     (setf (ldb (byte 8 0) actual )(aref (o4 cpu-memory) address))
		     (setf (ldb (byte 8 8) actual )(aref (o4 cpu-memory)(+ 1 address)))
		     )		   
		   )

	       (setf  (of PC) (cpu-mirror-adjust actual))

	       (incf cpu-cycles 5)
	       (when debug-output (format t "JMP ($~X) -> ~X ~%" address actual ))
	       )))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf (of PC))
	  (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  
	  (setf  (of PC) (cpu-mirror-adjust address))


	  (incf cpu-cycles 3)
	  (when debug-output (format t "JMP $~X ~%" address))
	  )))
    (otherwise (error "Dumbass!"))
    ))

;;ASL - Arithmetic Shift Left
;;A,Z,C,N = M*2 or M,Z,C,N = M*2
;;This operation shifts all the bits of the accumulator or memory contents one bit left.
;;Bit 0 is set to 0 and bit 7 is placed in the carry flag.
;;The effect of this operation is to multiply the memory contents by 2,setting the carry if the result will not fit in 8 bits.
(defmacro asl (address-mode)
  (case address-mode    
    (:accumulator
     ;; 1 bytes 2 cycles     
     `(progn
	(incf (of PC))
	(setf carry-flag (logbitp 7 AC))
	(setf AC (ldb (byte 8 0) (* AC 2 )))	
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "ASL A ~%" ))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(let ((value (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) ))
	  (setf carry-flag (logbitp 7 value))
	  (setf value (ldb (byte 8 0) (* value 2 )))
	  (setf (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) value)
	  (incf (of PC))
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "ASL $~X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref (o4 cpu-memory) (ldb (byte 8 0) (+ RX (aref (o4 cpu-memory) (of PC)))))))       
       `(progn
	  (incf (of PC))
	  (let ((value ,memory-value ))
	    (setf carry-flag (logbitp 7 value))
	    (setf value (ldb (byte 8 0) (* value 2 )))
	    (setf ,memory-value value)
	    
	    (incf (of PC))
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "ASL $~X,X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref (o4 cpu-memory) (cpu-mirror-write address))))       
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	    (let ((value ,memory-value ))	    
	      (setf carry-flag (logbitp 7 value))
	      (setf value (ldb (byte 8 0) (* value 2 )))
	      (setf ,memory-value value)
	      
	      (incf (of PC))
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "ASL $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref (o4 cpu-memory) (cpu-mirror-write (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (let ((value ,memory-value ))
	      (setf carry-flag (logbitp 7 value))
	      (setf value (ldb (byte 8 0) (* value 2 )))
	      (setf ,memory-value value)
	      
	      (set-nz-flags value)
	      (incf cpu-cycles 7)
	      (when debug-output (format t "ASL $~X,X ~%" address))
	      )))))        
    (otherwise (error "Dumbass!"))
    ))

;;ROL - Rotate Left
(defmacro rol (address-mode)
  (case address-mode    
    (:accumulator
     ;; 1 bytes 2 cycles     
     `(progn
	(incf (of PC))
	(let ((old-carry (if carry-flag 1 0)))
	  (setf carry-flag (logbitp 7 AC))
	  (setf AC (ldb (byte 8 0) (* AC 2 )))
	  (setf (ldb (byte 1 0) AC) old-carry)
	  (incf cpu-cycles 2)
	  (set-nz-flags AC)
	  (when debug-output (format t "ROL A ~%" ))
	  )))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(let ((value (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) )
	      (old-carry (if carry-flag 1 0)))	      
	  (setf carry-flag (logbitp 7 value))
	  (setf value (ldb (byte 8 0) (* value 2 )))
	  (setf (ldb (byte 1 0) value) old-carry)
	  
	  (setf (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) value)
	  (incf (of PC))
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "ROL $~X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref (o4 cpu-memory) (ldb (byte 8 0) (+ RX (aref (o4 cpu-memory) (of PC)))))))       
       `(progn
	  (incf (of PC))
	  (let ((value ,memory-value )
		(old-carry (if carry-flag 1 0)))

	    (setf carry-flag (logbitp 7 value))
	    (setf value (ldb (byte 8 0) (* value 2 )))
	    (setf (ldb (byte 1 0) value) old-carry)
	    (setf ,memory-value value)
	    
	    (incf (of PC))
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "ROL $~X,X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref (o4 cpu-memory) (cpu-mirror-write address))))       
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))

	    (let ((value ,memory-value )
		  (old-carry (if carry-flag 1 0)))

	      (setf carry-flag (logbitp 7 value))
	      (setf value (ldb (byte 8 0) (* value 2 )))
	      (setf (ldb (byte 1 0) value) old-carry)
	      (setf ,memory-value value)
	      
	      (incf (of PC))
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "ROL $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref (o4 cpu-memory) (cpu-mirror-write (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (let ((value ,memory-value )
		  (old-carry (if carry-flag 1 0)))

	      (setf carry-flag (logbitp 7 value))
	      (setf value (ldb (byte 8 0) (* value 2 )))
	      (setf (ldb (byte 1 0) value) old-carry)
	      (setf ,memory-value value)
	      
	      (set-nz-flags value)
	      (incf cpu-cycles 7)
	      (when debug-output (format t "ROL $~X,X ~%" address))
	      )))))        
    (otherwise (error "Dumbass!"))
    ))

  ;;ROR
(defmacro ror (address-mode)
  (case address-mode    
    (:accumulator
     ;; 1 bytes 2 cycles     
     `(progn
	(incf (of PC))
	(let ((old-carry (if carry-flag 1 0)))
	  (setf carry-flag (logbitp 0 AC))
	  (setf AC (ldb (byte 8 0) (floor (/ AC 2 ))))
	  (setf (ldb (byte 1 7) AC) old-carry)
	  (incf cpu-cycles 2)
	  (set-nz-flags AC)
	  (when debug-output (format t "ROR A ~%" ))
	  )))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(let ((value (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) )
	      (old-carry (if carry-flag 1 0)))	      
	  (setf carry-flag (logbitp 0 value))
	  (setf value (ldb (byte 8 0) (floor (/ value 2 ))))
	  (setf (ldb (byte 1 7) value) old-carry)
	  
	  (setf (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) value)
	  (incf (of PC))
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "ROR $~X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref (o4 cpu-memory) (ldb (byte 8 0) (+ RX (aref (o4 cpu-memory) (of PC)))))))       
       `(progn
	  (incf (of PC))
	  (let ((value ,memory-value )
		(old-carry (if carry-flag 1 0)))

	    (setf carry-flag (logbitp 0 value))
	    (setf value (ldb (byte 8 0) (floor (/ value 2 ))))
	    (setf (ldb (byte 1 7) value) old-carry)
	    (setf ,memory-value value)
	    
	    (incf (of PC))
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "ROR $~X,X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref (o4 cpu-memory) (cpu-mirror-write address))))       
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))

	    (let ((value ,memory-value )
		  (old-carry (if carry-flag 1 0)))

	      (setf carry-flag (logbitp 0 value))
	      (setf value (ldb (byte 8 0) (floor (/ value 2 ))))
	      (setf (ldb (byte 1 7) value) old-carry)
	      (setf ,memory-value value)
	      
	      (incf (of PC))
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "ROR $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref (o4 cpu-memory) (cpu-mirror-write (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (let ((value ,memory-value )
		  (old-carry (if carry-flag 1 0)))

	      (setf carry-flag (logbitp 0 value))
	      (setf value (ldb (byte 8 0) (floor (/ value 2 ))))
	      (setf (ldb (byte 1 7) value) old-carry)
	      (setf ,memory-value value)
	      
	      (set-nz-flags value)
	      (incf cpu-cycles 7)
	      (when debug-output (format t "ROR $~X,X ~%" address))
	      )))))        
    (otherwise (error "Dumbass!"))
    ))

;;LSR
;;LSR - Logical Shift Right
;;A,C,Z,N = A/2 or M,C,Z,N = M/2
;;Each of the bits in A or M is shift one place to the right. The bit that was in bit 0 is shifted into the carry flag.
;;Bit 7 is set to zero.
(defmacro lsr (address-mode)
  (case address-mode    
    (:accumulator
     ;; 1 bytes 2 cycles     
     `(progn
	(incf (of PC))
	(setf carry-flag (logbitp 0 AC))
	(setf AC (ldb (byte 8 0) (floor (/ AC 2 ))))
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "LSR A ~%" ))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(let ((value (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) ))	  
	  (setf carry-flag (logbitp 0 value))
	  (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	  (setf (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) value)
	  (incf (of PC))
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "LSR $~X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref (o4 cpu-memory) (ldb (byte 8 0) (+ RX (aref (o4 cpu-memory) (of PC)))))))       
       `(progn
	  (incf (of PC))
	  (let ((value ,memory-value ))	    
	    (setf carry-flag (logbitp 0 value))
	    (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	    (setf ,memory-value value)	    
	    (incf (of PC))
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "LSR $~X,X ~%" (aref (o4 cpu-memory) (- (of PC) 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref (o4 cpu-memory) (cpu-mirror-write address))))       
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))

	    (let ((value ,memory-value ))
	      (setf carry-flag (logbitp 0 value))
	      (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	      (setf ,memory-value value)
	      
	      (incf (of PC))
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "LSR $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref (o4 cpu-memory) (cpu-mirror-write (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf (of PC))
	    (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	    (incf (of PC))
	    (let ((value ,memory-value ))
	      (setf carry-flag (logbitp 0 value))
	      (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	      (setf ,memory-value value)
	      
	      (set-nz-flags value)
	      (incf cpu-cycles 7)
	      (when debug-output (format t "LSR $~X,X ~%" address))
	      )))))        
    (otherwise (error "Dumbass!"))
    ))

;;LDX/Y
(defmacro load-register (register address-mode debug-string)
  (case address-mode
    (:immediate
     ;; 2 bytes 2 cycles 
     `(progn
	(incf (of PC))
	(setf ,register (aref (o4 cpu-memory) (of PC)))
	(incf (of PC))
	(incf cpu-cycles 2)
	(set-nz-flags ,register)
	(when debug-output (format t "~A #~X ~%" ,debug-string ,register))
	))    
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(setf ,register (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) )
	(incf (of PC))
	(incf cpu-cycles 3)
	(set-nz-flags ,register)
	(when debug-output (format t "~A $~X ~%" ,debug-string (aref (o4 cpu-memory) (- (of PC) 1))))
	))
    (:zero-page-y
     ;;2 bytes 4 cycles
     `(progn
	(incf (of PC))
	(setf ,register (aref (o4 cpu-memory) (ldb (byte 8 0) (+ RY (aref (o4 cpu-memory) (of PC))))))
	(incf (of PC))
	(incf cpu-cycles 4)
	(set-nz-flags ,register)
	(when debug-output (format t "~A $~X,Y ~%" ,debug-string (aref (o4 cpu-memory) (- (of PC) 1))))	
	))
    (:zero-page-x
     ;;2 bytes 4 cycles
     `(progn
	(incf (of PC))
	(setf ,register (aref (o4 cpu-memory) (ldb (byte 8 0) (+ RX (aref (o4 cpu-memory) (of PC))))))
	(incf (of PC))
	(incf cpu-cycles 4)
	(set-nz-flags ,register)
	(when debug-output (format t "~A $~X,Y ~%" ,debug-string  (aref (o4 cpu-memory) (- (of PC) 1))))	
	))

    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf (of PC))
	  (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf ,register (aref (o4 cpu-memory) (cpu-mirror-read address)))
	  (set-nz-flags ,register)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "~A $~X ~%" ,debug-string address))
	)))
    (:absolute-y
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     `(progn
	(let ((address #x0000))
	  (incf (of PC))
	  (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf ,register (aref (o4 cpu-memory) (cpu-mirror-read (+ address RY))))			 
	  (set-nz-flags ,register )
	  (if (equal (ldb (byte 8 8) (of PC)) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "~A $~X,Y ~%" ,debug-string address))
	)))    
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     `(progn
	(let ((address #x0000))
	  (incf (of PC))
	  (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf ,register (aref (o4 cpu-memory) (cpu-mirror-read (+ address RX))))			 
	  (set-nz-flags ,register )
	  (if (equal (ldb (byte 8 8) (of PC)) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "~A $~X,Y ~%" ,debug-string address))
	)))    
    
    (otherwise (error "Dumbass!"))
    ))

;;STX/Y
(defmacro store-register (register address-mode debug-string)
  (case address-mode
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(setf (aref (o4 cpu-memory) (aref (o4 cpu-memory) (of PC))) ,register )
	(incf (of PC))
	(incf cpu-cycles 3)
	(when debug-output (format t "~A $~X ~%" ,debug-string (aref (o4 cpu-memory) (- (of PC) 1))))
	))
    (:zero-page-x
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(setf (aref (o4 cpu-memory) (ldb (byte 8 0) (+ RX (aref (o4 cpu-memory) (of PC))))) ,register )
	(incf (of PC))
	(incf cpu-cycles 4)
	(when debug-output (format t "~A $~X,X ~%" ,debug-string (aref (o4 cpu-memory) (- (of PC) 1))))	
	))
    (:zero-page-y
     ;;2 bytes 3 cycles
     `(progn
	(incf (of PC))
	(setf (aref (o4 cpu-memory) (ldb (byte 8 0) (+ RY (aref (o4 cpu-memory) (of PC))))) ,register )
	(incf (of PC))
	(incf cpu-cycles 4)
	(when debug-output (format t "~A $~X,Y ~%" ,debug-string (aref (o4 cpu-memory) (- (of PC) 1))))	
	))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf (of PC))
	  (setf (ldb (byte 8 0) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf (ldb (byte 8 8) address)(aref (o4 cpu-memory) (of PC)))
	  (incf (of PC))
	  (setf (aref (o4 cpu-memory) (cpu-mirror-write address)) ,register)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "~A $~X ~%" ,debug-string address))       
	)))
   
    (otherwise (error "Dumbass!"))
    ))

;; CMP a fucking again
(op-code compare ((let ((result (- AC !op)))
		    (set-nz-flags result)
		    (setf carry-flag (>=  result 0))))
	 "CMP" t
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4;+
	 :abx+ t
	 :aby t :aby-cy 4;+
	 :aby+ t
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5;+
	 :idix+ t
	 )


;;BIT - Bit Test
;;A & M, N = M7, V = M6
;;This instructions is used to test if one or more bits are set in a target memory location.
;;The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result is not kept.
;;Bits 7 and 6 of the value from memory are copied into the N and V flags.
(op-code bit-test ((let ((value !op))
		     (setf zero-flag (eq 0 (logand value AC)))
		     (setf negative-flag (logbitp 7 value))
		     (setf overflow-flag (logbitp 6 value))))
	 "BIT" t
	 :zp t :zp-cy 3
	 :ab t :ab-cy 4)


;;A,Z,C,N = A+M+C
;;This instruction adds the contents of a memory location to the accumulator together with the carry bit.
;;If overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
(op-code add-with-carry ((let* ((sign-bit-ac  (logbitp 7  AC))
				(value !op)
				(sign-bit-op  (logbitp 7 value)) 
				(result (+ AC value (if carry-flag 1 0) ))
				(sign-bit-result (logbitp 7 result)))
	  
			   (setf AC (ldb (byte 8 0) result))
			   (setf carry-flag (logbitp 8 result))
			   (setf overflow-flag (and (equal sign-bit-ac sign-bit-op) (not (equal sign-bit-result sign-bit-ac)))))
			 (set-nz-flags AC))
	 "ADC" t
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4 
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4 ;+
	 :abx+ t
	 :aby t :aby-cy 4 ;+
	 :aby+ t
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5 ;+
	 :idix+ t
	 )

;;SBC
(op-code subtract-with-carry ((let* ((value !op)
				     (sign-bit-ac  (logbitp 7  AC)) 
				     (sign-bit-op  (not(logbitp 7  value))) 
				     (result (+ AC (logxor  value #xff) (if carry-flag 1 0) ))
				     (sign-bit-result (logbitp 7 result)))
				
				(setf AC (ldb (byte 8 0) result))
				(setf carry-flag (logbitp 8 result))
				(setf overflow-flag (and (equal sign-bit-ac sign-bit-op) (not (equal sign-bit-result sign-bit-ac)))))
			      (set-nz-flags AC))
	 "SBC" t
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4;+
	 :abx+ t
	 :aby t :aby-cy 4;+
	 :aby+ t
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5;+
	 :idix+ t
	 )

;;DEC - Decrement Memory
;;M,Z,N = M-1
;;Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
(op-code dec ((let ((value   (ldb (byte 8 0) (- !op 1)) ))
			(setf !op  value )
			(set-nz-flags value)))
	 "DEC" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7)

;;INC
(op-code inc ((let ((value   (ldb (byte 8 0) (+ !op 1)) ))
			(setf !op  value )
			(set-nz-flags value)))
	 "INC" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7)

;;AND - Logical AND
;;A,Z,N = A&M
;;A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
(op-code logical-and ((setf AC (logand AC !op))
		     (set-nz-flags AC))
	 "AND" t
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4;+
	 :abx+ t
	 :aby t :aby-cy 4;+
	 :aby+ t
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5 ;+
	 :idix+ t
	 )

;; EOR - exsclusive or 
(op-code logical-xor ((setf AC (logxor AC !op))
		     (set-nz-flags AC))
	 "EOR" t
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4;+
	 :abx+ t
	 :aby t :aby-cy 4;+
	 :aby+ t
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5;+
	 :idix+ t
	 )

;;LAX
;;Load accumulator and X register with memory.
(defmacro lax (address-mode ) 
    (case address-mode
      (:zero-page (do-zero-page ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 3))    
      (:zero-page-y (do-zero-page-y ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 4))  
      (:absolute (do-absolute ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 4 t))
      (:absolute-y (do-absolute-y ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 4 t nil))  
      (:index-indirect (do-index-indirect ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 6 t))
      (:indirect-index (do-indirect-index ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 5 t nil))   
      (otherwise (error "Dumbass!"))
    ))

;;SAX
;;AND X register with accumulator and store result in memory. 
(defmacro sax (address-mode ) 
    (case address-mode
      (:zero-page (do-zero-page ((let ((result (logand AC RX))) (setf !op result ) )) "SAX" 3 ))
      (:zero-page-y (do-zero-page-y ((let ((result (logand AC RX)))(setf !op result ))) "SAX" 4 ))     
      (:absolute (do-absolute ((let ((result (logand AC RX))) (setf !op result ) )) "SAX" 4 nil))      
      (:index-indirect (do-index-indirect ((let ((result (logand AC RX))) (setf !op result ) )) "SAX" 6 nil))      
      (otherwise (error "Dumbass!"))
    ))

;;DCP
;;Subtract 1 from memory (without borrow).
(op-code dcp ((let* ((memory-value !op)				       
		     (result  (ldb (byte 8 0) (+ memory-value #xFF)) ))	  
		(setf !op  result)
		(setf carry-flag (>= AC result))
		(setf zero-flag (equal AC result))
		(setf negative-flag (logbitp 7 (- AC result)))
		))
	 "DCP" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7
	 :aby t :aby-cy 7
	 :ixid t :ixid-cy 8
	 :idix t :idix-cy 8)
;;ORA
(op-code logical-or ((setf AC (logior AC !op))
		     (set-nz-flags AC))
	 "ORA" t
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4;+
	 :abx+ t
	 :aby t :aby-cy 4;+
	 :aby+ t
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5;+
	 :idix+ t
	 )
;;STA
(op-code store-accumulator ((setf !op AC ))
	 "STA" nil
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 5
	 :aby t :aby-cy 5
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 6)

;;LDA load accumulator 
(op-code load-accumulator ((setf AC !op)
		     (set-nz-flags AC))
	 "LDA" t
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4;+
	 :abx+ t
	 :aby t :aby-cy 4;+
	 :aby+ t
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5;+
	 :idix+ t
	 )

 ;;ISB
;;Increase memory by one, then subtract memory from accumulator (with borrow).
(op-code isb ((let* ((value   (ldb (byte 8 0) (+ !op 1)) )
		     (sign-bit-ac  (logbitp 7  AC)) 
		     (sign-bit-op  (not(logbitp 7  value))) 
		     (result (+ AC (logxor  value #xff) (if carry-flag 1 0) ))
		     (sign-bit-result (logbitp 7 result)))
		(setf !op  value )
		(setf AC (ldb (byte 8 0) result))
		(setf carry-flag (logbitp 8 result))
		(setf overflow-flag (and (equal sign-bit-ac sign-bit-op) (not (equal sign-bit-result sign-bit-ac))))
		(set-nz-flags AC)    
		))
	 "ISC" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7
	 :aby t :aby-cy 7
	 :ixid t :ixid-cy 8
	 :idix t :idix-cy 8)	   

;;Shift left one bit in memory, then OR accumulator with memory.
(op-code slo ((let ((value !op ))
		(setf carry-flag (logbitp 7 value))
		(setf value (ldb (byte 8 0) (* value 2 )))
		(setf !op value)	    	  	    
		(setf AC (logior AC value))
		(set-nz-flags AC)
		))
	 "SLO" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7
	 :aby t :aby-cy 7
	 :ixid t :ixid-cy 8
	 :idix t :idix-cy 8)

;;RLA
;;Rotate one bit left in memory, then AND accumulator with memory. Status
(op-code rla ((let ((value !op )
		    (old-carry (if carry-flag 1 0)))

		(setf carry-flag (logbitp 7 value))
		(setf value (ldb (byte 8 0) (* value 2 )))
		(setf (ldb (byte 1 0) value) old-carry)
		(setf !op value)	    
		(setf AC (logand AC value))
		(set-nz-flags AC)
		))	     
	 "RLA" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7
	 :aby t :aby-cy 7
	 :ixid t :ixid-cy 8
	 :idix t :idix-cy 8)

;;Shift right one bit in memory, then EOR accumulator with memory. Status
(op-code sre ((let ((value !op))	    
	    (setf carry-flag (logbitp 0 value))
	    (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	    (setf !op value)	    	    
	    (setf AC (logxor AC value))
	    (set-nz-flags AC)))
	 "SRE" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7
	 :aby t :aby-cy 7
	 :ixid t :ixid-cy 8
	 :idix t :idix-cy 8)

;;Rotate one bit right in memory, then add memory to accumulator (with carry).
(op-code rra ((let* ((old-carry (if carry-flag 1 0))	    
		     (sign-bit-ac  (logbitp 7  AC))
		     (value !op)
		     (sign-bit-op  nil) 
		     (result nil)
		     (sign-bit-result nil)
		     )
		(setf carry-flag (logbitp 0 value))
		(setf value (ldb (byte 8 0) (floor (/ value 2 ))))
		(setf (ldb (byte 1 7) value ) old-carry)
		(setf !op value)
		
		(setf sign-bit-op (logbitp 7 value))
		(setf result (+ AC value (if carry-flag 1 0) ))
		(setf sign-bit-result (logbitp 7 result))

		(setf AC (ldb (byte 8 0) result))
		(setf carry-flag (logbitp 8 result))
		(setf overflow-flag (and (equal sign-bit-ac sign-bit-op) (not (equal sign-bit-result sign-bit-ac))))
		(set-nz-flags AC)
		))
	 "RRA" nil
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7
	 :aby t :aby-cy 7
	 :ixid t :ixid-cy 8
	 :idix t :idix-cy 8)

;;ANC
;;AND byte with accumulator. If result is negative then carry is set.
(op-code anc ((setf AC (logand AC !op))
		    (setf carry-flag (logbitp 7 AC))	
		    (set-nz-flags AC))	     
	 "ANC" t
	 :imm t :imm-cy 2)


(defvar *dump-character-tables* nil "Dump the patetern tables to the display because its neat")
(defvar *reset-pressed* nil )

(defun twos-complement (x)
  (if (minusp x)
      (1+ (logxor #xFF (abs x)))
      x))

(defmacro set-nz-flags (test-byte)
  `(progn
     (setf negative-flag (logbitp  7 ,test-byte))
     (setf zero-flag (equal  0 ,test-byte))))

(defmacro branch-if-flag (flag debug-string)
  `(progn
     (incf (of PC))		       
     (if ,flag
	 ;;great success 
	 (progn
	   (let ((displace (aref (o4 cpu-memory) (of PC))))
	     (incf (of PC))
	     (when (eq (ldb (byte 1 7) displace) 1) ; negative displacement calculate 2's complement
	       (setf displace (- 0 (1+ (logxor #xff displace)))))
	     ;; check if we are on the same page when we jump 
	     (if (equal (ldb (byte 8 8) (of PC)) (ldb (byte 8 8) (+ (of PC) displace)))
		 (incf cpu-cycles 3)
		 (incf cpu-cycles 4))			       
	     (setf (of PC) (ldb (byte 16 0) (+ (of PC) displace)))
	     (when debug-output (format t "~A ~X ~%" ,debug-string (of PC)))
	     ))
	 ;;else continue on 
	 (progn
	   (incf (of PC))		       
	   (incf cpu-cycles 2)
	   (when debug-output (format t "~A Failed~%" ,debug-string))
	   ))))
