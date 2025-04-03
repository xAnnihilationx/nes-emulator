(in-package :COMMON-LISP-USER)

(eval-when (:compile-toplevel :load-toplevel )
  (cl-user::gc :full t)
  (ql:quickload :cl-opengl)
    (ql:quickload :cl-glu)
    (ql:quickload :glop))

(defpackage :nes-emu
  (:nicknames :nes)
  (:use :common-lisp
	:glop
	:cl-opengl)
  (:export :load-rom
	   :run))
(in-package :nes-emu)


(defmacro do-immediate(code debug cycles)
  `(let ((operand  '(aref cpu-memory PC)))     
     `(progn
	(incf PC)	
	,@(sublis `((!op . ,operand)) ',code)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A #~X ~%" ,,debug  ,operand))
	(incf PC)
	)))


(defmacro do-zero-page(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (aref cpu-memory PC))))     
     `(progn
	(incf PC)	
	;,@(parser-thing ',code '!op operand)
	,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A $~X ~%" ,,debug  ,operand))
	(incf PC)
	)))

(defmacro do-zero-page-x(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))     
     `(progn
	(incf PC)		
	,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A $~X,X ~%" ,,debug  ,operand))
	(incf PC)
	)))

(defmacro do-zero-page-y(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (ldb (byte 8 0) (+ RY (aref cpu-memory PC))))))     
     `(progn
	(incf PC)	
					; ,@(parser-thing ',code '!op operand)
	,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	(incf cpu-cycles ,,cycles)
	(when debug-output (format t "~A $~X,Y ~%" ,,debug  ,operand))
	(incf PC)
	)))

(defmacro do-absolute(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (cpu-mirror-adjust address))))     
      `(progn
	 (let ((address #x0000))
	   (incf PC)
	   (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	   (incf PC)
	   (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
					;   ,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (incf cpu-cycles ,,cycles)
	   (when debug-output (format t "~A $~X ~%" ,,debug  address))
	   (incf PC)
	))))

(defmacro do-absolute-x(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (cpu-mirror-adjust (+ address RX)))))     
      `(progn
	 (let ((address #x0000))
	   (incf PC)
	   (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	   (incf PC)
	   (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (when debug-output (format t "~A $~X,X ~%" ,,debug  address))
	   (incf PC)
	   (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles ,,cycles)
	      (incf cpu-cycles (1+ ,,cycles)))
	))))

(defmacro do-absolute-y(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (cpu-mirror-adjust (+ address RY)))))     
      `(progn
	 (let ((address #x0000))
	   (incf PC)
	   (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	   (incf PC)
	   (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (when debug-output (format t "~A $~X,Y ~%" ,,debug  address))
	   (incf PC)
	   (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles ,,cycles)
	      (incf cpu-cycles (1+ ,,cycles)))
	))))


(defmacro do-index-indirect(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (cpu-mirror-adjust address ))))     
      `(progn
	 (let ((address #x0000))
	   (incf PC)
	   (let ((zero-page  (logand #x00FF(+ RX (aref cpu-memory PC )))))			   
	     (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	     (setf (ldb (byte 8 8) address) (aref cpu-memory  (logand #x00FF(+ 1 zero-page))))
	     (when debug-output (format t "~A ($~X,X) ~%" ,,debug (aref cpu-memory PC )))
	   )
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (incf PC)
	   (incf cpu-cycles ,,cycles)
	   ))))

(defmacro do-indirect-index(code debug cycles)
   `(let* ((operand  '(aref cpu-memory (cpu-mirror-adjust (+ address RY)))))     
      `(progn
	 (let ((address #x0000))
	   (incf PC)
	   (let ((zero-page (aref cpu-memory PC ) ))			   
	     (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	     (setf (ldb (byte 8 8) address) (aref cpu-memory  (logand #x00FF(+ 1 zero-page))))
	     (when debug-output (format t "~A ($~X),Y ~%" ,,debug zero-page))
	   )
					;,@(parser-thing ',code '!op operand)
	   ,@(sublis `((!op . ,operand)) ',code)  ;(parser-thing ',code '!op operand)
	   (incf PC)
	   (incf cpu-cycles ,,cycles)
	   (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles ,,cycles)
	      (incf cpu-cycles (1+ ,,cycles)))
	   ))))

(defmacro op-code (name code mnemonic  &key (imm nil) imm-cy
					 zp zp-cy
					 zpx zpx-cy
					 ab ab-cy
					 abx abx-cy
					 aby aby-cy
					 ixid ixid-cy
					 idix idix-cy)
  `(defmacro ,name  (address-mode)
     (case address-mode
       (:immediate ,(if imm `(do-immediate ,code ,mnemonic ,imm-cy) '(error "Adress mode not supported") ))
       (:zero-page ,(if zp `(do-zero-page ,code ,mnemonic ,zp-cy) '(error "Adress mode not supported") ))
       (:zero-page-x ,(if zpx `(do-zero-page-x ,code ,mnemonic ,zpx-cy) '(error "Adress mode not supported") ))
       (:absolute ,(if ab `(do-absolute ,code ,mnemonic ,ab-cy) '(error "Adress mode not supported") ))
       (:absolute-x ,(if abx `(do-absolute-x ,code ,mnemonic ,abx-cy) '(error "Adress mode not supported") ))
       (:absolute-y ,(if aby `(do-absolute-y ,code ,mnemonic ,aby-cy) '(error "Adress mode not supported") ))
       (:index-indirect ,(if ixid `(do-index-indirect ,code ,mnemonic ,ixid-cy) '(error "Adress mode not supported") ))
       (:indirect-index ,(if idix `(do-indirect-index ,code ,mnemonic ,idix-cy) '(error "Adress mode not supported") ))
       (otherwise (error "Dumbass!"))))) 

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter ppu-status-register #x2002)
(defparameter ppu-control-register #x2000)
(defparameter do-nestest nil) 
(defparameter upper-bank-rom #xC000 )
(defparameter lower-bank-rom #x8000 )
(defparameter cpu-memory (make-array #x10000 :element-type '(unsigned-byte 8)))
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

(defparameter ppu-memory (make-array #x10000 :element-type '(unsigned-byte 8)))
(defparameter spr-memory (make-array #xff :element-type '(unsigned-byte 8)) "The 256 Bytes of sprite ram the PPU has")
(defvar *rom-directory* (merge-pathnames  #P"projects/nes-emulator/roms/" (user-homedir-pathname)))
(defparameter program-rom nil "The program op codes and data")
(defparameter character-rom nil "The programs pattern tables if any")

(defparameter horizontal-mirroring nil )
(defparameter vertical-mirroring nil )
(defparameter battery-backed-ram nil )
(defparameter  debug-output nil)
(defparameter rest-ppu-status nil)
(defparameter previous-ppu-byte #x00)
)

(defmacro ppu-control-1 () `(aref cpu-memory ppu-control-register))
(defmacro ppu-status () `(aref cpu-memory ppu-status-register))

(defun initalize-memory-and-registers ()
  (dotimes (x (length cpu-memory))
    (setf (aref cpu-memory x) #x00))
  (dotimes (x (length ppu-memory))
    (setf (aref ppu-memory x) #x00))
  (dotimes (x (length spr-memory))
    (setf (aref spr-memory x) #x00))
  (setf AC 0)
  (setf RX 0)
  (setf RY 0)
  (setf SP #xFF)
  (setf PC #x8000)
  (set-status-register #x04) 
  )

(defun untwo (number)
  (if (logbitp 7 number)
      (- 0 (+ 1 (logxor  #xFF number)))
      number))

(defun io-catch (register)
  (let ((reg register))
    (case reg
      ;; reads to the ppu status register will clear it 
      (#x2002
       (setf previous-ppu-byte (ppu-status))
       (setf rest-ppu-status t ))      
      ) reg)
  )

;;CPU Memory locations $0000-$07FF are mirrored three times at $0800-$1FFF
;;CPU registers $2000-$2007 are also mirror at $2008-$3FFF
;; e.g. data written to  $0000 will also be written to $0800, $1000 and $1800
(defun cpu-mirror-adjust (address)
  "If the location is a mirror it adjusts it back to a non-mirror location also if the address exceeds 16bits it wraps it back around
and it also catches any read and writes to special memory mapped regions"
  (io-catch (cond 
  ((and (>= address #x0800 ) (<= address #x1FFF )) (mod address #x0800))
  ((and (>= address #x2008 ) (<= address #x3FFF ))  (+ #x2000 (mod address #x8)))
  ((> address #xFFFF ) (ldb (byte 16 0) address))
  (t address))))

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
  `(case (aref cpu-memory PC)
     ;;SEI Set interrupt disable status 1 byte 2 cycles
     (#x78  (setf interrupt-flag t)
	    (incf cpu-cycles 2)
	    (incf PC)
	    (when debug-output (format t "SEI~%"))
	    )

     ;;CLD Clear decimal mode 1 byte 2 cycles
     (#xD8  (setf decimal-mode-flag nil)
	    (incf cpu-cycles 2)
	    (incf PC)
	    (when debug-output (format t "CDL~%"))
	    )
     
     ;;SED - Set Decimal Flag
     (#xF8  (setf decimal-mode-flag t)
	    (incf cpu-cycles 2)
	    (incf PC)
	    (when debug-output (format t "SED~%"))
	    )
     
     ;;CLC - Clear Carry Flag
     (#x18  (setf carry-flag nil)
	    (incf cpu-cycles 2)
	    (incf PC)
	    (when debug-output (format t "CLC~%"))
	    )
     
     ;;SEC - Set Carry Flag
     (#x38  (setf carry-flag t)
	    (incf cpu-cycles 2)
	    (incf PC)
	    (when debug-output (format t "SEC~%"))
	    )
     
     ;;CLI - Clear Interrupt Disable
     (#x58 (setf interrupt-flag nil)
	   (incf cpu-cycles 2)
	   (incf PC)
	   (when debug-output (format t "CLI~%"))
	   )
     
     ;;CLV - Clear Overflow Flag
     (#xB8 (setf overflow-flag nil)
	   (incf cpu-cycles 2)
	   (incf PC)
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
     (#x9A (incf PC)
	   (setf SP RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TXS ~%"))
	   )
     ;;TSX - Transfer Stack Pointer to X
     (#xBA (incf PC)
	   (setf RX SP)
	   (incf cpu-cycles 2)
	   (set-nz-flags RX)
	   (when debug-output (format t "TSX ~%"))
	   )

     ;; TAX - Transfer Accumulator to X
     (#xAA (incf PC)
	   (setf RX AC)
	   (set-nz-flags RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TAX ~%" ))		       
	   )
     ;;TXA - Transfer X to Accumulator
     (#x8A (incf PC)
	   (setf AC RX)
	   (set-nz-flags AC)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TXA ~%" ))		       
	   )
     ;; TYA - Transfer Y to Accumulator
     (#x98 (incf PC)
	   (setf  AC RY)
	   (set-nz-flags AC)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "TYA ~%" ))
	   )

     ;; TAY - Transfer Accumulator to Y
     (#xA8 (incf PC)
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
     (#xCA (incf PC)
	   (if (zerop RX)
	       (setf RX #xFF)
	       (decf RX))
	   (set-nz-flags RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "DEX ~%" ))
	   )
     ;;DEY - Decrement Y Register Mode Implied, Bytes 1 , CPU Cycles 2 
     ;;Subtracts one from the Y register setting the zero and negative flags as appropriate.
     (#x88 (incf PC)
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
     (#xE8 (incf PC)
	   (setf RX (ldb (byte 8 0) (1+ RX)))
	   (set-nz-flags RX)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "INX ~%" ))
	   )
     ;;INY - Increment Y Register
     ;;Y,Z,N = Y+1
     ;;Adds one to the Y register setting the zero and negative flags as appropriate.
     (#xC8 (incf PC)
	   (setf RY (ldb (byte 8 0) (1+ RY)))
	   (set-nz-flags RY)
	   (incf cpu-cycles 2)
	   (when debug-output (format t "INY ~%" ))
	   )
     ;;JSR - Jump to Subroutine Mode Absolute Bytes 3 Cycles 6
     ;; The JSR instruction pushes the address (minus one) of the return point on to the stack and
     ;; then sets the program counter to the target memory address.
     (#x20 (let ((address #x0000))
	     (incf PC)
	     (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	     (incf PC)
	     (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	     ;; store the PC on the stack High then low byte
	     (when debug-output (format t "JSR ~X ~%" address ))
	     (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) PC))
	     (decw SP)
	     (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
	     (decw SP)
	     (setf PC address)
	     (incf cpu-cycles 6)))

     ;;BRK - Force Interrupt
     (#x00 (brk))
     
     ;;PHA - Push Accumulator
     (#x48 (incf PC)
	   (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) AC))
	   (decw SP)
	   (incf cpu-cycles 3)
	   (when debug-output (format t "PHA ~%"))
	   )
     ;;PLA - Pull Accumulator
     (#x68 (incf PC)
	   (incw SP)
	   (setf AC (aref cpu-memory (+ stack-bottom SP)))
	   (incf cpu-cycles 4)
	   (set-nz-flags AC)
	   (when debug-output (format t "PLA ~%"))
	   )		 
     ;;PHP - Push Processor Status
     (#x08 (incf PC)
	   (setf brk-flag t) ; PHP sets the break flag when pushed to the stack
	   (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) (status-register)))
	   (setf brk-flag nil)

	   (decw SP)
	   (incf cpu-cycles 3)
	   (when debug-output (format t "PHP ~%"))
	   )
     ;;PLP - Pull Processor Status
     (#x28 (let ((register #x00))
	     (incf PC)
	     (incw SP)
	     (setf register (aref cpu-memory (+ stack-bottom SP)))
	     (setf (ldb (byte 1 4) register) 0) ; ignore bit 4 
	     (set-status-register register)

	     (incf cpu-cycles 4)
	     (when debug-output (format t "PLP ~%"))
	     ))
     
     
     ;;RTS - Return from Subroutine Mode Implied Bytes 1  Cycles 6 
     ;;The RTS instruction is used at the end of a subroutine to return to the calling routine.
     ;;It pulls the program counter (minus one) from the stack.
     (#x60 (let ((address #x0000))
	     (incf PC)
	     (incw SP)
	     (setf (ldb (byte 8 0) address)(aref cpu-memory (+ stack-bottom SP)))
	     (incw SP)			 
	     (setf (ldb (byte 8 8) address)(aref cpu-memory (+ stack-bottom SP)))
	     (setf PC (+ 1 address))
	     (when debug-output (format t "RTS -> ~X ~%" address ))
	     (incf cpu-cycles 6)))

     ;;RTI - Return from Interrupt
     (#x40 (let ((address #x0000)
		 (register #x00))
	     (incf PC)
	     (incw SP)
	     (setf register (aref cpu-memory (+ stack-bottom SP)))
	     (setf (ldb (byte 1 4) register) 0) ; ignore bit 4 
	     (incw SP)
	     (setf (ldb (byte 8 0) address)(aref cpu-memory (+ stack-bottom SP)))
	     (incw SP)			 
	     (setf (ldb (byte 8 8) address)(aref cpu-memory (+ stack-bottom SP)))
	     (setf PC  address)
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
      (incf PC)
      (incf cpu-cycles 2)
      (when debug-output (format t "NOP ~%"))
      )

     ;; Undocumented op codes for 6502
     ;; double NOP
     ((#x04 #x14 #x34 #x44 #x54 #x64 #x74 #x80 #x82 #x89 #xC2 #xD4 #xE2 #xF4)
      (incf PC 2)
      (incf cpu-cycles 2)
      (when debug-output (format t "DOP ~%"))
      )

     ;; Triple NOP 
     ((#x0C #x1C #x3C #x5C #x7C #xDC #xFC)
      (incf PC 3)
      (incf cpu-cycles 4)
      (when debug-output (format t "TOP ~%"))
      )
     ((#x1A #x3A #x5A #x7A #xDA #xFA)
      (incf PC)
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
     
     (otherwise (error (format nil "Unknown Instruction ~X~%" (aref cpu-memory PC))))
     )
  )

(defmacro interrupt (maskable address-location-low address-location-high )
  (if maskable    
      `(let ((address #x0000))
	 (when (not interrupt-flag)

	   (setf (ldb (byte 8 0) address)(aref cpu-memory ,address-location-low))
	   (setf (ldb (byte 8 8) address)(aref cpu-memory ,address-location-high))     

	   (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) PC))
	   (decw SP)
	   (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
	   (decw SP)
	   (setf (aref cpu-memory (+ stack-bottom SP))(status-register))
	   (decw SP)
	   (setf interrupt-flag t)
	   (when debug-output (format t "IRQ -> ~X~% " address))
	   (setf PC address)
	   (incf cpu-cycles 7)))

      `(let ((address #x0000))    
	 (setf (ldb (byte 8 0) address)(aref cpu-memory ,address-location-low))
	 (setf (ldb (byte 8 8) address)(aref cpu-memory ,address-location-high))     
	 (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) PC))
	 (decw SP)
	 (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
	 (decw SP)
	 (setf (aref cpu-memory (+ stack-bottom SP))(status-register))
	 (decw SP)

	 (setf interrupt-flag t)
	 (when debug-output (format t "NMI -> ~X~% " address))
	 (setf PC address)
	 (incf cpu-cycles 7))      
      ))

(defmacro decw (x)
  `(if (eq  ,x 0)
      (setf ,x #xff)
      (decf ,x)))

(defmacro incw (x)
  `(if (eq  ,x #xff)
      (setf ,x 0)
      (incf ,x)))

(defmacro brk () 
  `(let ((address #x0000))
     (incf PC)
     (incf PC);; control always returns to the second byte past the BRK opcode.
     (setf (ldb (byte 8 0) address)(aref cpu-memory #xFFFE))
     (setf (ldb (byte 8 8) address)(aref cpu-memory #xFFFF))
     ;; store the PC on the stack High then low byte
     (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) PC))
     (decw SP)
     (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
     (decw SP)
     (setf brk-flag t)   
     (setf (aref cpu-memory (+ stack-bottom SP))(status-register))
     (decw SP)
     (setf brk-flag nil)
     (setf interrupt-flag t)    
     (setf PC address)
     (incf cpu-cycles 7)
     (when debug-output (format t "BRK  ~%"))
     ))

(defmacro compare-register (register address-mode debug-string)  
  (case address-mode
    (:immediate
     `(progn (incf PC)
	     (let ((result (- ,register (aref cpu-memory PC))))
	       (set-nz-flags result)
	       (setf carry-flag (>= result 0)))
	     (incf PC)
	     (incf cpu-cycles 2)
	     (when debug-output (format t "~A #~X ~%" ,debug-string (aref cpu-memory (- PC 1))))
	     ))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn (incf PC)
	     (let ((result (- ,register (aref cpu-memory (aref cpu-memory PC)))))
	       (set-nz-flags result)
	       (setf carry-flag (>= result 0)))
	     (incf PC)
	     (incf cpu-cycles 3)
	     (when debug-output (format t "~A $~X ~%" ,debug-string (aref cpu-memory (- PC 1))))
	     ))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (let ((result (- ,register (aref cpu-memory (cpu-mirror-adjust address)))))
	    (set-nz-flags result)
	    (setf carry-flag (>=  result 0)))
	  (incf PC)
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
     `(progn (incf PC)
	     (let ((address #x0000)
		   (actual #x0000))
	       (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	       (incf PC)
	       (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	       (incf PC)
	       (setf address (cpu-mirror-adjust address))

	       ;; An original 6502 has does not correctly fetch the target address if the
	       ;;indirect vector falls on a page boundary (e.g. $xxFF where xx is and value from $00 to $FF).
	       ;;In this case fetches the LSB from $xxFF as expected but takes the MSB from $xx00.
	       (if (eq (ldb (byte 8 0) address) #xFF)
		   (progn
		     (setf (ldb (byte 8 0) actual )(aref cpu-memory address))
		     (setf (ldb (byte 8 8) actual )(aref cpu-memory (logand #xFF00 address)))
		     )
		   (progn
		     (setf (ldb (byte 8 0) actual )(aref cpu-memory address))
		     (setf (ldb (byte 8 8) actual )(aref cpu-memory(+ 1 address)))
		     )		   
		   )

	       (setf  PC (cpu-mirror-adjust actual))

	       (incf cpu-cycles 5)
	       (when debug-output (format t "JMP ($~X) -> ~X ~%" address actual ))
	       )))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  
	  (setf  PC (cpu-mirror-adjust address))


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
	(incf PC)
	(setf carry-flag (logbitp 7 AC))
	(setf AC (ldb (byte 8 0) (* AC 2 )))	
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "ASL A ~%" ))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(let ((value (aref cpu-memory (aref cpu-memory PC)) ))
	  (setf carry-flag (logbitp 7 value))
	  (setf value (ldb (byte 8 0) (* value 2 )))
	  (setf (aref cpu-memory (aref cpu-memory PC)) value)
	  (incf PC)
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "ASL $~X ~%" (aref cpu-memory (- PC 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
       `(progn
	  (incf PC)
	  (let ((value ,memory-value ))
	    (setf carry-flag (logbitp 7 value))
	    (setf value (ldb (byte 8 0) (* value 2 )))
	    (setf ,memory-value value)
	    
	    (incf PC)
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "ASL $~X,X ~%" (aref cpu-memory (- PC 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	    (let ((value ,memory-value ))	    
	      (setf carry-flag (logbitp 7 value))
	      (setf value (ldb (byte 8 0) (* value 2 )))
	      (setf ,memory-value value)
	      
	      (incf PC)
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "ASL $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref cpu-memory (cpu-mirror-adjust (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	    (incf PC)
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
	(incf PC)
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
	(incf PC)
	(let ((value (aref cpu-memory (aref cpu-memory PC)) )
	      (old-carry (if carry-flag 1 0)))	      
	  (setf carry-flag (logbitp 7 value))
	  (setf value (ldb (byte 8 0) (* value 2 )))
	  (setf (ldb (byte 1 0) value) old-carry)
	  
	  (setf (aref cpu-memory (aref cpu-memory PC)) value)
	  (incf PC)
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "ROL $~X ~%" (aref cpu-memory (- PC 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
       `(progn
	  (incf PC)
	  (let ((value ,memory-value )
		(old-carry (if carry-flag 1 0)))

	    (setf carry-flag (logbitp 7 value))
	    (setf value (ldb (byte 8 0) (* value 2 )))
	    (setf (ldb (byte 1 0) value) old-carry)
	    (setf ,memory-value value)
	    
	    (incf PC)
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "ROL $~X,X ~%" (aref cpu-memory (- PC 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	    (let ((value ,memory-value )
		  (old-carry (if carry-flag 1 0)))

	      (setf carry-flag (logbitp 7 value))
	      (setf value (ldb (byte 8 0) (* value 2 )))
	      (setf (ldb (byte 1 0) value) old-carry)
	      (setf ,memory-value value)
	      
	      (incf PC)
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "ROL $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref cpu-memory (cpu-mirror-adjust (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	    (incf PC)
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
	(incf PC)
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
	(incf PC)
	(let ((value (aref cpu-memory (aref cpu-memory PC)) )
	      (old-carry (if carry-flag 1 0)))	      
	  (setf carry-flag (logbitp 0 value))
	  (setf value (ldb (byte 8 0) (floor (/ value 2 ))))
	  (setf (ldb (byte 1 7) value) old-carry)
	  
	  (setf (aref cpu-memory (aref cpu-memory PC)) value)
	  (incf PC)
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "ROR $~X ~%" (aref cpu-memory (- PC 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
       `(progn
	  (incf PC)
	  (let ((value ,memory-value )
		(old-carry (if carry-flag 1 0)))

	    (setf carry-flag (logbitp 0 value))
	    (setf value (ldb (byte 8 0) (floor (/ value 2 ))))
	    (setf (ldb (byte 1 7) value) old-carry)
	    (setf ,memory-value value)
	    
	    (incf PC)
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "ROR $~X,X ~%" (aref cpu-memory (- PC 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	    (let ((value ,memory-value )
		  (old-carry (if carry-flag 1 0)))

	      (setf carry-flag (logbitp 0 value))
	      (setf value (ldb (byte 8 0) (floor (/ value 2 ))))
	      (setf (ldb (byte 1 7) value) old-carry)
	      (setf ,memory-value value)
	      
	      (incf PC)
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "ROR $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref cpu-memory (cpu-mirror-adjust (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	    (incf PC)
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
	(incf PC)
	(setf carry-flag (logbitp 0 AC))
	(setf AC (ldb (byte 8 0) (floor (/ AC 2 ))))
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "LSR A ~%" ))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(let ((value (aref cpu-memory (aref cpu-memory PC)) ))	  
	  (setf carry-flag (logbitp 0 value))
	  (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	  (setf (aref cpu-memory (aref cpu-memory PC)) value)
	  (incf PC)
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "LSR $~X ~%" (aref cpu-memory (- PC 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
       `(progn
	  (incf PC)
	  (let ((value ,memory-value ))	    
	    (setf carry-flag (logbitp 0 value))
	    (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	    (setf ,memory-value value)	    
	    (incf PC)
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "LSR $~X,X ~%" (aref cpu-memory (- PC 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	    (let ((value ,memory-value ))
	      (setf carry-flag (logbitp 0 value))
	      (setf value (ldb (byte 8 0) (floor(/ value 2 ))))
	      (setf ,memory-value value)
	      
	      (incf PC)
	      (set-nz-flags value)
	      (incf cpu-cycles 6)
	      (when debug-output (format t "LSR $~X ~%" address))
	      )))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     (let ((memory-value '(aref cpu-memory (cpu-mirror-adjust (+ address RX)))))
       `(progn
	  (let ((address #x0000))
	    (incf PC)
	    (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	    (incf PC)
	    (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	    (incf PC)
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
	(incf PC)
	(setf ,register (aref cpu-memory PC))
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags ,register)
	(when debug-output (format t "~A #~X ~%" ,debug-string ,register))
	))    
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf ,register (aref cpu-memory (aref cpu-memory PC)) )
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags ,register)
	(when debug-output (format t "~A $~X ~%" ,debug-string (aref cpu-memory (- PC 1))))
	))
    (:zero-page-y
     ;;2 bytes 4 cycles
     `(progn
	(incf PC)
	(setf ,register (aref cpu-memory (ldb (byte 8 0) (+ RY (aref cpu-memory PC)))))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags ,register)
	(when debug-output (format t "~A $~X,Y ~%" ,debug-string (aref cpu-memory (- PC 1))))	
	))
    (:zero-page-x
     ;;2 bytes 4 cycles
     `(progn
	(incf PC)
	(setf ,register (aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC)))))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags ,register)
	(when debug-output (format t "~A $~X,Y ~%" ,debug-string  (aref cpu-memory (- PC 1))))	
	))

    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf ,register (aref cpu-memory (cpu-mirror-adjust address)))
	  (set-nz-flags ,register)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "~A $~X ~%" ,debug-string address))
	)))
    (:absolute-y
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf ,register (aref cpu-memory (cpu-mirror-adjust (+ address RY))))			 
	  (set-nz-flags ,register )
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "~A $~X,Y ~%" ,debug-string address))
	)))    
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf ,register (aref cpu-memory (cpu-mirror-adjust (+ address RX))))			 
	  (set-nz-flags ,register )
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
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
	(incf PC)
	(setf (aref cpu-memory (aref cpu-memory PC)) ,register )
	(incf PC)
	(incf cpu-cycles 3)
	(when debug-output (format t "~A $~X ~%" ,debug-string (aref cpu-memory (- PC 1))))
	))
    (:zero-page-x
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf (aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC)))) ,register )
	(incf PC)
	(incf cpu-cycles 4)
	(when debug-output (format t "~A $~X,X ~%" ,debug-string (aref cpu-memory (- PC 1))))	
	))
    (:zero-page-y
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf (aref cpu-memory (ldb (byte 8 0) (+ RY (aref cpu-memory PC)))) ,register )
	(incf PC)
	(incf cpu-cycles 4)
	(when debug-output (format t "~A $~X,Y ~%" ,debug-string (aref cpu-memory (- PC 1))))	
	))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (aref cpu-memory (cpu-mirror-adjust address)) ,register)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "~A $~X ~%" ,debug-string address))       
	)))
   
    (otherwise (error "Dumbass!"))
    ))

;; CMP a fucking again
(op-code compare ((let ((result (- AC !op)))
		    (set-nz-flags result)
		    (setf carry-flag (>=  result 0))))
	 "CMP"
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4
	 :aby t :aby-cy 4
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5)


;;BIT - Bit Test
;;A & M, N = M7, V = M6
;;This instructions is used to test if one or more bits are set in a target memory location.
;;The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result is not kept.
;;Bits 7 and 6 of the value from memory are copied into the N and V flags.
(op-code bit-test ((let ((value !op))
		     (setf zero-flag (eq 0 (logand value AC)))
		     (setf negative-flag (logbitp 7 value))
		     (setf overflow-flag (logbitp 6 value))))
	 "BIT"
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
	 "ADC"
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4
	 :aby t :aby-cy 4
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5)

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
	 "SBC"
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4
	 :aby t :aby-cy 4
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5)

;;DEC - Decrement Memory
;;M,Z,N = M-1
;;Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
(op-code dec ((let ((value   (ldb (byte 8 0) (- !op 1)) ))
			(setf !op  value )
			(set-nz-flags value)))
	 "DEC"
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7)

;;INC
(op-code inc ((let ((value   (ldb (byte 8 0) (+ !op 1)) ))
			(setf !op  value )
			(set-nz-flags value)))
	 "INC"
	 :zp t :zp-cy 5
	 :zpx t :zpx-cy 6
	 :ab t :ab-cy 6
	 :abx t :abx-cy 7)

;;AND - Logical AND
;;A,Z,N = A&M
;;A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
(op-code logical-and ((setf AC (logand AC !op))
		     (set-nz-flags AC))
	 "AND"
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4
	 :aby t :aby-cy 4
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5)

;; EOR - exsclusive or 
(op-code logical-xor ((setf AC (logxor AC !op))
		     (set-nz-flags AC))
	 "EOR"
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4
	 :aby t :aby-cy 4
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5)

;;LAX
;;Load accumulator and X register with memory.
(defmacro lax (address-mode ) 
    (case address-mode
      (:zero-page (do-zero-page ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 3))    
      (:zero-page-y (do-zero-page-y ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 4))  
      (:absolute (do-absolute ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 4))
      (:absolute-y (do-absolute-y ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 4))  
      (:index-indirect (do-index-indirect ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 6))
      (:indirect-index (do-indirect-index ((setf AC !op)(setf RX AC) (set-nz-flags AC)) "LAX" 5))   
      (otherwise (error "Dumbass!"))
    ))

;;SAX
;;AND X register with accumulator and store result in memory. 
(defmacro sax (address-mode ) 
    (case address-mode
      (:zero-page (do-zero-page ((let ((result (logand AC RX))) (setf !op result ) )) "SAX" 3))
      (:zero-page-y (do-zero-page-y ((let ((result (logand AC RX)))(setf !op result ))) "SAX" 4))     
      (:absolute (do-absolute ((let ((result (logand AC RX))) (setf !op result ) )) "SAX" 4))      
      (:index-indirect (do-index-indirect ((let ((result (logand AC RX))) (setf !op result ) )) "SAX" 6))      
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
	 "DCP"
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
	 "ORA"
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4
	 :aby t :aby-cy 4
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 6)
;;STA
(op-code store-accumulator ((setf !op AC ))
	 "STA"
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 5
	 :aby t :aby-cy 6
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 6)

;;LDA load accumulator 
(op-code load-accumulator ((setf AC !op)
		     (set-nz-flags AC))
	 "LDA"
	 :imm t :imm-cy 2
	 :zp t :zp-cy 3
	 :zpx t :zpx-cy 4
	 :ab t :ab-cy 4
	 :abx t :abx-cy 4
	 :aby t :aby-cy 4
	 :ixid t :ixid-cy 6
	 :idix t :idix-cy 5)

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
	 "ISC"
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
	 "SLO"
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
	 "RLA"
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
	 "SRE"
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
	 "RRA"
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
	 "ANC"
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
     (incf PC)		       
     (if ,flag
	 ;;great success 
	 (progn
	   (let ((displace (aref cpu-memory PC)))
	     (incf PC)
	     (when (eq (ldb (byte 1 7) displace) 1) ; negative displacement calculate 2's complement
	       (setf displace (- 0 (1+ (logxor #xff displace)))))
	     ;; check if we are on the same page when we jump 
	     (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ PC displace)))
		 (incf cpu-cycles 3)
		 (incf cpu-cycles 4))			       
	     (setf PC (ldb (byte 16 0) (+ PC displace)))
	     (when debug-output (format t "~A ~X ~%" ,debug-string PC))
	     ))
	 ;;else continue on 
	 (progn
	   (incf PC)		       
	   (incf cpu-cycles 2)
	   (when debug-output (format t "~A Failed~%" ,debug-string))
	   ))))


(defun rom-path (rom-name) 
  (merge-pathnames (merge-pathnames  #P"projects/nes-emulator/roms/" (user-homedir-pathname))
		   (make-pathname :directory '(:relative) :name  rom-name :type "nes") ))


(defmethod glop:on-key (window pressed keycode keysym text)
  (format t "~A" keysym )
  (when (and (not pressed) (eq keysym :escape))
    (glop:push-close-event window))
  (when (and (not pressed) (eq keysym :f9))
    (setf *reset-pressed* (not *reset-pressed*)))

  (when (and (not pressed) (eq keysym :f2))
    (setf debug-output (not debug-output)))
    
  (when (and (not pressed) (eq keysym :f1))    
    (if *dump-character-tables*
	(setf *dump-character-tables* nil)
	(setf *dump-character-tables* t))))

(defmethod glop:on-button (window pressed button)
  (declare (ignore window))
  (format t "Button ~:[released~;pressed~]: ~S~%" pressed button))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window x y dx dy)))

(defmethod glop:on-resize (window w h)
  (declare (ignore window))
    (gl:viewport 0 0 w h)
  )

(defmethod glop:on-draw (window)
  (declare (ignore window)))

(defmethod glop:on-close (window)
  (declare (ignore window))
  (format t "Close~%"))

(defun run ()
  "execute the rom and play the game"
  (let ((pattern-tables nil)
	(on-reset t)
	(cpu-cycles 0)
	(odd-frame t)
	(first-frame t)
;	(last-status 0)
;	(last-status-message nil)
	(nestestlines (if do-nestest (load-nestest-log) nil) )       
	)
	
    (dotimes (x (/ (length character-rom) #x1000))
      (push (dump-pattern-tables x) pattern-tables))    

    ;; NMI are disabled on reset 
    (setf (ldb (byte 1 7) (ppu-control-1)) 0)   
    (setf *reset-pressed* t)

    

    (when do-nestest
      (format t "Setting up for nestest")
      (setf *reset-pressed* nil)
      (setf on-reset nil)
      (setf debug-output t)
      )
    
    
    (glop:with-window (win "NES" 800 600)
      (glu:ortho-2d 0 800 600 0)
      (gl:clear-color 0 0 0 0)
      

      
      
      (loop while (glop:dispatch-events win :blocking nil) do
	 ;; gl code here
	   (gl:clear :color-buffer)
	   
	   (when *dump-character-tables*
	     (dotimes (x (/ (length character-rom) #x1000))
	       (gl:window-pos 0 (- 592 (* 8 x)))	  
	       (gl:draw-pixels 512 8 :rgb :unsigned-byte (nth x pattern-tables))))

	   (when *reset-pressed*
	     (setf *reset-pressed* nil)
	     (setf interrupt-flag nil) 
	     (setf on-reset t))
	   
	 (setf cpu-cycles 0)
	   
	 ;;Reset interrupts are triggered when the system first starts and when the user presses the
	 ;;reset button. When a reset occurs the system jumps to the address located at $FFFC and
	 ;;$FFFD.
	 ;; Generate a reset interrupt
	   (when on-reset
	     (setf cpu-cycles 0)
	     (format t "REST = ~A~%" on-reset)
	     (setf on-reset nil)
	     (interrupt t  #xFFFC  #xFFFD)
	     (setf interrupt-flag t)
	     ) 

	 ;;every other frame has one less pixel per scan line	  
	   (setf odd-frame (not odd-frame))
	   
	 ;; execute the program for:
	 ;; 1. 1/60 second of cpu cycles 29830 cycles
	 ;; 2. 1/60 second real time has elapsed
	 ;; 3. cpu is wating on a vblink
	  ;; (setf last-status (aref cpu-memory #x6000))
	   
	   (let* ((start-time (get-internal-real-time))
		 ; (end-time (+ start-time (/ internal-time-units-per-second 60)))		 
		  (refresh nil)
		  (cpu-cycles-start 0)
	;	  (cpu-cycles-elapsed 0)
	;	  (ppu-beam-x 0)
		  ;(ppu-beam-y 0)
		  )
			  
	     (do ((loop-time start-time (get-internal-real-time)))

		 ;;(>= loop-time end-time)
		 ((or (>= cpu-cycles 29781)
		      refresh))

	       ;;(when (not ( string-equal (print-blarg-status) last-status-message))
		;; (setf last-status-message (print-blarg-status))
		;; (format t "~A" (print-blarg-status)))
	       
	   ;;    (when (not (eq last-status (aref cpu-memory #x6000)))
	;;	 (setf last-status (aref cpu-memory #x6000)) 
;;		 (format t "~X~%" (aref cpu-memory #x6000)))
	       
	     ;  (if (eq (aref cpu-memory #x6000) #x80)
;		   (setf debug-output t)
;		   (setf debug-output nil))

	     ;;  (when (eq PC #xC000) (setf debug-output t))
	       (when debug-output (format t "CYCLE=~5D PC=~4X P=~2X X=~2X Y=~2X A=~2X SP=~2X "
					  cpu-cycles PC (status-register) RX RY AC SP))

	       (when do-nestest 
	       (when (eq (gethash (format nil "~4,'0,X" PC) nestestlines) nil)
		   (error "shits fucked up yo  ")
		  
		   ))
	       
	     ;;  (when debug-output (format t  " PPU-CONTROL-1=~X  PPU-STATUS= ~X " (ppu-control-1) (ppu-status)))
	       
	  ;     (when (eq PC #xE976) (error "Break point!" ))
	 ;      (when (eq PC #xE8D5) (error "Break point!" ))

	       

	       (when (minusp SP)
		 (setf SP  (ldb (byte 8 0) SP))
		 (warn "Stack is fuckerd")
		     )

	       
	       (when rest-ppu-status
		 ;; if a write was preformed  or it the first frame then ingnore
		 (if (or first-frame (not (equal (ppu-status) previous-ppu-byte)))
		     (setf (ppu-status)  previous-ppu-byte)
		     (setf (ldb (byte 1 7) (ppu-status)) 0))
		 (setf rest-ppu-status nil))

	       (setf cpu-cycles-start cpu-cycles)

	       ;; enter vblank at the start of the frame 
	       (when (eq cpu-cycles 0)
		 (setf (ldb (byte 1 7) (ppu-status)) 1)

		 ;; emit a NMI if we are in a vblank and it hasn't been disabled 
		 (when (logbitp  7 (ppu-control-1))
		   (interrupt nil  #xFFFA  #xFFFB))
		 ) 

	       ;;end vblank at the end of the 20th scan line 
	       (when (>= cpu-cycles 2274)
		 (setf (ldb (byte 1 7) (ppu-status)) 0)) 

	       ;; Update the CPU
	       (process-cpu)

	       ;;(setf cpu-cycles-elapsed (- cpu-cycles cpu-cycles-start))
	       
	       
	       ;; Update the PPU
	       ;; 341 pixels per scan line 340 if its an odd frame and BG and/or OBJ are enabled.
	       ;; 262 scanlines total
	       ;; 3 PPU clocks per CPU clock
	       ;; 1 Pixel per PPU clock	       
	       ;; calculate the PPUs beam position

	       ;; increment the 
	      ;;(setf ppu-beam-x  (+  (* 
	       
	       ;(/  cpu-clcles (/ 341 3 ))
	       ;; generate a VBLINK NMI

	       

	       ;; keep track of ppu clock cycles 3 ppu cycles for every cpu cycle
	       ;; 341x262 screen size 
	       ;; Vblank for the first 2273 cpu cycles
	       )

	       (when first-frame (setf  first-frame nil))
	       
	     ;; (format t "CPU-CYCLES ~D~%" cpu-cycles)
	     )
	   (glop:swap-buffers win)))
    ))



#|
 VRAM    Contents of                     Colour 
       Addr   Pattern Table                    Result
      ------ ---------------                  --------
      $0000: %00010000 = $10 --+              ...1.... Periods are used to
        ..   %00000000 = $00   |              ..2.2... represent colour 0.
        ..   %01000100 = $44   |              .3...3.. Numbers represent
        ..   %00000000 = $00   +-- Bit 0      2.....2. the actual palette
        ..   %11111110 = $FE   |              1111111. colour #.
        ..   %00000000 = $00   |              2.....2.
        ..   %10000010 = $82   |              3.....3.
      $0007: %00000000 = $00 --+              ........

      $0008: %00000000 = $00 --+
        ..   %00101000 = $28   |
        ..   %01000100 = $44   |
        ..   %10000010 = $82   +-- Bit 1
        ..   %00000000 = $00   |
        ..   %10000010 = $82   |
        ..   %10000010 = $82   |
      $000F: %00000000 = $00 --+
|#
(defun decode-pattern (pattern-number)
  (let ((index (* pattern-number 16 16)))
    (dotimes (y 8)
      (dotimes (z 16)
	(dotimes (x 8)
	  (let ((value 0))

	  ;; set lower bit 
	  (setf (ldb (byte 1 0) value) (ldb (byte 1  x) (aref character-rom (+ index 0 y (* z 16)))))
	  (setf (ldb (byte 1 1) value) (ldb (byte 1  x) (aref character-rom (+ index 8 y (* z 16)))))
	  (case value
	    (0 (format t "." ))
	    (1 (format t "M" ))
	    (2 (format t "#" ))
	    (3 (format t "N" )))

	  )))
      (format t "~%"))))


(defun dump-pattern-tables (table-number)
  (let ((index (* table-number #x1000))
	(table (make-array (* #x1000 3) :element-type '(unsigned-byte 8))))
      (dotimes (y 8)
	(dotimes (z 64)
	  (dotimes (x 8)
	    (let ((value 0)
		  (tile-index (+ (* 512 (- 7 y ) 3) (* (- 7 x) 3) (* 24 z)) ))
;		  (tile-index (+ (* 512 y 3) (* x 3) (* 24 z)) ))
	      
	  ;; set lower bit 
	      (setf (ldb (byte 1 0) value) (ldb (byte 1  x) (aref character-rom (+ index 0 y (* z 16)))))
	      (setf (ldb (byte 1 1) value) (ldb (byte 1  x) (aref character-rom (+ index 8 y (* z 16)))))
	      (case value
		(0  (setf (aref table (+ tile-index 0) ) #xFF)
		    (setf (aref table (+ tile-index 1) ) #x00)
		    (setf (aref table (+ tile-index 2) ) #x00))

		(1  (setf (aref table (+ tile-index 0) ) #x00)
		    (setf (aref table (+ tile-index 1) ) #xFF)
		    (setf (aref table (+ tile-index 2) ) #x00))

		(2  (setf (aref table (+ tile-index 0) ) #x00)
		    (setf (aref table (+ tile-index 1) ) #x00)
		    (setf (aref table (+ tile-index 2) ) #xFF))

		(3  (setf (aref table (+ tile-index 0) ) #xFF)
		    (setf (aref table (+ tile-index 1) ) #xFF)
		    (setf (aref table (+ tile-index 2) ) #xFF)))))))


      table))


#|
 A. iNES Format (.NES)
  ---------------------
    +--------+------+------------------------------------------+
    | Offset | Size | Content(s)                               |
    +--------+------+------------------------------------------+
    |   0    |  3   | 'NES'                                    |
    |   3    |  1   | $1A                                      |
    |   4    |  1   | 16K PRG-ROM page count                   |
    |   5    |  1   | 8K CHR-ROM page count                    |
    |   6    |  1   | ROM Control Byte #1                      |
    |        |      |   %####vTsM                              |
    |        |      |    |  ||||+- 0=Horizontal mirroring      |
    |        |      |    |  ||||   1=Vertical mirroring        |
    |        |      |    |  |||+-- 1=SRAM enabled              |
    |        |      |    |  ||+--- 1=512-byte trainer present  |
    |        |      |    |  |+---- 1=Four-screen mirroring     |
    |        |      |    |  |                                  |
    |        |      |    +--+----- Mapper # (lower 4-bits)     |
    |   7    |  1   | ROM Control Byte #2                      |
    |        |      |   %####0000                              |
    |        |      |    |  |                                  |
    |        |      |    +--+----- Mapper # (upper 4-bits)     |
    |  8-15  |  8   | $00                                      |
    | 16-..  |      | Actual 16K PRG-ROM pages (in linear      |
    |  ...   |      | order). If a trainer exists, it precedes |
    |  ...   |      | the first PRG-ROM page.                  |
    | ..-EOF |      | CHR-ROM pages (in ascending order).      |
    +--------+------+------------------------------------------+
|#
(defun load-rom (rom-name)
  "loads the rom"
  (initalize-memory-and-registers)
  
  (with-open-file (rom (rom-path rom-name) :element-type '(unsigned-byte 8) )
    (let ((s (make-array 16 :element-type '(unsigned-byte 8)))
	  (trainer nil)
	  (prg-rom-count 0)
	  (chr-rom-count 0)
	  (trainer-present nil)
	  (four-screen-mirroring nil)
	  (memory-mapper-number 0)
	  (ram-bank-count 1))

      (read-sequence s rom :end 16)

      (unless (string-equal (map 'string #'code-char (subseq s 0 3)) "NES")
	(error "not an nes rom!"))

      (setq prg-rom-count (aref s 4))
      (setq chr-rom-count (aref s 5))

      (format t "The ~A Cartrage has:~%~@(~R~) 16kb program rom bank~P (~D kb total)~%"
	      rom-name
	      prg-rom-count
	      prg-rom-count
	      (* prg-rom-count 16))

      (format t "~@(~R~) 8kb character rom bank~P (~D kb total)~%"
	      chr-rom-count
	      chr-rom-count
	      (* chr-rom-count  8))

      ;; get control bytes
      (format t "Control byte-1 (~8,'0,B)~%"(aref s 6))
      (format t "Control byte-2 (~8,'0,B)~%"(aref s 7))

      (if (eq (ldb (byte 1 0) (aref s 6)) 1)
	  (progn (setf vertical-mirroring t) (format t "Vertical Mirroring engaged!~%"))
	  (progn (setf horizontal-mirroring t) (format t "Horizontal Mirroring engaged!~%")))

      (when (eq (ldb (byte 1 1) (aref s 6)) 1)
	  (progn (setf battery-backed-ram t) (format t "Batter back ram engaged!~%")))

      (when (eq (ldb (byte 1 2) (aref s 6)) 1)
	(progn (setf trainer-present  t) (format t "Trainer engaged!~%")
	       (setf trainer (make-array 512 :element-type '(unsigned-byte 8))))) 

      (when (eq (ldb (byte 1 3) (aref s 6)) 1)
	  (progn (setf four-screen-mirroring  t) (format t "Four screen mirroring engaged!~%")))
      
      (setf (ldb (byte 4 0) memory-mapper-number) (ldb (byte 4 4) (aref s 6)))
      (setf (ldb (byte 4 4)  memory-mapper-number) (ldb (byte 4 4) (aref s 7)))

      (format t "Using memory mapper ~D~%" memory-mapper-number)

      (when (> (aref s 8) 1)
	(setf ram-bank-count (aref s 8)))

      (format t "Using ~R 8kb ram banks (~Dkb total)" ram-bank-count (* ram-bank-count 8))
      
      (when trainer-present
	(read-sequence trainer rom :end 512)
	(format t  "Trainer is loaded!~%"))
      
      ;;load the program rom
      (let ((rom-byte-count (* prg-rom-count 1024 16)))
	(setf program-rom (make-array rom-byte-count :element-type '(unsigned-byte 8)))
	(read-sequence program-rom rom :end rom-byte-count))

      ;;load the character rom
      (let ((rom-byte-count (* chr-rom-count 1024 8)))
	(setf character-rom (make-array rom-byte-count :element-type '(unsigned-byte 8)))
	(read-sequence character-rom rom :end rom-byte-count))

      ;; move the program into cpu memory
      (cond
	;; copy it into the lower and upper bank
	((eq prg-rom-count 1)
	 (setf (subseq cpu-memory upper-bank-rom (+ upper-bank-rom (length program-rom))) program-rom)
	 (setf (subseq cpu-memory lower-bank-rom (+ lower-bank-rom (length program-rom))) program-rom)
	 (setf PC upper-bank-rom))

	;; copy it into the lower bank 
	((eq prg-rom-count 2)
	 (setf (subseq cpu-memory lower-bank-rom (+ lower-bank-rom (length program-rom))) program-rom)
	 (setf PC lower-bank-rom))	 

	;; copy the first 32k into the lower bank
;	((> 2 eq prg-rom-count)
;	 (setf (subseq cpu-memory lower-bank-rom #x8000) program-rom))
	((error "wat")))

      ;; move the pattern tables into the PPU memory
      (cond
	;;load the character rom into memory
	((eq chr-rom-count 1)
	 (setf (subseq ppu-memory 0  (length character-rom)) character-rom))

	;; load just the first 8kb 
	((> chr-rom-count 1)
	 (setf (subseq ppu-memory 0  #x2000) character-rom)))

      ))
(format t "~%~%")

  )

(defun test-1 ()
  (progn (load-rom "DEMO") (run))
  )

(defun test-2 ()
  (progn (load-rom "Super Mario Bros.") (run))
  )
(defun test-3 ()
  (setf do-nestest t)
  (progn (load-rom "nestest") (run))
  )

(defun print-blarg-status ()
  (let ((message nil))
    (do ((x #x6004 (incf x)))
	((eq (aref cpu-memory x) 0))
      (setf message (concatenate 'string message (format nil "~A" (code-char (aref cpu-memory x)))))) message))


(defun load-nestest-log ()
  (let ((addresses (make-hash-table :test 'equal)))   
  (with-open-file (rom "/home/frank/projects/nes-emulator/nestest-addresses.log" )
    (do ((x (read-line rom) (read-line rom nil 'eof)))
	((equal x 'eof))
     (setf (gethash x addresses)  t)
      ))
  addresses 
  ))
