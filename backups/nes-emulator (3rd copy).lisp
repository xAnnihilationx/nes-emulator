(in-package :COMMON-LISP-USER)

(eval-when (:compile-toplevel :load-toplevel )
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

#|
Please note the two separate 16K PRG-ROM segments; they may be linear,
    but they play separate roles depending upon the size of the cartridge.
    Some games only hold one (1) 16K bank of PRG-ROM, which should be
    loaded into both $C000 and $8000.

    Most games load themselves into $8000, using 32K of PRG-ROM space. The
    first game to use this method is Super Mario Brothers. However, all
    games wit more than one (1) 16K bank of PRG-ROM load themselves into
    $8000 as well. These games use Memory Mappers to swap in and out PRG-ROM
    data, as well as CHR-ROM.
|#
(defparameter ppu-status-register #x2002)

(defparameter upper-bank-rom #xC000 )

(defparameter lower-bank-rom #x8000 )

(defvar cpu-memory (make-array #x10000 :element-type '(unsigned-byte 8)))

(defparameter PC lower-bank-rom "The cpus program counter" )

(defparameter stack-bottom #x0100 "The stacks bottom ") 

(defparameter SP #x00 "The stack pointer for the cpu")

(defparameter AC #x00 "The cpus accumulator")

(defparameter RX 0 "The cpus X register")

(defparameter RY 0 "The cpus Y register")


;;CPU Memory locations $0000-$07FF are mirrored three times at $0800-$1FFF
;;CPU registers $2000-$2007 are also mirror at $2008-$3FFF
;; e.g. data written to  $0000 will also be written to $0800, $1000 and $1800
(defun cpu-mirror-adjust (address)
  "If the location is a mirror it adjusts it back to a non-mirror location"
  (cond 
  ((and (>= address #x0800 ) (<= address #x1FFF )) (mod address #x0800))
  ((and (>= address #x2008 ) (<= address #x3FFF )) (+ #x2000 (mod address #x8)) )
  (t address)))



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

(defparameter interrupt-flag nil
  "This is an interrupt enable/disable flag. If it is set,
   interrupts are disabled. If it is cleared, interrupts are enabled.")

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

(defun status-register ()
  "returns the process status byte"
  (let ((register #x00))
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
  (setf carry-flag (if (eq (ldb (byte 1 0) register-byte) 1) t nil))
  (setf zero-flag  (if (eq (ldb (byte 1 1) register-byte) 1) t nil))
  (setf interrupt-flag  (if (eq 1 (ldb (byte 1 2) register-byte)) t nil))
  (setf decimal-mode-flag  (if (eq 1 (ldb (byte 1 3) register-byte)) t nil))
  (setf brk-flag (if (eq 1 (ldb (byte 1 4) register-byte))  t nil))
  (setf overflow-flag (if (eq 1 (ldb (byte 1 6) register-byte)) t nil))
  (setf negative-flag (if (eq 1 (ldb (byte 1 7) register-byte)) t nil))
  )

(defvar ppu-memory (make-array #x10000 :element-type '(unsigned-byte 8)))
(defparameter spr-memory (make-array #xff :element-type '(unsigned-byte 8)) "The 256 Bytes of sprite ram the PPU has")
(defvar *rom-directory* (merge-pathnames  #P"projects/nes-emulator/roms/" (user-homedir-pathname)))
(defvar program-rom nil "The program op codes and data")
(defvar character-rom nil "The programs pattern tables if any")

(defparameter horizontal-mirroring nil )
(defparameter vertical-mirroring nil )
(defparameter battery-backed-ram nil )
(defparameter  debug-output nil)


(defmacro interrupt (maskable address-location-low address-location-high )
  (if maskable    
      `(let ((address #x0000))
	 (when (not interrupt-flag)

	   (setf (ldb (byte 8 0) address)(aref cpu-memory ,address-location-low))
	   (setf (ldb (byte 8 8) address)(aref cpu-memory ,address-location-high))     

	   (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) PC))
	   (decf SP)
	   (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
	   (decf SP)
	   (setf (aref cpu-memory (+ stack-bottom SP))(status-register))
	   (decf SP)
	   (setf interrupt-flag t)
	   (when debug-output (format t "IRQ -> ~X~% " address))
	   (setf PC address)
	   (incf cpu-cycles 7)))

      `(let ((address #x0000))    
	 (setf (ldb (byte 8 0) address)(aref cpu-memory ,address-location-low))
	 (setf (ldb (byte 8 8) address)(aref cpu-memory ,address-location-high))     
	 (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) PC))
	 (decf SP)
	 (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
	 (decf SP)
	 (setf (aref cpu-memory (+ stack-bottom SP))(status-register))
	 (decf SP)
	 (setf interrupt-flag t)
	 (when debug-output (format t "NMI -> ~X~% " address))
	 (setf PC address)
	 (incf cpu-cycles 7))
      
      ))


(defmacro brk () 
  `(let ((address #x0000))
     (incf PC)
     (incf PC) ;; control always returns to the second byte past the BRK opcode.
     (setf (ldb (byte 8 0) address)(aref cpu-memory #xFFFE))
     (setf (ldb (byte 8 8) address)(aref cpu-memory #xFFFF))
     ;; store the PC on the stack High then low byte
     (when debug-output (format t "BRK  ~%"))
     (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) PC))
     (decf SP)
     (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
     (decf SP)
     (setf (aref cpu-memory (+ stack-bottom SP))(status-register))
     (decf SP)
     (setf brk-flag t)
     (setf PC address)
     (incf cpu-cycles 7)))


(defmacro compare-register (register address-mode )
  (case address-mode
    (:immediate
     `(progn (incf PC)
	     (let ((result (- ,register (aref cpu-memory PC))))
	       (set-nz-flags result)
	       (when (plusp result)
		 (setf carry-flag t)))
	     (incf PC)
	     (incf cpu-cycles 2)
	     (when debug-output (format t "CP #~X ~%"  (aref cpu-memory (- PC 1))))
	     ))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn (incf PC)
	     (let ((result (- ,register (aref cpu-memory (aref cpu-memory PC)))))
	       (set-nz-flags result)
	       (when (plusp result)
		 (setf carry-flag t)))
	     (incf PC)
	     (incf cpu-cycles 3)
	     (when debug-output (format t "CP $~X ~%"  (aref cpu-memory (- PC 1))))
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
	    (when (plusp result)
	      (setf carry-flag t)))
	  (incf PC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "CP $(~X) ~%" address))
	  )))
    (otherwise (error "Dumbass!"))
    ))

;; CMP a fucking again 
(defmacro compare  (address-mode )
  (case address-mode
    (:immediate
     `(progn (incf PC)
	     (let ((result (- AC (aref cpu-memory PC))))
	       (set-nz-flags result)
	       (when (plusp result)
		 (setf carry-flag t)))
	     (incf PC)
	     (incf cpu-cycles 2)
	     (when debug-output (format t "CMP #~X ~%"  (aref cpu-memory (- PC 1))))
	     ))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn (incf PC)
	     (let ((result (- AC (aref cpu-memory (aref cpu-memory PC)))))
	       (set-nz-flags result)
	       (when (plusp result)
		 (setf carry-flag t)))
	     (incf PC)
	     (incf cpu-cycles 3)
	     (when debug-output (format t "CMP $~X ~%"  (aref cpu-memory (- PC 1))))
	     ))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
       `(progn
	  (incf PC)
	  (let ((result (- AC ,memory-value)))
	    (set-nz-flags result)
	    (when (plusp result)
	      (setf carry-flag t)))
	  
	  (incf PC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "CMP $~X,X ~%" (aref cpu-memory (- PC 1))))	
	  )))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (let ((result (- AC (aref cpu-memory (cpu-mirror-adjust address)))))
	    (set-nz-flags result)
	    (when (plusp result)
	      (setf carry-flag t)))
	  (incf PC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "CMP $(~X) ~%" address))
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

	  (let ((result (- AC (aref cpu-memory (cpu-mirror-adjust (+ address RX))))))
	    (set-nz-flags result)
	    (when (plusp result)
	      (setf carry-flag t)))	
	  
	  
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "CMP $~X,X ~%" address))
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

	  (let ((result (- AC (aref cpu-memory (cpu-mirror-adjust (+ address RY))))))
	    (set-nz-flags result)
	    (when (plusp result)
	      (setf carry-flag t)))  

	  
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "CMP $~X,Y ~%" address))
	  )))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "CMP ($~X,X) ~%" zero-page))
	   )
	 (incf PC)
	 (let ((result (- AC (aref cpu-memory (cpu-mirror-adjust address)))))
	   (set-nz-flags result)
	   (when (plusp result)
	     (setf carry-flag t)))	 
	 (incf cpu-cycles 6)
	 )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "CMP ($~X),Y ~%" zero-page))
	   )
	 
	 (let ((result (- AC (aref cpu-memory (cpu-mirror-adjust (+ address RY))))))
	   (set-nz-flags result)
	   (when (plusp result)
	     (setf carry-flag t)))

	 
	 (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	     (incf cpu-cycles 5)
	     (incf cpu-cycles 6))
	 (incf PC)
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
	       (setf (ldb (byte 8 0) actual )(aref cpu-memory address))
	       (setf (ldb (byte 8 8) actual )(aref cpu-memory(+ 1 address)))

	       (setf  PC (cpu-mirror-adjust actual))

	       (incf cpu-cycles 5)
	       (when debug-output (format t "JMP ($~X) ~%" address ))
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
	  (when debug-output (format t "JMP $(~X) ~%" address))
	  )))
    (otherwise (error "Dumbass!"))
    ))

;;BIT - Bit Test
;;A & M, N = M7, V = M6
;;This instructions is used to test if one or more bits are set in a target memory location.
;;The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result is not kept.
;;Bits 7 and 6 of the value from memory are copied into the N and V flags.
(defmacro bit-test (address-mode)
  (case address-mode
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(let ((value (aref cpu-memory (aref cpu-memory PC))))
	  (when (eq 0 (logand value AC)) (setf zero-flag t))
	  (if (logbitp 7 value)
	      (setf negative-flag t)
	      (setf negative-flag nil))
	  (if (logbitp 6 value)
	      (setf overflow-flag t)
	      (setf overflow-flag nil)))
	
	(incf PC)
	(incf cpu-cycles 3)
	(when debug-output (format t "BIT $~X ~%" (aref cpu-memory (- PC 1))))
	))
    
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (let ((value (aref cpu-memory  (cpu-mirror-adjust address))))
	    (when (eq 0 (logand value AC)) (setf zero-flag t))
	    (if (logbitp 7 value)
		(setf negative-flag t)
		(setf negative-flag nil))
	    (if (logbitp 6 value)
		(setf overflow-flag t)
		(setf overflow-flag nil)))
	  (incf cpu-cycles 4)
	  (when debug-output (format t "BIT $~X ~%" address))       
	)))
    (otherwise (error "Dumbass!"))
    ))





(defmacro shit (immediate-body immediate-post immediate-cycle-count immediate-debug )
  `(defmacro pooper (address-mode)
     (case address-mode    
       (:immediate
	(let* ((memory-value  '(aref cpu-memory PC))
	      (body ,immediate-body)
	      (cycle-count ,immediate-cycle-count)
	      (post ',immediate-post)
	      (debug-string ,immediate-debug)
	      )      
	  `(progn
	     (incf PC)	
	     ,body	
	     (incf PC)
	     ,post
	     (incf cpu-cycles ,cycle-count)	
	     (when debug-output (format t ,debug-string (aref cpu-memory (- PC 1))))
	     )))))

  )

#|
(:immediate
     ;; 2 bytes 2 cycles 
     `(progn
	(incf PC)
	(let ((result (+ AC (aref cpu-memory PC) (if carry-flag 1 0) )))  
	  (setf AC (ldb (byte 8 0) result))
	  (when (> result #xff)
	    (setf carrt-flag t)))
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "ADC #~X ~%" (aref cpu-memory (- PC 1))))
	))|#

;;A,Z,C,N = A+M+C
;;This instruction adds the contents of a memory location to the accumulator together with the carry bit.
;;If overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
(defmacro add-with-carry (address-mode)
  (case address-mode    
    (:immediate
     ;; 2 bytes 2 cycles
     (let ((memory-value  '(aref cpu-memory PC)))
     `(progn
	(incf PC)
	
	(let ((result (+ AC ,memory-value (if carry-flag 1 0) )))  
	  (setf AC (ldb (byte 8 0) result))
	  (when (> result #xff)
	    (setf carry-flag t)))
	
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "ADC #~X ~%" (aref cpu-memory (- PC 1))))
	)))
    (:zero-page
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (aref cpu-memory PC))))
     `(progn
	(incf PC)
	(let ((result (+ AC ,memory-value (if carry-flag 1 0) )))  
	  (setf AC (ldb (byte 8 0) result))
	  (when (> result #xff)
	    (setf carry-flag t)))	
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags AC)
	(when debug-output (format t "ADC $~X ~%" (aref cpu-memory (- PC 1))))
	)))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
     `(progn
	(incf PC)
	(let ((result (+ AC ,memory-value (if carry-flag 1 0) )))  
	  (setf AC (ldb (byte 8 0) result))
	  (when (> result #xff)
	    (setf carry-flag t)))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags AC)
	(when debug-output (format t "ADC $~X,X ~%" (aref cpu-memory (- PC 1))))	
	)))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	  (let ((result (+ AC ,memory-value (if carry-flag 1 0) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag t)))
	  
	  (incf PC)
	  (set-nz-flags AC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "ADC $~X ~%" address))
	))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)    
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)

	  (let ((result (+ AC (aref cpu-memory (cpu-mirror-adjust (+ address RX))) (if carry-flag 1 0) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag t)))
	  
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "ADC $~X,X ~%" address))
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

	   (let ((result (+ AC (aref cpu-memory (cpu-mirror-adjust (+ address RY))) (if carry-flag 1 0) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag t)))
	  
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "ADC $~X,Y ~%" address))
	)))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
        (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "ADC ($~X,X) ~%" zero-page))
	   )
	 (incf PC)
	 (let ((result (+ AC (aref cpu-memory (cpu-mirror-adjust address) ) (if carry-flag 1 0) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag t)))

	 (set-nz-flags AC)
	 (incf cpu-cycles 6)
       )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "ADC ($~X),Y ~%" zero-page))
	   )

	 (let ((result (+ AC (aref cpu-memory (cpu-mirror-adjust (+ address RY))) (if carry-flag 1 0) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag t)))
	 (set-nz-flags AC)
	 (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	     (incf cpu-cycles 5)
	     (incf cpu-cycles 6))
	 (incf PC)
       )))
    
    (otherwise (error "Dumbass!"))
    ))

;;SBC
(defmacro subtract-with-carry (address-mode)
  (case address-mode    
    (:immediate
     ;; 2 bytes 2 cycles
     (let ((memory-value  '(aref cpu-memory PC)))
     `(progn
	(incf PC)
	
	(let ((result (+ AC (logxor  ,memory-value #xff) (if (not carry-flag) 1 0) )))  
	  (setf AC (ldb (byte 8 0) result))
	  (when (> result #xff)
	    (setf carry-flag nil)))
	
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "SBC #~X ~%" (aref cpu-memory (- PC 1))))
	)))
    (:zero-page
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (aref cpu-memory PC))))
     `(progn
	(incf PC)
	(let ((result (+ AC (logxor ,memory-value #xff) (if carry-flag 0 1) )))  
	  (setf AC (ldb (byte 8 0) result))
	  (when (> result #xff)
	    (setf carry-flag nil)))	
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags AC)
	(when debug-output (format t "SBC $~X ~%" (aref cpu-memory (- PC 1))))
	)))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
     `(progn
	(incf PC)
	(let ((result (+ AC (logxor ,memory-value #xff) (if carry-flag 0 1) )))  
	  (setf AC (ldb (byte 8 0) result))
	  (when (> result #xff)
	    (setf carry-flag nil)))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags AC)
	(when debug-output (format t "SBCC $~X,X ~%" (aref cpu-memory (- PC 1))))	
	)))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	  (let ((result (+ AC (logxor ,memory-value #xff) (if carry-flag 0 1) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag nil)))
	  
	  (incf PC)
	  (set-nz-flags AC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "SBC $~X ~%" address))
	))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)    
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)

	  (let ((result (+ AC (logxor (aref cpu-memory (cpu-mirror-adjust (+ address RX))) #xff) (if carry-flag 0 1) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag nil)))
	  
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "SBC $~X,X ~%" address))
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

	   (let ((result (+ AC (logxor (aref cpu-memory (cpu-mirror-adjust (+ address RY))) #xff) (if carry-flag 0 1) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag nil)))
	  
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "SBC $~X,Y ~%" address))
	)))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
        (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "SBC ($~X,X) ~%" zero-page))
	   )
	 (incf PC)
	 (let ((result (+ AC (logxor (aref cpu-memory (cpu-mirror-adjust address) ) #xff) (if carry-flag 0 1) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag nil)))

	 (set-nz-flags AC)
	 (incf cpu-cycles 6)
       )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "SBC ($~X),Y ~%" zero-page))
	   )

	 (let ((result (+ AC (logxor (aref cpu-memory (cpu-mirror-adjust (+ address RY))) #xff) (if carry-flag 0 1) )))  
	    (setf AC (ldb (byte 8 0) result))
	    (when (> result #xff)
	      (setf carry-flag nil)))
	 (set-nz-flags AC)
	 (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	     (incf cpu-cycles 5)
	     (incf cpu-cycles 6))
	 (incf PC)
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
	(if (logbitp 7 AC) (setf carry-flag t) (setf carry-flag nil))
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
	  (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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
	    (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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
	    (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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
	    (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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
	  (if (logbitp 7 AC) (setf carry-flag t) (setf carry-flag nil))
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
	      (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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

	    (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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

	      (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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

	    (if (logbitp 7 value) (setf carry-flag t) (setf carry-flag nil))
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
	  (if (logbitp 0 AC) (setf carry-flag t) (setf carry-flag nil))
	  (setf AC (ldb (byte 8 0) (/ AC 2 )))
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
	      (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	      (setf value (ldb (byte 8 0) (/ value 2 )))
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

	    (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	    (setf value (ldb (byte 8 0) (/ value 2 )))
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

	      (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	      (setf value (ldb (byte 8 0) (/ value 2 )))
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

	    (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	    (setf value (ldb (byte 8 0) (/ value 2 )))
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
	(if (logbitp 0 AC) (setf carry-flag t) (setf carry-flag nil))
	(setf AC (ldb (byte 8 0) (round (/ AC 2 ))))
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "LSR A ~%" ))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(let ((value (aref cpu-memory (aref cpu-memory PC)) ))	  
	  (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	  (setf value (ldb (byte 8 0) (round(/ value 2 ))))
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
	    (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	    (setf value (ldb (byte 8 0) (round(/ value 2 ))))
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
	    (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	    (setf value (ldb (byte 8 0) (round(/ value 2 ))))
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
	    (if (logbitp 0 value) (setf carry-flag t) (setf carry-flag nil))
	    (setf value (ldb (byte 8 0) (round(/ value 2 ))))
	    (setf ,memory-value value)
	  	  
	    (set-nz-flags value)
	    (incf cpu-cycles 7)
	  (when debug-output (format t "LSR $~X,X ~%" address))
	  )))))        
    (otherwise (error "Dumbass!"))
    ))



;;DEC - Decrement Memory
;;M,Z,N = M-1
;;Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
(defmacro dec (address-mode)
  (case address-mode        
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(let ((value   (ldb (byte 8 0) (- (aref cpu-memory (aref cpu-memory PC)) 1)) ))
	  (setf (aref cpu-memory (aref cpu-memory PC)) value )
	  (incf PC)
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "DEC $~X ~%" (aref cpu-memory (- PC 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
       `(progn
	  (incf PC)
	  (let ((value   (ldb (byte 8 0) (- ,memory-value 1)) ))
	    (setf  ,memory-value  value )
	    (incf PC)
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "DEC $~X,X ~%" (aref cpu-memory (- PC 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (let ((value   (ldb (byte 8 0) (- ,memory-value 1))))
	    (setf  ,memory-value  value )
	    (incf PC)
	    (set-nz-flags value)
	    (incf cpu-cycles 6)
	    (when debug-output (format t "DEC $~X ~%" address))
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
	  (let ((value   (ldb (byte 8 0) (- ,memory-value 1))))
	    (setf  ,memory-value  value )	    
	    (set-nz-flags value)
	    (incf cpu-cycles 7)
	  (when debug-output (format t "DEX $~X,X ~%" address))
	  )))))        
    (otherwise (error "Dumbass!"))
    ))

;;INC
(defmacro inc (address-mode)
  (case address-mode        
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(let ((value   (ldb (byte 8 0) (+ (aref cpu-memory (aref cpu-memory PC)) 1)) ))
	  (setf (aref cpu-memory (aref cpu-memory PC)) value )
	  (incf PC)
	  (incf cpu-cycles 5)
	  (set-nz-flags value)
	  (when debug-output (format t "INC $~X ~%" (aref cpu-memory (- PC 1))))
	  )))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
       `(progn
	  (incf PC)
	  (let ((value   (ldb (byte 8 0) (+ ,memory-value 1)) ))
	    (setf  ,memory-value  value )
	    (incf PC)
	    (incf cpu-cycles 6)
	    (set-nz-flags value)
	    (when debug-output (format t "INC $~X,X ~%" (aref cpu-memory (- PC 1))))	
	    ))))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (let ((value   (ldb (byte 8 0) (+ ,memory-value 1))))
	    (setf  ,memory-value  value )
	    (incf PC)
	    (set-nz-flags value)
	    (incf cpu-cycles 6)
	    (when debug-output (format t "INC $~X ~%" address))
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
	  (let ((value   (ldb (byte 8 0) (+ ,memory-value 1))))
	    (setf  ,memory-value  value )	    
	    (set-nz-flags value)
	    (incf cpu-cycles 7)
	  (when debug-output (format t "INC $~X,X ~%" address))
	  )))))        
    (otherwise (error "Dumbass!"))
    ))

;;AND - Logical AND
;;A,Z,N = A&M
;;A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
(defmacro logical-and (address-mode)
  (case address-mode    
    (:immediate
     ;; 2 bytes 2 cycles
     `(progn
	(incf PC)
	(setf AC (logand AC (aref cpu-memory PC)))       
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "AND #~X ~%" (aref cpu-memory (- PC 1))))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (aref cpu-memory PC))))
     `(progn
	(incf PC)
	(setf AC (logand AC ,memory-value))
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags AC)
	(when debug-output (format t "AND $~X ~%" (aref cpu-memory (- PC 1))))
	)))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
     `(progn
	(incf PC)
	(setf AC (logand AC ,memory-value))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags AC)
	(when debug-output (format t "AND $~X,X ~%" (aref cpu-memory (- PC 1))))	
	)))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	  (setf AC (logand AC ,memory-value))
	  
	  (incf PC)
	  (set-nz-flags AC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "AND $~X ~%" address))
	))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)    
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf AC (logand AC (aref cpu-memory (cpu-mirror-adjust (+ address RX)))))
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "AND $~X,X ~%" address))
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
	  (setf AC (logand AC (aref cpu-memory (cpu-mirror-adjust (+ address RY)))))	  
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "AND $~X,Y ~%" address))
	)))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
        (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "AND ($~X,X) ~%" zero-page))
	   )
	 (incf PC)	
	 (setf AC (logand AC (aref cpu-memory (cpu-mirror-adjust address ))))
	 (set-nz-flags AC)
	 (incf cpu-cycles 6)
       )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "AND ($~X),Y ~%" zero-page))
	   )
	 (setf AC (logand AC (aref cpu-memory (cpu-mirror-adjust (+ address RY)))))
	 (set-nz-flags AC)
	 (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	     (incf cpu-cycles 5)
	     (incf cpu-cycles 6))
	 (incf PC)
       )))
    
    (otherwise (error "Dumbass!"))
    ))

;;EOR 
(defmacro logical-xor (address-mode)
  (case address-mode    
    (:immediate
     ;; 2 bytes 2 cycles
     `(progn
	(incf PC)
	(setf AC (logxor AC (aref cpu-memory PC)))       
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "EOR #~X ~%" (aref cpu-memory (- PC 1))))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (aref cpu-memory PC))))
     `(progn
	(incf PC)
	(setf AC (logxor AC ,memory-value))
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags AC)
	(when debug-output (format t "EOR $~X ~%" (aref cpu-memory (- PC 1))))
	)))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
     `(progn
	(incf PC)
	(setf AC (logxor AC ,memory-value))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags AC)
	(when debug-output (format t "EOR $~X,X ~%" (aref cpu-memory (- PC 1))))	
	)))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	  (setf AC (logxor AC ,memory-value))
	  
	  (incf PC)
	  (set-nz-flags AC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "EOR $~X ~%" address))
	))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)    
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf AC (logxor AC (aref cpu-memory (cpu-mirror-adjust (+ address RX)))))
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "EOR $~X,X ~%" address))
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
	  (setf AC (logxor AC (aref cpu-memory (cpu-mirror-adjust (+ address RY)))))	  
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "EOR $~X,Y ~%" address))
	)))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
        (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "EOR ($~X,X) ~%" zero-page))
	   )
	 (incf PC)	
	 (setf AC (logxor AC (aref cpu-memory (cpu-mirror-adjust address) )))
	 (set-nz-flags AC)
	 (incf cpu-cycles 6)
       )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "EOR ($~X),Y ~%" zero-page))
	   )
	 (setf AC (logxor AC (aref cpu-memory (cpu-mirror-adjust(+ address RY)))))
	 (set-nz-flags AC)
	 (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	     (incf cpu-cycles 5)
	     (incf cpu-cycles 6))
	 (incf PC)
       )))
    
    (otherwise (error "Dumbass!"))
    ))

(defmacro logical-or (address-mode)
  (case address-mode    
    (:immediate
     ;; 2 bytes 2 cycles
     `(progn
	(incf PC)
	(setf AC (logior AC (aref cpu-memory PC)))       
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "ORA #~X ~%" (aref cpu-memory (- PC 1))))
	))
    (:zero-page
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (aref cpu-memory PC))))
     `(progn
	(incf PC)
	(setf AC (logior AC ,memory-value))
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags AC)
	(when debug-output (format t "ORA $~X ~%" (aref cpu-memory (- PC 1))))
	)))
    (:zero-page-x
     ;;2 bytes 3 cycles
     (let ((memory-value  '(aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC))))))       
     `(progn
	(incf PC)
	(setf AC (logior AC ,memory-value))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags AC)
	(when debug-output (format t "ORA $~X,X ~%" (aref cpu-memory (- PC 1))))	
	)))
    (:absolute
     (let ((memory-value  '(aref cpu-memory (cpu-mirror-adjust address))))       
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))

	  (setf AC (logior AC ,memory-value))
	  
	  (incf PC)
	  (set-nz-flags AC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "ORA $~X ~%" address))
	))))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 4 (+1 if page crossed)    
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf AC (logior AC (aref cpu-memory (cpu-mirror-adjust (+ address RX)))))
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "ORA $~X,X ~%" address))
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
	  (setf AC (logior AC (aref cpu-memory (cpu-mirror-adjust (+ address RY)))))	  
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "ORA $~X,Y ~%" address))
	)))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
        (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "ORA ($~X,X) ~%" zero-page))
	   )
	 (incf PC)	
	 (setf AC (logior AC (aref cpu-memory (cpu-mirror-adjust address ))))
	 (set-nz-flags AC)
	 (incf cpu-cycles 6)
       )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "ORA ($~X),Y ~%" zero-page))
	   )
	 (setf AC (logior AC (aref cpu-memory (cpu-mirror-adjust (+ address RY)))))
	 (set-nz-flags AC)
	 (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	     (incf cpu-cycles 5)
	     (incf cpu-cycles 6))
	 (incf PC)
       )))
    
    (otherwise (error "Dumbass!"))
    ))

(defmacro store-accumulator (address-mode)
  (case address-mode
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf (aref cpu-memory (aref cpu-memory PC)) AC )
	(incf PC)
	(incf cpu-cycles 3)
	(when debug-output (format t "STA $~X ~%" (aref cpu-memory (- PC 1))))
	))
    (:zero-page-x
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf (aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC)))) AC )
	(incf PC)
	(incf cpu-cycles 4)
	(when debug-output (format t "STA $~X,X ~%" (aref cpu-memory (- PC 1))))	
	))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (aref cpu-memory (cpu-mirror-adjust address)) AC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "STA $~X ~%" address))       
	)))
    (:absolute-x
     ;;Bytes 3, CPU Cycles 5
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (aref cpu-memory (cpu-mirror-adjust (+ address RX))) AC)			 
	  (incf cpu-cycles 5)
	  (when debug-output (format t "STA $~X,X ~%" address))
	  )))
    (:absolute-y
     ;;Bytes 3, CPU Cycles 6
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (aref cpu-memory (cpu-mirror-adjust (+ address RY))) AC)			 
	  (incf cpu-cycles 6)
	  (when debug-output (format t "STA $~X,Y ~%" address))
	  )))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
        (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "STA ($~X,X) ~%" zero-page))
	   )
	 (incf PC)
	 (setf (aref cpu-memory (cpu-mirror-adjust address) ) AC)			 
	 (incf cpu-cycles 6)
       )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "STA ($~X),Y ~%" zero-page))
	   )
	 (setf (aref cpu-memory (cpu-mirror-adjust (+ address RY))) AC)			 
	 (incf cpu-cycles 6)	 
	 (incf PC)
	 )))    
    (otherwise (error "Dumbass!"))
    ))


(defmacro store-register (register address-mode)
  (case address-mode
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf (aref cpu-memory (aref cpu-memory PC)) ,register )
	(incf PC)
	(incf cpu-cycles 3)
	(when debug-output (format t "ST $~X ~%" (aref cpu-memory (- PC 1))))
	))
    (:zero-page-x
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf (aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC)))) ,register )
	(incf PC)
	(incf cpu-cycles 4)
	(when debug-output (format t "ST $~X,X ~%" (aref cpu-memory (- PC 1))))	
	))
    (:zero-page-y
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf (aref cpu-memory (ldb (byte 8 0) (+ RY (aref cpu-memory PC)))) ,register )
	(incf PC)
	(incf cpu-cycles 4)
	(when debug-output (format t "ST $~X,Y ~%" (aref cpu-memory (- PC 1))))	
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
	  (when debug-output (format t "ST $~X ~%" address))       
	)))
   
    (otherwise (error "Dumbass!"))
    ))




(defmacro load-accumulator (address-mode)
  (case address-mode
    (:immediate
     ;; 2 bytes 2 cycles 
     `(progn
	(incf PC)
	(setf AC (aref cpu-memory PC))
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags AC)
	(when debug-output (format t "LDA #~X ~%" AC))
	))    
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf AC (aref cpu-memory (aref cpu-memory PC)) )
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags AC)
	(when debug-output (format t "LDA $~X ~%" (aref cpu-memory (- PC 1))))
	))
    (:zero-page-x
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf AC (aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC)))))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags AC)
	(when debug-output (format t "LDA $~X,X ~%" (aref cpu-memory (- PC 1))))	
	))
    (:absolute
     `(progn
	(let ((address #x0000))
	  (incf PC)
	  (setf (ldb (byte 8 0) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf (ldb (byte 8 8) address)(aref cpu-memory PC))
	  (incf PC)
	  (setf AC (aref cpu-memory (cpu-mirror-adjust address)))
	  (set-nz-flags AC)
	  (incf cpu-cycles 4)
	  (when debug-output (format t "LDA $~X ~%" address))
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
	  (setf AC (aref cpu-memory (cpu-mirror-adjust (+ address RX))))			 
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RX))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "LDA $~X,X ~%" address))
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
	  (setf AC (aref cpu-memory (cpu-mirror-adjust (+ address RY))))			 
	  (set-nz-flags AC)
	  (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	      (incf cpu-cycles 4)
	      (incf cpu-cycles 5))
	  (when debug-output (format t "LDA $~X,Y ~%" address))
	)))    
    ;; LDA ($00,X)
    (:index-indirect
     '(progn
        (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (+ RX (aref cpu-memory PC ))))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "LDA ($~X,X) ~%" zero-page))
	   )
	 (incf PC)
	 (setf AC (aref cpu-memory (cpu-mirror-adjust address) ))			 
	 (set-nz-flags AC)
	 (incf cpu-cycles 6)
       )))
    ;; LDA ($00),Y
    (:indirect-index
     '(progn
       (let ((address #x0000))			 
	 (incf PC)
	 (let ((zero-page (aref cpu-memory PC )))			   
	   (setf (ldb (byte 8 0) address) (aref cpu-memory zero-page))
	   (setf (ldb (byte 8 8) address) (aref cpu-memory (+ 1 zero-page)))
	   (when debug-output (format t "LDA ($~X),Y ~%" zero-page))
	   )
	 (setf AC (aref cpu-memory (cpu-mirror-adjust (+ address RY))))			 
	 (set-nz-flags AC)
	 (if (equal (ldb (byte 8 8) PC) (ldb (byte 8 8) (+ address RY))) ; check if we cross page boundires
	     (incf cpu-cycles 5)
	     (incf cpu-cycles 6))
	 (incf PC)
       )))
    
    (otherwise (error "Dumbass!"))
    ))


(defmacro load-register (register address-mode)
  (case address-mode
    (:immediate
     ;; 2 bytes 2 cycles 
     `(progn
	(incf PC)
	(setf ,register (aref cpu-memory PC))
	(incf PC)
	(incf cpu-cycles 2)
	(set-nz-flags ,register)
	(when debug-output (format t "LD #~X ~%" ,register))
	))    
    (:zero-page
     ;;2 bytes 3 cycles
     `(progn
	(incf PC)
	(setf ,register (aref cpu-memory (aref cpu-memory PC)) )
	(incf PC)
	(incf cpu-cycles 3)
	(set-nz-flags ,register)
	(when debug-output (format t "LD $~X ~%" (aref cpu-memory (- PC 1))))
	))
    (:zero-page-y
     ;;2 bytes 4 cycles
     `(progn
	(incf PC)
	(setf ,register (aref cpu-memory (ldb (byte 8 0) (+ RY (aref cpu-memory PC)))))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags ,register)
	(when debug-output (format t "LD $~X,Y ~%" (aref cpu-memory (- PC 1))))	
	))
    (:zero-page-x
     ;;2 bytes 4 cycles
     `(progn
	(incf PC)
	(setf ,register (aref cpu-memory (ldb (byte 8 0) (+ RX (aref cpu-memory PC)))))
	(incf PC)
	(incf cpu-cycles 4)
	(set-nz-flags ,register)
	(when debug-output (format t "LD $~X,Y ~%" (aref cpu-memory (- PC 1))))	
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
	  (when debug-output (format t "LD $~X ~%" address))
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
	  (when debug-output (format t "LD $~X,Y ~%" address))
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
	  (when debug-output (format t "LD $~X,Y ~%" address))
	)))    
    
    (otherwise (error "Dumbass!"))
    ))

	 


(defvar *dump-character-tables* nil "Dump the patetern tables to the display because its neat")

(defun twos-complement (x)
  (if (minusp x)
      (1+ (logxor #xFF (abs x)))
      x))

(defmacro set-nz-flags (test-byte)
  `(progn
     (if (logbitp  7 ,test-byte) (setf negative-flag t)(setf negative-flag nil))
     (if (eq ,test-byte 0) (setf zero-flag t) (setf zero-flag nil))))

(defmacro branch-if-flag (flag)
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
	     (setf PC (+ PC displace))
	     (when debug-output (format t "B ~X ~%" displace))
	     ))
	 ;;else continue on 
	 (progn
	   (incf PC)		       
	   (incf cpu-cycles 2)))))


(defun rom-path (rom-name) 
  (merge-pathnames (merge-pathnames  #P"projects/nes-emulator/roms/" (user-homedir-pathname))
		   (make-pathname :directory '(:relative) :name  rom-name :type "nes") ))


(defmethod glop:on-key (window pressed keycode keysym text)
  (format t "~A" keysym )
  (when (and (not pressed) (eq keysym :escape))
    (glop:push-close-event window))
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
  (setf SP #xFF)  ; reset the stack pointer
  
  (let ((pattern-tables nil)
	(on-reset t)
	(cpu-cycles 0))
    (dotimes (x (/ (length character-rom) #x1000))
      (push (dump-pattern-tables x) pattern-tables))    
    
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

	 (setf cpu-cycles 0)
	   
	 ;;Reset interrupts are triggered when the system first starts and when the user presses the
	 ;;reset button. When a reset occurs the system jumps to the address located at $FFFC and
	 ;;$FFFD.
	 ;; Generate a reset interrupt
	   (when on-reset
	     (setf cpu-cycles 0)
	     (format t "REST = ~A~%" on-reset)
	     (setf on-reset nil)
	     (interrupt t  #xFFFC  #xFFFD)) 

	 ;; generate a VBLINK NMI
	 ;; vblink
	 ;; When entering the V-Blank period, the PPU indicates this by setting bit 7 of I/O
	 ;; register $2002. This bit is reset when the CPU next reads from $2002.
	   (if (eq (ldb (byte 1 7) (aref cpu-memory ppu-status-register)) 1)
	       (interrupt nil  #xFFFA  #xFFFB)
	       (setf (ldb (byte 1 7) (aref cpu-memory ppu-status-register)) 1))

	   
	 ;; execute the program for:
	 ;; 1. 1/60 second of cpu cycles 29830 cycles
	 ;; 2. 1/60 second real time has elapsed
	 ;; 3. cpu is wating on a vblink
	   (let* ((start-time (get-internal-real-time))
		 (end-time (+ start-time (/ internal-time-units-per-second 60)))		 
		 (refresh nil))

			  
	     (do ((loop-time start-time (get-internal-real-time)))

		 ((or (>= loop-time end-time)
		      (>= cpu-cycles 29830)
		      refresh))

	       (when debug-output (format t "~% CYCLE= ~D PC = ~X RX = ~X RY = ~X AC = ~X SP = ~X "cpu-cycles PC RX RY AC SP))
	       
	       (case (aref cpu-memory PC)
		 ;;SEI Set interrupt disable status 1 byte 2 cycles
		 (#x78  (setf interrupt-flag t)
			(incf cpu-cycles 2)
			(incf PC)
			(when debug-output (format t "SEI~%"))
			)

		 ;CLD Clear decimal mode 1 byte 2 cycles
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
		 (#xA2 (load-register RX :immediate))
		 (#xA6 (load-register RX :zero-page))
		 (#xB6 (load-register RX :zero-page-y))
		 (#xAE (load-register RX :absolute))
		 (#xBE (load-register RX :absolute-y))

		 ;; LDY Loads a byte of memory into the Y register setting the zero and negative flags as appropriate.
		 (#xA0 (load-register RY :immediate))
		 (#xA4 (load-register RY :zero-page))
		 (#xB4 (load-register RY :zero-page-x))
		 (#xAC (load-register RY :absolute))
		 (#xBC (load-register RY :absolute-x))

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
		 ;; BMI - Branch if Minus
		 (#x30 (branch-if-flag  negative-flag))	
		 ;;BPL - Branch if Positive
		 (#x10 (branch-if-flag (not negative-flag)))
		 ;;BCS - Branch if Carry Set, Relative, bytes 2  CPU cycles 2 (+1 if branch succeeds +2 if to a new page)
		 (#xB0 (branch-if-flag carry-flag))
		 ;;BCC - Branch if Carry Clear, Relative, bytes 2  CPU cycles 2 (+1 if branch succeeds +2 if to a new page)
		 (#x90 (branch-if-flag (not carry-flag)))
		 ;;BNE - Branch if Not Equal
		 ;;If the zero flag is clear then add the relative displacement to the program counter to cause a branch.
		 (#xD0 (branch-if-flag (not zero-flag)))
		 ;;BEQ - Branch if equal 
		 (#xF0 (branch-if-flag  zero-flag))
		 ;;BVS - Branch if Overflow Set 
		 (#x70 (branch-if-flag  overflow-flag))
		 ;;BVC - Branch if Overflow clear 
		 (#x50 (branch-if-flag  (not overflow-flag)))
 
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
		 (#xE0 (compare-register RX :immediate ))
		 (#xE4 (compare-register RX :zero-page ))
		 (#xEC (compare-register RX :absolute ))

		 ;;CPY - Compare Y Register Mode Immediate Bytes 2  Cycles 2
		 (#xC0 (compare-register RY :immediate ))
		 (#xC4 (compare-register RY :zero-page ))
		 (#xCC (compare-register RY :absolute ))
		 
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
			 (decf SP)
			 (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 0) PC))
			 (decf SP)
			 (setf PC address)
			 (incf cpu-cycles 6)))

		 ;;BRK - Force Interrupt
		 ;;The BRK instruction forces the generation of an interrupt request.
		 ;;The program counter and processor status are pushed on the stack then the IRQ interrupt vector at $FFFE/F is loaded
		 ;; into the PC and the break flag in the status set to one.
		 (#x00 (brk))
		 
		 ;;PHA - Push Accumulator
		 (#x48 (incf PC)
		       (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) AC))
		       (decf SP)
		       (incf cpu-cycles 3)
		       (when debug-output (format t "PHA ~%"))
		       )
		 ;;PLA - Pull Accumulator
		 (#x68 (incf PC)
		       (incf SP)
		       (setf AC (aref cpu-memory (+ stack-bottom SP)))
		       (incf cpu-cycles 4)
		       (set-nz-flags AC)
		       (when debug-output (format t "PLA ~%"))
		       )		 
		 ;;PHP - Push Processor Status
		 (#x08 (incf PC)
		       (setf (aref cpu-memory (+ stack-bottom SP)) (ldb (byte 8 8) (status-register)))
		       (decf SP)
		       (incf cpu-cycles 3)
		       (when debug-output (format t "PHP ~%"))
		       )
		 ;;PLP - Pull Processor Status
		 (#x28 (let ((register #x00))
			 (incf PC)
			 (setf register (aref cpu-memory (+ stack-bottom SP)))
			 (decf SP)
			 (set-status-register register)
			 (incf cpu-cycles 4)
			 (when debug-output (format t "PLP ~%"))
		       ))
		 
		 
		 ;;RTS - Return from Subroutine Mode Implied Bytes 1  Cycles 6 
		 ;;The RTS instruction is used at the end of a subroutine to return to the calling routine.
		 ;;It pulls the program counter (minus one) from the stack.
		 (#x60 (let ((address #x0000))
			 (incf PC)
			 (incf SP)
			 (setf (ldb (byte 8 0) address)(aref cpu-memory (+ stack-bottom SP)))
			 (incf SP)			 
			 (setf (ldb (byte 8 8) address)(aref cpu-memory (+ stack-bottom SP)))
			 (setf PC (+ 1 address))
			 (when debug-output (format t "RTS -> ~X ~%" address ))
			 (incf cpu-cycles 6)))

		 ;;RTI - Return from Interrupt
		 (#x40 (let ((address #x0000)
			     (register #x00))
			 (incf PC)
			 (incf SP)
			 (setf register (aref cpu-memory (+ stack-bottom SP)))			 
			 (incf SP)
			 (setf (ldb (byte 8 0) address)(aref cpu-memory (+ stack-bottom SP)))
			 (incf SP)			 
			 (setf (ldb (byte 8 8) address)(aref cpu-memory (+ stack-bottom SP)))
			 (setf PC  address)
			 (set-status-register register) 
			 (when debug-output (format t "RTI -> ~X ~%" address ))
			 (incf cpu-cycles 6)))

		 
		 ;;STX - Store X Register
		 (#x86 (store-register RX :zero-page))
		 (#x96 (store-register RX :zero-page-y))
		 (#x8E (store-register RX :absolute))

		 ;;STY - Store Y Register
		 (#x84 (store-register RY :zero-page))
		 (#x94 (store-register RY :zero-page-y))
		 (#x8C (store-register RY :absolute))

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
		 (#xE9 (subtract-with-carry :immediate))
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
		 
		 (otherwise (error (format nil "Unknown Instruction ~X~%" (aref cpu-memory PC)))))

	       )

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
  (progn (load-rom "nestest") (run))
  )
