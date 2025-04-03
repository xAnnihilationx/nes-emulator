(in-package :nes-emulator)

(defparameter *nes-rgb-pal* (make-array  #xC0 :element-type '(unsigned-byte 8) :initial-contents
'(124 124 124 0 0 252 0 0 188 68 40 188 148 0 132 168 0 32 168 16 0 136 20 0 80 48 0 0 120 0 0 104 0 0 88 0 0 64 88 0 0 0 
  0 0 0 0 0 0 188 188 188 0 120 248 0 88 248 104 68 252 216 0 204 228 0 88 248 56 0 228 92 16 172 124 0 0 184 0 0 168 0 
  0 168 68 0 136 136 0 0 0 0 0 0 0 0 0 248 248 248 60 188 252 104 136 252 152 120 248 248 120 248 248 88 152 248 120 88 
  252 160 68 248 184 0 184 248 24 88 216 84 88 248 152 0 232 216 120 120 120 0 0 0 0 0 0 252 252 252 164 228 252 184 184 248 
  216 184 248 248 184 248 248 164 192 240 208 176 252 224 168 248 216 120 216 248 120 184 248 184 184 248 216 0 252 252 
  248 216 248 0 0 0 0 0 0 )))

(defparameter frame (make-array #x2D000  :element-type '(unsigned-byte 8) :initial-element #x00 ))

;;probably won't need theses 
(defvar bkg-pt  (make-array 256 :element-type  '(unsigned-byte 2)  :initial-element #x00)) ; pallet number
(defvar bkg-clr  (make-array 256 :element-type  '(unsigned-byte 2)  :initial-element #x00)) ; color number
(defvar decoded-scanline  (make-array 256 :element-type  '(unsigned-byte 8) :initial-element #x00))
(defvar  bg-nt-indexes (make-array 32  :element-type '(unsigned-byte 8) :initial-element #x00))


(defun get-pal-red (index)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum index))
  (declare (type (simple-array (unsigned-byte 8) (#xC0))  *nes-rgb-pal*)) 
   (aref  *nes-rgb-pal*  (the fixnum (* 3 index)) ))
(defun get-pal-green (index)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum index))  
  (declare (type (simple-array (unsigned-byte 8) (#xC0))  *nes-rgb-pal*)) 
  (declare (optimize (speed 3) (safety 0)))
  (aref  *nes-rgb-pal*  (the fixnum (+ (the fixnum (* 3 index)) 1)) ))
(defun get-pal-blue (index)
  (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum index))
  (declare (type (simple-array (unsigned-byte 8) (#xC0))  *nes-rgb-pal*)) 
  (declare (optimize (speed 3) (safety 0)))
  (aref  *nes-rgb-pal*  (the fixnum (+ (the fixnum (* 3 index)) 2 )) ))

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


(defun decode-tile-sliver (y tile-bank tile-number &key (flip-v nil)  (flip-h nil) )
  (let* ((pt (make-array 8 :element-type  '(unsigned-byte 2)))
	 (i (+ (of tile-bank) (of (* tile-number 16)) 0 (of (if flip-v (- 7 y) y)) ))
	 (p0 (aref (o5 ppu-memory) i))
	 (p1 (aref (o5 ppu-memory) (+ i 8))))
    (dotimes (x 8) ; 
      (setf (aref pt x) (ldb (byte 1 (if flip-h  x (- 7 x)) ) p0))
      (setf (ldb (byte 1 1) (aref pt x)) (ldb (byte 1 (if flip-h x  (- 7 x))) p1)))
    pt
    ))




	 
(defun decode-scanline (scan-line)
; (declare (optimize (speed 3) (safety 0)))
  (declare (fixnum scan-line))
  "fetches a scalines pallete indexes and returns the 256 bytes that make up the scan line"

  ;; just  draws the background without scrolling 
  (let ((bg-nt (ppuctrlp :nt-select t))
	(bg-tile-index  (ppuctrlp :bg-tile-select t)))
    
	;; find the  name table pattern indexes for this scan line

    ;; 32 tiles that make up the scanline background
    (when (ppumaskp :enable-bg t)
      (let ((idx (* (floor   (/ scan-line 8)) 32)))
	(declare (fixnum idx))
	(dotimes (x 32)
	  (declare (fixnum x))
	  (setf (aref (o1 bg-nt-indexes)  x) (aref (o5 ppu-memory) (+ bg-nt idx x)))))

	;;fetch and decode the pattern table colors for the scan line  
	(dotimes (ti 32) ; for each tile get the bit planes that make up the sliver 
	  (let ((p0 (aref (o5 ppu-memory) (+ bg-tile-index (* (aref (o1 bg-nt-indexes) ti) 16) 0 (mod scan-line 8))))
		(p1 (aref (o5 ppu-memory) (+ bg-tile-index (* (aref (o1 bg-nt-indexes) ti) 16) 8 (mod scan-line 8)))))
	    (dotimes (x 8) ; 
	      (setf (ldb (byte 1 0) (aref (o2 bkg-pt) (+ (* ti 8 ) x))) (ldb (byte 1 (- 7 x)) p0))
	      (setf (ldb (byte 1 1) (aref (o2 bkg-pt) (+ (* ti 8 ) x))) (ldb (byte 1 (- 7 x)) p1)))))

	;;fetch the attribute data for this scan line and get the pallet number
	(dotimes (a 8) ; for each of the bytes that make up the color number 
	  (let ((cl (aref (o5 ppu-memory) (+ (+ bg-nt #x3C0) (* 8 (floor  (/ scan-line 32))) a)))
		(left-cl 0)
		(right-cl 0))	    
	    (if (< (mod scan-line 32) 16)		
		(progn ; upper tiles
		  (setf  left-cl (ldb (byte 2 0) cl))
		  (setf right-cl (ldb (byte 2 2) cl)))		  		
		(progn; lower tiles 
		  (setf left-cl (ldb (byte 2 4) cl))
		  (setf right-cl (ldb (byte 2 6) cl))))
	    
	    (dotimes (x 32)
	      (if (< x 16)
		  (setf (aref (o2 bkg-clr) (+ (* 32 a) x)) left-cl); left color
		  (setf (aref (o2 bkg-clr) (+ (* 32 a) x)) right-cl))); right color		  
	    ))

	
	(dotimes (x 256)
	  (case (aref (o2 bkg-clr) x)
	    (0 (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F00 (aref (o2 bkg-pt) x) )) ))
	    (1 (if (equal (aref (o2 bkg-pt) x) 0)
		   (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) #x3F00))
		   (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F04 (aref (o2 bkg-pt) x) )) )))
	    (2 (if (equal (aref (o2 bkg-pt) x) 0)
		   (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) #x3F00))
		   (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F08 (aref (o2 bkg-pt) x) )) )))
	    (3 (if (equal (aref (o2 bkg-pt) x) 0)
		   (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) #x3F00))
		   (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F0C (aref (o2 bkg-pt) x) )) )))	    
	    )))

	;;Just lazly draw the sprites just on top over errvery thing else\
	(when (ppumaskp :enable-spr t)
	(let ((sprite-height (if (ppuctrlp :sprite-height t) 16 8)))	  
	  (dotimes (s 64) ; 64 sprites
	    (let* ((sprite-top  (1+ (aref (o3 spr-memory) (* 4 s))))
		   (sprite-index (aref (o3 spr-memory) (1+ (* 4 s))))
		   (atr (aref (o3 spr-memory) (+ 2 (* 4 s) )))
		   (sprite-left (aref (o3 spr-memory) (+ 3 (* 4 s) )))
		   (sprite-right (+ sprite-left 8) )
		   (sprite-bottom (+ sprite-top sprite-height))
		   (flip-v  (logbitp 7 atr))
		   (flip-h (logbitp 6 atr))
		   (pri (logbitp 5 atr))
		   (pal (ldb (byte 2 0) atr))
		   )
	      (declare (type fixnum sprite-top sprite-index atr sprite-left sprite-right sprite-bottom))
	      ;; if the sprite is visable then draw it 
	      (when (and (>= scan-line  sprite-top) (< scan-line sprite-bottom ) (< sprite-right #xf9 ))
		(let ((tile-bank nil)
		      (tile-number nil)
		      (scan-pos (- scan-line sprite-top))
		      )

		  ;;find the tile bank and tile number  
		  (if (ppuctrlp :sprite-height t)
		      (progn
			(setf tile-bank (if (logbitp 0 sprite-index) #x1000 #x0000))
			(setf tile-number (ldb (byte 7 1) sprite-index))
			 ;; are we on the top or bottom half 
			(if (<= (- scan-line sprite-top) 8)					
			    (progn;bottom
			      (when (not flip-v) (incf tile-number))
			      (setf scan-pos (- scan-line (of (+ sprite-top 8)))))
			    (when flip-v (incf tile-number))) ; on top 			    
			)
		      (progn
			(setf tile-bank (ppuctrlp :sprite-tile-select t))
			(setf tile-number sprite-index)))

		  
		  ;; decode the tile
		  (let ((spt (decode-tile-sliver scan-pos  tile-bank tile-number :flip-h flip-h :flip-v flip-v )) 
			(x sprite-left))
		    (declare (fixnum x)) 
		    ;; when a sprite 0 not back ground pixel overlaps a non backgound pixel of the background the sprite 0 hit
		    (dotimes (i 8)
		      (when (and (zerop s ) (not (zerop (aref spt i)))  (not (zerop (aref (o2 bkg-pt) x))) (not sprite-0-hit))
			(setf sprite-0-hit t)
			(setf sprite-0-hit-x x))
		
		      (case pal
			(0 (when (not (equal (aref spt i) 0))			     
			     (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F10 (aref spt i) )) )))
			(1 (when (not (equal (aref spt i) 0))
			     (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F14 (aref spt i) )) )))
			(2 (when (not (equal (aref spt i) 0))
			     (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F18 (aref spt i) )) )))
			(3 (when (not (equal (aref spt i) 0))
			     (setf (aref (o3 decoded-scanline) x) (aref (o5 ppu-memory) (+ #x3F1C (aref spt i) )) )))	    
			)
		      (incf x)
		      ))
		))))))	  
		

	  (o3 decoded-scanline)
	))

(defparameter beam-x 1)
(defparameter beam-y 241)
(defparameter ppu-cycle-count 0)

;; PPU internal registers for scrolling and other fuckery 
(defparameter %v  #x0000)
(defparameter %t  #x0000)
(defparameter %x  #x0000)
(defparameter %w  nil)

(defmacro toggle(x)
  `(setf ,x (not ,x)))

(defmacro ldbb (size position integer)
  `(ldb (byte ,size ,position) ,integer))

(defun process-ppu-registers()
  "We intercept ppu register read and writes here"

  ;; $2002 read 
  (when  rest-ppu-status
    (setf %w nil)
    (setf (ppustatus-bit :vblank t) 0)
    (setf  NMI-occured nil)
    (setf rest-ppu-status nil))

  ;; A write to a readable register 
  (when restore-ppu-status
    (setf (ppu-status)  previous-ppu-byte)
    (setf restore-ppu-status nil))
  
  ;;ppu scroll $2005
  (when ppu-scroll-write
   
    (setf ppu-scroll-write nil)
    (cond
      (%w
       (setf ppu-scroll-h (ppu-scroll))
       (setf (ldbb 3 12 %t) (ldbb 3 0 (ppu-scroll)))
       (setf (ldbb 2 8 %t) (ldbb 2 6 (ppu-scroll)))
       (setf (ldbb 3 5 %t) (ldbb 3 3 (ppu-scroll)))
       (setf ppu-scroll-write-count 0)
       )

      ((not %w)
       (setf ppu-scroll-v (ppu-scroll))
       (setf (ldbb 5 0 %t) (ldbb 5 3 (ppu-scroll)))
       (setf %x (ldbb 3 0 (ppu-scroll)))
       )
      )         
    (toggle %w)
    
    
   #| (if (eq ppu-scroll-write-count 1)		     
	(setf ppu-scroll-h (ppu-scroll)); horizontal offset		     
	(progn
	  (setf ppu-scroll-v (ppu-scroll)); vertical offset
	  (setf ppu-scroll-write-count 0)))|#

    )

  ;;PPU Address 2006
  (when ppu-adder-write

    (setf ppu-adder-write nil)

    (cond
      (%w
       (setf (ldbb 8 0 %t) (ppu-address))
       (setf %v %t)
       (setf (ldb (byte 8 0) ppu-adder-set) (ppu-address))
       (when (> ppu-adder-set  #x3FFF)
	 (setf  ppu-adder-set (mod ppu-adder-set  #x3FFF)))
       (setf ppu-adder ppu-adder-set)
       (setf ppu-adder-write-count 0)
       )
      
      ((not %w)       
       (setf (ldbb 6 8 %t) (ldbb 6 0 (ppu-address)))
       (setf (ldbb 1 14 %t) 0)

       (setf (ldb (byte 8 8) ppu-adder-set) (ppu-address))
       
      
       )
      )
    (toggle %w)    
  #|  (if (eq  ppu-adder-write-count 1)
	(setf (ldb (byte 8 8) ppu-adder-set) (ppu-address))
	(progn (setf (ldb (byte 8 0) ppu-adder-set) (ppu-address))
	       (when (> ppu-adder-set  #x3FFF)
		 (setf  ppu-adder-set (mod ppu-adder-set  #x3FFF)))
	       (setf ppu-adder ppu-adder-set)
	       (setf ppu-adder-write-count 0))
	)|#
    )
  
  (when ppu-control-write
    (setf ppu-control-write nil)
    (setf (ldbb 2 10 %t) (ldbb 2 0 (ppuctrl)))
    )
  
  ;; PPU data
  (when ppu-data-write
    (setf ppu-data-write nil)
    (setf (aref (o5 ppu-memory) (ppu-mirror-adjust ppu-adder)) (ppu-data)) ;;
    (if (ppuctrlp :inc-mode t)
	(incw14 ppu-adder 32)
	(incw14 ppu-adder)))
  
  (when ppu-data-read
    (setf ppu-data-read nil))


  )

(defun initalize-ppu ()
  ;; we start in vblank
  (setf %w nil)
  (setf %v 0)
  (setf %t 0)
  (setf %x 0)
  (setf beam-x 1)
  (setf beam-y 241)
  (setf ppu-cycle-count 0) ;(+ (* 341 241) 1)

  (dotimes (x  #x2D000 )
    (setf (aref frame x) (get-pal-red 0 ))
    (setf (aref frame (+ x 1)) (get-pal-green 0 ))
    (setf (aref frame (+ x 2) ) (get-pal-blue 0 ))
    (setf x (+ x 2)) 
    )
  )


;; Since we start at cycle 0 point 1 line 241 
(defun cycles-to-x (cycles)
  "PPU cycles to beam x position"
  (if (< cycles  7160)
      (mod (+ 82182 cycles) 341)
      (mod (- cycles 7160) 341))
  )

(defun cycles-to-y (cycles)
  "PPU cycles to beam y position"
  (if (< cycles 7160)
      (floor (/ (+ cycles 82182)  341))
      (floor (/ (- cycles 7160)  341)))
  )

(defun inc-h()
#|
if ((v & 0x001F) == 31) // if coarse X == 31
  v &= ~0x001F          // coarse X = 0
  v ^= 0x0400           // switch horizontal nametable
else
  v += 1                // increment coarse X
  |#

  (if (= (logand %v #x001F ) 31)
      (progn
	(setf (ldbb 5 0 %v) 0) 
	(setf %v (logxor %v #x0400)));toggle bit 10
      (incf %v))
  
  )
(defun inc-v()
#|
if ((v & 0x7000) != 0x7000)        // if fine Y < 7
  v += 0x1000                      // increment fine Y
else
  v &= ~0x7000                     // fine Y = 0
  int y = (v & 0x03E0) >> 5        // let y = coarse Y
  if (y == 29)
    y = 0                          // coarse Y = 0
    v ^= 0x0800                    // switch vertical nametable
  else if (y == 31)
    y = 0                          // coarse Y = 0, nametable not switched
  else
    y += 1                         // increment coarse Y
  v = (v & ~0x03E0) | (y << 5)     // put coarse Y back into v
|#
  (if (not (= (logand %v  #x7000) #x7000))
      (incf %v #x1000)
      (progn
	(setf %v  (logand %v #x0FFF ))
	(let ((y (ldbb 5 5 %v)))
	  (case y
	    (29
	     (setf y 0)
	     (setf %v (logxor %v #x0800)))
	    (31 (setf y 0))
	    (otherwise (incf y))) 
	    (setf (ldbb 5 5  %v) y)  
	  )))
  )


(defmacro some-macro-magic  (x y do-shit )  
       (let ((code nil))
       (dotimes (z 43) 
	 (when (plusp z) 
	   (push `(when  (and (< ,x ,(* z 8))  (>= ,y  ,(* z 8))) ,do-shit) code)))
       (push 'progn code)
       code))

(defun oh-what-in-the-fuck (x y)
  (some-macro-magic  x y (inc-h)))


(defvar bg-tile  (make-array 8 :element-type  '(unsigned-byte 2)  :initial-element #x00))
(defvar bg-tile  (make-array 8 :element-type  '(unsigned-byte 2)  :initial-element #x00))

(let ((cycles-per-frame  (* 262 341)))
  (defun advance-ppu (cpu-cycles odd-frame)
    "Advances the ppu and renders the pixels to the frame"
    (let ((x0 0)
	  (x1 0)
	  (y0 0)
	  (y1 0))  
      
      (setf x0 (cycles-to-x ppu-cycle-count)
	    y0 (cycles-to-y ppu-cycle-count))
      
      (incf ppu-cycle-count (* cpu-cycles 3))
      
      ;; wrap counter around
      (cond
	((and odd-frame  (>= ppu-cycle-count (1- cycles-per-frame)) (or (ppumaskp :enable-bg t) (ppumaskp :enable-spr t)) )
	 (setf ppu-cycle-count (mod ppu-cycle-count (1- cycles-per-frame))))	
	;; wrap counter around 	
	((>= ppu-cycle-count cycles-per-frame)
	 (setf ppu-cycle-count (mod ppu-cycle-count cycles-per-frame))))
	

      (setf x1 (cycles-to-x ppu-cycle-count)
	    y1 (cycles-to-y ppu-cycle-count))


      (when (and (or (ppumaskp :enable-bg t) (ppumaskp :enable-spr t))  (or (< y0 240) (and (>= x0 1) (= y0 261)))   )
	(setf %x (mod x1 8))
					;	(incf %x (if (> x1 x0 ) (- x1 x0) (- x0 x1) ))
	
;	(when (>  %x  7 ) (setf %x (mod %x 8)))
	;; PPU increments the vertical position in v
	(when (and (< x0 256) (>= x1 256))(inc-v))
           ;;EDCBA9876543210       EDCBA9876543210
	;;v: ....A.....43210 = t:  ....A.....43210
	(when (and (< x0 257) (>= x1 257))
	  (setf (ldbb 5 0 %v) (ldbb 5 0 %t)) 
	  (setf (ldbb 1 10 %v) (ldbb 1 10 %t)))

	;;   EDCBA9876543210       EDCBA9876543210
	;;v: IHGF.EDCBA.....  = t: IHGF.EDCBA.....
	(when (and (= y1 261) (< x0 304) (>= x1 304))
	  (setf (ldbb 4 11 %v) (ldbb 4 11 %t)) 
	  (setf (ldbb 5 5 %v) (ldbb 5 5 %t)))

	(when (or (>= x1  328) (<= x1 256))

	  (if (> x0 x1)
	      (progn 
	      (oh-what-in-the-fuck x0 340)
	      (oh-what-in-the-fuck 0 x1))
	      (oh-what-in-the-fuck x0 x1))

	  ;; Dray the scan line and load t

	  )
	
;	(format t "Scanline: (~3D,~3D ) X=(~3D,~3D) CX=~2D CY=~2D FX=~D FY=~D NT=~4X ~%" y0 y1 x0 x1 (ldbb 5 0 %v) (ldbb 5 5 %v)
;		%x (ldbb 3 12 %v) (+ #x2000 (* 1024 (ldbb 2 10 %v)))   )
	)
      
      )))
