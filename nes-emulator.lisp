;(declaim (optimize (speed 3) (debug 0) (safety 0)))

(eval-when (:compile-toplevel :load-toplevel :execute )
  (cl-user::gc :full t))

(in-package :COMMON-LISP-USER)
(in-package :nes-emulator)
	    

(defmacro process-registers()
`(progn

   (process-ppu-registers)
  
  ;; Conrtoler strobe
  (when controler-write	 		
    (setf controler-write nil)
    (setf controler-read-count 0))       
  
  ;; OMA read/write
  (when oam-data-read  (setf oam-data-read nil))

  ;; Store the data that was writen to the spr-memory
  (when oam-data-write
    (if (equal (mod  (oam-address)  4) 2)
	(setf (aref (o3 spr-memory) (oam-address)) (logand #xe3 (oam-data)))
	(setf (aref (o3 spr-memory) (oam-address)) (oam-data)))
    (incw (oam-address))
    (setf oam-data-write nil))

  
  ;; OMA dma write
  (when oam-dma-write 
    (setf oam-dma-write nil)
    (dotimes (x 256)
      (let ((address #x0000))
	(setf (ldb (byte 8 8) address) (oam-dma) )
	(setf (ldb (byte 8 0) address)  x )
	(if (equal (mod (oam-address) 4 ) 2) 
	    (setf (aref (o3 spr-memory) (oam-address)) (logand #xe3 (aref (o4 cpu-memory) address )))
	    (setf (aref (o3 spr-memory) (oam-address)) (aref (o4 cpu-memory) address )))			 
	(incw (oam-address)))))

  ))

(defparameter A-press  nil)
(defparameter B-press  nil)
(defparameter Select-press  nil)
(defparameter Start-press  nil)
(defparameter Up-press  nil)
(defparameter Down-press  nil)
(defparameter Left-press  nil)
(defparameter Right-press  nil)
(defparameter controler-read-count 0) 
(defparameter controler-write  nil) 

(defvar %saved-pc nil)
(defvar %saved-stat nil)
(defvar %saved-rx nil)
(defvar %saved-ry nil)
(defvar %saved-ac nil)
(defvar %saved-sp nil)

(defun save-cpu-state ()
  (setf %saved-pc PC)
  (setf %saved-stat  (status-register))
  (setf %saved-rx RX)
  (setf %saved-ry RY)
  (setf %saved-ac AC)
  (setf %saved-sp SP))

(defun restore-cpu-state ()
  (setf PC %saved-pc)
  (set-status-register %saved-stat)
  (setf RX %saved-rx)
  (setf RY %saved-ry)
  (setf AC %saved-ac)
  (setf SP %saved-sp))


(defun rom-path (rom-name) 
  (merge-pathnames (merge-pathnames  #P"projects/nes-emulator/roms/" (user-homedir-pathname))
		   (make-pathname :directory '(:relative) :name  rom-name :type "nes") ))


(defmethod glop:on-key (window pressed keycode keysym text)
 ; (format t "~A~%" keycode )
  (when (and (not pressed) (eq keysym :escape))
    (glop:push-close-event window))
  (when (and (not pressed) (eq keysym :f9))
    (setf *reset-pressed* (not *reset-pressed*)))

  (when (and (not pressed) (eq keysym :f2))
    (setf debug-output (not debug-output)))
    
  (when (and (not pressed) (eq keysym :f1))    
    (if *dump-character-tables*
	(setf *dump-character-tables* nil)
	(setf *dump-character-tables* t)))

  (when (eq keycode 38)
      (setf A-Press pressed))
  (when  (eq keycode 39)    
      (setf B-Press pressed))
  (when  (eq keycode 50)
      (setf Select-Press pressed) )    
  (when  (eq keycode 36)
      (setf Start-Press pressed))     
  (when  (eq keycode 111)
      (setf Up-Press pressed))     
  (when  (eq keycode 116)
      (setf Down-Press pressed))     
  (when  (eq keycode 113)
      (setf Left-Press pressed))     
  (when  (eq keycode 114)
    (setf Right-Press pressed))
  )

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

	(cpu-cycles-per-frame (/ (* 341 262) 3))
	(cpu-cycles-per-odd-frame (/ (- (* 341 262) 1) 3))
	(cpu-cycles-at-sl-0 (/ (+ (* 20 341)  340) 3))
	(cpu-cycles-per-scanline (/ 341 3))
	(vblank-end (/ 6820  3))
	(thing-1 nil)
	(vblank-starts-clear nil)
	(odd-frame t)	
	(first-frame t)
	(frame-start-time 0)	
	(NMI-latency 1)
	(NMI-started nil)
	(frame-ticks  (/ INTERNAL-TIME-UNITS-PER-SECOND 60))
	
	)
    (declare (ignorable  pattern-tables))

    ;; NMI are disabled on reset 
    (setf (ppuctrl-bit :nmi-enable t) 0)
    (setf *reset-pressed* t) 

    (when do-nestest
      (format t "Setting up for nestest")
      (setf *reset-pressed* nil)
      (setf on-reset nil)
      (setf debug-output t)
      )

    
    (setf glop:*ignore-auto-repeat* t)
    (glop:with-window (win "NES" 256 240)
      (cl-glu:ortho-2d 0 256 240 0)
      (gl:clear-color 0 0 0 0)

      (setf  frame-start-time (get-internal-run-time))
      (initalize-ppu)

      ;; Main loop
      (loop while (glop:dispatch-events win :blocking nil) do
   	   
	 ;; draw all the things!!
	   (gl:draw-pixels 256 240 :rgb :unsigned-byte frame)

	 ;; Snycronize with real time 
	   (let* ((time-elapsed (- (get-internal-run-time) frame-start-time))
		  (ticks-remaining (- frame-ticks time-elapsed)))
	     (when ( > ticks-remaining 0)
	       (sleep (/ ticks-remaining INTERNAL-TIME-UNITS-PER-SECOND)) 
	       ))
	   (setf  frame-start-time (get-internal-run-time))
	   
	   (when *reset-pressed*
	     (setf *reset-pressed* nil)
	     (setf interrupt-flag nil) 
	     (setf on-reset t))

	 ;; set the cpu-cycle counter to what was left over from the last frame 
	   (when (> cpu-cycles 0)
	     (if (and odd-frame (or (ppumaskp :enable-bg t) (ppumaskp :enable-spr t)))
		 (setf cpu-cycles (mod cpu-cycles cpu-cycles-per-odd-frame))
		 (setf cpu-cycles (mod cpu-cycles cpu-cycles-per-frame))))

	   
	 ;; Generate a reset interrupt
	   (when on-reset
	     (setf cpu-cycles 0)
	     (format t "REST = ~A~%" on-reset)
	     (setf on-reset nil)
	     (interrupt t  #xFFFC  #xFFFD)
	     (setf interrupt-flag t)
	     (initalize-ppu)
	     ) 

	   (setf (ppustatus-bit :vblank t) (if vblank-starts-clear 0 1))
	   
	   (setf vblank-starts-clear nil)
	   (setf NMI-occured t)
	   (setf sprite-hit-cycle 0)
	   
	 ;; supress NMI for this frame 
	   (when thing-1	
	     (setf (ppustatus-bit :vblank t) 0)
	     (setf NMI-occured nil))	   
	   (setf thing-1 nil)  
	   
	   (setf odd-frame (not odd-frame));every other frame has one less pixel per scan line	  

	   (let* ((cpu-cycles-this-frame (if (and odd-frame (or (ppumaskp :enable-bg t) (ppumaskp :enable-spr t)))
					     cpu-cycles-per-odd-frame
					     cpu-cycles-per-frame))
		  (begining-cycles nil)
					; (frame-start t) 
		  (last-scanline-renderd -1))
	     
	     (do ()	 
		 ((>= cpu-cycles cpu-cycles-this-frame))

	       #|      (setf debug-output t)
	       (if (gethash PC %debug-table%)
	       (setf debug-output nil)
	       (setf (gethash PC %debug-table%) t))
	       |#

	       #|
	       (setf debug-output nil)	      	       
	       (case PC	       
		 ((#xE34E #xE350 #xE352 #xE354 #xE356)
		  ((#xE347 #xE345 #xE343 #xE349 #xE34C #xE34D #xE34E #xE351 #xE352)		 
		   (( #xE33F #xE341 #xE343 #xE345 #xE348 #xE349 #xE34A #xE34C #xE34F)
		    (setf debug-output t)
		    ))
	       |#  	       
	       ;;(format t "cycle ~F  BG-ENABLE=~A SP-HIT=~A ~%" cpu-cycles (ppumaskp :enable-bg t) (ppustatusp :sprite-0-hit t))       
	       (when debug-output (format t "CYCLE=~11,4F PC=~4X P=~2X X=~2X Y=~2X A=~2X SP=~2X "
					  cpu-cycles (of PC) (status-register) RX RY AC SP))

	       (setf begining-cycles cpu-cycles)
	       (let ((cyc-start cpu-cycles)
		     (vblank-ending nil)
		     (vblank-at-end nil)
		     (last-nmi-status (ppuctrlp :nmi-enable t))
		     
		     )
		 (tagbody		  
		    ;; if we are a less that say 10  cycles from vblank end then save the state just in case it gets read 
		    (when  (and (<= cyc-start vblank-end) (< (- vblank-end cyc-start) 10))
		      (setf  vblank-ending t)
		      (save-cpu-state))

		    (when (< (- cpu-cycles-this-frame cyc-start) 10) ;; close to the end of the frame 
		      (setf vblank-at-end t)
		      (save-cpu-state))
		    
		  fuck-yourself
		    (process-cpu)

		    ;; NMI Should occur again if enabling after disabled without reading the vblank status
		    (when ppu-control-write
		      (setf ppu-control-write nil)

		      ;; Toggled ON 
		      (when (and (ppuctrlp :nmi-enable t) (not last-nmi-status))
			(cond
			  ((<= cpu-cycles  (+ vblank-end (/ 1 3))) ;; before vblank end
			   ;;(format t "Before VBL end ")
			   (setf NMI-started  (ppustatusp :vblank t)))
			  ((> cpu-cycles (+ vblank-end (/ 1 3))) ;; post vblank
			   ;;(format t "after VBL end ")
			   (setf NMI-occured t))
			  )

			;;(format t "NMI Enable at ~F - ~F ~%" cyc-start cpu-cycles)			
			)
		      ;; Toggled OFf
		      (when (and (not (ppuctrlp :nmi-enable t))  last-nmi-status)
			;;(format t "NMI Disable at ~F - ~F  (~F)  ~%" cyc-start cpu-cycles (- cpu-cycles-this-frame    (1- cpu-cycles)))
			;; If the effective write happens before 1 PPU cycle after  vblank then stop the NMI
			(when (> (- cpu-cycles-this-frame    (1- cpu-cycles)) (/ -2 3))
			  (setf NMI-latency 1)
			  (setf NMI-started nil) 
			  (setf NMI-occured nil)
			  ))
		      )
		    
		    ;; cpu crossed the vblank 
		    (when (>= cpu-cycles cpu-cycles-this-frame)
		      ;; vblank was read 
		      (when rest-ppu-status
			(let ((cycles-untill-end  (- cpu-cycles-this-frame    (1- cpu-cycles))  ))
			  (cond
			    ;;Reading on the same PPU clock or one later reads it as set, clears it, and suppresses the NMI
			    ;;for that frame
			    ((or (zerop cycles-untill-end) (equal  cycles-untill-end (/ -1 3)))
			     (when vblank-at-end
					;	 (format t " same or after ~X " %saved-pc  )
					;			 (setf debug-output t)				 
			       (restore-cpu-state)  ;
			       (setf cpu-cycles cyc-start)
			       (setf (ppustatus-bit :vblank t) 1)
			       (setf vblank-at-end nil)
			       (go fuck-yourself))  ;; read it as set
			     (setf thing-1 t)
			     
			     (setf NMI-latency 1)
			     (setf NMI-started nil) 
			     (setf  NMI-occured nil)
			     
			     )
			    ;;Reading one PPU clock before reads it as clear and never sets the flag or generates NMI for that frame.
			    ((equal cycles-untill-end (/ 1 3))
					;     (format t " one before ~X " %saved-pc )			       
					;  (setf (ppustatus-bit :vblank t) 0)
			     (setf thing-1 t)
			     
			     (setf NMI-latency 1)
			     (setf NMI-started nil) 
			     (setf  NMI-occured nil)
			     
			     (setf vblank-at-end nil))

			    ;;Reading two or more PPU clocks before it's set behaves normally 
			    ((>  cycles-untill-end (/ 1 3)) ; read as clear 			       
			     (when vblank-at-end
					;	 (format t " normalC  ~X  cs=~F ce=~F " %saved-pc cyc-start  cpu-cycles)
					;			 (setf debug-output t)				 
			       (restore-cpu-state)  ;
			       (setf cpu-cycles cyc-start)
			       (setf (ppustatus-bit :vblank t) 0)
			       (setf vblank-at-end nil)
			       (go fuck-yourself))  		       			       
			     )

			    ((<  cycles-untill-end (/ -1 3)) ; read as set
			     (when vblank-at-end
					;		 (format t " normalS  ~X  cs=~F ce=~F " %saved-pc cyc-start  cpu-cycles)
					;		 (setf debug-output t)				 
			       (restore-cpu-state)  ;
			       (setf cpu-cycles cyc-start)
			       (setf (ppustatus-bit :vblank t) 1)
			       (setf vblank-at-end nil)
			       (go fuck-yourself))  ;; read it as set
					; (setf thing-1 t)

			     (setf NMI-latency 1)
			     (setf NMI-started nil) 
			     (setf  NMI-occured nil)
			     
			     (setf vblank-starts-clear t)
			     (setf NMI-occured t)
			     )))			  
			))

					;		    (ppustatusp :vblank t)
		    (when (and  (<= cyc-start vblank-end) (>=  cpu-cycles vblank-end ))	      
		      (setf (ppustatus-bit :sprite-0-hit t) 0)
		      (setf (ppustatus-bit :vblank t) 0)
		      (setf NMI-occured  nil)
		      
					;  (format t "VBL Start=~F End=~F  PC=~X ~% " cyc-start cpu-cycles  %saved-pc)
		      ;; read occured >1/3 cycle before vblank end
					;    (format t "diff = ~F" (-  vblank-end cyc-start))
		      (when (and vblank-ending
				 rest-ppu-status
				 (<= (-  vblank-end (1- cpu-cycles)) 0 ))
					;		(format t "VBL Start=~F End=~F  PC=~X ~% " cyc-start cpu-cycles  %saved-pc)
			(restore-cpu-state) ; you sneaky mother fucker do it again
			(setf cpu-cycles cyc-start)
			(setf vblank-ending nil)
			(go fuck-yourself))
		      )
		    )
		 
		 (when (and NMI-occured (ppuctrlp :nmi-enable t)  )
		   (setf NMI-started t))

		 ;; NMI has been pipelined 
		 (when NMI-started
		   ;; Check if we can execute a NMI yet
		   (cond
		     ((and
		       (or (<= cyc-start  (+ vblank-end (/ 1 3))) 
			   (<= (- cpu-cycles-this-frame  cyc-start) (/ 1 3)) )
		       (zerop NMI-latency)
		       (not thing-1)
		       )
					;   (format t "NMI at ~F ~%" cyc-start)
		      (setf NMI-latency 1)
		      (setf NMI-started nil) 
		      (setf  NMI-occured nil)
		      (interrupt nil  #xFFFA  #xFFFB))
		     ((not (zerop NMI-latency))
		      (decf NMI-latency))))
		 );; let for the cyc-start 

	       
	       
	       (process-registers)


	      ; (advance-ppu (- cpu-cycles begining-cycles) odd-frame)
	       
	       ;;256x240 is the screen size displayed 
	       ;; figure out the scan line number and draw that frame
	       ;; if the beam x is at the location for a sprite 0 hit then set the flag
	       ;; 341 pixels per scan line cpu-cycle 0 is scanline 241 point 1 and rendering starts at
	       ;; 340 for 241 + 341 for 242 through 261 + 1
	       (when (>= cpu-cycles cpu-cycles-at-sl-0) ;if we should render this line
		 (let ((scan-line (floor (/ (- cpu-cycles cpu-cycles-at-sl-0) cpu-cycles-per-scanline))))		   
		   (when (and (not (equal scan-line last-scanline-renderd)) (< scan-line 240) )
		     (setf last-scanline-renderd scan-line)

		     (decode-scanline scan-line)
		     (let ( ;(sl (decode-scanline scan-line))
			   (idx0 (+ (* 3 256 (- 239 scan-line)) )))
		       (declare (type (simple-array (unsigned-byte 8) (256)) decoded-scanline))
		       (let ((last-clr -1)
			     (last-r 0)
			     (last-g 0)
			     (last-b 0))
			 (dotimes (x 256)
			   (let* ((clr  (aref decoded-scanline x))
				  (r  (if (equal clr last-clr) last-r (get-pal-red clr )))
				  (g  (if (equal clr last-clr) last-g (get-pal-green clr)))
				  (b  (if (equal clr last-clr) last-b (get-pal-blue clr)))
				  (idx (+ idx0 (+ (* 3 x) 0)) ))     

			     (when (not (equal clr last-clr))
			       (setf last-clr clr)
			       (setf last-r r)
			       (setf last-g g)
			       (setf last-b b))
			     
			     (setf (aref frame idx ) r)
			     (setf (aref frame (+ idx 1)) g)
			     (setf (aref frame (+ idx 2)) b) 
			     )))))
		   ;; Check for sprite 0 hit
		   (when sprite-0-hit 
		     (setf sprite-hit-cycle (+ (* scan-line cpu-cycles-per-scanline) (/ sprite-0-hit-x 3) cpu-cycles-at-sl-0))      
		     (setf sprite-0-hit nil))
		   ))

	       (when (and (not (zerop sprite-hit-cycle)) (>= cpu-cycles sprite-hit-cycle))
		 (when (ppumaskp :enable-bg t)	   
		   
		   (when (or  (ppumaskp :clp-spr t) (>= sprite-0-hit-x  8))
		 ;    (format t "Sprite 0 hit at X=~D cycle=~F clp=~A ~%" sprite-0-hit-x sprite-hit-cycle (ppumaskp :clp-spr t))
		     (setf (ppustatus-bit :sprite-0-hit t) 1)
		     )
		   )
		 (setf sprite-hit-cycle 0))	       
	       ))

	   (when first-frame (setf  first-frame nil))
	   (glop:swap-buffers win)
	 ;;(format t "FPS = ~F~%" (/ 1 (/ (- ( get-internal-real-time) frame-start)  internal-time-units-per-second)))
	   
	   ))
    ))



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
	 (setf (subseq (o4 cpu-memory) upper-bank-rom (+ upper-bank-rom (length program-rom))) program-rom)
	 (setf (subseq (o4 cpu-memory) lower-bank-rom (+ lower-bank-rom (length program-rom))) program-rom)
	 (setf PC upper-bank-rom))

	;; copy it into the lower bank 
	((eq prg-rom-count 2)
	 (setf (subseq (o4 cpu-memory) lower-bank-rom (+ lower-bank-rom (length program-rom))) program-rom)
	 (setf PC lower-bank-rom))	 

	;; copy the first 32k into the lower bank
;	((> 2 eq prg-rom-count)
;	 (setf (subseq cpu-memory lower-bank-rom #x8000) program-rom))
	((error "wat")))

      ;; move the pattern tables into the PPU memory
      (cond
	;;load the character rom into memory
	((eq chr-rom-count 1)
	 (setf (subseq (o5 ppu-memory) 0  (length character-rom)) character-rom))

	;; load just the first 8kb 
	((> chr-rom-count 1)
	 (setf (subseq (o5 ppu-memory) 0  #x2000) character-rom)))

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

(defun ppu-test-1 (number)
  (case number
    (0 (load-rom "nes-test-roms-master/blargg_ppu_tests_2005.09.15b/palette_ram"))
    (1 (load-rom "nes-test-roms-master/blargg_ppu_tests_2005.09.15b/sprite_ram"))
    (2 (load-rom "nes-test-roms-master/blargg_ppu_tests_2005.09.15b/vram_access"))
    (3 (load-rom "nes-test-roms-master/blargg_ppu_tests_2005.09.15b/power_up_palette"))
    (4 (load-rom "nes-test-roms-master/blargg_ppu_tests_2005.09.15b/vbl_clear_time")))    
  (run)
  )

(defun ppu-test-vblank-1 (number)
  (case number 
    (0 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/01-vbl_basics"))
    (1 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/02-vbl_set_time"))
    (2 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/03-vbl_clear_time"))
    (3 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/04-nmi_control"))
    (4 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/05-nmi_timing"))
    (5 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/06-suppression"))
    (6 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/07-nmi_on_timing"))
    (7 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/08-nmi_off_timing"))
    (8 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/09-even_odd_frames")) 
    (9 (load-rom "nes-test-roms-master/ppu_vbl_nmi/rom_singles/10-even_odd_timing"))) ;f 
  (run)
  )

(defun nmi-vbl-test-1 (number)
  (case number 
    (0 (load-rom "nes-test-roms-master/vbl_nmi_timing/1.frame_basics"))
    (1 (load-rom "nes-test-roms-master/vbl_nmi_timing/2.vbl_timing"))
    (2 (load-rom "nes-test-roms-master/vbl_nmi_timing/3.even_odd_frames"))
    (3 (load-rom "nes-test-roms-master/vbl_nmi_timing/4.vbl_clear_timing"))
    (4 (load-rom "nes-test-roms-master/vbl_nmi_timing/5.nmi_suppression"))
    (5 (load-rom "nes-test-roms-master/vbl_nmi_timing/6.nmi_disable"))
    (6 (load-rom "nes-test-roms-master/vbl_nmi_timing/7.nmi_timing")))
  (run)
  )

(defun ppu-sprite-hit-tests-1 (number)
  (case number
    (0 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/01.basics"))
    (1 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/02.alignment"));f
    (2 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/03.corners"));f
    (3 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/04.flip"));f
    (4 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/05.left_clip"));f
    (5 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/06.right_edge"));f
    (6 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/07.screen_bottom"));f
    (7 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/08.double_height"));f
    (8 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/09.timing_basics"));f
    (9 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/10.timing_order"));f
    (10 (load-rom "nes-test-roms-master/sprite_hit_tests_2005.10.05/11.edge_timing")));f
  (run))
    
(defun print-blarg-status ()
  (let ((message nil))
    (do ((x #x6004 (incf x)))
	((eq (aref (o4 cpu-memory) x) 0))
      (setf message (concatenate 'string message (format nil "~A" (code-char (aref (o4 cpu-memory) x)))))) message))


(defun load-nestest-log ()
  (let ((addresses (make-hash-table :test 'equal)))   
  (with-open-file (rom "/home/frank/projects/nes-emulator/nestest-addresses.log" )
    (do ((x (read-line rom) (read-line rom nil 'eof)))
	((equal x 'eof))
     (setf (gethash x addresses)  t)
      ))
  addresses 
  ))
