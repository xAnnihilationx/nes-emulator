(defsystem :nes-emulator
  :author "Frank A. cashio"
  :license "CONFIDENTAL"
  :description "A lispy emulator for the Nintendo Entertainment System (NES)"
  :version "0.7.0"
  :components ((:file "package")
	       (:file "common")
	       (:file "cpu" :depends-on ("common"))
	       (:file "nes-emulator" :depends-on ("common" "cpu" "ppu") )
	       (:file "ppu" :depends-on ("common" "cpu")))
  :depends-on (:cl-opengl
	       :cl-glu
	       :cl-glut
	       :cl-openal
	       :glop
	       :cffi))
