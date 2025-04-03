.PHONY:  nes

nes:
	mkdir -p ~/.config/common-lisp/source-registry.conf.d/
	echo "(:tree \"/home/frank/projects/nes-emulator/\")" > ~/.config/common-lisp/source-registry.conf.d/nes.conf


