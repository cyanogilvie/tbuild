all: tbuild

tbuild: tbuild.tcl
	sdx wrap tbuild -interp kbskit8.6

clean:
	-rm -rf tbuild
