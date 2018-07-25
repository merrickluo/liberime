EMACS = emacs

build/liberime.so:
	mkdir build
	cd build && cmake .. && make

clean:
	rm -rf build

test: build/liberime.so
	${EMACS} -Q -L build test.el
