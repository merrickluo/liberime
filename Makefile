EMACS = emacs

liberime:
	mkdir -p build
	cd build && cmake .. && make

clean:
	rm -rf build

test: liberime
	${EMACS} -Q -L build test.el
