EMACS = emacs
TYPE=Release

liberime:
	mkdir -p build
	cd build && cmake -DCMAKE_BUILD_TYPE=${TYPE} .. && make

clean:
	rm -rf build

test: liberime
	${EMACS} -Q -L build test.el
