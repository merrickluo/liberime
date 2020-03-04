EMACS = emacs
TYPE=Release

liberime-core:
	mkdir -p build
	cd build && cmake -DCMAKE_BUILD_TYPE=${TYPE} .. && make

clean:
	rm -rf build

test: liberime-core
	${EMACS} -Q -L build -L . test.el

ccls:
	cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=YES
