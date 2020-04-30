EMACS=emacs
TYPE=Release

liberime-core:
        ifdef MINGW_PREFIX
	cmake -H. -Bbuild -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=${TYPE}
        else
	cmake -H. -Bbuild -DCMAKE_BUILD_TYPE=${TYPE}
        endif
	cmake --build build

clean:
	rm -rf build

test: liberime-core
	${EMACS} -Q -L build -L . liberime-test.el

ccls:
	cmake -H. -BDebug -DCMAKE_BUILD_TYPE=Debug \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=YES
