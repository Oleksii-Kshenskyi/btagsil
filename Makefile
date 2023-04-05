CC=clang
CXX=clang++

fullclean:
	rm -rf ./build

localclean:
	cd build && mv _deps .. && rm -r ./* && mv ../_deps .

cleancache:
	cd build && rm CMakeCache.txt

cmakegen:
	mkdir -p build && cd build && (CC=${CC} CXX=${CXX} cmake .. -G "Unix Makefiles")

compile: cmake
	cd build && make -j16

run: compile
	echo && ./build/btagsil-cli

sandbox: compile
	echo && ./build/sandbox