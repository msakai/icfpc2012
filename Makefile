# Linux向け
all:
	rm -rf icfp-96192366
	mkdir  icfp-96192366
	cabal configure --flags="Static"
	cabal build
	cp -a dist/build/lifter/lifter icfp-96192366/
	cp -a src PACKAGES README icfpc2012.cabal install icfp-96192366/
	tar zcf icfp-96192366.tgz icfp-96192366
