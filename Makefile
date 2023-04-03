all:
	dune build
	ln -sf _build/default/bin/main.exe euler

clean:
	dune clean
	rm euler
