all:
	cd src && erl -make

clean:
	rm -f ebin/*.beam
