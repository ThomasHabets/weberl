all:
	mkdir -p ../ebin
	erl -make

run: all
	(cd ../ebin && erl -s weberltest test)
