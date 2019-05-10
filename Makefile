SRC = $(shell find src/c -name "*.c")
DLL = $(patsubst src/c/%.c, src/c/%, $(SRC))

make:
	swipl-ld -shared -o $(DLL) $(SRC)

clean:
	$(RM) src/c/*.so
	$(RM) src/c/*.o

.PHONY: clean