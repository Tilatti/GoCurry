CC = gcc
EXE = mkcache
OBJECT = mkcache.o
CFLAGS =

all: $(EXE)

$(EXE): $(OBJECT)
	$(CC) -g -ggdb $(OBJECT) -o $(EXE)

%.o: %.c
	$(CC) $(CFLAGS) -g -ggdb -c $^ -o $@

