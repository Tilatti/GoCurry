SRC = main.hs net.hs parse_request.hs parser.hs read_cache.hs			\
      connection_list.hs functions_map.hs config.hs Syslog.hs opts.hs		\
      file_utils.hs dir_utils.hs
OBJECT = $(SRC:.hs=.o)
OBJECT_INT = $(SRC:.hs=.hi)
MAIN = "main.hs"
EXE = "gopherd"
GHC = ghc

all: $(EXE)

$(EXE):
	$(GHC) --make $(SRC) -o $@

clean:
	-rm $(OBJECT) $(OBJECT_INT)
