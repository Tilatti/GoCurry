all: gopherd mkcache

gopherd:
	$(MAKE) -C ./src all

mkcache:
	$(MAKE) -C ./tools/mkcache all

clean:
	$(MAKE) -C ./src clean

