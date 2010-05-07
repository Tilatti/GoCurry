all: gopherd mkcache

gopherd:
	$(MAKE) -C ./src all

mkcache:
	$(MAKE) -C ./mkcache all

clean:
	$(MAKE) -C ./src clean

