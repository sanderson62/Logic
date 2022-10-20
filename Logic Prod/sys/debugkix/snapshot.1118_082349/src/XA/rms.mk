.c.o:
	rm -f kxglobalrmtable.o
	$(CC) $(CFLAGS) -c $<

RMSOBJECTS=kxglobalrmtable.o

all:	rms
rms:	$(RMSOBJECTS)
	$(CC) $(LDFLAGS) -shared $(RMSOBJECTS) -o libkxrm.so  -lc
