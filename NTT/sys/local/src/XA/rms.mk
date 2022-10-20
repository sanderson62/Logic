.c.o:
	rm -f kxglobalrmtable.o
	$(CC) $(CFLAGS) -c $<

RMSOBJECTS=kxglobalrmtable.o

all:	rms
rms:	$(RMSOBJECTS)
	$(CC) $(SHARED_LIB_FLAGS) $(RMSOBJECTS) -o libkxrm.so  -lc
