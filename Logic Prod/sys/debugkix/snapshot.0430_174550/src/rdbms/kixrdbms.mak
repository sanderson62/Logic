CPP=cl.exe
PCC=procob
PCCFLAGS=
INCDIR=
COB=cobol
BINDIR=$(UNIKIX)\local\bin
LIBDIR=$(UNIKIX)\lib

!IF "$(RDBMS)" == "ORACLE"
RDBMS_LIBS="$(ORACLE_HOME)"/pro80/lib/sqllib80.lib
TARGET=KXORACLE
!ENDIF

!IF "$(RDBMS)" == "MSSQL"
RDBMS_LIBS=
TARGET=KXMSSQL
!ENDIF

!IF "$(RDBMS)" == ""
!MESSAGE "You need to set RDBMS= ORACLE/MSSQL"
TARGET=NULL
!ENDIF

ALL :  $(TARGET).dll


CLEAN : 
	-@erase  $(TARGET).obj
	-@erase  $(TARGET).cbl
	-@erase  "$(BINDIR)\$(TARGET).dll"
	-@erase  "$(BINDIR)\$(TARGET).exp"
	-@erase  "$(LIBDIR)\$(TARGET).lib"

.pco.cbl:
   $(PCC) $(PCCFLAGS) iname=$*.pco oname=$*.cbl include=$(INCDIR) \
	ireclen=132 mode=ansi hold_cursor=no  

.pgm.obj:
	cp $*.pgm $*.cbl
	$(COB) $*.cbl,,,;

.obj.dll:
	cbllink -V -D -K $(RDBMS_LIBS) $*.obj
	cp $*.dll $(BINDIR)
	cp $*.exp $(BINDIR)
	cp $*.lib $(LIBDIR)
	echo "Going to make kixuser.dll"
	cd ../trans
	nmake -f kixuser.mak clean
	nmake -f kixuser.mak RDBMS="$(RDBMS)"

KXORACLE.dll:	$(BINDIR)\KXORACLE.dll

$(BINDIR)\KXORACLE.dll:	KXORACLE.obj

KXORACLE.obj:	KXORACLE.pco 
   $(PCC) $(PCCFLAGS) iname=$*.pco oname=$*.cbl \
	ireclen=132 mode=ansi hold_cursor=no  
	$(COB) $*.cbl,,,;

KXMSSQL.dll:	$(BINDIR)\KXMSSQL.dll

$(BINDIR)\KXMSSQL.dll:	KXMSSQL.obj

KXMSSQL.obj:	KXMSSQL.pgm
	cp $*.pgm $*.cbl
	$(COB) $*.cbl,,,;

NULL.dll:
	Echo "Nothing to make"
