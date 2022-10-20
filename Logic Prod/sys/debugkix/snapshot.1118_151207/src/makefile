#**********************************************************************#
#*                                                                    *#
#* Copyright (c) 2007 by Clerity Solutions, Inc.                      *#
#* All rights reserved.                                               *#
#*                                                                    *#
#**********************************************************************#


#* $Date:   29 Mar 2012 03:16:30  $
#* $Modtime:   28 Mar 2012 03:13:22  $

# ***********************************************
# $Workfile:   usermake.src  $ $Revision:   1.31.2.0  $
# ***********************************************
#
# This makefile should be used to generate all user modifiable
# executable modules.
#
# The makefile may be invoked in the following manner:
#
# make all - Create all user modifiable objects and executables, except security
#
# or, if ESM security customization is also to be done:
#
# make security - Create all user modifiable objects & executables, and security
#
# Individual executables can be also be built, as listed:
#
# make kixmakecnv
#
# make nlsmsg
#
# make rtsvsam
#
# make sortx
#
# make unikixrcv
#
# make unikixtran - a COBOL or non-COBOL based transaction processor
#		    will be built. Use 'kixinstall' to generate the
#		    proper makefile.
#
# make	line.in	Individual files
#	line.out
#	mfrcd.in
#	mfrcd.out
#	record.in
#	record.out
#	recordv.in
#	recordv.out
#	mfrcdin
#	mfrcdout
#
# The following executables can be rebuilt if SECURITYOBJECTS changes.  They
# will be built by a "make security", along with those built with "make all"
#
# make unikixcemt
#
# make unikixfms
#
# make unikixred
#
# make unikixmnu
#
# make unikixstrt
#
# The following executables can be rebuilt if BATCHSECURITYOBJECTS changes.
#
# make dfhusdup
#
# make kixfile
#
# make unikixbld
#
# If cobol is present, the following executables can be built.  They will
# not be built by "make all".
#
# make CSSNCESN
#
# make kxalt008
#
# make kxalt016
#
# make kxalt032
#
# make kxalt064
#
# make kxalt128
#
# make kxalt256
#
# make kxalt516
#
# make clean - Remove all user modifiable objects.
# 
# make clobber - Remove all user modifiable objects and executables.
# 
# for transaction processor with database set the following
# variables prior to make
#
#----------- RDBMSFLAG = -DORACLE, -DINFORMIX, -DSYBASE or -DDBTWO -----------
#-----------             -DDATACOM -DINGRES                        -----------
#----------- RDBMSFLAG = -I(mqseries include file path)
RDBMSFLAG= -DKX_SERVER_EXPRESS 
#------ Set include path for SQLCA ---------------------------
# oracle (pro*cobol 1.3)             INCDIR = $(ORACLE_HOME)/cobol/lib
# oracle (pro*cobol 1.4 and upwards) INCDIR = $(ORACLE_HOME)/procob/lib
# sybase                             INCDIR = $(SYBASE)/include
INCDIR=
#-------------- for oracle database -----------------------
PCC=
PCCFLAGS=
ORACLELIB=
#
#-------------- for informix database -----------------------
INFORMIXLIB=
#
#-------------- for sybase database -----------------------
SYBCC=
SYBFLAGS=
SYBCFLAGS=
SYBASELIB=
#
#-------------- for datacom database -----------------------
DCOM=
DFLAGS=
#-------------- for ingres database -----------------------
ESQLCBL=
INGFLAGS=

#
#-------------- for MQSeries ------------------------------
MQSERIESLIB=

#
#-------------- for EBM Runtime System ------------------------------
EBM_MF_INX=n
EBM_MF_REL=y
LDEXCI=

# ***********************************************
# rules
# ***********************************************


.SUFFIXES:	.c .int .o .cbl .cl2 .pc .pco .eco .cop .pgm .dco .scb .clt .ppl .sqc .sqb .cp .pli .so .class

.clt.cbl: makefile
	( cd `dirname $<` ; \
	   rm -f $*.err ; \
	   rm -f $*.cbl ; \
	   kixclt $*.clt )
.cl2.cbl: makefile
	( cd `dirname $<` ; \
	   rm -f $*.err ; \
	   rm -f $*.cbl ; \
	   kixclt $*.cl2 )
#
# for .cbl->.int
# cd to dirname $< since we are issuing make from src
# directory and the source and objects files are in another
# directory; e.g. src/record
#
.cbl.int: makefile
	( cd `dirname $<` ; \
		rm -f $*.int $*.o ; \
		$(COBOL) $(COBFLAGS) -C $(COBOPT) $< )
#
# for .cbl->.class
# cd to dirname $< since we are issuing make from src
# directory and the source and objects files are in another
# directory; e.g. src/record
#
.cbl.class: makefile
	( cd `dirname $<` ; \
		rm -f $*.class ; \
		$(JAVACOBCMD) $(JAVACOBOPT) `basename $<` )
#
# for .c->.o
# cd to dirname $< since we are issuing make from src
# directory and the source and objects files are in another
# directory; e.g. src/convert. Also, need to specify the 
# .o name so it gets put in the right directory.
#
.c.o:
	(cd `dirname $<` ; \
		rm -f $*.o ; \
		$(CC) $(CFLAGS)	$(RDBMSFLAG) -I ../CICS_structures \
                                             -o $*.o -c $< )

#
# for all database compiles, must do similar to above and cd
# to the appropriate directory before doing the compiles
#
.pc.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cx $*.c $*.o ; \
	   $(PCC) $(PCCFLAGS) iname=$*.pc oname=$*.cx include=$(INCDIR) \
	       ireclen=132 mode=ansi release_cursor=yes hold_cursor=no ; \
	   	mv $*.cx $*.c ; \
		$(CC) $(CFLAGS) $(RDBMSFLAG) -o $*.o -c $*.c ;\
	   	mv $*.c $*.cx )
.pco.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cob $*.idy $*.int $*.lis $*.o ; \
	   $(PCC) $(PCCFLAGS) iname=$*.pco oname=$*.cob include=$(INCDIR) \
	       ireclen=132 mode=ansi release_cursor=yes hold_cursor=no ; \
	   $(COBOL) -x -c -C $(COBOPT) $*.cob )

.pco.class: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cob $*.class $*.lis $*.err; \
	   $(PCC) $(PCCFLAGS) iname=$*.pco oname=$*.cob include=$(INCDIR) \
	       ireclen=132 mode=ansi release_cursor=yes hold_cursor=no ; \
	   $(JAVACOBCMD) $(JAVACOBOPT) $*.cob; \
	   rm -f $*.cob $*.lis $*.err)

.ppl.o: makefile
	( cd `dirname $<` ; \
	   rm -f temp.lis temp.ppl *.temppli tempplt $*.err $*.o ; \
	   $(PP)  $(PPFLAGS) $< -o temp.ppl ; \
	   $(PPL) iname=temp.ppl oname=tempplt include=$(INCDIR) ireclen=256 \
		maxliteral=160 mode=ansi ; \
	   $(PLT) $(PLTFLAGS) -i tempplt    -o $*.temppli ; \
	   $(PLI) $(PLIFLAGS) $*.temppli -o $*.o ; \
	   rm -f temp.lis temp.ppl *.temppli tempplt )

.pli.a: makefile
	( cd `dirname $<` ; \
	   rm -f $*.temppli $*.o $*.so ; \
	   $(PP)  $(PPFLAGS)  $< -o $*.temppli ; \
	   $(PLI) $(PLIFLAGS) $*.temppli ; \
	   ar cq $*.a $*.o; \
	   rm -f $*.temppli $*.o)

.pli.so: makefile
	( cd `dirname $<` ; \
	   rm -f $*.temppli $*.o $*.so ; \
	   $(PP)  $(PPFLAGS)  $< -o $*.temppli ; \
	   $(PLI) $(PLIFLAGS) $*.temppli ; \
	   $(LD) $(PLISHRFLAGS) -o $*.so $*.o; \
	   rm -f $*.temppli $*.o)

.cop.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cob $*.idy $*.int $*.o ; \
	   $(SYBCC) $(SYBFLAGS) -O$*.cob $*.cop ; \
	   $(COBOL) -x -c -C $(COBOPT) $*.cob )

.cop.class: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cob $*.idy $*.int $*.o ; \
	   $(SYBCC) $(SYBFLAGS) -O$*.cob $*.cop ; \
	   $(JAVACOBCMD) $(JAVACOBOPT) $*.cob; \
	   rm -f $*.cob $*.lis $*.err)

.cp.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.c $*.o ; \
	   $(SYBCC) $(SYBFLAGS) -O$*.c $*.cp ; \
	   $(CC) $(CFLAGS) $(SYBCFLAGS) -c -I$(INCDIR) $*.c ; \
	   rm -f $*.c )
.eco.o: makefile
	( cd `dirname $<` ; \
	   esqlcobol -e $*.eco ; \
	   $(COBOL) -x -c -C $(COBOPT) $*.cob )
.pgm.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.int ; \
	   $(COBOL) -x -c -C $(COBOPT) -k $*.pgm )
.sqb.o: makefile
	( cd `dirname $<` ; \
		rm -f $*.o $*.cbl ; \
		db2 connect to $(DB2DBDFT); \
		db2 prep $< bindfile target mfcob; \
		db2 bind $*.bnd ; \
		db2 connect reset ; \
		db2 terminate ; \
		COBCPY=$(INSTHOME)/sqllib/include/cobol_mf ;\
		export COBCPY ;\
		$(COBOL) -x -c  -C $(COBOPT) -k $*.cbl )
.sqb.class: makefile
	( cd `dirname $<` ; \
		rm -f $*.class $*.cbl ; \
		db2 connect to $(DB2DBDFT); \
		db2 prep $< bindfile target ansi_cobol; \
		db2 bind $*.bnd ; \
		db2 connect reset ; \
		db2 terminate ; \
		$(JAVACOBCMD) $(JAVACOBOPT) -sp=$(INSTHOME)/sqllib/include/cobol_a:$(INSTHOME)/sqllib/include/cobol_mf:. $*.cbl)

.sqc.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.o ; \
	   db2 connect to $(DB2DBDFT); \
	   db2 prep $< ; \
	   db2 terminate ; \
	   $(CC) $(CFLAGS) -c $*.c -I $(INSTHOME)/sqllib/include ; \
	   rm -f $*.c ) 

.dco.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cob *.lis SQL*.LIB ; \
	   $(DCOM) $(DFLAGS) -g KXDATACOM $*.dco ; \
	   $(COBOL) -x -c -C $(COBOPT) $*.cob ; \
	   rm -f *.lis )
.scb.o: makefile
	( cd `dirname $<` ; \
	   $(ESQLCBL) $(INGFLAGS) -f$*.cob $*.scb ; \
	   $(COBOL) -x -c -C $(COBOPT) $*.cob )

# ***********************************************
# variables to rules
# ***********************************************

#
# Default machine/platform, etc.
#

MACHINE=LINUX
PLATFORM=UNIX
USERLIBDIR=${UNIKIX}/lib
E=

#
# Default make command
#

MAKE=make

#
# Default C compiler location
#

CC=cc

#
# Default C flag settings (optimize).
#

CFLAGS=-O -m32 -DLINUX -D_LFS64_LARGEFILE=1 -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -DTERMINFO -DCOBOL_HAS_LONGJMP   -DINTEL

#
# Invoke Linker command.
#

LD=ld

#
# Default Linker flag settings (none).
#

LDFLAGS=-m32 -Wl,-E
LDFLAGS2=-melf_i386

#
# Default Linker flag settings RS600(only) shared object export.
#

RS6_SHR_MF=
RS6_SHR_ISCOBOL=
RS6_SHR_NC=

#
# Linker flag settings for dynamic libraries
#

DL_FLAGS_RS6=-blibpath:/usr/lib:/lib:.

#
# Default Linker flag settings for PLI (none).
#

PLILDFLAGS=
PLISHRFLAGS=-m32 -shared

#
# Special compile options
#
SPECIALOPTS_UNIX=
SPECIALOPTS_INTERIX=-Wl,--export-dynamic
SPECIALOPTS=$(SPECIALOPTS_$(PLATFORM))

#
# Invoke copy command.
#

CP=cp

#
# Invoke Micro Focus Cobol command.
#

COBOL=cob

#
# Invoke isCOBOL command.
#

ISCP_UNIX=
ISCP_INTERIX=`echo ${KIX_JDK_ROOT}/lib/tools.jar\;${ISCOBOL}\;${ISCOBOL}/lib/iscobol.jar\;${ISCOBOL}/lib/isxms.jar | sed 's|/dev/fs/\(.\)|\1:|g'`
ISCP=$(ISCP_$(PLATFORM))

ISCC_UNIX=${ISCOBOL}/bin/iscc
ISCC_INTERIX=java$(E) -classpath $(ISCP) com.iscobol.compiler.Pcc
ISCC=$(ISCC_$(PLATFORM))

#
# Invoke vCOBOL command.
#

VCOBOL_DIR=`echo $(VCOBOL) | sed 's|\(.\):|/dev/fs/\1|g'`
VCC_UNIX=${VCOBOL}/bin/vcc
VCC_INTERIX=${VCOBOL_DIR}/bin/vcc$(E)
VCC=$(VCC_$(PLATFORM))

#
# setup the correct cobol for is or v Cobol
#

JAVACOBCMD_iscob=$(ISCC)
JAVACOBOPT_iscob=$(ISCOBOPT)

JAVACOBCMD_vcob=$(VCC)
JAVACOBOPT_vcob=$(VCOBOPT)

JAVACOBCMD_MFCOBOL=
JAVACOBOPT_MFCOBOL=

JAVACOBCMD=$(JAVACOBCMD_$(COBOL_EXT))
JAVACOBOPT=$(JAVACOBOPT_$(COBOL_EXT))


#
# Default JVM library/linker flags
#
KIXJVMLDFLAGS_UNIX=-L${KIX_JVM_DIR} -L${KIX_JSIG_DIR} -ljsig -ljvm
KIXJVMLDFLAGS_INTERIX=-R ${KIX_JDK_ROOT}/lib/jvm.lib
KIXJVMLDFLAGS=$(KIXJVMLDFLAGS_$(PLATFORM))

BRIDGE_LIBS_UNIX=
BRIDGE_LIBS_INTERIX=$(USERLIBDIR)/kixuser.lib $(USERLIBDIR)/kixmq.lib $(MSSDK_KERNEL32)
BRIDGE_LIBS=$(BRIDGE_LIBS_$(PLATFORM))

#
# Default Cobol flag settings 
#
COBFLAGS=-I CICS

#
# Default Micro Focus Cobol compile options
#
COBOPT="ibmcomp noalter ans85 notrunc defaultbyte=0"

#
# Default Cobol compile options for CICS transactions
#
COBOPT1="cics ibmcomp noalter ans85 notrunc defaultbyte=0"

#
# Default isCOBOL/vCOBOL compile options for CICS transactions
#
include ${UNIKIX}/lib/Makefile.include
ISCOBOPT_UNIX=${KIX_ISCOBOL_ONLINE_FLAGS} -sp=${INCDIR}
ISCOBOPT_INTERIX=`echo ${KIX_ISCOBOL_ONLINE_FLAGS} -sp=${INCDIR} | sed 's|/dev/fs/\(.\)|\1:|g'`
ISCOBOPT=$(ISCOBOPT_$(PLATFORM))
VCOBOPT=${KIX_VCOBOL_ONLINE_FLAGS} -sp=${INCDIR}

#
# By default unikixtran is built without PLI, using kixnopli.so 
# if PLI is chosen during install then kixpli.so is used
# which sets PLI language
#          
           
PLI_MOD=kixnopli

#
TRAN_LIBS=-l${PLI_MOD}
#

#
# Default cobol extension for transaction proc is 'cob'.
# Will be changed by kixinstall to 'nocob' if Shared
# Library makefile environment is chosen
#
COBOL_EXT=mfcob

#
# The kxnoranim.o module contains stubs for remote animation
# for users who do not have cobol or who do not have Server
# Express v2.0.11 SP1 or greater. Erased by kixinstall if user
# has the above version.
#
RANIMSTUB=

#
# variables for Liant PL/I compiler
# and PL/I oracle support variables
#

PLI=${LPI_PRODUCT_DIR}/bin/lpipl1
PLT=${UNIKIX}/bin/kixplt
PLX=${LPI_PRODUCT_DIR}/bin/lpiplx
PP=${LPI_PRODUCT_DIR}/bin/lpipp
PPL=${LPI_PRODUCT_DIR}/bin/propli

INCPATH=${LPI_PRODUCT_DIR}/include
PPFLAGS=-isuffix .inc -margins 1,256 -ipath ${INCPATH}
PLIFLAGS=-zp1 -opt 3 -pic -ipath ${INCPATH}
PLTFLAGS=

# ***********************************************
# standard variables
# ***********************************************

#
# The following variables assume the external setting of the
# following environment variables.
#
# UNIKIX - base directory of the product
#
# Declare variable for the location of "user" bin directory.
# The default value is the local/bin directory, as defined in
# the Transaction Processing Software Administrator's Guide.
#
# This directory is the target for the user executable files.
#

BINDIR=${UNIKIX}/local/bin

#
# Declare variable for the location of the lib directory.
#
# This directory is the target for the user and system library
# modules.
#

LIBDIR=${UNIKIX}/lib

#
#  link library directory for libraries like PL/I
#

LINKLIBDIR=

#
# Declare variable for the location of the src directory
# This directory is the target for the user modifiable source and
# object files.
#

USERSRCDIR=/apps/mtp/mtp/src

USERRDBMSSRCDIR=

USERCONVERT=$(USERSRCDIR)/convert

USERINSTALL=$(USERSRCDIR)/install

USERISC=$(USERSRCDIR)/isc

USERRECORD=$(USERSRCDIR)/record

USERRECOVERY=$(USERSRCDIR)/recovery

USERSECURITY=$(USERSRCDIR)/security

USERSOCKET=$(USERSRCDIR)/socket

USERTERMINAL=$(USERSRCDIR)/terminal

USERTRANS=$(USERSRCDIR)/trans

USERXA=$(USERSRCDIR)/XA

#
# during kixinstall if isCOBOL and any RDBMS is chosen this
# will contain the target .class
#

USERISCOBOLINT=

#
# Declare variable for the source modules within the
# user source directory.
#

USERSOURCES=

#
# Declare variable for the user specific object modules within the
# user source directory.
#

USERSPECOBJECTS=

#
# Declare variable for the user specific compiler/linker options
#

USERSPECOPTIONS=

#
# Declare objects specific to online (unikixtran).
# By default USERSPECOBJECTS will be added to this
#

TRANSPECOBJECTS=

#
# Declare objects specific to batch (rtsvsam).
# By default USERSPECOBJECTS will be added to this
#

VSAMSPECOBJECTS=

#
# Declare variable for the object modules within the
# user source directory.
#

USEROBJECTS=

#
# variables providing machine-specific required options
#    KXTABLES_LIB is for building most executables
#    BASE_UNIX_LIBS is for building executables like unikixstrt
#    TRAN_UNIX_LIBS is for building unikixtran
#    RTSVSAM_UNIX_LIBS is for building rtsvsam
#    SORTX_UNIX_LIBS is for building sortx
#    TRAN_COB_OPTS is for unikixtran
#    RTSVSAM_COB_OPTS is for rtsvsam

KXTABLES_LIB=-L ${UNIKIX}/lib -lkxtables

BASE_UNIX_LIBS=-lnsl -lncurses -ldl -lcrypt -lrt -lpthread

TRAN_UNIX_LIBS=-L ${UNIKIX}/lib -lcrypt -lncurses -lnsl -lm -ldl -lkxtables -lkxtm -lkxrm -lrt -lpthread

RTSVSAM_UNIX_LIBS=-lm -lnsl -lrt -lpthread

SORTX_UNIX_LIBS=-lrt -lpthread

TRAN_COB_OPTS=-x

RTSVSAM_COB_OPTS=

#
# LIBBASAP will currently only have a value for RS6/AIX machines. The
# value should be the path, ${UNIKIX}/lib/libbasap.a
#
LIBBASAP=


#
# Declare variable for the C language shared objects 
#

C_LANG_OBJECTS=-llang_c

#
# Declare variable for the Micro Focus language shared objects 
#

MF_LANG_OBJECTS=-llang_mf

#
# Declare variable for the isCOBOL language shared objects
#

ISCOBOL_LANG_OBJECTS=-llang_iscobol

ISCOBOL_LANG_LIB_UNIX=-L$(ISCOBOL)/native/lib -liscobolc_n
ISCOBOL_LANG_LIB_INTERIX=$(ISCOBOL)/native/static/lib/iscobolc_n.lib
ISCOBOL_LANG_LIB=$(ISCOBOL_LANG_LIB_$(PLATFORM))

#
# Declare variable for the vCOBOL language shared objects
#

VCOBOL_LANG_OBJECTS=-llang_vcobol

VCOBOL_LANG_LIB_UNIX=-L$(VCOBOL)/native/lib -lvcobol
VCOBOL_LANG_LIB_INTERIX=$(VCOBOL_DIR)/native/lib/vcobol.lib
VCOBOL_LANG_LIB=$(VCOBOL_LANG_LIB_$(PLATFORM))

#
# Declare variable for .class files required for building
# database support with ISCOBOL
#
DATABASE_CLASSES=
DATABASE_CLEAN=
DATABASE_CLOBBER=

#
# Declare variable for the Oracle archives and objects.
#
# Note that ORACLELIB must be set prior to executing the
# make for Oracle.
#

ORACLEOBJECTS=

ORACLE_LINKS=

#
# Declare variable for the Informix archives and objects.
#
# Note that INFORMIXLIB must be set prior to executing the
# make for Informix.
#

INFORMIXOBJECTS=

INFORMIX_LINKS=

#
# Declare variable for the Sybase archives and objects.
#
# Note that INCDIR, SYBASELIB must be set prior to executing the
# make for Sybase/UniKix transaction processor.

SYBASEOBJECTS=

SYBASE_LINKS=

#
# Declare variable for the DB2 archives and objects.
#
# make for DB26000/UniKix transaction processor.

DB2OBJECTS=

DB2_LINKS=

#
# Declare variable for the DATACOM archives and objects.
#

DATACOMOBJECTS=

DATACOM_LINKS=

#
# Declare variable for the INGRES archives and objects.
#

INGRESOBJECTS=

INGRES_LINKS=

#
# Declare variable for the PL/I archives and objects.
#

PLIOBJECTS=

PLI_LINKS=

#
# Declare variable for the EHLLAPI archives and objects.
#
# Note that EHLLAPILIB must be set prior to executing the
# make for the EHLLAPI server.
#

# EHLLAPIOBJECTS = \
#	${EHLLAPILIB}/ehllapi.o

EHLLAPIOBJECTS=

#
# Declare XA Resource Manager table objects
#

RMSOBJECTS= \
	$(USERXA)/kxglobalrmtable.o

RM_LIBRARY= \
	$(BINDIR)/libkxrm.so

#
# Declare variable for external security archives
#
# Note that UNICENTER_HOME must be set prior to executing the make, whenever
# CA-Unicenter has been selected to be installed with the kixinstall
#  shell script.
#
# no external security, by default

SECURITYOBJECTS= \
	$(USERSECURITY)/kxsec_exits.o

#
# Declare variable for null external security "stub" for batch executables
#
BATCHSECURITYOBJECTS= \
	$(USERSECURITY)/kxsec_exitsNULL.o

#
# Declare variable for the MQSeries libraries and objects.
#

MQSERIESOBJECTS=

MQSERIES_LINKS=


#
# Declare variable for all source modules plus "system" source
# modules, such as this makefile.
#

SOURCES= \
	makefile \
	$(USERTRANS)/kxusrexit.c \
	$(USERTRANS)/kximsexit.c \
	$(USERCONVERT)/kxusrxlt.c \
	$(USERCONVERT)/kxcvtxlt.c \
	$(USERISC)/kxdynrte.c \
	$(USERTERMINAL)/kxtnfxit.c \
	$(USERSOCKET)/kxsktxit.c \
	$(USERCONVERT)/cvlwrupr.c \
	$(USERCONVERT)/kxcnvtbl.c \
	$(USERCONVERT)/kxsyscnvtbl.c \
	$(USERSECURITY)/kxsec_exits.c \
	$(USERRECOVERY)/kxesdsxlt.c \
	$(USERRECORD)/kxread.c \
	$(USERRECORD)/kxwrite.c \
	$(USERRECORD)/linein.c \
	$(USERRECORD)/lineout.c \
	$(USERRECORD)/recordin.c \
	$(USERRECORD)/recordout.c \
	$(USERRECORD)/recordvin.c \
	$(USERRECORD)/recordvout.c \
	$(USERRECORD)/mfrcdin.cbl \
	$(USERRECORD)/mfrcdout.cbl \
	$(USERRECORD)/kxalt008.cbl \
	$(USERRECORD)/kxalt016.cbl \
	$(USERRECORD)/kxalt032.cbl \
	$(USERRECORD)/kxalt064.cbl \
	$(USERRECORD)/kxalt128.cbl \
	$(USERRECORD)/kxalt256.cbl \
	$(USERRECORD)/kxalt516.cbl \
	$(USERSOURCES)

IMSOBJECTS= \
	$(USERTRANS)/kximsexit.o

#
# Declare variable for user object module storage.
# Note CLEAN does not clean User Specific Objects
#

OBJECTS= \
	$(USERTRANS)/kxusrexit.o \
	$(USERCONVERT)/kxusrxlt.o \
	$(USERCONVERT)/kxcvtxlt.o \
	$(USERISC)/kxdrrtns.o \
	$(USERISC)/kxdynrte.o \
	$(USERTERMINAL)/kxtnfxit.o \
	$(USERSOCKET)/kxsktxit.o \
	$(USERCONVERT)/cvlwrupr.o \
	$(USERCONVERT)/kxcnvtbl.o \
	$(USERCONVERT)/kxsyscnvtbl.o \
	$(USEROBJECTS)

EBMKXUSEROBJ= \
	$(USERTRANS)/kxusrexit.o \
	$(USERCONVERT)/cvlwrupr.o \
	$(USERCONVERT)/kxusrxlt.o \
	$(USERCONVERT)/kxcvtxlt.o \
	$(USERCONVERT)/kxcnvtbl.o \
	$(USERCONVERT)/kxsyscnvtbl.o \
	$(USEROBJECTS)

#
# Define variable containing the names of all object modules
# to be used in the lu62 build process.
#

APPCOBJECTS= 

BLDOBJECTS= \
	$(USERCONVERT)/kixmakecnv.o \
	$(USERRECORD)/kxread.o \
	$(USERRECORD)/kxwrite.o \
	$(USERRECORD)/linein.o \
	$(USERRECORD)/lineout.o \
	$(USERRECORD)/recordin.o \
	$(USERRECORD)/recordout.o \
	$(USERRECORD)/recordvin.o \
	$(USERRECORD)/recordvout.o \
	$(USERRECORD)/mfrcdin.i* \
	$(USERRECORD)/mfrcdin.o \
	$(USERRECORD)/mfrcdout.i* \
	$(USERRECORD)/mfrcdout.o \
	$(USERRECORD)/kxalt*.i* \
	$(USERRECORD)/kxalt*.o

# ***********************************************************
# Define objects for building kixfile
# ***********************************************************

KIXFILE_OBJECTS= \
	$(LIBDIR)/kixfile.o \
	$(BATCHSECURITYOBJECTS)

# ***********************************************************
# Define objects for building dfhusdup
# ***********************************************************

DFHUSDUP_OBJECTS= \
	$(LIBDIR)/dfhusdup.o \
	$(BATCHSECURITYOBJECTS)

# ***********************************************************
# Define objects for building kixmakecnv
# ***********************************************************

KIXMAKECNV_OBJECTS= \
	$(USERCONVERT)/kixmakecnv.o \
	$(USERCONVERT)/kxcnvtbl.o \
	$(USERCONVERT)/kxsyscnvtbl.o

# ***********************************************************
# Define objects for building mfrcdin and mfrcdout
# ***********************************************************

MFRCDIN_OBJECTS= \
	$(USERRECORD)/mfrcdin.int \
	$(USERRECORD)/kxwrite.o \
	$(USERRECORD)/kxread.o

MFRCDOUT_OBJECTS= \
	$(USERRECORD)/mfrcdout.int \
	$(USERRECORD)/kxread.o

# ***********************************************************
# Define objects for building sortx
# ***********************************************************

SORTX_OBJECTS= \
	$(LIBDIR)/kixsortx.o \
	$(BATCHSECURITYOBJECTS)

# ***********************************************************
# Define objects for building unikixbld
# ***********************************************************

UNIKIXBLD_OBJECTS= \
	$(LIBDIR)/unikixbld.o \
	$(BATCHSECURITYOBJECTS)

# ***********************************************************
# Define objects for building unikixCEMT
# ***********************************************************

UNIKIXCEMT_OBJECTS= \
	$(LIBDIR)/unikixcemt.o \
	$(SECURITYOBJECTS)

# ***********************************************************
# Define objects for building unikixFMS
# ***********************************************************

UNIKIXFMS_OBJECTS= \
	$(LIBDIR)/unikixfms.o \
	$(SECURITYOBJECTS)

# ***********************************************************
# Define objects for building unikixRED
# ***********************************************************

UNIKIXRED_OBJECTS= \
	$(LIBDIR)/unikixred.o \
	$(SECURITYOBJECTS)

# ***********************************************************
# Define objects for building unikixMNU
# ***********************************************************

UNIKIXMNU_OBJECTS= \
	$(LIBDIR)/unikixmnu.o \
	$(SECURITYOBJECTS)

# ***********************************************************
# Define objects for building unikixrcv
# ***********************************************************

UNIKIXRCV_SOURCES= \
	$(USERRECOVERY)/kxesdsxlt.c

UNIKIXRCV_OBJECTS= \
	$(LIBDIR)/unikixrcv.o \
	$(USERRECOVERY)/kxesdsxlt.o

# ***********************************************************
# Define objects for building unikixstrt
# ***********************************************************

UNIKIXSTRT_OBJECTS= \
	$(LIBDIR)/unikixstrt.o \
	$(SECURITYOBJECTS)

# ***********************************************************
# Define objects for building unikixtran
# ***********************************************************

UNIKIXTRAN_OBJECTS= \
	$(LIBDIR)/unikixtran.o \
	$(IMSOBJECTS) \
	$(OBJECTS) \
	$(TRANSPECOBJECTS) \
	$(APPCOBJECTS) \
	$(CINTOBJECTS) \
	$(SECURITYOBJECTS) \
	$(ORACLEOBJECTS) \
	$(INFORMIXOBJECTS) \
	$(SYBASEOBJECTS) \
	$(DB2OBJECTS) \
	$(DATACOMOBJECTS) \
	$(INGRESOBJECTS) \
	$(MQSERIESOBJECTS) \
	$(PLIOBJECTS) \
	$(RANIMSTUB) \
	$(LIBBASAP)

DUMMY_BRIDGE_OBJECTS= \
	$(LIBDIR)/kxdumbridge.o

# ***********************************************************

EBMRTSOBJECTS= \
	$(EBMKXUSEROBJ) \
	$(VSAMSPECOBJECTS) \
	$(ORACLEOBJECTS) \
	$(INFORMIXOBJECTS) \
	$(SYBASEOBJECTS) \
	$(DB2OBJECTS) \
	$(DATACOMOBJECTS) \
	$(INGRESOBJECTS) \
	$(BATCHSECURITYOBJECTS) \
	$(MQSERIESOBJECTS)

#
# Define link libraries for the various databases.  To be used in
# the process for building unikixtran 
#

DBLINKS=	\
	$(ORACLE_LINKS) \
	$(INFORMIX_LINKS) \
	$(SYBASE_LINKS) \
	$(DB2_LINKS) \
	$(DATACOM_LINKS) \
	$(INGRES_LINKS) \
	$(PLI_LINKS) \
	$(MQSERIES_LINKS)


# ****************************************************************
# Define standard UniKix and user executables that can be rebuilt
# ****************************************************************

INSTMAPS= \
	$(USERTRANS)/CSIGNMP.map

EXECUTABLES= \
	$(BINDIR)/sortx \
	$(BINDIR)/nlsmsg \
	$(BINDIR)/unikixtran \
	$(USEREXECUTABLES)

SECEXECUTABLES= \
	$(BINDIR)/kixfile \
	$(BINDIR)/unikixbld \
	$(BINDIR)/unikixCEMT \
	$(BINDIR)/unikixFMS \
	$(BINDIR)/unikixRED \
	$(BINDIR)/unikixMNU \
	$(BINDIR)/unikixstrt \
	$(BINDIR)/dfhusdup

#
# define some miscellaneous executables that may be 
# created. Used only for the CLOBBER macro
#
MISC_EXECUTABLES= \
	$(BINDIR)/rtsvsam* \
	$(BINDIR)/RUNB \
	$(BINDIR)/mfrcdout_dummy \
	$(BINDIR)/mfrcdin_dummy \
	$(BINDIR)/mfrcdout \
	$(BINDIR)/mfrcdin \
	$(BINDIR)/kxalt*

USEREXECUTABLES= \
	$(BINDIR)/kixmakecnv \
	$(BINDIR)/line.in \
	$(BINDIR)/line.out \
	$(BINDIR)/mfrcd.in \
	$(BINDIR)/mfrcd.out \
	$(BINDIR)/mfrcdin_$(COBOL_EXT) \
	$(BINDIR)/mfrcdout_$(COBOL_EXT) \
	$(BINDIR)/record.in \
	$(BINDIR)/record.out \
	$(BINDIR)/recordv.in \
	$(BINDIR)/recordv.out \
	$(BINDIR)/kxcallkix_$(COBOL_EXT) \
	$(BINDIR)/kxcallbridge_$(COBOL_EXT) \
	$(BINDIR)/libkxrm.so \
	$(USERISCOBOLINT)

COBSOURCES= \
	kxcallkix.cbl.iscobol \
	kxcallkix.cbl.vcobol \
	kxcallkix.cbl.mf \
	kxcallbridge.cbl.iscobol \
	kxcallbridge.cbl.vcobol

COBINT= \
	$(BINDIR)/kxcallkix.int

COBIDY= \
	$(BINDIR)/kxcallkix.idy

COBGNT= \
	$(BINDIR)/kxcallkix.gnt

COBCLASS= \
	$(BINDIR)/KXCALLKIX.class \
	$(BINDIR)/KXCALLBRIDGE.class

# ****************************************************************
# CLEAN will only remove various object files. It will not
# remove user specific objects, USERSPECOBJECTS
# ****************************************************************

CLEAN= \
	$(USERCONVERT)/kxusrxlt.o \
	$(USERCONVERT)/kxcvtxlt.o \
	$(USERCONVERT)/cvlwrupr.o \
	$(USERCONVERT)/kxcnvtbl.o \
	$(USERCONVERT)/kxsyscnvtbl.o \
	$(USERISC)/kxdynrte.o \
	$(USERRECOVERY)/kxesdsxlt.o \
	$(USERSOCKET)/kxsktxit.o \
	$(USERTERMINAL)/kxtnfxit.o \
	$(USERTRANS)/kxusrexit.o \
	$(USERTRANS)/kximsexit.o \
	$(USERTRANS)/CSIGNMP \
	$(USERTRANS)/CSSNCESN.cbl \
	$(USERTRANS)/CSSNCESN.int \
	$(BLDOBJECTS) \
	$(RMSOBJECTS) \
	$(RM_LIBRARY) \
	$(USEROBJECTS) \
	$(COBINT) \
	$(COBIDY) \
	$(COBCLASS) \
	$(COBGNT) \
	$(DATABASE_CLEAN)

# ****************************************************************
# CLOBBER will remove object files AND all the executables
# ****************************************************************

CLOBBER= \
	$(CLEAN) \
	$(EXECUTABLES) \
	$(SECEXECUTABLES) \
	$(INSTMAPS) \
	$(MISC_EXECUTABLES) \
	$(DATABASE_CLOBBER)

# ***********************************************
# standard targets
# ***********************************************

# ********************
#  all target
# ********************

all:	$(EXECUTABLES)

# ********************
#  security target
# ********************

security:	$(SECEXECUTABLES) $(EXECUTABLES)

# ********************
#  clean target
# ********************

clean:
	rm -f $(CLEAN) 

# ********************
#  clobber target
# ********************

clobber:
	rm -f $(CLOBBER)

# ********************
#  kixmakecnv target
# ********************

kixmakecnv:	$(BINDIR)/kixmakecnv
	@echo ""

$(BINDIR)/kixmakecnv:	$(KIXMAKECNV_OBJECTS) makefile
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(KIXMAKECNV_OBJECTS)

# ********************
#  RM table target
# ********************

RMS:	$(BINDIR)/libkxrm.so
	@echo ""
	
$(BINDIR)/libkxrm.so:	$(USERXA)/RM
	rm -f $@ ; \
	( cd $(USERXA) ; \
		rm -f rms.mk kxglobalrmtable.c kxglobalrmtable.o libkxrm.so ; \
		rmutil -f RM -g; \
		$(MAKE) -f rms.mk "CC=$(CC)" "CFLAGS=$(CFLAGS)" "LDFLAGS=$(LDFLAGS)" "LDFLAGS2=$(LDFLAGS2)" )
	cp $(USERXA)/libkxrm.so $@

# ********************
#  mfrcdin targets; cobol and non-cobol
#  cd to $(USERRECORD) required to get all the
#  object files in the right directory
# ********************

mfrcdin:	$(BINDIR)/mfrcdin_$(COBOL_EXT) makefile
	@echo ""

$(BINDIR)/mfrcdin_mfcob:	$(MFRCDIN_OBJECTS) makefile
	rm -f $(BINDIR)/mfrcdin $(USERRECORD)/mfrcdin.o
	( cd $(USERRECORD) ; \
		$(COBOL) -x -o $(BINDIR)/mfrcdin $(MFRCDIN_OBJECTS) )

$(BINDIR)/mfrcdin_iscob:	makefile
	rm -f $(BINDIR)/mfrcdin $(BINDIR)/mfrcdin_dummy
	( cd `dirname $@` ; \
	touch mfrcdin_dummy; \
	ln -s mfrcdin_dummy mfrcdin )

$(BINDIR)/mfrcdin_vcob:        makefile
	rm -f $(BINDIR)/mfrcdin $(BINDIR)/mfrcdin_dummy
	( cd `dirname $@` ; \
	touch mfrcdin_dummy; \
	ln -s mfrcdin_dummy mfrcdin )
	
$(BINDIR)/mfrcdin_nocob:	makefile
	rm -f $(BINDIR)/mfrcdin $(BINDIR)/mfrcdin_dummy
	( cd `dirname $@` ; \
        touch mfrcdin_dummy; \
	ln -s mfrcdin_dummy mfrcdin )

# ********************
#  mfrcdout targets; cobol and non-cobol
#  cd to $(USERRECORD) required to get all the
#  object files in the right directory
# ********************

mfrcdout:	$(BINDIR)/mfrcdout_$(COBOL_EXT) makefile
	@echo ""

$(BINDIR)/mfrcdout_mfcob:	$(MFRCDOUT_OBJECTS) makefile
	rm -f $(BINDIR)/mfrcdout $(USERRECORD)/mfrcdout.o
	( cd $(USERRECORD) ; \
		$(COBOL) -x -o $(BINDIR)/mfrcdout $(MFRCDOUT_OBJECTS) )

$(BINDIR)/mfrcdout_iscob:	makefile
	rm -f $(BINDIR)/mfrcdout $(BINDIR)/mfrcdout_dummy
	( cd `dirname $@` ; \
	touch mfrcdout_dummy; \
	ln -s mfrcdout_dummy mfrcdout )

$(BINDIR)/mfrcdout_vcob:       makefile
	rm -f $(BINDIR)/mfrcdout $(BINDIR)/mfrcdout_dummy
	( cd `dirname $@` ; \
	touch mfrcdout_dummy; \
	ln -s mfrcdout_dummy mfrcdout )

$(BINDIR)/mfrcdout_nocob:	makefile
	rm -f $(BINDIR)/mfrcdout $(BINDIR)/mfrcdout_dummy
	( cd `dirname $@` ; \
        touch mfrcdout_dummy; \
	ln -s mfrcdout_dummy mfrcdout )

# *******************
# kxcallkix targets
# *******************
kxcallkix:	$(BINDIR)/kxcallkix_$(COBOL_EXT) makefile
	@echo ""

$(BINDIR)/kxcallkix_mfcob:	kxcallkix.gnt makefile

$(BINDIR)/kxcallkix_iscob:	$(USERTRANS)/kxcallkix.cbl.iscobol makefile
	rm -f $(BINDIR)/kxcallkix.cbl $(BINDIR)/KXCALLKIX.class
	cp $(USERTRANS)/kxcallkix.cbl.iscobol kxcallkix.cbl
	$(ISCC) $(ISCOBOPT) kxcallkix.cbl
	chmod 755 KXCALLKIX.class
	cp KXCALLKIX.class $(BINDIR)/KXCALLKIX.class

$(BINDIR)/kxcallkix_vcob:	$(USERTRANS)/kxcallkix.cbl.vcobol makefile
	rm -f kxcallkix.cbl KXCALLKIX.class
	cp $(USERTRANS)/kxcallkix.cbl.vcobol kxcallkix.cbl
	$(VCC) $(VCOBOPT) kxcallkix.cbl
	chmod 755 KXCALLKIX.class
	cp KXCALLKIX.class $(BINDIR)/KXCALLKIX.class

$(BINDIR)/kxcallkix_nocob:	makefile
	rm -f $(BINDIR)/kxcallkix $(BINDIR)/kxcallkix_dummy
	( cd `dirname $@` ; \
	touch kxcallkix_dummy; \
	ln -s kxcallkix_dummy kxcallkix )

kxcallkix.gnt: $(BINDIR)/kxcallkix.gnt

$(BINDIR)/kxcallkix.gnt:    $(USERTRANS)/kxcallkix.cbl.mf makefile
	rm -f $@ kxcallkix.cbl kxcallkix.gnt kxcallkix.int kxcallkix.idy
	cp $(USERTRANS)/kxcallkix.cbl.mf kxcallkix.cbl
	$(COBOL) -u kxcallkix.cbl
	cp kxcallkix.gnt $@

# *******************
# kxcallbridge targets
# *******************
kxcallbridge: $(BINDIR)/kxcallbridge_$(COBOL_EXT) makefile
	@echo ""

$(BINDIR)/kxcallbridge_mfcob:	makefile

$(BINDIR)/kxcallbridge_iscob:	KXCALLBRIDGE.class makefile

$(BINDIR)/kxcallbridge_vcob:	KXCALLBRIDGE.class makefile

$(BINDIR)/kxcallbridge_nocob:	makefile
	rm -f $(BINDIR)/kxcallbridge $(BINDIR)/kxcallbridge_dummy
	( cd `dirname $@` ; \
	touch kxcallbridge_dummy; \
	ln -s kxcallbridge_dummy kxcallbridge )

KXCALLBRIDGE.class: $(BINDIR)/KXCALLBRIDGE.class

$(BINDIR)/KXCALLBRIDGE.class:   $(USERTRANS)/kxcallbridge.cbl.$(COBOL_EXT)ol makefile
	rm -f $@ kxcallbridge.cbl KXCALLBRIDGE.class
	cp $(USERTRANS)/kxcallbridge.cbl.$(COBOL_EXT)ol kxcallbridge.cbl
	$(JAVACOBCMD) $(JAVACOBOPT) kxcallbridge.cbl
	chmod 755 KXCALLBRIDGE.class
	cp KXCALLBRIDGE.class $@

# ********************
#  kxalt??? targets
# ********************
kxalt008:	$(BINDIR)/kxalt008 makefile
	@echo ""

$(BINDIR)/kxalt008:	$(USERRECORD)/kxalt008.cbl
	rm -f $(BINDIR)/kxalt008
	( cd $(USERRECORD) ; \
		rm -f kxalt008.int kxalt008.idy kxalt008.o ; \
		$(COBOL) -x -o $(BINDIR)/kxalt008 $(USERRECORD)/kxalt008.cbl )

kxalt016:	$(BINDIR)/kxalt016 makefile
	@echo ""

$(BINDIR)/kxalt016:	$(USERRECORD)/kxalt016.cbl
	rm -f $(BINDIR)/kxalt016
	( cd $(USERRECORD) ; \
		rm -f kxalt016.int kxalt016.idy kxalt016.o ; \
		$(COBOL) -x -o $(BINDIR)/kxalt016 $(USERRECORD)/kxalt016.cbl )

kxalt032:	$(BINDIR)/kxalt032 makefile
	@echo ""

$(BINDIR)/kxalt032:	$(USERRECORD)/kxalt032.cbl
	rm -f $(BINDIR)/kxalt032
	( cd $(USERRECORD) ; \
		rm -f kxalt032.int kxalt032.idy kxalt032.o ; \
		$(COBOL) -x -o $(BINDIR)/kxalt032 $(USERRECORD)/kxalt032.cbl )

kxalt064:	$(BINDIR)/kxalt064 makefile
	@echo ""

$(BINDIR)/kxalt064:	$(USERRECORD)/kxalt064.cbl
	rm -f $(BINDIR)/kxalt064
	( cd $(USERRECORD) ; \
		rm -f kxalt064.int kxalt064.idy kxalt064.o ; \
		$(COBOL) -x -o $(BINDIR)/kxalt064 $(USERRECORD)/kxalt064.cbl )

kxalt128:	$(BINDIR)/kxalt128 makefile
	@echo ""

$(BINDIR)/kxalt128:	$(USERRECORD)/kxalt128.cbl
	rm -f $(BINDIR)/kxalt128
	( cd $(USERRECORD) ; \
		rm -f kxalt128.int kxalt128.idy kxalt128.o ; \
		$(COBOL) -x -o $(BINDIR)/kxalt128 $(USERRECORD)/kxalt128.cbl )

kxalt256:	$(BINDIR)/kxalt256 makefile
	@echo ""

$(BINDIR)/kxalt256:	$(USERRECORD)/kxalt256.cbl
	rm -f $(BINDIR)/kxalt256
	( cd $(USERRECORD) ; \
		rm -f kxalt256.int kxalt256.idy kxalt256.o ; \
		$(COBOL) -x -o $(BINDIR)/kxalt256 $(USERRECORD)/kxalt256.cbl )

kxalt516:	$(BINDIR)/kxalt516 makefile
	@echo ""

$(BINDIR)/kxalt516:	$(USERRECORD)/kxalt516.cbl
	rm -f $(BINDIR)/kxalt516
	( cd $(USERRECORD) ; \
		rm -f kxalt516.int kxalt516.idy kxalt516.o ; \
		$(COBOL) -x -o $(BINDIR)/kxalt516 $(USERRECORD)/kxalt516.cbl )

# ********************
#  line.in target
# ********************

line.in:	$(BINDIR)/line.in makefile
	@echo ""

$(BINDIR)/line.in:	$(USERRECORD)/linein.o
	rm -f $@
	$(CC) $(CFLAGS) -o $@ $?

# ********************
#  line.out target
# ********************

line.out:	$(BINDIR)/line.out makefile
	@echo ""

$(BINDIR)/line.out:	$(USERRECORD)/lineout.o
	rm -f $@
	$(CC) $(CFLAGS) -o $@ $?

# ********************
#  mfrcd.in target
# ********************

mfrcd.in:	$(BINDIR)/mfrcd.in makefile
	@echo ""

$(BINDIR)/mfrcd.in:	$(USERRECORD)/mfrcd.in
	rm -f $@
	$(CP) $? $@
	chmod 755 $@

# ********************
#  mfrcd.out target
# ********************

mfrcd.out:	$(BINDIR)/mfrcd.out makefile
	@echo ""

$(BINDIR)/mfrcd.out:	$(USERRECORD)/mfrcd.out
	rm -f $@
	$(CP) $? $@
	chmod 755 $@

# ********************
#  record.in target
# ********************

record.in:	$(BINDIR)/record.in makefile
	@echo ""

$(BINDIR)/record.in:	$(USERRECORD)/recordin.o
	rm -f $@
	$(CC) $(CFLAGS) -o $@ $?

# ********************
#  record.out target
# ********************

record.out:	$(BINDIR)/record.out makefile
	@echo ""

$(BINDIR)/record.out:	$(USERRECORD)/recordout.o
	rm -f $@
	$(CC) $(CFLAGS) -o $@ $?

# ********************
#  recordv.in target
# ********************

recordv.in:	$(BINDIR)/recordv.in makefile
	@echo ""

$(BINDIR)/recordv.in:	$(USERRECORD)/recordvin.o
	rm -f $@
	$(CC) $(CFLAGS) -o $@ $?

# ********************
#  recordv.out target
# ********************

recordv.out:	$(BINDIR)/recordv.out makefile
	@echo ""

$(BINDIR)/recordv.out:	$(USERRECORD)/recordvout.o
	rm -f $@
	$(CC) $(CFLAGS) -o $@ $?

# ********************
#  kixfile target
# ********************

kixfile:	$(BINDIR)/kixfile
	@echo ""

$(BINDIR)/kixfile:	$(KIXFILE_OBJECTS)
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(KIXFILE_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
#  dfhusdup target
# ********************

dfhusdup:	$(BINDIR)/dfhusdup
	@echo ""

$(BINDIR)/dfhusdup:	$(DFHUSDUP_OBJECTS)
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(DFHUSDUP_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
#  rtsvsam target
# ********************

rtsvsam:	$(BINDIR)/rtsvsam
	@echo ""

$(BINDIR)/rtsvsam:	$(BINDIR)/rtsvsam_$(COBOL_EXT)
	@echo ""

$(BINDIR)/rtsvsam_mfcob:	$(EBMRTSOBJECTS) makefile
	rm -f $(BINDIR)/rtsvsam_*
	rm -f $(BINDIR)/RUNB
	rm -f $(BINDIR)/rtsvsam
	(sh ${UNIKIX}/src/RTSVSAM/inst_rtsvsam "$(MACHINE)" "$(EBM_MF_REL)" \
	"$(EBM_MF_INX)" "$(BINDIR)" "$(EBMRTSOBJECTS)" \
	"$(DBLINKS) $(RS6_SHR_MF) $(RTSVSAM_UNIX_LIBS) $(KXTABLES_LIB)" \
	"$(LINKLIBDIR)" "$(LDEXCI)" "$(RTSVSAM_COB_OPTS)" )

$(BINDIR)/rtsvsam_iscob:        $(EBMRTSOBJECTS) $(DATABASE_CLASSES) makefile
	rm -f $(BINDIR)/rtsvsam_*
	rm -f $(BINDIR)/RUNB
	rm -f $(BINDIR)/rtsvsam
	(sh ${UNIKIX}/src/RTSVSAM/inst_rtsvsam "$(MACHINE)" "$(EBM_MF_REL)" \
	"$(EBM_MF_INX)" "$(BINDIR)" "$(EBMRTSOBJECTS)" \
	"$(DBLINKS) $(RTSVSAM_UNIX_LIBS) $(KXTABLES_LIB)" \
	"$(LINKLIBDIR)" "$(LDEXCI)" )

$(BINDIR)/rtsvsam_vcob:        $(EBMRTSOBJECTS) $(DATABASE_CLASSES) makefile
	rm -f $(BINDIR)/rtsvsam_*
	rm -f $(BINDIR)/RUNB
	rm -f $(BINDIR)/rtsvsam
	(sh ${UNIKIX}/src/RTSVSAM/inst_rtsvsam "$(MACHINE)" "$(EBM_MF_REL)" \
	"$(EBM_MF_INX)" "$(BINDIR)" "$(EBMRTSOBJECTS)" \
	"$(DBLINKS) $(RTSVSAM_UNIX_LIBS) $(KXTABLES_LIB)" \
	"$(LINKLIBDIR)" "$(LDEXCI)" )

$(BINDIR)/rtsvsam_nocob:	makefile $(LIBDIR)/unikixvsam_dummy
	rm -f $(BINDIR)/rtsvsam_*
	rm -f $(BINDIR)/RUNB
	rm -f $(BINDIR)/rtsvsam
	( cd `dirname $@` ; \
        cp $(LIBDIR)/unikixvsam_dummy rtsvsam_dummy; \
	ln -s rtsvsam_dummy rtsvsam; \
	ln -s rtsvsam_dummy RUNB )

# ********************
#  sortx target
# ********************

sortx:	$(BINDIR)/sortx
	@echo ""

$(BINDIR)/sortx:	$(SORTX_OBJECTS) makefile
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(SORTX_OBJECTS) $(SORTX_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

nlsmsg:	$(BINDIR)/nlsmsg
	@echo ""

$(BINDIR)/nlsmsg:	$(LIBDIR)/nlsmsg.txt makefile
	rm -f $@
	gencat $@ $(LIBDIR)/nlsmsg.txt 
	chmod 755 $@

# ********************
#  unikixbld target
# ********************

unikixbld:	$(BINDIR)/unikixbld
	@echo ""

$(BINDIR)/unikixbld:	$(UNIKIXBLD_OBJECTS)
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(UNIKIXBLD_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
#  unikixcemt target
# ********************

unikixcemt:	$(BINDIR)/unikixCEMT
	@echo ""

$(BINDIR)/unikixCEMT:	$(UNIKIXCEMT_OBJECTS)
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(UNIKIXCEMT_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
# unikixfms target
# ********************

unikixfms:	$(BINDIR)/unikixFMS
	@echo ""

$(BINDIR)/unikixFMS:	$(UNIKIXFMS_OBJECTS)
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(UNIKIXFMS_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
#  unikixrcv target
# ********************

unikixrcv:      $(BINDIR)/unikixrcv makefile
	@echo ""

$(BINDIR)/unikixrcv:    $(UNIKIXRCV_OBJECTS) makefile
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(UNIKIXRCV_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
#  unikixred target
# ********************

unikixred:	$(BINDIR)/unikixRED
	@echo ""

$(BINDIR)/unikixRED:	$(UNIKIXRED_OBJECTS)
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(UNIKIXRED_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
#  unikixmnu target
# ********************

unikixmnu:	$(BINDIR)/unikixMNU
	@echo ""

$(BINDIR)/unikixMNU:	$(UNIKIXMNU_OBJECTS)
	rm -f $@
	$(CC) $(LDFLAGS) -o $@ $(UNIKIXMNU_OBJECTS) $(BASE_UNIX_LIBS) $(KXTABLES_LIB)
	chmod 755 $@

# ********************
# unikixstrt target
# ********************

unikixstrt:	$(BINDIR)/unikixstrt
	@echo ""

$(BINDIR)/unikixstrt:	$(UNIKIXSTRT_OBJECTS)
	rm -f $@
	$(CC) $(DL_FLAGS_$(MACHINE)) $(LDFLAGS) -o $@ $(UNIKIXSTRT_OBJECTS) $(BASE_UNIX_LIBS) 
	chmod 755 $@

# ********************
#  unikixtran target
# ********************
# Do the following .tbl link only when PLI is in use

unikixtran:	$(BINDIR)/unikixtran
	@echo ""

$(BINDIR)/unikixtran:	$(BINDIR)/unikixtran_$(COBOL_EXT)
	rm -f $@
	mv $? $@
	if [ $(MACHINE) = HP ] && [ -x /opt/langtools/bin/pxdb ] ; \
	then \
	        /opt/langtools/bin/pxdb -s on $@ ; \
	fi
	chmod 755 $@

#
# The KIXJVMLDFLAGS values must be first on the link line
# as libjsig.so chains the signal handling and so
# must appears before libc, libprthread etc to work
# correctly.
#
$(BINDIR)/unikixtran_mfcob:	$(UNIKIXTRAN_OBJECTS) $(RM_LIBRARY) makefile
	rm -f $@
	$(COBOL) $(COBFLAGS) $(TRAN_COB_OPTS) $(USERSPECOPTIONS) -o $@ \
		$(KIXJVMLDFLAGS) \
		$(LINKLIBDIR) \
		$(TRAN_UNIX_LIBS) \
		$(UNIKIXTRAN_OBJECTS) \
		$(DBLINKS) \
		$(CINTLIBS) \
		$(RS6_SHR_MF) \
		$(TRAN_UNIX_LIBS) \
		$(TRAN_LIBS) \
		$(MF_LANG_OBJECTS)

#
# The KIXJVMLDFLAGS values must be first on the link line
# as libjsig.so chains the signal handling and so
# must appears before libc, libprthread etc to work
# correctly.
#
$(BINDIR)/unikixtran_iscob:     $(UNIKIXTRAN_OBJECTS) $(DATABASE_CLASSES) $(RM_LIBRARY) makefile
	rm -f $@
	$(CC) $(LDFLAGS) $(SPECIALOPTS) $(USERSPECOPTIONS) -o $@ \
		$(KIXJVMLDFLAGS) \
		$(LINKLIBDIR) \
		$(TRAN_UNIX_LIBS) \
		$(UNIKIXTRAN_OBJECTS) \
		$(BRIDGE_LIBS) \
		$(DBLINKS) \
		$(CINTLIBS) \
		$(RS6_SHR_ISCOBOL) \
		$(TRAN_UNIX_LIBS) \
		$(TRAN_LIBS) \
		$(ISCOBOL_LANG_OBJECTS) \
		$(ISCOBOL_LANG_LIB)

$(BINDIR)/unikixtran_vcob:     $(UNIKIXTRAN_OBJECTS) $(DATABASE_CLASSES) $(RM_LIBRARY) makefile
	rm -f $@
	$(CC)  $(LDFLAGS) $(SPECIALOPTS) $(USERSPECOPTIONS) -o $@ \
		$(KIXJVMLDFLAGS) \
		$(LINKLIBDIR) \
		$(TRAN_UNIX_LIBS) \
		$(UNIKIXTRAN_OBJECTS) \
		$(BRIDGE_LIBS) \
		$(DBLINKS) \
		$(CINTLIBS) \
		$(RS6_SHR_ISCOBOL) \
		$(TRAN_UNIX_LIBS) \
		$(TRAN_LIBS) \
		$(VCOBOL_LANG_OBJECTS) \
		$(VCOBOL_LANG_LIB)
#
# The KIXJVMLDFLAGS values must be first on the link line
# as libjsig.so chains the signal handling and so
# must appears before libc, libprthread etc to work
# correctly.
#
$(BINDIR)/unikixtran_nocob:	$(UNIKIXTRAN_OBJECTS) $(RM_LIBRARY) makefile
	rm -f $@
	$(CC) $(LDFLAGS) $(PLILDFLAGS) $(SPECIALOPTS) $(USERSPECOPTIONS) -o $@ \
		$(KIXJVMLDFLAGS) \
		$(LINKLIBDIR) \
		$(TRAN_UNIX_LIBS) \
		$(UNIKIXTRAN_OBJECTS) \
		$(DUMMY_BRIDGE_OBJECTS) \
		$(DBLINKS) \
		$(CINTLIBS) \
		$(RS6_SHR_NC) \
		$(TRAN_UNIX_LIBS) \
		$(TRAN_LIBS) \
		$(C_LANG_OBJECTS)

#
# Targets for the Database classes building for ISCOBOL/VCOBOL
#
# MF: 7001494 vCOBOL produces multiple class files when there
# are "ENTRY" statements in the cobol source, hence the need
# for wildcard characters.
$(BINDIR)/KXISESQL.class: $(USERSRCDIR)/rdbms/KXISESQL.class
	rm -f $(BINDIR)/KXISESQL*.class
	cp $(USERSRCDIR)/rdbms/KXISESQL*.class $(BINDIR)

$(BINDIR)/KXORACLE.class: $(USERSRCDIR)/rdbms/KXORACLE.class
	rm -f $(BINDIR)/KXORACLE*.class
	cp $(USERSRCDIR)/rdbms/KXORACLE*.class $(BINDIR)

$(BINDIR)/KXDB27.class: $(USERSRCDIR)/rdbms/KXDB27.class
	rm -f $(BINDIR)/KXDB27*.class
	cp $(USERSRCDIR)/rdbms/KXDB27*.class $(BINDIR)

$(BINDIR)/KXSYBASE.class: $(USERSRCDIR)/rdbms/KXSYBASE.class
	rm -f $(BINDIR)/KXSYBASE*.class
	cp $(USERSRCDIR)/rdbms/KXSYBASE*.class $(BINDIR)

#**********************************************************************
# ---------------------------------------------------------------------
# The following are the system supplied transactions compile options
# to recompile the system supplied transactions do the following.
# The following are not a part of the default all.
#
# make CSSNCESN
#
# ---------------------------------------------------------------------
#**********************************************************************

# 
# ----------- Pseudo conversational transactions ----------
# 

CSSNCESN: 
	COBCPY=$(USERTRANS)
	rm -f $(USERTRANS)/CSIGNMP
	cd $(USERTRANS); kixbms -c $(USERTRANS)/CSIGNMP.bms
	rm -f $(USERTRANS)/CSSNCESN.cbl
	cd $(USERTRANS); kixclt $(USERTRANS)/CSSNCESN.cl2
	rm -f $(USERTRANS)CSSNCESN.int
	cd $(USERTRANS); $(COBOL) -i $(COBFLAGS) -C $(COBOPT) $(USERTRANS)/CSSNCESN.cbl
