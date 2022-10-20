#**********************************************************************#
#*                                                                    *#
#* Copyright (c) 2016-2020 NTT DATA, Inc.                             *#
#* All rights reserved.                                               *#
#*                                                                    *#
#**********************************************************************#


#* $Date:   17 May 2013 14:40:36  $
#* $Modtime:   17 May 2013 14:40:28  $

# ***********************************************
# $Workfile:   usermake.src  $ $Revision:   1.40  $
# ***********************************************
#
# This makefile should be used to generate all user modifiable
# executable modules.
#
# The makefile may be invoked in the following manner:
#
# make all - Create all user modifiable objects and executables
#
# Individual executables can be also be built, as listed:
#
# make kixmakecnv
#
# make nlsmsg
#
# make rtsvsam
#
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
#----------- RDBMSFLAG = -DORACLE or -DDBTWO -----------
#----------- RDBMSFLAG = -I(mqseries include file path)
RDBMSFLAG= -DVCOBOL 
#------ Set include path for SQLCA ---------------------------
# oracle (pro*cobol 1.3)             INCDIR = $(ORACLE_HOME)/cobol/lib
# oracle (pro*cobol 1.4 and upwards) INCDIR = $(ORACLE_HOME)/procob/lib
INCDIR=
#-------------- for oracle database -----------------------
PCC=
PCCFLAGS=
ORACLELIB=
#

#
#-------------- for EBM Runtime System ------------------------------
EBM_MF_INX=n
EBM_MF_REL=n
LDEXCI=

# ***********************************************
# rules
# ***********************************************


.SUFFIXES:	.c .int .o .gnt .cbl .cl2 .pc .pco .cop .pgm .clt .ppl .sqc .sqb .cp .pli .so .class

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

.pco.gnt: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cob $*.idy $*.int $*.lis $*.gnt ; \
	   $(PCC) $(PCCFLAGS) iname=$*.pco oname=$*.cob include=$(INCDIR) \
	       ireclen=132 mode=ansi release_cursor=yes hold_cursor=no ; \
	   $(COBOL) -u -C $(COBOPT) $*.cob )

.pco.class: makefile
	( cd `dirname $<` ; \
	   rm -f $*.cob $*.class $*.lis $*.err; \
	   $(PCC) $(PCCFLAGS) iname=$*.pco oname=$*.cob include=$(INCDIR) \
	       ireclen=132 mode=ansi release_cursor=yes hold_cursor=no ; \
	   $(JAVACOBCMD) $(JAVACOBOPT) $*.cob; \
	   rm -f $*.cob $*.lis $*.err)

.pgm.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.int ; \
	   $(COBOL) -x -c -C $(COBOPT) -k $*.pgm )

.sqb.o: makefile
	( cd `dirname $<` ; \
		rm -f $*.o $*.cbl ; \
		if [ "$(KIX_DB2USER)" = "" -o "$(KIX_DB2PSWD)" = "" ] ; \
		then \
			db2 connect to $(DB2DBDFT) ; \
		else \
			db2 connect to $(DB2DBDFT) user $(KIX_DB2USER) using $(KIX_DB2PSWD) ; \
		fi ; \
		db2 prep $< bindfile VERSION AUTO target mfcob; \
		db2 bind $*.bnd ; \
		db2 connect reset ; \
		db2 terminate ; \
		COBCPY=$(INSTHOME)/sqllib/include/cobol_mf ;\
		export COBCPY ;\
		$(COBOL) -x -c  -C $(COBOPT) -k $*.cbl )

.sqb.gnt: makefile
	( cd `dirname $<` ; \
		rm -f $*.gnt $*.cbl ; \
		if [ "$(KIX_DB2USER)" = "" -o "$(KIX_DB2PSWD)" = "" ] ; \
		then \
			db2 connect to $(DB2DBDFT) ; \
		else \
			db2 connect to $(DB2DBDFT) user $(KIX_DB2USER) using $(KIX_DB2PSWD) ; \
		fi ; \
		db2 prep $< bindfile VERSION AUTO target mfcob; \
		db2 bind $*.bnd ; \
		db2 connect reset ; \
		db2 terminate ; \
		COBCPY=$(INSTHOME)/sqllib/include/cobol_mf ;\
		export COBCPY ;\
		$(COBOL) -u -C $(COBOPT) -k $*.cbl )

.sqb.class: makefile
	( cd `dirname $<` ; \
		rm -f $*.class $*.cbl ; \
		if [ "$(KIX_DB2USER)" = "" -o "$(KIX_DB2PSWD)" = "" ] ; \
		then \
			db2 connect to $(DB2DBDFT) ; \
		else \
			db2 connect to $(DB2DBDFT) user $(KIX_DB2USER) using $(KIX_DB2PSWD) ; \
		fi ; \
		db2 prep $< bindfile  VERSION AUTO target ansi_cobol; \
		db2 bind $*.bnd ; \
		db2 connect reset ; \
		db2 terminate ; \
		$(JAVACOBCMD) $(JAVACOBOPT) -sp=$(INSTHOME)/sqllib/include/cobol_a:$(INSTHOME)/sqllib/include/cobol_mf:. $*.cbl)

.sqc.o: makefile
	( cd `dirname $<` ; \
	   rm -f $*.o ; \
	   if [ "$(KIX_DB2USER)" = "" -o "$(KIX_DB2PSWD)" = "" ] ; \
	   then \
		db2 connect to $(DB2DBDFT) ; \
	   else \
		db2 connect to $(DB2DBDFT) user $(KIX_DB2USER) using $(KIX_DB2PSWD) ; \
	   fi ; \
	   db2 prep $< ; \
	   db2 terminate ; \
	   $(CC) $(CFLAGS) -c $*.c -I $(INSTHOME)/sqllib/include ; \
	   rm -f $*.c ) 

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

MAKE=/usr/bin/make

#
# Default C compiler location
#

CC=/usr/bin/cc

#
# Default C flag settings (optimize).
#

CFLAGS=-O -m32 -DTPE_EXTERNAL_BUILD -DLINUX -D_LFS64_LARGEFILE=1 -D_LARGEFILE64_SOURCE -D_FILE_OFFSET_BITS=64 -DTERMINFO -DCOBOL_HAS_LONGJMP   -DINTEL

#
# Invoke Linker command.
#

LD=ld

#
# Default Linker flag settings (none).
#

LDFLAGS=-m32 -Wl,-E
LDFLAGS2=-melf_i386
SHARED_LIB_FLAGS=-m32 -shared
SHARED_LIB_FLAGS2=-m32 -shared

#
# Default Linker flag settings RS600(only) shared object export.
#

RS6_SHR_MF=
RS6_SHR_VCOBOL=
RS6_SHR_NC=
RS6_OBJ_BIT=

#
# Linker flag settings for dynamic libraries
#

DL_FLAGS_RS6=-blibpath:/usr/lib:/lib:.

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
# Micro Focus Cobol linker flags.
#

MF_SHARED_LIB_FLAGS=-m32 -shared -L${COBDIR}/lib -lcobscreen -lcobcrtn -lcobmisc -lcobrts
MF_EXP_FLAGS=

#
# Invoke NTT DATA COBOL command. Ensure that we cope with
# paths with spaces in their names.
#

VCOBOL_DIR=`echo "$(VCOBOL)" | sed 's|\(.\):|/dev/fs/\1|g'`
VCC_UNIX=${VCOBOL}/bin/vcc
VCC_INTERIX=${VCOBOL_DIR}/bin/vcc$(E)
VCC="$(VCC_$(PLATFORM))"

#
# setup the correct cobol for is or v Cobol
#

JAVACOBCMD_vcob=$(VCC)
JAVACOBOPT_vcob=$(VCOBOPT)

JAVACOBCMD_MFCOBOL=
JAVACOBOPT_MFCOBOL=

JAVACOBCMD=$(JAVACOBCMD_$(COBOL_EXT))
JAVACOBOPT=$(JAVACOBOPT_$(COBOL_EXT))


#
# Default JVM library/linker flags
#
KIXJVMLDFLAGS_UNIX=-L${KJVDIR} -L${KJSDIR} -ljsig -ljvm
KIXJVMLDFLAGS_INTERIX="${KIX_JDK_ROOT}/lib/jvm.lib"
KIXJVMLDFLAGS=$(KIXJVMLDFLAGS_$(PLATFORM))

BRIDGE_LIBS_UNIX=
BRIDGE_LIBS_INTERIX="${KIX_JDK_ROOT}/lib/jvm.lib" $(USERLIBDIR)/kixuser.lib $(USERLIBDIR)/kixmq.lib $(USERLIBDIR)/Kernel32.Lib
BRIDGE_LIBS=$(BRIDGE_LIBS_$(PLATFORM))

CUA_LIBS_UNIX=$(USERLIBDIR)/libkxCUA.a
CUA_LIBS_INTERIX=$(USERLIBDIR)/kxCUA.lib
CUA_LIBS=$(CUA_LIBS_$(PLATFORM))

CUA_FLAGS_UNIX=
CUA_FLAGS_INTERIX=-R
CUA_FLAGS=$(CUA_FLAGS_$(PLATFORM))

#
# Default Cobol flag settings 
#
COBFLAGS=-I CICS

#
# Default Micro Focus Cobol compile options
#
COBOPT="ibmcomp noalter ans85 notrunc defaultbyte=0 callfh"

#
# Default Cobol compile options for CICS transactions
#
COBOPT1="cics ibmcomp noalter ans85 notrunc defaultbyte=0"

#
# Default NTT DATA COBOL compile options for CICS transactions
#
include ${UNIKIX}/lib/Makefile.include
VCOBOPT_UNIX=${KIX_VCOBOL_ONLINE_FLAGS} -sp=${INCDIR}
VCOBOPT_INTERIX=`echo '${KIX_VCOBOL_ONLINE_FLAGS} "-sp=${INCDIR}"' | sed 's|/dev/fs/\(.\)|\1:|g'`
VCOBOPT=$(VCOBOPT_$(PLATFORM))

#
# Default cobol extension for transaction proc is 'cob'.
# Will be changed by kixinstall to 'nocob' if Shared
# Library makefile environment is chosen
#
COBOL_EXT=vcob

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

BINDIR=/apps/test/mdoff/sys/local/bin

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
# Bugzilla 7004808 (BPE should ship with RTSVSAM objects)
# Bugzilla 7005057 (TPE should not build RTSVSAM objects)
# for rtsvsam_* target declare a variable for the location of RTSVSAM directory (now in BPE)
# $EBMHOME is set/available at execution time of this makefile.

EBMPACKDIR=${EBMHOME}/pack

#
# Declare variable for the location of the src directory
# This directory is the target for the user modifiable source and
# object files.
#

USERSRCDIR=/apps/test/mdoff/sys/local/src

USERRDBMSSRCDIR=

USERCONVERT=$(USERSRCDIR)/convert

USERINSTALL=$(USERSRCDIR)/install

USERISC=$(USERSRCDIR)/isc

USERMF=$(USERSRCDIR)/mf_lang

USERRECORD=$(USERSRCDIR)/record

USERRECOVERY=$(USERSRCDIR)/recovery

USERSECURITY=$(USERSRCDIR)/security

USERSOCKET=$(USERSRCDIR)/socket

USERTERMINAL=$(USERSRCDIR)/terminal

USERTRANS=$(USERSRCDIR)/trans

USERXA=$(USERSRCDIR)/XA

#
#  jvm and jsig library locations
#
KJVDIR=${KIX_JVM_DIR}
KJSDIR=${KJVDIR}/..

#
# during kixinstall if NTT DATA COBOL and any RDBMS is chosen this
# will contain the target .class
#

USERVCOBOLINT=

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
#    TRAN_COB_OPTS is for unikixtran
#    RTSVSAM_COB_OPTS is for rtsvsam

KXTABLES_LIB=-L ${UNIKIX}/lib -lkxtables

BASE_UNIX_LIBS=-ldl -lcrypt -lrt -lpthread

TRAN_UNIX_LIBS=-L ${UNIKIX}/lib -lcrypt -lm -ldl -lkxtables -lkxtm -lkxrm -lrt -lpthread

RTSVSAM_UNIX_LIBS=-lm -lrt -lpthread -ldl -L ${UNIKIX}/lib -lkxtm -lkxrm

SORTX_UNIX_LIBS=-lrt -lpthread -ldl

TRAN_COB_OPTS=-x

RTSVSAM_COB_OPTS=

#
# LIBBASAP will currently only have a value for RS6/AIX machines. The
# value should be the path, ${UNIKIX}/lib/libbasap.a
#
LIBBASAP=


#
# Declare variable for the dynamic language shared object
#

DYN_LANG_OBJECTS=-L$(USERLIBDIR) -llang_dyn

#
# Declare variable for the Micro Focus language shared objects 
#

MF_LANG_OBJECTS=-llang_mf

#
# Declare variable for .class files required for building
# database support with NTT DATA COBOL
#
DATABASE_CLASSES=
DATABASE_CLEAN=
DATABASE_CLOBBER=

#
# Declare variable for .gnt files required for building database
# support with MFCOBOL
#
DATABASE_GNT_FILES=

#
# Declare variable for the Oracle archives and objects.
#
# Note that ORACLELIB must be set prior to executing the
# make for Oracle.
#

ORACLEOBJECTS=

ORACLE_LINKS=

#
# Declare variable for the DB2 archives and objects.
#
# make for DB26000/UniKix transaction processor.

DB2OBJECTS=

DB2_LINKS=

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
# Declare Micro Focus COBOL objects
#

MFCOBOL_OBJECTS= \
	$(USERMF)/kxlangint.o

MFCOBOL_LIBRARY= \
	$(BINDIR)/liblang_mf.so

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
	$(USERTERMINAL)/kxAutoInstTermID_exit.c \
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

TRANSOBJECTS= \
	$(USERTRANS)/kxusrexit.o \
	$(USEROBJECTS)

CONVOBJECTS= \
	$(USERCONVERT)/kxusrxlt.o \
	$(USERCONVERT)/kxcvtxlt.o \
	$(USERCONVERT)/cvlwrupr.o

ISCOBJECTS= \
	$(USERISC)/kxdrrtns.o \
	$(USERISC)/kxdynrte.o

TERMOBJECTS= \
	$(USERTERMINAL)/kxtnfxit.o

AUTOINSTTERMIDOBJ= \
	$(USERTERMINAL)/kxAutoInstTermID_exit.o

SOCKOBJECTS= \
	$(USERSOCKET)/kxsktxit.o

RCVOBJECTS= \
	$(USERRECOVERY)/kxesdsxlt.o

RTSVSAM_LIBS= \
	$(BINDIR)/libkxusrexit.so \
	$(USERLIBDIR)/libkxsecexNULL.so

USREXIT_LIBRARIES= \
	$(BINDIR)/libkxusrexit.so \
	$(BINDIR)/libkxrm.so \
	$(BINDIR)/libkxconvexit.so \
	$(BINDIR)/libkxiscexit.so \
	$(BINDIR)/libkxsockexit.so \
	$(BINDIR)/libkxesdsxit.so \
	$(BINDIR)/libkxtermexit.so \
	$(BINDIR)/libAutoInstTermID.so

MFLANG_LIBRARY= \
	$(BINDIR)/liblang_mf.so

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

DUMMY_BRIDGE_OBJECTS= \
	$(LIBDIR)/kxdumbridge.o

# ***********************************************************

EBMRTSOBJECTS= \
	$(VSAMSPECOBJECTS) \
	$(ORACLEOBJECTS) \
	$(DB2OBJECTS) \
	$(RTSVSAM_LIBS) \
	$(MQSERIESOBJECTS) \
	$(CUA_LIBS)

#
# Define link libraries for the various databases.  To be used in
# the process for building unikixtran 
#

DBLINKS=	\
	$(ORACLE_LINKS) \
	$(DB2_LINKS) \
	$(MQSERIES_LINKS)


# ****************************************************************
# Define standard UniKix and user executables that can be rebuilt
# ****************************************************************

INSTMAPS= \
	$(USERTRANS)/CSIGNMP.map

EXECUTABLES= \
	$(BINDIR)/nlsmsg \
	$(USEREXECUTABLES)

#
# define some miscellaneous executables that may be 
# created. Used only for the CLOBBER macro
#
MISC_EXECUTABLES= \
	$(BINDIR)/rtsvsam* \
	$(BINDIR)/RUNB \
	$(BINDIR)/kxcallbridge_dummy \
	$(BINDIR)/kxcallkix_dummy \
	$(BINDIR)/kxcallbridge \
	$(BINDIR)/kxcallkix \
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
	$(USERVCOBOLINT)

COBSOURCES= \
	kxcallkix.cbl \
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
	$(USREXIT_LIBRARIES) \
	$(MFCOBOL_LIBRARY) \
	$(COBINT) \
	$(COBIDY) \
	$(COBCLASS) \
	$(COBGNT) \
	$(DATABASE_CLEAN) \
	$(DATABASE_GNT_FILES)

# ****************************************************************
# CLOBBER will remove object files AND all the executables
# ****************************************************************

CLOBBER= \
	$(CLEAN) \
	$(EXECUTABLES) \
	$(INSTMAPS) \
	$(MISC_EXECUTABLES) \
	$(USERSRCDIR)/*.class \
	$(USERSRCDIR)/kxcallbridge.* \
	$(USERSRCDIR)/kxcallkix.* \
	$(USERXA)/kxglobalrmtable.c \
	$(USERXA)/libkxrm.so \
	$(USERXA)/rms.mk \
	$(DATABASE_CLOBBER)

# ***********************************************
# standard targets
# ***********************************************

# ********************
#  all target
# ********************

all:	$(EXECUTABLES) libs

# ********************
#  libs target
# ********************

libs:	$(USREXIT_LIBRARIES) $(MFLANG_LIBRARY)

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

# *********************
#  kxmqseries.o target
# *********************

$(USERSRCDIR)/rdbms/kxmqseries.o:
	(cd `dirname $(@D)` ; \
	rm -f $*.o ; \
	$(CC) $(CFLAGS) $(RDBMSFLAG) -w -I ../CICS_structures \
	                             -o $*.o -c $*.c )

# ********************
#  kixmakecnv target
# ********************

kixmakecnv:	$(BINDIR)/kixmakecnv
	@echo ""

$(BINDIR)/kixmakecnv:	$(KIXMAKECNV_OBJECTS) makefile
	rm -f $@
	export OBJECT_MODE=$(RS6_OBJ_BIT) ; \
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
		$(MAKE) -f rms.mk "CC=$(CC)" "CFLAGS=$(CFLAGS)" "SHARED_LIB_FLAGS=$(SHARED_LIB_FLAGS)" "SHARED_LIB_FLAGS2=$(SHARED_LIB_FLAGS2)" )
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

$(BINDIR)/kxcallkix_mfcob:	kxcallkix.gnt makefile $(DATABASE_GNT_FILES)

$(BINDIR)/kxcallkix_vcob:	$(USERTRANS)/kxcallkix.cbl makefile $(DATABASE_CLASSES)
	rm -f kxcallkix.cbl KXCALLKIX.class
	cp $(USERTRANS)/kxcallkix.cbl kxcallkix.cbl
	$(VCC) $(VCOBOPT) kxcallkix.cbl
	chmod 755 KXCALLKIX.class
	cp KXCALLKIX.class $(BINDIR)/KXCALLKIX.class

$(BINDIR)/kxcallkix_nocob:	makefile
	rm -f $(BINDIR)/kxcallkix $(BINDIR)/kxcallkix_dummy
	( cd `dirname $@` ; \
	touch kxcallkix_dummy; \
	ln -s kxcallkix_dummy kxcallkix )

kxcallkix.gnt: $(BINDIR)/kxcallkix.gnt

$(BINDIR)/kxcallkix.gnt:    $(USERTRANS)/kxcallkix.cbl makefile
	rm -f $@ kxcallkix.cbl kxcallkix.gnt kxcallkix.int kxcallkix.idy
	cp $(USERTRANS)/kxcallkix.cbl kxcallkix.cbl
	$(COBOL) -u kxcallkix.cbl
	cp kxcallkix.gnt $@

# *******************
# kxcallbridge targets
# *******************
kxcallbridge: $(BINDIR)/kxcallbridge_$(COBOL_EXT) makefile
	@echo ""

$(BINDIR)/kxcallbridge_mfcob:	makefile

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
#  rtsvsam target
# ********************

rtsvsam:	$(BINDIR)/rtsvsam
	@echo ""

$(BINDIR)/rtsvsam:	$(BINDIR)/rtsvsam_$(COBOL_EXT)
	@echo ""

$(BINDIR)/rtsvsam_mfcob:	$(EBMRTSOBJECTS) $(DATABASE_GNT_FILES) makefile
	rm -f $(BINDIR)/rtsvsam_*
	rm -f $(BINDIR)/RUNB
	rm -f $(BINDIR)/rtsvsam
	(sh ${EBMPACKDIR}/RTSVSAM/inst_rtsvsam "$(MACHINE)" "$(EBM_MF_REL)" \
	"$(EBM_MF_INX)" "$(BINDIR)" "$(EBMRTSOBJECTS)" \
	"$(RTSVSAM_UNIX_LIBS) $(DBLINKS) $(RS6_SHR_MF) $(KXTABLES_LIB)" \
	"$(LINKLIBDIR)" "$(LDEXCI)" "$(RTSVSAM_COB_OPTS)" "$(LDFLAGS2)" )

$(BINDIR)/rtsvsam_vcob:        $(EBMRTSOBJECTS) $(DATABASE_CLASSES) makefile
	rm -f $(BINDIR)/rtsvsam_*
	rm -f $(BINDIR)/RUNB
	rm -f $(BINDIR)/rtsvsam
	(sh ${EBMPACKDIR}/RTSVSAM/inst_rtsvsam "$(MACHINE)" "$(EBM_MF_REL)" \
	"$(EBM_MF_INX)" "$(BINDIR)" "$(EBMRTSOBJECTS)" \
	"$(RTSVSAM_UNIX_LIBS) $(DBLINKS) $(KXTABLES_LIB)" \
	"$(LINKLIBDIR)" "$(LDEXCI)" "" "$(LDFLAGS2)" )

$(BINDIR)/rtsvsam_nocob:	makefile $(LIBDIR)/unikixvsam_dummy
	rm -f $(BINDIR)/rtsvsam_*
	rm -f $(BINDIR)/RUNB
	rm -f $(BINDIR)/rtsvsam
	( cd `dirname $@` ; \
        cp $(LIBDIR)/unikixvsam_dummy rtsvsam_dummy; \
	ln -s rtsvsam_dummy rtsvsam; \
	ln -s rtsvsam_dummy RUNB )

nlsmsg:	$(BINDIR)/nlsmsg
	@echo ""

$(BINDIR)/nlsmsg:	$(LIBDIR)/nlsmsg.txt makefile
	rm -f $@
	gencat $@ $(LIBDIR)/nlsmsg.txt 
	chmod 755 $@

# ********************
#  libs target
# ********************
$(BINDIR)/libkxusrexit.so: $(TRANSOBJECTS) $(IMSOBJECTS) $(TRANSPECOBJECTS)
	rm -f $@
	if [ $(MACHINE) = RS6 ] ; \
	  then \
	    $(CC) $(SPECIALOPTS) $(USERSPECOPTIONS) $(CUA_FLAGS) \
	        $(SHARED_LIB_FLAGS2) -o $@ \
		$(TRANSOBJECTS) \
		$(IMSOBJECTS) \
	        $(DYN_LANG_OBJECTS) \
		$(DBLINKS) \
		$(TRANSPECOBJECTS); \
	else \
	    $(CC) $(SPECIALOPTS) $(USERSPECOPTIONS) $(CUA_FLAGS) \
	        $(SHARED_LIB_FLAGS) -o $@ \
	        $(TRANSOBJECTS) \
	        $(IMSOBJECTS) \
	        $(DYN_LANG_OBJECTS) \
		$(DBLINKS) \
	        $(TRANSPECOBJECTS); \
	fi

$(BINDIR)/libkxconvexit.so: $(CONVOBJECTS)
	rm -f $@
	$(CC) $(CUA_FLAGS) $(SHARED_LIB_FLAGS2) -o $@ \
	$(CONVOBJECTS)

$(BINDIR)/libkxiscexit.so: $(ISCOBJECTS)
	rm -f $@
	$(CC) $(CUA_FLAGS) $(SHARED_LIB_FLAGS2) -o $@ \
	$(ISCOBJECTS)

$(BINDIR)/libkxsockexit.so: $(SOCKOBJECTS)
	rm -f $@
	$(CC) $(CUA_FLAGS) $(SHARED_LIB_FLAGS2) -o $@ \
	$(SOCKOBJECTS)

$(BINDIR)/libkxtermexit.so: $(TERMOBJECTS)
	rm -f $@
	$(CC) $(CUA_FLAGS) $(SHARED_LIB_FLAGS2) -o $@ \
	$(TERMOBJECTS)

$(BINDIR)/libAutoInstTermID.so: $(AUTOINSTTERMIDOBJ)
	rm -f $@
	$(CC) $(CUA_FLAGS) $(SHARED_LIB_FLAGS2) -o $@ \
	$(AUTOINSTTERMIDOBJ)

$(BINDIR)/libkxesdsxit.so: $(RCVOBJECTS)
	rm -f $@
	$(CC) $(CUA_FLAGS) $(SHARED_LIB_FLAGS2) -o $@ \
	      $(RCVOBJECTS)
 
# ********************
#  Micro Focus shared object library target
# ********************

liblang_mf.so: $(MFCOBOL_LIBRARY)

# note that we don't 'depend' on MFCOBOL_OBJECTS because it may not be there
$(BINDIR)/liblang_mf.so: makefile
	rm -f $@
	if [ "$(KIXCOBOL)" = "microfocus" ] ; \
	  then \
	    $(CC) $(MF_SHARED_LIB_FLAGS) $(MF_EXP_FLAGS) -o $@ \
	    $(MFCOBOL_OBJECTS); \
	fi

#
# Targets for the Database classes building for NTT DATA COBOL
#
# MF: 7001494 NTT DATA COBOL produces multiple class files when there
# are "ENTRY" statements in the cobol source, hence the need
# for wildcard characters.
$(BINDIR)/KXESQL.class: $(USERSRCDIR)/rdbms/KXESQL.class
	rm -f $(BINDIR)/KXESQL*.class
	cp $(USERSRCDIR)/rdbms/KXESQL*.class $(BINDIR)

$(BINDIR)/KXORACLE.class: $(USERSRCDIR)/rdbms/KXORACLE.class
	rm -f $(BINDIR)/KXORACLE*.class
	cp $(USERSRCDIR)/rdbms/KXORACLE*.class $(BINDIR)

$(BINDIR)/KXDB27.class: $(USERSRCDIR)/rdbms/KXDB27.class
	rm -f $(BINDIR)/KXDB27*.class
	cp $(USERSRCDIR)/rdbms/KXDB27*.class $(BINDIR)

$(BINDIR)/KXDB2DOXA.class: $(USERSRCDIR)/rdbms/KXDB2DOXA.class
	rm -f $(BINDIR)/KXDB2DOXA*.class
	cp $(USERSRCDIR)/rdbms/KXDB2DOXA*.class $(BINDIR)

#
# Targets for the Database generated code (.gnt) building for MFCOBOL
#
$(BINDIR)/KXORACLE.gnt: $(USERSRCDIR)/rdbms/KXORACLE.gnt
	rm -f $(BINDIR)/KXORACLE*.gnt
	cp $(USERSRCDIR)/rdbms/KXORACLE*.gnt $(BINDIR)

$(BINDIR)/KXDB27.gnt: $(USERSRCDIR)/rdbms/KXDB27.gnt
	rm -f $(BINDIR)/KXDB27*.gnt
	cp $(USERSRCDIR)/rdbms/KXDB27*.gnt $(BINDIR)

$(BINDIR)/KXDB2DOXA.gnt: $(USERSRCDIR)/rdbms/KXDB2DOXA.gnt
	rm -f $(BINDIR)/KXDB2DOXA.gnt
	cp $(USERSRCDIR)/rdbms/KXDB2DOXA.gnt $(BINDIR)

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
