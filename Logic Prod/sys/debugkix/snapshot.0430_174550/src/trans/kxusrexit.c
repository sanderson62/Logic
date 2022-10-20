/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   11 Apr 2011 11:11:24  $ */
/* $Modtime:   11 Apr 2011 11:16:38  $ */

#ifndef	lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   kxusrexit.c  $ $Revision:   1.9  $";
#endif

/*
 * $Log:   /home/dd134127/unikix/TPE11.3.1/unikixsrc/user/trans/PVCS/kxusrexit.c_v  $
 * 
 *    Rev 1.9   11 Apr 2011 11:11:24   dd134127
 * Merge 1.3.1.2 and 1.8
 * 
 *    Rev 1.8   11 Jun 2010 15:29:06   dd134127
 * Merge 1.6.2.2 and 1.7
 * 
 *    Rev 1.7   14 Oct 2009 09:22:38   dd134127
 * B7001341
 * Remove code that treated xexit_state as a pointer on WINTEL.
 * 
 *    Rev 1.6.2.2   10 May 2010 14:52:16   dd134127
 * Merge 1.6.1.0 and 1.6.2.1
 * 
 *    Rev 1.6.2.1   05 May 2010 13:16:48   ss134151
 * D7001566
 * EIP rework; redid kxtsq_usrexit parameter to remove TCTUA, added example
 *  kxget_tctua() calling sequence to get local memory copy of TCTUA (safer!)
 * 
 *    Rev 1.6.2.0   29 Apr 2010 11:56:00   ss134151
 * D7001566
 * Add kxtsq_usrexit; called at the start of each TSQ command request
 * 
 *    Rev 1.6.1.0   19 Feb 2010 15:24:48   mf134128
 * b7001494
 * vCOBOL integration
 * 
 *    Rev 1.6   21 Jul 2009 08:07:32   dd134127
 * Merge 1.3.1.1 and 1.5
 * 
 *    Rev 1.5   05 Nov 2008 12:16:58   dd134127
 * Merge 1.2.1.2 and 1.4
 * 
 *    Rev 1.4   25 Sep 2008 09:46:58   dd134127
 * Merge 1.2.1.1 and 1.3
 * 
 *    Rev 1.3.1.2   21 May 2010 06:12:14   sr104371
 * B7001418
 * Provide database exit for post xa_open() processing in DB2 XA environment
 * 
 *    Rev 1.3.1.1   14 May 2009 15:35:48   mf134128
 * b7001196
 * EIP rework
 * 
 *    Rev 1.3   03 Jun 2008 10:07:38   fv134158
 * B7000673
 * Chnaged DllExport int to static int so thing would compile on linux and z/linux
 * for db2 and sybase. Oracle and MQ had already been changed.
 * 
 *    Rev 1.2.1.2   07 Oct 2008 07:37:08   sm133957
 * D000842
 * ISESQL support
 * 
 *    Rev 1.2.1.1   05 Aug 2008 04:57:26   sm133957
 * D000796
 * Allow DB2 and Sybase to be used with isCOBOL
 * Also, remove some old Win32 fluff
 * 
 *    Rev 1.2.1.0   07 Jul 2008 03:10:24   sm133957
 * Add ISCOBOL support
 * 
 *    Rev 1.2   17 Jan 2008 15:17:58   fv134158
 * D7000540
 * Made porting changes to support Oracle 11g on Linux
 * 
 *    Rev 1.1   14 Nov 2007 14:05:04   dd134127
 * PORTS
 * Changes to get MQ to build on Linux.
 * 
 *    Rev 1.0   31 May 2007 13:40:10   unikix
 * Initial TPE11.0.0a
 * 
 *    Rev 1.10   24 May 2006 13:30:36   dd134127
 * Merge 1.8.1.0 and 1.9
 * 
 *    Rev 1.9   18 Oct 2005 10:36:16   dd134127
 * D6335989
 * Clean up warning messages during kixinstall with Oracle.
 * 
 *    Rev 1.8.1.0   20 Feb 2006 04:23:46   dv133961
 * B6332387
 * Warning messages during kixinstall with DB2 (with Cobol user exits)
 * 
 *    Rev 1.8   13 Apr 2005 10:00:44   rh134138
 * Bug# 6245518 warning message for kxusrexit.c during 
 *              kixinstall with MF Cobol
 * 
 *    Rev 1.7   04 Apr 2005 14:16:08   dd134127
 * Merge 1.1.1.0 and 1.6
 * 
 *    Rev 1.6   30 Mar 2005 05:48:56   ks134152
 * 6246569
 * used constants
 * 
 *    Rev 1.5   14 Mar 2005 13:25:58   ks134152
 * 6237592
 * Changed the code so that only for ACUCOBOL kxcobload is called. For others
 * KXOR* routines are called.
 * Included the increased number of arugments to kxcobload.
 * 6th arugument to kxcoblaod is set to 2 so that error message
 * dis displayed in the err file if KXORACLE.acu is not set in
 * CODE_PREFIX
 * D
 * 
 *    Rev 1.4   13 Jan 2005 11:49:58   ks134152
 * 6206896
 * Changed the call to KXORA* use kxcobload.
 * 
 *    Rev 1.3   12 Dec 2004 20:28:34   ks134152
 * 6206089
 * removed the extern definition of CBL_DEBUGBREAK
 * 
 *    Rev 1.2   08 Dec 2004 18:53:10   ks134152
 * 6204089
 * replaced CBL_DEBUGBREAK call with kxcobol_debug 
 * 
 *    Rev 1.1.1.0   01 Feb 2005 13:46:16   jt134157
 * B6217302
 * Ensure MQ resources are closed at TX termination.
 * 
 *    Rev 1.1   26 Jan 2004 12:54:14   dd134127
 * AIX port: Call waitpid correctly on AIX platform.
 * 
 *    Rev 1.0   17 Nov 2003 13:51:22   unikix
 * New 8.0 archive
 * 
 *    Rev 1.8   21 Oct 2003 10:01:24   dd134127
 * Merge 1.6.1.0 and 1.7
 * 
 *    Rev 1.7   15 Oct 2003 09:43:18   dv133961
 * B4825517
 * Warning error messages during kixinstall
 * 
 *    Rev 1.6.1.0   15 Oct 2003 17:22:16   gr134150
 * B4934678
 * changed the way we invoked kxchgdbg: replaced system with fork/exec. In this way kxchg_dbg is a direct child process
 * of unikixtran and can invoke pmfctl using getppid() rather than receiving the unikixtran pid as input.
 * This prevent some hacker to use kxchg_dbg (which runs as root) for unauthorized PMF monitoring suspension of
 * any running process in the system. Now kxchg_dbg attempts to suspend the invoking father.
 * 
 *    Rev 1.6   05 Aug 2003 17:13:56   rh134138
 * BugTraq# 4895329 Unable to debug transaction while running in a
 *                  Sun Cluster environment
 * 
 *    Rev 1.5   03 Apr 2003 16:26:48   pd134126
 * B4835928
 * MBM Batch Job under MTP did not roll back RDBMS after COBOL ABEND
 * In effect removed fix 2410, that was placed for WINNT
 * 
 *    Rev 1.4   06 Sep 2002 12:34:28   dd134127
 * Merge 1.0.1.0 and 1.3
 * 
 *    Rev 1.3   26 Aug 2002 14:46:10   mf134128
 * b4713042
 * Problem with kixinstall.
 * 
 *    Rev 1.2   23 Aug 2002 14:10:38   mf134128
 * 
 * 
 * 
 *    Rev 1.1   09 Aug 2002 13:51:48   mf134128
 * b4713042
 * Localize messages, remove hard coded print strings.
 * 
 *    Rev 1.0.1.0   26 Aug 2002 18:22:24   ks134152
 * 4700502
 * Added support for MQSERIES
 * 
 *    Rev 1.0   19 Sep 2001 16:47:56   unikix
 * Initial 7.2
 * 
 *    Rev 1.4   05 Apr 2001 15:24:42   daved
 * Correct misspellings.
 * 
 *    Rev 1.3   19 May 2000 08:44:30   daved
 * Remove references to UniKix.
 * 
 *    Rev 1.2   14 Apr 2000 15:31:38   daved
 * Merge 1.0.1.0 and 1.1
 * 
 *    Rev 1.1   07 Apr 2000 10:47:42   rick
 * B006314
 * Add function that will invoke Animator in the ServerExpress environment
 * by calling the CBL_DEBUGBREAK function. This function called from the
 * functions in kxsvcobws.c
 * 
 *    Rev 1.0.1.0   10 Apr 2000 18:57:26   unikix
 * Changes for NT platform and Nutcracker 4.2a
 * 
 *    Rev 1.0   30 Jan 2000 11:48:02   unikix
 * Initial 7.0
 * 
 *    Rev 1.1   19 May 1999 15:56:02   rick
 * B003858
 * For WINTEL, remove the rdbms bridge interface. No longer needed. Also, 
 * corrected error in parameter name and order for one the bridge
 * function declarations.
 * 
 *    Rev 1.0   15 Mar 1999 20:00:06   unikix
 * Initial 6.0
 * 
 *    Rev 1.9   17 Feb 1999 09:33:02   daved
 * Merge 1.7.1.0 and 1.8
 * 
 *    Rev 1.8   12 Feb 1999 12:05:00   prabha
 * D002946
 * Bypass user exits when database made with RDBMS, so that system
 * transactions etc. can work
 * 
 *    Rev 1.7.1.0   17 Feb 1999 07:45:50   prabha
 * D003662
 * Changes to support rcode for userexits written in 'C'
 * 
 *    Rev 1.7   10/14/97 11:20:50   prabha
 * D002388
 * 1. Changes for supporting MS SQL Server
 * 2. Changes for supporting RDBMS DLL's, exporting usrexit functions
 * 
 *    Rev 1.6   09/26/97 14:23:10   daved
 * Oops, restore null_function to the way it was in rev 1.4.
 * 
 *    Rev 1.5   09/26/97 13:48:12   daved
 * Change "extern char *xmyname" to "extern char xmyname[]" and fix compiler
 * warnings.
 * 
 *    Rev 1.4   09/25/97 10:02:22   daved
 * Merge 1.2.1.0 and 1.3
 * 
 *    Rev 1.3   09/22/97 11:23:08   rick
 * B002346
 * found during 2346 testing....remove some C++ type comments. Gives 
 * warnings on AIX.
 * 
 *    Rev 1.2.1.0   09/24/97 16:57:04   prabha
 * D002410
 * xmyname added as an extern for check in user_exit routines.  If this
 * not unikixtran or unikixvsam, return from the user_exit routine
 * without continuing any processing.
 * 
 *    Rev 1.2   09/03/97 14:04:54   kellyc
 * Nutcracker:WindowsNT Port
 * Removed DllExport before static functions.
 * 
 *    Rev 1.0   05/22/97 17:34:40   unikix
 * Initial 5.1
 * 
 *    Rev 1.3   04/17/97 08:12:04   daved
 * Merge 1.1.1.1 and 1.2
 * 
 *    Rev 1.2   08/23/96 15:04:00   nialb
 * T001697
 * case #1697
 * added user exit for end of batch script
 * 
 *    Rev 1.1.1.1   02/12/97 14:30:50   prabha
 * B001928
 * Changes to support Sybase 11 with HP-Cobol 3.2 onwards
 * 
 *    Rev 1.1.1.0   10/04/96 11:47:16   ald
 * DEXMEM
 * Added function kxCheckMemory to detect local and shared memory corruption.
 * 
 *    Rev 1.1   07/25/96 15:18:08   rick
 * D001620
 * Add code to check for RDBMS authorization.
 * 
 *    Rev 1.0   04/10/96 14:13:42   unikix
 * Initial 5.0
 * 
 *    Rev 1.9   03/15/96 16:39:14   rick
 * merge 1.5.1.1 - actually 1.5.1.1 had changes up to rev 1.8 so this
 * rev( 1.5.1.1 ) just overlaid 1.8
 * 
 *    Rev 1.8   03/07/96 13:50:20   rick
 * Case 1284 - added missing parameter to function declaration
 * 
 *    Rev 1.7   03/06/96 14:26:22   rick
 * merge 1.5.1.0
 * 
 *    Rev 1.6   02/21/96 12:49:30   daved
 * Changes for Stratus platform.
 * 
 *    Rev 1.5.1.1   03/15/96 15:25:48   prabha
 * DINGRES
 * Support for CA-OpenIngres
 * 
 *    Rev 1.5.1.0   02/28/96 11:09:42   prabha
 * D001284
 * Support for CA-DATACOM
 * Also changes for handling SIGTERM during deallocate
 * 
 *    Rev 1.5   07/31/95 14:07:06   prabha
 * D000996
 * Changed ifndef definition to include correctly for errno.h
 * 
 *    Rev 1.4   06/26/95 14:01:10   prabha
 * Support for RS6v41 and cobinit check for RS6000
 * 
 *    Rev 1.3   03/01/95 16:17:58   som
 * Add #defines for non-4K VSAM blocksize systems.
 * 
 *    Rev 1.2   02/27/95 16:57:42   som
 * Changes for ORACLE-XA 2 phase commit implementation
 * 
 *    Rev 1.1   02/21/95 09:20:20   prabha
 * Support for DB2/6000 with UniKix
 * 
 *    Rev 1.0   12/28/94 15:18:28   unikix
 * Initial revision (from V410ad)
 * 
 *    Rev 1.1.1.6   09/27/94 12:01:56   alan
 * Merge 1.1.1.3.1.0
 * 
 *    Rev 1.1.1.5   09/23/94 12:33:40   randyh
 * Correct an Ifdef/endif problem
 * 
 *    Rev 1.1.1.4   09/16/94 17:00:04   randyh
 * Move changes from rev 1.5 to V410u
 * 
 *    Rev 1.1.1.3.1.0   09/26/94 13:58:24   prabha
 * D000509
 *  Update sybase allocate to support SYBASE10.  Note this update will change
 * the Process group id on AIX machines.
 * 
 *    Rev 1.1.1.3   08/12/94 07:35:52   prabha
 * Correct sqlca variable assignment for 4.9.2 and higher release
 * 
 *    Rev 1.1.1.2   06/21/94 08:45:58   prabha
 * Add support for 4.9.2 SYBASE and SYBASE10
 * 
 *    Rev 1.1.1.1   02/14/94 10:37:34   unikix
 * Merge 4.0 changes
 * 
 *    Rev 1.1.1.0   11/18/93 16:34:50   unikix
 * UniKix 4.1 baseline (from V400m)
 * 
 *    Rev 1.2   01/25/94 15:19:34   prabha
 * Defined TRACEMSG variable for conditional msg.
 * Fixed start_tran
 * 
 *    Rev 1.5   09/12/94 08:03:56   alan
 * Merge 1.3.1.0
 * 
 *    Rev 1.4   08/30/94 15:53:18   som
 * Add new userexits for entry and exit at kxdfhei1(...)
 * 
 *    Rev 1.3.1.0   09/09/94 14:35:08   prabha
 * Correct sqlca variable assignment for 4.9.2 and higher release
 * 
 *    Rev 1.3   05/27/94 14:06:50   prabha
 * Add support for 4.9.2 SYBASE and SYBASE10
 * 
 *    Rev 1.1   08/13/93 12:47:26   prabha
 * Added error routines, conditional processing, SYBASE userexits
 * Deleted, check for application id before calling function.
 * 
 *    Rev 1.0   06/10/93 11:58:54   unikix
 * UniKix 3.1.2 version
 * 
 *    Rev 1.0   05/21/93 16:14:20   unikix
 * UniKix 3.1.2 version
 */

/************************************************************************/
/*    User exit module to accommodate external access methods such      */
/*    as Oracle, Informix, etc...                                       */
/*    User exit calls currently supported are:                          */
/*                                                                      */
/*      1. Allocate/open    - called when txn processor is initiated    */
/*      2. Deallocate/close - called when a txn processor is terminated */
/*      3. Start of txn     - called when a transaction begins; 4 char  */
/*                            txn id is passed to the function          */
/*      4. End of txn       - called when a transaction completes; 4    */
/*                            char txn id is passed to function         */
/*      5. Commit           - called when syncpoint is requested        */
/*      6. Rollback         - called when work is to be canceled        */

/*    To code a user exit, the following are required:                  */

/*    Once the user exit functions are coded, both the                  */
/*    transaction processor and batch vsam module need to be rebuilt    */
/*    for the changes to take effect. There is a 'makefile' under the   */
/*    'src' catalog which can be used to rebuild these processors       */
/************************************************************************/

#include <stdio.h>
#include <signal.h>  /* Case # 1284 */
#include <string.h>
#include <stdlib.h>             /*defines getenv, malloc*/
#include <errno.h>
#include <unistd.h>

#if defined(ISCOBOL) || defined(VCOBOL)
#define USE_KXCOBLOAD
#endif

/*----------------------------------------------------------------------*/
/* RS6000 and SYBASE10 specific for changing process group id           */
/* Do not change any part of this code                                  */
/*----------------------------------------------------------------------*/
#ifdef SYBASE10
#ifdef RS6000
#include <sys/types.h>
#if RS6000 < 41
#include <errno.h>
#endif  /*  RS6000 < 41 condition */
#endif  /*  RS6000 condition */
#endif  /*  SYBASE10 condition */
/* #include "kxgetmem.h" */  /* bogus STRATUS change */
#include <sys/wait.h>
#include "cics_eib.h"
/*----------------------------------------------------------------------*/

extern int kxcobol_debug();
extern void kxprtf();
extern void kxerror1();
extern void kxerror2();
extern void kxerror2str();
extern void kxerrorstr();
extern char *kxerrorprt();
extern int kxusr_chkerr();
extern char *kxget_tctua();

/* Bug 4825517 */
extern int kxdbcheck();
extern void kxerror();

/* Local definitions here */
#define E_PRINT		1
#define DONT_SUPPRESS_COMMAND 0
#define SUPPRESS_COMMAND 1

/* Local errno defined for RDBMS */

#define E_964		964	 /*  RDBMS not licensed */
#define E_1001		1001     /*  Allocate     */
#define E_1002		1002     /*  DeAllocate   */
#define E_1003		1003     /*  Start Txn    */
#define E_1004 		1004     /*  End Txn      */
#define E_1005		1005     /*  Commit Txn   */
#define E_1006		1006     /*  Rollback Txn */
#define E_1097		1097     /*  KIXRDBMS_BYPASS message     */
#define E_1099		1099     /*  Generic errno for databases */
#define E_1649          1649     /*  $KIXSYS not set */
#define E_2206          2206     /*  malloc error */
#define E_6711          6711     /*  recovery file failed */

/* Local errno defined for Sun Cluster UniKix HA PMF error */
#define E_5100          5100     /* SunCluster PMF error */
#define E_5101          5101     /* SunCluster PMF error */
#define E_5104          5104     /* SunCluster PMF error */
#define E_5105          5105     /* SunCluster PMF error */
#define E_5106          5106     /* SunCluster PMF error */
#define E_5107          5107     /* SunCluster PMF error */
#define E_5108          5108     /* SunCluster PMF error */

/* the following defines are from comlang.h */

#define KXDEBUG_NONE            0
#define KXDEBUG_TERMINAL        1
#define KXDEBUG_XTERM           2
#define LOGERR                  2

/************************************************************************/
/* To enable TRACEMSG after every transaction                           */
/* uncomment the following  code                                        */
/************************************************************************/
/* #define TRACEMSG 1                                                   */

/************************************************************************/
/*  XA implementation needed flags					*/
/************************************************************************/
char recvsamarea[8];
char vsmflagr;
char vsmflagw;

extern int xuanum;
extern char xmyname[];
/* structures needed for localization */

struct errorbuf {
    int         eb_kxerrno;             /* UniKix error number */
unsigned char   eb_severity;            /* 0 = informational
                                           1 = warning
                                           2 = transaction error
                                           3 = system error
                                           4 = fatal to process */
    char        eb_filler;              /* 0x0001 = printed
                                           0x0002 = displayed
                                           0x0004 = fatal error has occurred */
    int         eb_errno;               /* Unix errno */
    int         eb_errcode[3];          /* data dependent on type of error */
    int         eb_count;               /* error counter */
    char        eb_kxroutine[28];       /* UniKix routine detecting error */
    char        eb_unixfunc[14];        /* Unix routine */
    char        eb_data[200];           /* character data dependent on type
                                           of error; e.g. file name */
    char        eb_data2[200];
    char        eb_data3[200];
    } ;

extern struct errorbuf xerrorbuf;
extern struct kxsm0 *xsm0;
extern struct kxgta *xgta;
extern struct gva *xgva;
extern struct nuctadef *xnuc;
extern struct users *xusr;
extern struct pid_tbl *xpidptr;
extern int xlock;
extern int xuanum;
extern int xpprup;
extern char xmyname[20];
/* end structures needed for localization */


static int g_rdbms_bypass; /* Case 2946 */
char * herr_msg;

/************************************************************************/
/*  Define user functions prototypes ORACLE, INFORMIX and SYBASE        */
/*  Also added support DBTWO and CA-DATACOM 				*/
/*  Also added ISCOBOL-ESQL                                             */
/*  These names can be changed if necessary or desired                  */
/************************************************************************/
#ifdef ORACLE
#define ORACLE_AM 1
static int oracle_allocate();
static int oracle_deallocate();
static int oracle_start_txn();
static int oracle_end_txn();
static int oracle_commit();
static int oracle_rollback();
#else
#define ORACLE_AM 0
#endif

#ifdef INFORMIX
#define INFORMIX_AM 1
int informix_allocate();
int informix_deallocate();
int informix_start_txn();
int informix_end_txn();
int informix_commit();
int informix_rollback();
#else
#define INFORMIX_AM 0
#endif

#ifdef SYBASE
#define SYBASE_AM 1
static int sybase_allocate();
static int sybase_deallocate();
static int sybase_start_txn();
static int sybase_end_txn();
static int sybase_commit();
static int sybase_rollback();
#else
#define SYBASE_AM 0
#endif

/* Case # 2388 */
#ifdef MSSQL
#define  MSSQL_AM 1
static int  mssql_allocate();
static int  mssql_deallocate();
static int  mssql_start_txn();
static int  mssql_end_txn();
static int  mssql_commit();
static int  mssql_rollback();
#else
#define MSSQL_AM 0
#endif

#ifdef DBTWO
#define DBTWO_AM 1
static int  dbtwo_allocate();
static int  dbtwo_deallocate();
static int  dbtwo_start_txn();
static int  dbtwo_end_txn();
static int  dbtwo_commit();
static int  dbtwo_rollback();
static int  dbtwo_dosql4xa();
#else
#define  DBTWO_AM 0
#endif

#ifdef DATACOM
#define DATACOM_AM 1
int  datacom_allocate();
int  datacom_deallocate();
int  datacom_start_txn();
int  datacom_end_txn();
int  datacom_commit();
int  datacom_rollback();
#else
#define  DATACOM_AM 0
#endif

#ifdef INGRES
#define INGRES_AM 1
int ingres_allocate();
int ingres_deallocate();
int ingres_start_txn();
int ingres_end_txn();
int ingres_commit();
int ingres_rollback();
#else
#define INGRES_AM 0
#endif

#ifdef ESQL
#define ESQL_AM 1
static int isesql_allocate();
static int isesql_deallocate();
static int isesql_start_txn();
static int isesql_end_txn();
static int isesql_commit();
static int isesql_rollback();
#else
#define ESQL_AM 0
#endif

#ifdef MQSERIES
#define MQSERIES_AM 1
static int MQSeries_allocate();
static int MQSeries_deallocate();
static int MQSeries_start_txn();
static int MQSeries_end_txn();
static int MQSeries_commit();
static int MQSeries_rollback();
#else
#define MQSERIES_AM 0
#endif

static int dummy_function();

/************************************************************************/
/*  Calculate the maximum number of access methods.                     */
/************************************************************************/

/* Case # 2388 */
#define MAX_AM ORACLE_AM + INFORMIX_AM + SYBASE_AM + MSSQL_AM + DBTWO_AM + DATACOM_AM + INGRES_AM + ESQL_AM + MQSERIES_AM

/************************************************************************/
/*  Define the prototype structure for pointers to user functions       */
/*  DO NOT CHANGE                                                       */
/************************************************************************/

struct user_exits {
  int (*user_allocate)();
  int (*user_deallocate)();
  int (*user_start_txn)();
  int (*user_end_txn)();
  int (*user_commit)();
  int (*user_rollback)();
  char user_rdbms[11]; /* Case 2388 */
};

/************************************************************************/
/*  Define the occurrences of access method user exits and              */
/*  initialize the prototype ORACLE,INFORMIX, SYBASE 			*/
/*  DBTWO, CA-DATACOM and CA-OpenIngres function addresses 		*/
/*  Functions must be initialized in the order as defined in the        */
/*  'user_exits' prototype; i.e. allocate, deallocate, start txn,       */
/*  end txn, commit, rollback                                           */
/************************************************************************/

struct user_exits kxexits[MAX_AM+1] = {
#ifdef ORACLE
     oracle_allocate,
     oracle_deallocate,
     oracle_start_txn,
     oracle_end_txn,
     oracle_commit,
     oracle_rollback,
     "Oracle",
#endif
#ifdef INFORMIX
     informix_allocate,
     informix_deallocate,
     informix_start_txn,
     informix_end_txn,
     informix_commit,
     informix_rollback,
     "Informix",
#endif
#ifdef SYBASE
     sybase_allocate,
     sybase_deallocate,
     sybase_start_txn,
     sybase_end_txn,
     sybase_commit,
     sybase_rollback,
     "Sybase",
#endif
/* Case # 2388 */
#ifdef  MSSQL
      mssql_allocate,
      mssql_deallocate,
      mssql_start_txn,
      mssql_end_txn,
      mssql_commit,
      mssql_rollback,
      "SQL Server",
#endif
#ifdef DBTWO
    dbtwo_allocate,
    dbtwo_deallocate,
    dbtwo_start_txn,
    dbtwo_end_txn,
    dbtwo_commit,
    dbtwo_rollback,
    "DB2",
#endif
#ifdef DATACOM
    datacom_allocate,
    datacom_deallocate,
    datacom_start_txn,
    datacom_end_txn,
    datacom_commit,
    datacom_rollback,
    "DATACOM",
#endif
#ifdef INGRES
     ingres_allocate,
     ingres_deallocate,
     ingres_start_txn,
     ingres_end_txn,
     ingres_commit,
     ingres_rollback,
     "Ingres",
#endif
#ifdef ESQL
     isesql_allocate,
     isesql_deallocate,
     isesql_start_txn,
     isesql_end_txn,
     isesql_commit,
     isesql_rollback,
     "ESQL",
#endif
#ifdef MQSERIES
     MQSeries_allocate,
     MQSeries_deallocate,
     MQSeries_start_txn,
     MQSeries_end_txn,
     MQSeries_commit,
     MQSeries_rollback,
     "MQSeries",
#endif

/************************************************************************/
/*  Do not remove the following function definition, it is required to  */
/*  satisfy C syntax.                                                   */ 
/************************************************************************/
     dummy_function
     };

/************************************************************************/
/* The following variables to determine the state of the exits          */
/************************************************************************/
#define CONNECT 1
#define START 	2
#define KXSUCCESS  0   /*  Continue with the transaction */
#define KXCOBOLABORT 65535
#define KXABORT   -1   /*  Abort transaction server      */
#define KXDISCONNECT -2 /* rdbms connection lost */
#define KXFAILURE  1   /*  Transaction setup failure     */
#define KXNOTAPPLICABLE  2   /*  The function is not called    */
extern int xexit_state;

static int i;
static int rcode;
static int errind;
/* Case 2388 */
#if defined ORACLE || defined INFORMIX || defined SYBASE || defined MSSQL \
    || defined DBTWO || defined DATACOM || defined INGRES \
    || defined ESQL  || defined MQSERIES
static int dbms_rcode;
#endif
static int g_rdbms_connected[MAX_AM+1];
static char CurFunc[40];

/* Define the kxcobload function if we need to use it. This is the case for */
/* isCOBOL, and may be the case for other things in the future I guess.     */
#ifdef USE_KXCOBLOAD
extern int kxcobload(char *pprog_name,
                     int  pdebug,
                     int  pnumargs,
                     char *pparms[],
                     int  pparm_length[],
                     int  err_action,
                     char **buffer,
                     int  bufferlent);
#endif

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of Oracle databases. */
/*   The routines called by the functions below can be found in the     */
/*   KXORACLE.pco source file which is located in the $UNIKIX/src       */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef ORACLE

#ifdef XOPEN
extern int KXORALGN_XA();
extern int KXORALGF_XA();
extern int KXORABTRN_XA();
extern int KXORAETRN_XA();
extern int KXORAUNDO_XA();
extern int oraxa_prepare();
extern int oraxa_commit();
extern int KXVSAMXAW();
extern int KXVSAMXAR();
#else
#ifdef USE_KXCOBLOAD
static int KXORALGN() {
    return kxcobload("KXORACLE",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXORALGF() {
    return kxcobload("KXORALGF",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXORABTRN() {
    return kxcobload("KXORABTRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXORAETRN() {
    return kxcobload("KXORAETRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXORASAVE() {
    return kxcobload("KXORASAVE",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXORAUNDO() {
    return kxcobload("KXORAUNDO",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
#else
extern int KXORALGN();
extern int KXORALGF();
extern int KXORABTRN();
extern int KXORAETRN();
extern int KXORASAVE();
extern int KXORAUNDO();
#endif
#endif

/********************************************************************/
/*     ORACLE          allocate/open function                       */
/********************************************************************/

static int oracle_allocate() {

#ifdef XOPEN
      strcpy(CurFunc,"XA_oracle_allocate");  /*  Do not change */
      dbms_rcode = KXORALGN_XA();
      return(dbms_rcode);
#else
      strcpy(CurFunc,"oracle_allocate");  /*  Do not change */
      dbms_rcode = KXORALGN();
      return(dbms_rcode);
#endif
}

/********************************************************************/
/*     ORACLE          deallocate/close function                    */
/********************************************************************/

static int oracle_deallocate() {

#ifdef XOPEN
      strcpy(CurFunc,"XA_oracle_deallocate");  /*  Do not change */
      dbms_rcode = KXORALGF_XA();
      return(dbms_rcode);
#else
      strcpy(CurFunc,"oracle_deallocate");  /*  Do not change */
      dbms_rcode = KXORALGF();
      return(dbms_rcode);
#endif

}

/********************************************************************/
/*     ORACLE          start_txn function                           */
/********************************************************************/

static int oracle_start_txn() {

#ifdef XOPEN
      strcpy(CurFunc,"XA_oracle_start_txn");  /*  Do not change */
      dbms_rcode = KXORABTRN_XA();
      return(dbms_rcode);
#else
      strcpy(CurFunc,"oracle_start_txn");  /*  Do not change */
      dbms_rcode = KXORABTRN();
      return(dbms_rcode);
#endif

}

/********************************************************************/
/*     ORACLE          end of txn function                          */
/********************************************************************/

static int oracle_end_txn() {

#ifdef XOPEN
      strcpy(CurFunc,"XA_oracle_end_txn");  /*  Do not change */
      dbms_rcode = KXORAETRN_XA();
      return(dbms_rcode);
#else
      strcpy(CurFunc,"oracle_end_txn");  /*  Do not change */
      dbms_rcode = KXORAETRN();
      return(dbms_rcode);
#endif

}

/********************************************************************/
/*     ORACLE          commit function                              */
/********************************************************************/

static int oracle_commit() {

#ifndef XOPEN
      strcpy(CurFunc,"oracle_commit");  /*  Do not change */
      dbms_rcode = KXORASAVE();
      return(dbms_rcode);
#else
      return (0);
#endif

}

/********************************************************************/
/*     ORACLE          rollback function                            */
/********************************************************************/

static int oracle_rollback() {

#ifdef XOPEN
      strcpy(CurFunc,"XA_oracle_rollback");  /*  Do not change */
      dbms_rcode = KXORAUNDO_XA();
      return(dbms_rcode);
#else
      strcpy(CurFunc,"oracle_rollback");  /*  Do not change */
      dbms_rcode = KXORAUNDO();
      return(dbms_rcode);
#endif

}

#endif           /* End Oracle code */

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of databases that    */
/*   use XA library.                                                    */
/*   The routines called by the functions below can be found in the     */
/*   KXPROGXA.clt source file and in the kxoracle_xa.c file which are   */
/*   located in the $UNIKIX/src directory.                              */
/*                                                                      */
/************************************************************************/

/********************************************************************/
/*     kxaprepare function                                          */
/********************************************************************/

int kxaprepare() {

#ifdef ORACLE
#ifdef XOPEN
    if (oraxa_prepare()==-1)
            return(-1);

#endif
#endif

return(0);
}

/********************************************************************/
/*     kxacommit function                                         */
/********************************************************************/

int kxacommit() {

#ifdef ORACLE
#ifdef XOPEN
      oraxa_commit();
#endif
#endif

    return(0);
}

/********************************************************************/
/*     kxvsmrecw function  write the flag in the vsam recovery file */
/********************************************************************/

int kxvsmrecw() {

#ifdef ORACLE
#ifdef XOPEN
    KXVSAMXAW();
    vsmflagw=recvsamarea[4];
#endif
#endif

    return(0);
}

/********************************************************************/
/*     kxvsmrecr function                                           */
/********************************************************************/

int kxvsmrecr() {

#ifdef ORACLE
#ifdef XOPEN
    KXVSAMXAR();
    vsmflagr=recvsamarea[4];
#endif
#endif

    return(0);
}

/********************************************************************/
/*     kxvsmrecrfile function                                       */
/********************************************************************/

#define N4K		4096
#define N8K		8192
#define N16K		16384
#define N32K		32768
#define BLOCKSIZE       N4K
#define BLOCKHEAD       20
#define MAXXUANUM       1025
#define numrecb(a)      ((BLOCKSIZE - BLOCKHEAD)/a)
#define RECSIZE         12
#define RECHEAD         4
#define LENGTHVSAMFILE  10
#define RECVSAMFILE     "/RECVSXA"

int kxvsmrecrfile()
{
    FILE *fd;
    char *ret_get;
    char *ret_mall;
    int k,recnum,offsetrec,ret_read;

    ret_get = (char *) getenv("KIXSYS");
    if (ret_get == NULL )
        {
        strcpy(xerrorbuf.eb_kxroutine, "kxvsmrecrfile");
        xerrorbuf.eb_kxerrno = E_1649;
        herr_msg = kxerrorprt(0);
        printf("\n%s\n", herr_msg);
        return(-1);
        }
    ret_mall =(char *)malloc(strlen(ret_get)+LENGTHVSAMFILE);
    if(ret_mall == NULL )
        {
        strcpy(xerrorbuf.eb_kxroutine, "kxvsmrecrfile");
        xerrorbuf.eb_errcode[0] = strlen(ret_get)+LENGTHVSAMFILE;
        xerrorbuf.eb_kxerrno = E_2206;
        herr_msg = kxerrorprt(0);
        printf("\n%s\n", herr_msg);
        return(-1);
        }

    strcpy(ret_mall,ret_get);
    strcat(ret_mall,RECVSAMFILE);

    k=(xuanum+1)/numrecb(RECSIZE);
    recnum=((xuanum+1)-(numrecb(RECSIZE)*k));
    if (!recnum) recnum++;
    offsetrec=(BLOCKSIZE*k)+(BLOCKHEAD+((recnum-1)*(RECSIZE))+RECHEAD);

    fd=fopen(ret_mall,"r");
    if(fd == NULL)
        {
        strcpy(xerrorbuf.eb_data, "open");
        strcpy(xerrorbuf.eb_kxroutine, "kxvsmrecrfile");
        xerrorbuf.eb_errcode[0] = errno;
        xerrorbuf.eb_kxerrno = E_6711;
        herr_msg = kxerrorprt(0);
        printf("\n%s\n", herr_msg);
        return(-1);
        }

    fseek(fd,offsetrec,0);
    ret_read=fread(recvsamarea,RECSIZE-RECHEAD,1,fd);
    if (ret_read != 1)
        {
        strcpy(xerrorbuf.eb_data, "fread");
        strcpy(xerrorbuf.eb_kxroutine, "kxvsmrecrfile");
        xerrorbuf.eb_errcode[0] = errno;
        xerrorbuf.eb_kxerrno = E_6711;
        herr_msg = kxerrorprt(0);
        printf("\n%s\n", herr_msg);
                return(-1);
        }
    fclose(fd);
    vsmflagr=recvsamarea[4];
    return(0);
}

/********************************************************************/
/*     read_xuanum function         read the xuanum                 */
/********************************************************************/

#ifdef __STDC__
int read_xuanum(char *recarea)
#else
int read_xuanum(recarea)
char *recarea;
#endif
{
    char xxuanum[5];

    sprintf(xxuanum,"%.4d",xuanum);
    memcpy(recarea,xxuanum,4);
    return(0);

}
/********************************************************************/
/*     read_recvsxa function     read the vsam recovery file       */
/********************************************************************/
#ifdef __STDC__
int read_recvsxa(char *recarea)
#else
char read_recvsxa(recarea)
char *recarea;
#endif
{
    memcpy(recvsamarea,recarea,8);
    return (0);
}


/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of Informix databases*/
/*   The routines called by the functions below can be found in the     */
/*   KXINFSRV.eco source file which is located in the $UNIKIX/src       */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef INFORMIX

/********************************************************************/
/*     INFORMIX        allocate/open function                       */
/********************************************************************/

static int informix_allocate() {

      strcpy(CurFunc,"informix_allocate");
      dbms_rcode = KXINFLGN();
      return(dbms_rcode);

}

/********************************************************************/
/*     INFORMIX        deallocate/close function                    */
/********************************************************************/

static int informix_deallocate() {


      strcpy(CurFunc,"informix_deallocate");  /*  Do not change */
      dbms_rcode = KXINFLGF();
      return(dbms_rcode);

}

/********************************************************************/
/*     INFORMIX        start_txn function                           */
/********************************************************************/

static int informix_start_txn() {

      strcpy(CurFunc,"informix_start_txn");
      dbms_rcode = KXINFBGN();
      return(dbms_rcode);

}

/********************************************************************/
/*     INFORMIX        end of txn function                          */
/********************************************************************/

static int informix_end_txn() {

      strcpy(CurFunc,"informix_end_txn");  /*  Do not change */
      dbms_rcode = KXINFEND();
      return(dbms_rcode);

}

/********************************************************************/
/*     INFORMIX        commit function                              */
/********************************************************************/

static int informix_commit() {

      strcpy(CurFunc,"informix_commit");  /*  Do not change */
      dbms_rcode = KXINFCOM();
      return(dbms_rcode);

}

/********************************************************************/
/*     INFORMIX        rollback function                            */
/********************************************************************/

static int informix_rollback() {

      strcpy(CurFunc,"informix_rollback");
      dbms_rcode = KXINFROL();
      return(dbms_rcode);

}

#endif              /* End Informix code */

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of Sybase databases. */
/*   The routines called by the functions below can be found in the     */
/*   KXSYBASE.cop source file which is located in the $UNIKIX/src       */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef SYBASE
#ifdef USE_KXCOBLOAD
/*-----------------------------------------------------------------------*/
/* When using kxcobload to get to the sybase routines, it is unfortunate */
/* that (some of) the actual sybase symbols live in a '.a' and that if   */
/* they are never explicitly referenced, they never get included into    */
/* the processes where they are required.                                */
/* Because of that, we have to put in a dummy reference to one on them   */
/* here so that it will be linked in, and can thus be found when         */
/* the kxcrload is done.                                                 */
extern void CSBCTXGLOBAL();	/* Dummy definition - doesn't matter     */
                                /* what it really is - we just need the  */
                                /* symbol.                               */
static void (*dummy_func_needed_to_include_sybase_symbols)() = CSBCTXGLOBAL;
/*-----------------------------------------------------------------------*/

static int KXSYBLGN() {
    return kxcobload("KXSYBASE",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXSYBLGF() {
    return kxcobload("KXSYBLGF",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXSYBBTRN() {
    return kxcobload("KXSYBBTRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXSYBETRN() {
    return kxcobload("KXSYBETRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXSYBSAVE() {
    return kxcobload("KXSYBSAVE",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXSYBUNDO() {
    return kxcobload("KXSYBUNDO",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
#else
extern int KXSYBLGN();
extern int KXSYBLGF();
extern int KXSYBBTRN();
extern int KXSYBETRN();
extern int KXSYBSAVE();
extern int KXSYBUNDO();
#endif /* USE_KXCOBLOAD */

/********************************************************************/
/*     SYBASE          allocate/open function                       */
/********************************************************************/

static int sybase_allocate() {

/*----------------------------------------------------------------------*/
/* RS6000 and SYBASE10 specific for changing process group id           */
/* Do not change any part of the code in ifdef                          */
/*----------------------------------------------------------------------*/
#ifdef SYBASE10
#ifdef RS6000
	int lrc;
	pid_t l_pgid;

	l_pgid = getpid();
	lrc    = setpgid(l_pgid, l_pgid);
	if (lrc != 0)
	   {
	   strcpy(CurFunc,"sybase_allocate");  /*  Do not change */
	   kxprtf("Error changing process group id  - Errno %d, returncode %d\n"                   , errno,lrc);
	   return(KXCOBOLABORT);
	   }
#endif  /*  RS6000 condition */
#endif  /*  SYBASE10 condition */
/*----------------------------------------------------------------------*/

      strcpy(CurFunc,"sybase_allocate");  /*  Do not change */
      dbms_rcode = KXSYBLGN();
      return(dbms_rcode);

}

/********************************************************************/
/*     SYBASE          deallocate/close function                    */
/********************************************************************/

static int sybase_deallocate() {

      strcpy(CurFunc,"sybase_deallocate");  /*  Do not change */
      dbms_rcode = KXSYBLGF();
      return(dbms_rcode);

}

/********************************************************************/
/*     SYBASE          start_txn function                           */
/********************************************************************/

static int sybase_start_txn() {

      strcpy(CurFunc,"sybase_start_txn");  /*  Do not change */
      dbms_rcode = KXSYBBTRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     SYBASE          end of txn function                          */
/********************************************************************/

static int sybase_end_txn() {

      strcpy(CurFunc,"sybase_end_txn");  /*  Do not change */
      dbms_rcode = KXSYBETRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     SYBASE          commit function                              */
/********************************************************************/

static int sybase_commit() {

      strcpy(CurFunc,"sybase_commit");  /*  Do not change */
      dbms_rcode = KXSYBSAVE();
      return(dbms_rcode);

}

/********************************************************************/
/*     SYBASE          rollback function                            */
/********************************************************************/

static int sybase_rollback() {

      strcpy(CurFunc,"sybase_rollback");  /*  Do not change */
      dbms_rcode = KXSYBUNDO();
      return(dbms_rcode);

}

#ifndef SYBASE10
/************************************************************************/
/*  Do not remove the following function definition, it is required to  */
/*  satisfy SYBASE 4.9.x objects syntax.                                */ 
/************************************************************************/

void sybase_null_func()
{
int void_number;
void_number = sql_put_tds_vsn();
void_number = sqlchkxact();
void_number = sqlallrel();
void_number = sqlbadassert();
void_number = sqlbind();
/* void_number = sqlca(); Uncomment to use in 4.9.1 */
void_number = sqlcancel();
void_number = sqlchkfetch();
void_number = sqlcobvalchk();
void_number = sqlconvert ();
void_number = sqlcstring ();
void_number = sqlcurcloseconn ();
void_number = sqlcurfind ();
void_number = sqlcurisopenconn ();
void_number = sqlcurset ();
void_number = sqldispnum ();
void_number = sqldynexec ();
void_number = sqldyngetexpct ();
void_number = sqlerror ();
void_number = sqlexecarg ();
void_number = sqlexecdo ();
void_number = sqlexecinit ();
void_number = sqlfetchfree ();
void_number = sqlfetchsuspend ();
void_number = sqlfindconn ();
void_number = sqlflogin ();
void_number = sqlgetcmd ();
void_number = sqlgetdbtype ();
void_number = sqlgetexpct ();
void_number = sqlgetfetch ();
void_number = sqlgetfetchqual ();
void_number = sqlgetstype ();
void_number = sqlgetsvargs ();
void_number = sqlimmexec ();
void_number = sqlinitca ();
void_number = sqllinkv ();
void_number = sqllogin ();
void_number = sqlmakcur ();
void_number = sqlmem ();
void_number = sqlmkcmd ();
void_number = sqlnewconn ();
void_number = sqlnext1 ();
void_number = sqlnextr ();
void_number = sqlnprocrow ();
void_number = sqlnullstr ();
void_number = sqloproc ();
void_number = sqlpproc ();
void_number = sqlrelease ();
void_number = sqlrproc ();
void_number = sqlrterr ();
void_number = sqlrtlibstr ();
void_number = sqlrtlibvsn ();
void_number = sqlsetbadassert ();
void_number = sqlsetcmd ();
void_number = sqlsetcurbind ();
void_number = sqlseterr ();
void_number = sqlstproc ();
void_number = sqlulinkv ();
void_number = sqluxact ();
void_number = sqlwarn ();
void_number = sqlwarning ();
void_number = sqlxact ();
void_number = sqlxactcmd ();
void_number = sqlxactuse ();
void_number = sqlxcmd ();
}

int sql_put_tds_vsn()
{
	return -1;
}

int sqlchkxact()
{
	return -1;
}
#endif           /* End SYBASE10 check */

/*
    Case # 1928, Sybase require that signal be set for MFCOBOL 32 onwards
   For HP, it has to be before a sybase call.  This is call from KXSYBASE.cop
   For other platforms, it can $COBDIR/cobconfig 
*/

#ifdef SYBASE10
#ifdef MFCOBOL32
void kxsetsig()
{
#ifdef HPUX
   signal(17,0);
   signal(22,0);
#endif
}
#endif           /* End MFCOBOL32 check */
#endif           /* End SYBASE10 check */
#endif           /* End Sybase code */

/* Case # 2388 */
/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of SQL Server DB.    */
/*   The routines called by the functions below can be found in the     */
/*   KXMSSQL.cbl  source file which is located in the $UNIKIX/src       */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef MSSQL
/********************************************************************/
/*     MSSQL          allocate/open function                       */
/********************************************************************/

static int  mssql_allocate() {

      strcpy(CurFunc,"mssql_allocate");  /*  Do not change */
      dbms_rcode = KXMSQLGN();
      return(dbms_rcode);

}

/********************************************************************/
/*     SQL Server          deallocate/close function                    */
/********************************************************************/

static int  mssql_deallocate() {

      strcpy(CurFunc,"mssql_deallocate");  /*  Do not change */
      dbms_rcode = KXMSQLGF();
      return(dbms_rcode);

}

/********************************************************************/
/*     SQL Server      start_txn function                           */
/********************************************************************/

static int  mssql_start_txn() {

      strcpy(CurFunc,"mssql_start_txn");  /*  Do not change */
      dbms_rcode = KXMSQBTRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     SQL Server      end of txn function                          */
/********************************************************************/

static int  mssql_end_txn() {

      strcpy(CurFunc,"mssql_end_txn");  /*  Do not change */
      dbms_rcode = KXMSQETRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     SQL Server      commit function                              */
/********************************************************************/

static int mssql_commit() {

      strcpy(CurFunc,"mssql_commit");  /*  Do not change */
      dbms_rcode = KXMSQSAVE();
      return(dbms_rcode);

}

/********************************************************************/
/*     SQL Server      rollback function                            */
/********************************************************************/

static int mssql_rollback() {

      strcpy(CurFunc,"mssql_rollback");  /*  Do not change */
      dbms_rcode = KXMSQUNDO();
      return(dbms_rcode);

}

#endif

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of isCOBOL ESQL      */
/*   database access.                                                   */
/*   The routines called by the functions below can be found in the     */
/*   KXISESQL.cbl source file which is located in the $UNIKIX/src       */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef ESQL
#ifdef USE_KXCOBLOAD
static int KXESQLLGN() {
    return kxcobload("KXISESQL",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXESQLLGF() {
    return kxcobload("KXESQLLGF",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXESQLBTRN() {
    return kxcobload("KXESQLBTRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXESQLETRN() {
    return kxcobload("KXESQLETRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXESQLSAVE() {
    return kxcobload("KXESQLSAVE",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXESQLUNDO() {
    return kxcobload("KXESQLUNDO",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
#else
extern int KXESQLLGN();
extern int KXESQLLGF();
extern int KXESQLBTRN();
extern int KXESQLETRN();
extern int KXESQLSAVE();
extern int KXESQLUNDO();
#endif

/********************************************************************/
/*     ESQL        allocate/open function                           */
/********************************************************************/

static int isesql_allocate() {

      strcpy(CurFunc,"isesql_allocate");  /*  Do not change */
      dbms_rcode = KXESQLLGN();
      return(dbms_rcode);

}

/********************************************************************/
/*     ESQL         deallocate/close function                       */
/********************************************************************/

static int isesql_deallocate() {

      strcpy(CurFunc,"isesql_deallocate");  /*  Do not change */
      dbms_rcode = KXESQLLGF();
      return(dbms_rcode);

}

/********************************************************************/
/*     ESQL         start_txn function                              */
/********************************************************************/

static int isesql_start_txn() {

      strcpy(CurFunc,"isesql_start_txn");  /*  Do not change */
      dbms_rcode = KXESQLBTRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     ESQL         end of txn function                             */
/********************************************************************/

static int isesql_end_txn() {

      strcpy(CurFunc,"isesql_end_txn");  /*  Do not change */
      dbms_rcode = KXESQLETRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     ESQL         commit function                                 */
/********************************************************************/

static int isesql_commit() {

      strcpy(CurFunc,"isesql_commit");  /*  Do not change */
      dbms_rcode = KXESQLSAVE();
      return(dbms_rcode);

}

/********************************************************************/
/*     ESQL         rollback function                               */
/********************************************************************/

static int isesql_rollback() {

      strcpy(CurFunc,"isesql_rollback");  /*  Do not change */
      dbms_rcode = KXESQLUNDO();
      return(dbms_rcode);

}

#endif           /* End ESQL  code */

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of db26000 databases.*/
/*   The routines called by the functions below can be found in the     */
/*   KXDB2.cbl source file which is located in the $UNIKIX/src          */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef DBTWO
#ifdef USE_KXCOBLOAD
static int KXDB2LGN() {
    return kxcobload("KXDB27",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXDB2LGF() {
    return kxcobload("KXDB2LGF",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXDB2BTRN() {
    return kxcobload("KXDB2BTRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXDB2ETRN() {
    return kxcobload("KXDB2ETRN",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXDB2SAVE() {
    return kxcobload("KXDB2SAVE",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXDB2UNDO() {
    return kxcobload("KXDB2UNDO",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
static int KXDB2DOXA() {
    return kxcobload("KXDB2DOXA",KXDEBUG_NONE,0,NULL,0,LOGERR,NULL,0);
}
#else
extern int KXDB2LGN();
extern int KXDB2LGF();
extern int KXDB2BTRN();
extern int KXDB2ETRN();
extern int KXDB2SAVE();
extern int KXDB2UNDO();
extern int KXDB2DOXA();
#endif

/********************************************************************/
/*     DB26000        do some SQL in XA environment at 'allocate'   */
/*                    so that we can override DB2 signal handling   */
/********************************************************************/

static int dbtwo_dosql4xa() {

      strcpy(CurFunc,"dbtwo_dosql4xa\0");  /*  Do not change */
      dbms_rcode = KXDB2DOXA();
      return(dbms_rcode);

}

/********************************************************************/
/********************************************************************/
/*     DB26000        allocate/open function                        */
/********************************************************************/

static int dbtwo_allocate() {

      strcpy(CurFunc,"dbtwo_allocate");  /*  Do not change */
      dbms_rcode = KXDB2LGN();
      return(dbms_rcode);

}

/********************************************************************/
/*     DB26000         deallocate/close function                    */
/********************************************************************/

static int dbtwo_deallocate() {

      strcpy(CurFunc,"dbtwo_deallocate");  /*  Do not change */
      dbms_rcode = KXDB2LGF();
      return(dbms_rcode);

}

/********************************************************************/
/*     DB26000         start_txn function                           */
/********************************************************************/

static int dbtwo_start_txn() {

      strcpy(CurFunc,"dbtwo_start_txn");  /*  Do not change */
      dbms_rcode = KXDB2BTRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     DB26000         end of txn function                          */
/********************************************************************/

static int dbtwo_end_txn() {

      strcpy(CurFunc,"dbtwo_end_txn");  /*  Do not change */
      dbms_rcode = KXDB2ETRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     DB26000         commit function                              */
/********************************************************************/

static int dbtwo_commit() {

      strcpy(CurFunc,"dbtwo_commit");  /*  Do not change */
      dbms_rcode = KXDB2SAVE();
      return(dbms_rcode);

}

/********************************************************************/
/*     DB26000         rollback function                            */
/********************************************************************/

static int dbtwo_rollback() {

      strcpy(CurFunc,"dbtwo_rollback");  /*  Do not change */
      dbms_rcode = KXDB2UNDO();
      return(dbms_rcode);

}

#endif           /* End db26000  code */

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of DATACOM databases.*/
/*   The routines called by the functions below can be found in the     */
/*   KXDATACOM.dco source file which is located in the $UNIKIX/src      */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef DATACOM
/********************************************************************/
/*     DATACOM        allocate/open function                        */
/********************************************************************/

static int datacom_allocate() {

      strcpy(CurFunc,"datacom_allocate");  /*  Do not change */
      dbms_rcode = KXDCOMLGN();
      return(dbms_rcode);

}

/********************************************************************/
/*     DATACOM         deallocate/close function                    */
/********************************************************************/

static int datacom_deallocate() {

      strcpy(CurFunc,"datacom_deallocate");  /*  Do not change */
      dbms_rcode = KXDCOMLGF();
      return(dbms_rcode);

}

/********************************************************************/
/*     DATACOM         start_txn function                           */
/********************************************************************/

static int datacom_start_txn() {

      strcpy(CurFunc,"datacom_start_txn");  /*  Do not change */
      dbms_rcode = KXDCOMBTRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     DATACOM         end of txn function                          */
/********************************************************************/

static int datacom_end_txn() {

      strcpy(CurFunc,"datacom_end_txn");  /*  Do not change */
      dbms_rcode = KXDCOMETRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     DATACOM         commit function                              */
/********************************************************************/

static int datacom_commit() {

      strcpy(CurFunc,"datacom_commit");  /*  Do not change */
      dbms_rcode = KXDCOMSAVE();
      return(dbms_rcode);

}

/********************************************************************/
/*     DATACOM         rollback function                            */
/********************************************************************/

static int datacom_rollback() {

      strcpy(CurFunc,"datacom_rollback");  /*  Do not change */
      dbms_rcode = KXDCOMUNDO();
      return(dbms_rcode);

}

#endif           /* End DATACOM  code */

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of Ingres databases. */
/*   The routines called by the functions below can be found in the     */
/*   KXINGRES.scb source file which is located in the $UNIKIX/src       */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef INGRES
/********************************************************************/
/*     INGRES          allocate/open function                       */
/********************************************************************/

static int ingres_allocate() {

      strcpy(CurFunc,"ingres_allocate");  /*  Do not change */
      dbms_rcode = KXINGLGN();
      return(dbms_rcode);
}

/********************************************************************/
/*     INGRES          deallocate/close function                    */
/********************************************************************/

static int ingres_deallocate() {

      strcpy(CurFunc,"ingres_deallocate");  /*  Do not change */
      dbms_rcode = KXINGLGF();
      return(dbms_rcode);

}

/********************************************************************/
/*     INGRES          start_txn function                           */
/********************************************************************/

static int ingres_start_txn() {

      strcpy(CurFunc,"ingres_start_txn");  /*  Do not change */
      dbms_rcode = KXINGBTRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     INGRES          end of txn function                          */
/********************************************************************/

static int ingres_end_txn() {

      strcpy(CurFunc,"ingres_end_txn");  /*  Do not change */
      dbms_rcode = KXINGETRN();
      return(dbms_rcode);

}

/********************************************************************/
/*     INGRES          commit function                              */
/********************************************************************/

static int ingres_commit() {

      strcpy(CurFunc,"ingres_commit");  /*  Do not change */
      dbms_rcode = KXINGSAVE();
      return(dbms_rcode);

}

/********************************************************************/
/*     INGRES          rollback function                            */
/********************************************************************/

static int ingres_rollback() {

      strcpy(CurFunc,"ingres_rollback");  /*  Do not change */
      dbms_rcode = KXINGUNDO();
      return(dbms_rcode);

}

#endif           /* End Ingres code */

/************************************************************************/
/*                                                                      */
/*   The following code is specific to the support of MQSeries manager. */
/*   The routines called by the functions below can be found in the     */
/*   kxmqseries.c source file which is located in the $UNIKIX/src       */
/*   directory.                                                         */
/*                                                                      */
/************************************************************************/

#ifdef MQSERIES
extern int KXMQSALLOC();
extern int KXMQSETRN(); 
extern int KXMQSSAVE();
extern int KXMQSUNDO();

/********************************************************************/
/*     MQSeries          allocate/open function                       */
/********************************************************************/

static int MQSeries_allocate() {

      strcpy(CurFunc,"MQSeries_allocate");  /*  Do not change */
      dbms_rcode = KXMQSALLOC();
      return(dbms_rcode);
}

/********************************************************************/
/*     MQSeries          deallocate/close function                    */
/********************************************************************/

static int MQSeries_deallocate() {

      strcpy(CurFunc,"MQSeries_deallocate");  /*  Do not change */
      dbms_rcode = KXNOTAPPLICABLE;
      return(dbms_rcode);

}

/********************************************************************/
/*     MQSeries          start_txn function                           */
/********************************************************************/

static int MQSeries_start_txn() {

      strcpy(CurFunc,"MQSeries_start_txn");  /*  Do not change */
      dbms_rcode = KXNOTAPPLICABLE;
      return(dbms_rcode);

}

/********************************************************************/
/*     MQSeries          end of txn function                          */
/********************************************************************/

static int MQSeries_end_txn() {

      strcpy(CurFunc,"MQSeries_end_txn");  /*  Do not change */
      dbms_rcode = KXMQSETRN(); 
      return(dbms_rcode);

}

/********************************************************************/
/*     MQSeries          commit function                              */
/********************************************************************/

static int MQSeries_commit() {

      strcpy(CurFunc,"MQSeries_commit");  /*  Do not change */
      dbms_rcode = KXMQSSAVE();
      return(dbms_rcode);

}

/********************************************************************/
/*     MQSeries          rollback function                            */
/********************************************************************/

static int MQSeries_rollback() {

      strcpy(CurFunc,"MQSeries_rollback");  /*  Do not change */
      dbms_rcode = KXMQSUNDO();
      return(dbms_rcode);

}

#endif           /* End MQSeries code */

/********************************************************************/
/*                                                                  */
/*  The following code is the interface code between the user exit  */
/*  module and the transaction or batch processor.                  */
/*                                                                  */
/*  It should not be necessary to change this code, but if you      */
/*  do then be very careful.                                        */
/*                                                                  */
/********************************************************************/

/*  Entry points called by the system - DO NOT CHANGE           */

extern int kxuser_allocate();
#ifdef __STDC__
  extern int kxuser_deallocate(int);  /* Case # 1284 */
  extern int kxuser_start_txn(char *);
  extern int kxuser_end_txn(char *);
#else
  extern int kxuser_deallocate();  /* Case # 1284 */
  extern int kxuser_start_txn();
  extern int kxuser_end_txn();
#endif
extern int kxuser_commit();
extern int kxuser_rollback();

static int dummy_function() {
	return(0);
}
/**********************************************************/
/* Function to make sure parameter functions are included */
/**********************************************************/
void null_function() {
void kxsysinfo();
void kxtctinfo();
void kxsetmsg();
}

/********************************************************************/
/*  entry point for user ALLOCATE/OPEN function. For each access    */
/*  method in the table, the designated allocate/open function will */
/*  be executed                                                     */
/********************************************************************/

int kxuser_allocate() {
char *lrdbms_bypass; /* Case 2946 */


g_rdbms_bypass = 0; /* Case 2946 */
errind = 0;
if (kxdbcheck(MAX_AM+1, 0))
   {
   kxerror(E_964,E_PRINT,"kxuser_allocate");
   return(KXFAILURE);
   }
for (i=0; i < MAX_AM; i++)
    {
    /* ------------------ ---------------------------------
       Case # 2946
       Check and set variable if KIXRDBMS_BYPASS is set
       ------------------ --------------------------------- */
    if ( g_rdbms_bypass == 0)
       {
	 if ((lrdbms_bypass = getenv("KIXRDBMS_BYPASS"))!=NULL)
	    {
	    kxerror(E_1097,E_PRINT,"kxuser_allocate");
	    g_rdbms_bypass=1;
	    }
         else
	    {
	    g_rdbms_bypass=0;
	    }
       }

    /* Case # 2946 */
    if ((kxexits[i].user_allocate != NULL) &&
	(!g_rdbms_bypass) )
       {
       xexit_state = CONNECT;
       rcode = kxexits[i].user_allocate();
       errind = kxusr_chkerr(rcode,E_1001);
	   if (errind != KXSUCCESS)
	       {
		   g_rdbms_connected[i] = 0; /* connection failed */
		   }
		else
		   {
		   g_rdbms_connected[i] = 1; /* connection successful */
		   }
			
       }
       xexit_state = START;
    }
	/*
	 * If the user has bypassed the content manager exits (for example,
	 * they have XA configured), then MQ setup will not have been
	 * called. We must ensure that we intercept MQ calls regardless
	 * of the transaction environment. 
	 */
#ifdef MQSERIES
	if (g_rdbms_bypass) {
		rcode = MQSeries_allocate();
		errind = kxusr_chkerr(rcode,E_1001);
	}
#endif /* MQSERIES */

	/*
	 * If the user has bypassed the content manager exits (for example,
	 * they have XA configured), then no DB2 SQL will have been done.
         * The first DB2 SQL resets some signal handling so lets do that 
         * now as we restore signal handling on return from kxuser_allocate()
         */
#ifdef DBTWO
	if (g_rdbms_bypass && (getenv("KIX_RM_PATH") != NULL) ) {
                xexit_state = CONNECT;
		rcode = dbtwo_dosql4xa();
                xexit_state = START;
	}
#endif /* DB2TWO */

return(KXSUCCESS);
}

/********************************************************************/
/*  entry point for user DEALLOCATE/CLOSE function. For each access */
/*  method in the table, the designated deallocate/close function   */
/*  will be executed                                                */
/********************************************************************/

/* Changes made for Case # 1284 */
/* Pass the signal on deallocate */
#ifdef __STDC__
int kxuser_deallocate(int kxsignal)
#else
int kxuser_deallocate(kxsignal) 
int kxsignal;
#endif
{

errind = 0;
for (i=0; i < MAX_AM; i++)
    {
    /* Case # 2946 */
    if ((kxexits[i].user_deallocate != NULL) &&
	(!g_rdbms_bypass) )
       {
	   if (g_rdbms_connected[i] == 1)
           {
               rcode = kxexits[i].user_deallocate();
               errind = kxusr_chkerr(rcode,E_1002);
	       if (errind != KXSUCCESS)
	       {
                   g_rdbms_connected[i] = 0; /* we have no connection */
               }
	   }
       }
       xexit_state = START;
    }
return(errind);
}


/********************************************************************/
/*  entry point for user START_TXN function. For each access        */
/*  method in the table, the designated start of txn function will  */
/*  be executed                                                     */
/********************************************************************/

#ifdef __STDC__
int kxuser_start_txn (
   char *tranid
   )
#else
int kxuser_start_txn(tranid)
char *tranid;
#endif
{
    tranid = tranid;
    errind = 0;
    for (i=0; i < MAX_AM; i++)
    {
        /* Case # 2946 */
        if ((kxexits[i].user_start_txn != NULL) &&
	    (!g_rdbms_bypass))
        {
	    if (g_rdbms_connected[i] == 0) /* no connection */
	    {
	        xexit_state = CONNECT;
	        rcode = kxexits[i].user_allocate(); /* attempt a reconnect */
	        xexit_state = START;
	        if (rcode != KXSUCCESS)
	        {
		    g_rdbms_connected[i] = 0; /* connection failed */
	        }
	        else
	        {
		    g_rdbms_connected[i] = 1; /* connection successful */
	        }
	    }	
	    rcode = kxexits[i].user_start_txn();
	    errind = kxusr_chkerr(rcode,E_1003);
	    if (errind != KXSUCCESS)
	    {
	        g_rdbms_connected[i] = 0; /* disconnected */
	    }
        }
    }
    return(KXSUCCESS);
}

/********************************************************************/
/*  entry point for user END_TXN  function. For each access         */
/*  method in the table, the designated end of txn function will    */
/*  be executed                                                     */
/********************************************************************/

#ifdef __STDC__
int kxuser_end_txn (
   char *tranid
   )
#else
int kxuser_end_txn(tranid)
char *tranid;
#endif
{
    tranid = tranid;
    errind = 0;
    for (i=0; i < MAX_AM; i++)
    {
    /* Case # 2946 */
    if ((kxexits[i].user_end_txn != NULL) &&
	(!g_rdbms_bypass) )
	{
	    if (g_rdbms_connected[i] == 1) /* only call if we have a connection */
	    {
		rcode = kxexits[i].user_end_txn();
		errind = kxusr_chkerr(rcode,E_1004);
		if (errind != KXSUCCESS)
		{
		    g_rdbms_connected[i] = 0; /* we must have lost the connection */
		}
	    }
	}
    }
	/*
	 * We must ensure that MQ resources are released correctly.
	 * If the user has bypassed the content manager exits (for example,
	 * they have XA configured), then there may still be MQ resources
	 * active to this TX. 
	 */
#ifdef MQSERIES
    if (g_rdbms_bypass) {
	/* ensure any active handle is closed down */
	rcode = MQSeries_end_txn();
	errind = kxusr_chkerr(rcode,E_1004);
    } 
#endif /* MQSERIES */
    return(KXSUCCESS);
}

/********************************************************************/
/*  entry point for user COMMIT   function. For each access         */
/*  method in the table, the designated commit function will        */
/*  be executed                                                     */
/********************************************************************/

/********************************************************************/
/*  WARNING! Synchronicity between databases, e.g Oracle & MQ can be*/
/*  lost using this method. To ensure synchronicity across all data */
/*  use XA.                                                         */
/********************************************************************/

int kxuser_commit() {
errind = 0;
for (i=0; i < MAX_AM; i++)
    {
    /* Case # 2946 */
    if ((kxexits[i].user_commit != NULL) &&
	(!g_rdbms_bypass) )
       {
           rcode = kxexits[i].user_commit();
           errind = kxusr_chkerr(rcode,E_1005);
       }
    }
return(errind);
}

/********************************************************************/
/*  entry point for user ROLLBACK function. For each access         */
/*  method in the table, the designated rollback function will      */
/*  be executed                                                     */
/********************************************************************/

int kxuser_rollback() {
errind = 0;
for (i=0; i < MAX_AM; i++)
    {
    /* Case # 2946 */
    if ((kxexits[i].user_rollback != NULL) &&
	(!g_rdbms_bypass) )
       {
           rcode = kxexits[i].user_rollback();
           errind = kxusr_chkerr(rcode,E_1006);
       }
    }
return(errind);
}

/* Function to check error and set it accordingly */
#ifdef __STDC__
int kxusr_chkerr(int prcode,int pmsgnbr)
#else
int kxusr_chkerr(prcode,pmsgnbr)
int prcode;
int pmsgnbr;
#endif
{
       int lerrind;
       lerrind = KXFAILURE;  /* Set return code to failure */
       if (prcode == KXCOBOLABORT) { /* Fatal error */
         kxerror1(E_1099,E_PRINT, (char *)CurFunc,rcode);
         lerrind = KXABORT;
	 }
      else if (prcode == KXSUCCESS) { /* successful rollback */
#ifdef TRACEMSG
	      kxerrorstr(pmsgnbr,E_PRINT,(char *)CurFunc,kxexits[i].user_rdbms);
#else
           if ((pmsgnbr == E_1001) || (pmsgnbr == E_1002) )
	      {
	      kxerrorstr(pmsgnbr,E_PRINT,(char *)CurFunc,kxexits[i].user_rdbms);
	      }
#endif /* TRACEMSG */
              lerrind = KXSUCCESS;
	      }
            else if (prcode == KXNOTAPPLICABLE) {
              lerrind = KXSUCCESS;
              }
            else
	      {
              kxerror1(E_1099,E_PRINT, (char *)CurFunc,rcode);
	      lerrind = prcode;
	      }
        return(lerrind);
}


/********************************************************************/
/*  entry point for kxdfhei1 entry to CICS command call. 	    */
/*  Can be used for user debugging 				    */
/********************************************************************/

#ifdef __STDC__
void kxdfhei1_entry_usrexit(
int pindx1,
int pindx2
)
#else
void kxdfhei1_entry_usrexit(pindx1, pindx2)
int pindx1, pindx2;
#endif
{
#ifdef CHECKMEM
kxCheckMemory(SUPPRESS_COMMAND, "kxusrexit, function kxdfhei1_entry_usrexit",
	      pindx1, pindx2);
#else
pindx1 = pindx1;
pindx2 = pindx2;
#endif
return ;
}


/********************************************************************/
/*  entry point for kxdfhei1 exit after CICS command call. 	    */
/*  Can be used for user debugging 				    */
/********************************************************************/

#ifdef __STDC__
void kxdfhei1_exit_usrexit(
int pindx1,
int pindx2
)
#else
void kxdfhei1_exit_usrexit(pindx1, pindx2)
int pindx1, pindx2;
#endif
{
#ifdef CHECKMEM
kxCheckMemory(DONT_SUPPRESS_COMMAND, "kxusrexit, function kxdfhei1_exit_usrexit",
	      pindx1, pindx2);
#else
pindx1 = pindx1;
pindx2 = pindx2;
#endif
return ;
}

/********************************************************************/
/*  entry point for CICS application Temporary Storage commands     */
/*  Input:                                                          */
/*  eib is the DFHEIBLK which contains (for example);               */
/*      eibfn - the two-byte CICS command                           */
/*      eibtrmid - the 4-character terminal ID                      */
/*  queue_name is the 16-byte value requested by the application.   */
/*  tctua is the TCT User Area of the terminal running this tx.     */
/*  Output:                                                         */
/*  queue_name is the updated value to be used for this request.    */
/********************************************************************/

#ifdef __STDC__
void kxtsq_usrexit(DFHEIBLK *peib, char *pqueue_name)
#else
void kxtsq_usrexit(peib, pqueue_name)
DFHEIBLK *peib;
char *pqueue_name;
#endif
{
/* example definitions of TCTUA fields used for kxget_tctua() function
int ltctual;
char *ltctua;
*/

peib = peib;
pqueue_name = pqueue_name;

/* example pseudo-code for calling kxget_tctua and using the returned values
ltctua = kxget_tctua(&ltctual);
if (ltctual > 0)
   OK to reference ltctua;
else
   no TCTUA to use;
*/

return ;
}

/********************************************************************/
/*  entry point for kxstrtbch after a batch script has ended        */
/*  batch_script_rc is the return code from the batch shell script. */
/********************************************************************/

#ifdef __STDC__
void kxusrjobend(int batch_script_rc)
#else
void kxusrjobend(batch_script_rc)
int batch_script_rc;
#endif
{
batch_script_rc = batch_script_rc;
return ;
}

/********************************************************************/
/*  entry point for UNIX related animating on Server Express        */
/*  Attach/start animator by calling ServerExpress function         */
/*  immediately after entering COBOL program; i.e. first kxdfhei1   */
/*  call							    */
/*  NOTE: if running with ServerExpress, requires KX_SERVER_EXPRESS */
/*        flag to be set in the $UNIKIX/src/makefile when           */
/*        recompiling this user exit module			    */
/********************************************************************/

int kxuser_ServerExpress_debug(PMFflg)
int PMFflg;
{
#ifdef KX_SERVER_EXPRESS
int lstatus;
int lsys_lrc;
char db[1024];
pid_t pchild=-1;
#ifdef _BSD
union wait pidstatus = { -1 };
#else
int pidstatus=-1;
#endif
int childstatus=-1;
int sigstatus=-1;
pid_t mypid=getpid();


 if (PMFflg)
    {
      /* Disable monitoring */
      sprintf(db,"%s/scbin/kxchg_dbg",getenv("UNIKIX"));
      if((pchild=fork())>0) { /* parent */
#ifdef _BSD
	  if(waitpid(pchild,&pidstatus.w_status,0)<0)
#else
	  if(waitpid(pchild,&pidstatus,0)<0)
#endif
	  {
            kxerror2(E_5104,E_PRINT,"debug",mypid,errno);
	  } else {
	      if(WIFEXITED(pidstatus)) {
		  childstatus=WEXITSTATUS(pidstatus);
		  if(childstatus!=0) {
                    kxerror2(E_5105,E_PRINT,"debug",mypid,childstatus);
		  }
	      } else {
		  sigstatus=WTERMSIG(pidstatus);
                  kxerror2(E_5106,E_PRINT,"debug",mypid,sigstatus);
	      }
	  }
      } else if(pchild==0) { /* child */
	  execl(db,"kxchg_dbg",(char*)0);
          kxerror2str(E_5107,E_PRINT,"debug",db,mypid,errno);
	  exit(-1);
      } else {              /* fork failed */
        kxerror2(E_5108,E_PRINT,"debug",mypid,errno);
      }
    lstatus = kxcobol_debug();
    } else {
    lstatus = kxcobol_debug();
    }
return(lstatus);
#else
return(0);
#endif
}
