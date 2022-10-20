/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:20:24  $ */
/* $Modtime:   23 Apr 2013 13:20:24  $ */

/* $Workfile:   CICS_epi.h  $ $Revision:   1.2  $ */

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/network/public/PVCS/CICS_epi.h_v  $
 *
 *    Rev 1.2   23 Apr 2013 13:20:24   unikix
 * Update Dell copyright
 *
 *    Rev 1.1   18 Jan 2013 08:50:42   unikix
 * Dell rebranding
 *
 *    Rev 1.0   31 May 2007 13:38:06   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:36:56   unikix
 * New 8.0 archive
 *
 *    Rev 1.1   26 Sep 2001 11:24:44   unikix
 * Rebrand80
 *
 *    Rev 1.1   05 Apr 2001 15:27:26   daved
 * Correct misspellings.
 *
 *    Rev 1.0   30 Jan 2000 11:43:42   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 19:56:10   unikix
 * Initial 6.0
 *
 *    Rev 1.4   28 Aug 1998 14:27:38   daved
 * Fix the kxEpiSetTrace() prototype, and move the kxepi_trace_t typedef
 * above it.
 *
 *    Rev 1.3   17 Aug 1998 09:09:48   daved
 * Correct the use of __STDC__
 *
 *    Rev 1.2   02/10/98 16:12:54   ewane
 * B002723
 * Support for LU0 and LU1 terminals
 *
 *    Rev 1.1   09/17/97 13:45:06   daved
 * Changes to avoid compile warnings on RS6000 and Sequent platforms.
 *
 *    Rev 1.0   05/22/97 17:27:02   unikix
 * Initial 5.1
 *
 *    Rev 1.3   10/14/96 11:10:20   rickd
 * B001759
 * Added EPI timeout return codes and added prototypes for the timeout functions.
 *
 *    Rev 1.2   08/28/96 10:10:40   rick
 * B001722
 * remove extraneous comma on last element of typedef enum
 *
 *    Rev 1.1   07/16/96 09:56:22   ewane
 * D001132
 * Add support for ISC over TCP. Phil's changes to increase performance
 *
 *    Rev 1.0   04/10/96 14:04:54   unikix
 * Initial 5.0
 *
 *    Rev 1.0   12/28/94 15:11:00   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.4   12/09/94 09:30:34   rickd
 * Porting changes for NCR, Solaris, and RS6000.
 *
 *    Rev 1.3   09/28/94 07:45:42   rickd
 * Add trace macro and enum.  Also added string arrays for states, error,
 * end reasons, etc.
 *
 *    Rev 1.2   04/18/94 07:17:06   alan
 * Merge 1.0.1.0
 *
 *    Rev 1.0.1.0   03/09/94 08:49:16   rick
 * Clean up prototype definitions revealed on Solaris port
 *
 *    Rev 1.1   04/15/94 15:55:22   rickd
 * Add #define for the version information.
 *
 *    Rev 1.0   02/07/94 16:41:08   rickd
 * Initial revision.
 */

/**********************************************************************/
#ifndef _CICS_epi_h
#define _CICS_epi_h

#include <sys/types.h>

/*
 * Defined type not already included.
 */

#ifndef u_byte
typedef unsigned char u_byte;
#endif

#ifndef IN
#define IN
#endif
#ifndef OUT
#define OUT
#endif
#ifndef INOUT
#define INOUT
#endif

#define CICS_EPI_SYSTEM_NAME_MAX  8
#define CICS_EPI_SYSTEM_MAX       8
#define CICS_EPI_DESCRIPTION_MAX 60
#define CICS_EPI_TERMNAME_MAX     4
#define CICS_EPI_NETNAME_MAX      8
#define CICS_EPI_TRANID_MAX       4
#define CICS_EPI_ABEND_MAX        4
#define CICS_EPI_DEVTYPE_MAX     16
#define CICS_EPI_ERROR_MAX       60
#define CICS_EPI_TERM_INDEX_NONE 0xffff
#define CICS_EPI_VERSION_100    100             /* the original */
#define CICS_EPI_VERSION_101    101             /* Use lsock instead of msg q*/

/*
** EPI Events.
** NOTE:  If additions/deletions are made, please add/remove the entry
**        to the following array.
*/
typedef enum {
   CICS_EPI_EVENT_SEND,
   CICS_EPI_EVENT_CONVERSE,
   CICS_EPI_EVENT_END_TRAN,
   CICS_EPI_EVENT_START_ATI,
   CICS_EPI_EVENT_END_TERM,
   CICS_EPI_EVENT_HELP
} CICS_EpiEvent_t;

static char        Events [] [30] = { "CICS_EPI_EVENT_SEND",
                                      "CICS_EPI_EVENT_CONVERSE",
                                      "CICS_EPI_EVENT_END_TRAN",
                                      "CICS_EPI_EVENT_START_ATI",
                                      "CICS_EPI_EVENT_END_TERM",
                                      "CICS_EPI_EVENT_HELP"
                                    };

/*
** EPI End Reasons.
** NOTE:  If additions/deletions are made, please add/remove the entry
**        to the following array.
*/
typedef enum {
   CICS_EPI_END_SIGNOFF,
   CICS_EPI_END_SHUTDOWN,
   CICS_EPI_END_OUTSERVICE,
   CICS_EPI_END_UNKNOWN,
   CICS_EPI_END_FAILED
} CICS_EpiEnd_t;

static char        EndReasons [] [30] = {"CICS_EPI_END_SIGNOFF",
                                         "CICS_EPI_END_SHUTDOWN",
                                         "CICS_EPI_END_OUTSERVICE",
                                         "CICS_EPI_END_UNKNOWN",
                                         "CICS_EPI_END_FAILED"
                                        };

/*
** EPI ATI State Info.
** NOTE:  If additions/deletions are made, please add/remove the entry
**        to the following array.
*/
typedef enum {
   CICS_EPI_ATI_ON,
   CICS_EPI_ATI_HOLD,
   CICS_EPI_ATI_QUERY
} CICS_EpiATIState_t;

static char        ATIStateInfo [] [20] = {"CICS_EPI_ATI_ON",
                                           "CICS_EPI_ATI_HOLD",
                                           "CICS_EPI_ATI_QUERY"
                                          };

typedef enum {
   CICS_EPI_SENSE_OPCHECK,
   CICS_EPI_SENSE_REJECT
} CICS_EpiSenseCode_t;

typedef enum {
   CICS_EPI_WAIT,
   CICS_EPI_NOWAIT
} CICS_EpiWait_t;

typedef struct {
   char SystemName[CICS_EPI_SYSTEM_MAX + 1];
   char Description[CICS_EPI_DESCRIPTION_MAX + 1];
} CICS_EpiSystem_t;

typedef struct {
   char        TermName[ CICS_EPI_TERMNAME_MAX + 1];
   char        NetName[CICS_EPI_NETNAME_MAX + 1];
   short       NumLines;
   short       NumColumns;
   u_short     MaxData;
   short       ErrLastLine;
   short       ErrIntensify;
   short       ErrColor;
   short       ErrHilight;
   short       Hilight;
   short       Color;
   short       Printer;
} CICS_EpiDetails_t;

typedef struct {
   u_short             TermIndex;
   CICS_EpiEvent_t     Event;
   CICS_EpiEnd_t       EndReason;
   char                TranId[CICS_EPI_TRANID_MAX + 1];
   char                AbendCode[CICS_EPI_ABEND_MAX + 1];
   u_byte              *Data;
   u_short             Size;
} CICS_EpiEventData_t;

typedef struct {
   u_long    Cause;
   u_long    Value;
   char      Msg[CICS_EPI_ERROR_MAX + 1];
} CICS_EpiSysError_t;

#ifdef __STDC__
typedef void (*CICS_EpiNotify_t)(u_short TermIndex);
#else
typedef void (*CICS_EpiNotify_t)();
#endif

/*
 * Trace definitions.
 */
#ifdef sequent
#define KXEPI_TRACE_ON -1               /* Turn all traces on */
#endif

typedef enum {
#ifndef sequent
   KXEPI_TRACE_ON =  -1,               /* Turn all traces on */
#endif
   KXEPI_TRACE_OFF =  0,               /* Turn all traces off */
   KXEPI_TRACE_API = 1,                /* Trace API calls */
   KXEPI_TRACE_SRVR = 2,               /* Trace API backend routines */
   KXEPI_TRACE_SERVICE = 4             /* Trace internal service routines */
                         /* = 8 */
                         /* = 16 */
                         /* = 32 */
                         /* = 64 */
} kxepi_trace_t;

/*
 * EPI Function prototypes.
 */

short CICS_EpiInitialize (
#ifdef __STDC__
   IN u_long Version
#endif
);

short CICS_EpiTerminate (
#ifdef __STDC__
   void
#endif
);

short CICS_EpiListSystems (
#ifdef __STDC__
   IN char              *Namespace,
   INOUT u_short        *Systems,
   INOUT CICS_EpiSystem_t List []
#endif
);

short CICS_EpiAddTerminal (
#ifdef __STDC__
   IN char             *Namespace,
   IN char             System[CICS_EPI_SYSTEM_MAX + 1],
   IN char             NetName[CICS_EPI_NETNAME_MAX + 1],
   IN char             DevType[CICS_EPI_DEVTYPE_MAX + 1],
   IN CICS_EpiNotify_t NotifyFn,

   OUT CICS_EpiDetails_t *Details,
   OUT u_short         *TermIndex
#endif
);

short CICS_EpiDelTerminal (
#ifdef __STDC__
   IN u_short TermIndex
#endif
);

short CICS_EpiStartTran (
#ifdef __STDC__
   IN u_short TermIndex,
   IN char    TranId[CICS_EPI_TRANID_MAX + 1],
   IN u_byte  *Data,
   IN u_short Size
#endif
);

short CICS_EpiReply (
#ifdef __STDC__
   IN u_short TermIndex,
   IN u_byte  *Data,
   IN u_short Size
#endif
);

short CICS_EpiATIState (
#ifdef __STDC__
   IN u_short          TermIndex,
   IN CICS_EpiATIState_t *ATIState
#endif
);

short CICS_EpiSenseCode (
#ifdef __STDC__
   IN u_short             TermIndex,
   IN CICS_EpiSenseCode_t SenseCode
#endif
);

short CICS_EpiGetEvent (
#ifdef __STDC__
   IN    u_short             TermIndex,
   IN    CICS_EpiWait_t      Wait,
   INOUT CICS_EpiEventData_t *Event
#endif
);

short EpiGetSysError (
#ifdef __STDC__
   IN  u_short            TermIndex,
   OUT CICS_EpiSysError_t *SysErr
#endif
);

short kxEpiSetTrace (
#ifdef __STDC__
   IN kxepi_trace_t trace,
   IN char          *filename
#endif
);

int kxEpiTimeOut (
#ifdef __STDC__
   IN  u_short         TermIndex
#endif
);

int kxEpiCheckTimeOuts (
#ifdef __STDC__
   IN OUT  u_short         *TermStart,
   IN OUT  u_short         *TermEnd,
   OUT  char             TimeOutArray[],
   OUT  int             *NextTimeOut
#endif
);

/*
** Return Codes.
** NOTE:  If additions/deletions are made, please add/remove the entry
**        to the following array.
*/
#define CICS_EPI_OK               1     /* Complete success */
#define CICS_EPI_ERR_FAILED       2     /* CICS_EpiTerminate - Unable to
                                             terminate the EPI interface
                                           CICS_EpiAddTerminal - Unable to
                                             install the terminal
                                           CICS_EpiDelTerminal - unable to 
                                             delete the terminal
                                           CICS_EpiStartTran - unable to start
                                             the transaction
                                           CICS_EpiReply - unable to send
                                             reply data
                                           CICS_EpiATIState - unable to set or
                                             query the ATI state
                                           CICS_EpiSenseCode - unable to 
                                             return sense code error
                                           CICS_EpiGetEvent - unable to get
                                             next event
                                           CICS_EpiGetSysError - no further
                                             information is available */
#define CICS_EPI_ERR_VERSION      3     /* The EPI cannot support the version
                                           requested */
#define CICS_EPI_ERR_IS_INIT      4     /* The EPI interface is already
                                           initialized for this process */
#define CICS_EPI_ERR_NOT_INIT     5     /* CICS_EpiInitialize() has not been
                                           executed */
#define CICS_EPI_ERR_NO_SYSTEMS   6     /* No candidate systems can be
                                           located */
#define CICS_EPI_ERR_MORE_SYSTEMS 7     /* There was not enough space in the
                                           list array to store all the
                                           candidate systems */
#define CICS_EPI_ERR_SYSTEM       8     /* The specified system does not
                                           exist in the specified namespace */
#define CICS_EPI_ERR_MAX_TERMS    9     /* The maximum number of terminals
                                           support by this EPI process has
                                           been reached */
#define CICS_EPI_ERR_BAD_INDEX   10     /* The TermIndex does not represent a
                                           valid terminal */
#define CICS_EPI_ERR_TRAN_ACTIVE 11     /* A transaction is currently running
                                           for this terminal */
#define CICS_EPI_ERR_TTI_ACTIVE  12     /* A TTI transaction is active for
                                           this terminal */
#define CICS_EPI_ERR_ATI_ACTIVE  13     /* A ATI transaction is active for this
                                           terminal */
#define CICS_EPI_ERR_NO_DATA     14     /* No initial data was provided */
#define CICS_EPI_ERR_NO_CONVERSE 15     /* No converse event is outstanding */
#define CICS_EPI_ERR_ATI_STATE   16     /* Invalid ATIState value provided */
#define CICS_EPI_ERR_SENSE_CODE  17     /* Invalid sense code provided */
#define CICS_EPI_ERR_WAIT        18     /* Wait parameter is invalid */
#define CICS_EPI_ERR_NO_EVENT    19     /* No events outstanding for this
                                           terminal */
#define CICS_EPI_ERR_MORE_DATA   20     /* Data buffer supplied was not large
                                           to contain the terminal data */
#define CICS_EPI_ERR_MORE_EVENTS 21     /* An event was successfully obtained,
                                           but there are more outstanding */
#define CICS_EPI_ERR_TERMINAL_TIMEDOUT 22 /* A terminal has been exceeded
                                             the allowed idle time */

static char        ReturnCode [] [31] = { "",
                                          "CICS_EPI_OK",
                                          "CICS_EPI_ERR_FAILED",
                                          "CICS_EPI_ERR_VERSION",
                                          "CICS_EPI_ERR_IS_INIT",
                                          "CICS_EPI_ERR_NOT_INIT",
                                          "CICS_EPI_ERR_NO_SYSTEMS",
                                          "CICS_EPI_MORE_SYSTEMS",
                                          "CICS_EPI_ERR_SYSTEM",
                                          "CICS_EPI_ERR_MAX_TERMS",
                                          "CICS_EPI_ERR_BAD_INDEX",
                                          "CICS_EPI_ERR_TRAN_ACTIVE",
                                          "CICS_EPI_ERR_TTI_ACTIVE",
                                          "CICS_EPI_ERR_ATI_ACTIVE",
                                          "CICS_EPI_ERR_NO_DATA",
                                          "CICS_EPI_ERR_NO_CONVERSE",
                                          "CICS_EPI_ERR_ATI_STATE",
                                          "CICS_EPI_ERR_SENSE_CODE",
                                          "CICS_EPI_ERR_WAIT",
                                          "CICS_EPI_ERR_NO_EVENT",
                                          "CICS_EPI_ERR_MORE_DATA",
                                          "CICS_EPI_ERR_MORE_EVENTS",
                                          "CICS_EPI_ERR_TERMINAL_TIMEDOUT"
                                        };
/*
 * Device types supported.
 */
#define LU_0       "IBM-LU-0"
#define LU_1       "IBM-LU-1"
#define MODEL_2    "IBM-3278-2"
#define MODEL_2_E  "IBM-3278-2-E"
#define MODEL_4    "IBM-3278-4"
#define MODEL_4_E  "IBM-3278-4-E"
#define MODEL_5    "IBM-3278-5"
#define MODEL_5_E  "IBM-3278-5-E"

#define PRINTER    "IBM-3287"
#define PRINTER_E  "IBM-3287-E"


#endif /* _CICS_epi_h */
