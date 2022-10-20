/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/****************************************************************************
**
** HEADER: kxdynrte
**
** DESCRIPTION:
**         This is the header file for the Dynamic Routing Exit. See the
**         file 'kxdynrte.c' for an explanation of how to use the exit.
**
******************************************************************************/

#ifndef _KXDYNRTE_H
#define _KXDYNRTE_H

#ifndef LEN_TSQ
#define LEN_TSQ 16
#endif

/*
** The return code is used to inform UniKix of what action to perform on
** return from the exit.
**
** The possible return codes are as follows:
**
** DR_RETURN_OK
**     Try the ALLOCATE and if it fails, restart the exit
**
** DR_RETURN_ERROR
**     Pass back a SYSIDERR to the calling program
*/
typedef enum {
   DR_RETURN_OK,
   DR_RETURN_ERROR
} kxdynrte_return_t;

static char kxdynrte_return_s [] [20] = {
   "DR_RETURN_OK",
   "DR_RETURN_ERROR"
};

typedef enum {
   DR_END_NOP = 0,
   DR_END_OVRD_TIOA, /* override TIOA only                        */
   DR_END_WRT_TIOA,  /* override TIOA, set WCI to no-ERASE WRITE  */
   DR_END_EWRT_TIOA  /* override TIOA, set WCI to    ERASE WRITE  */
} kxdynrte_end_return_t;

#define KXDYNRTE_END_RETURN_S_SIZE 4
static char kxdynrte_end_return_s [KXDYNRTE_END_RETURN_S_SIZE] [20] = {
   "DR_END_NOP",
   "DR_END_OVRD_TIOA",
   "DR_END_WRT_TIOA",
   "DR_END_EWRT_TIOA"
};

typedef enum {
   DR_TERMINATION_CODE_OK = 0,
   DR_TERMINATION_CODE_NOK
} kxdynrte_end_termination_code_t;

#define KXDYNRTE_END_TERMINATION_CODE_SIZE 2
static char kxdynrte_end_termination_code_s [KXDYNRTE_END_TERMINATION_CODE_SIZE] [30] = {
   "DR_TERMINATION_CODE_OK",
   "DR_TERMINATION_CODE_NOK"
};

/*
** The reason code is used to inform the exit why it has been started. The
** code DR_REASON_FIRSTTIME is used the first time into the routine. All the
** other codes will be used in error situations to restart the exit. For
** example, if the exit is used to route a request to a region but this region
** is not currently available, a DR_REASON_OUTSERVICE may be returned and will
** be used as the reason code when the exit is restarted. The exit is then able
** to reject the request, causing the program to return with a SYSIDERR, or to
** try to route to an alternate region.
**
** The possible reason codes are as follows:
**
** DR_REASON_FIRSTTIME
**     The first time into the exit for this request
**
** DR_REASON_NOTDEFINED
**     The SYSID was neither defined in the TCT - System Entries table
**     nor autoinstalled
**
** DR_REASON_OUTSERVICE
**     The connection was out of service
*/
typedef enum {
   DR_REASON_FIRSTTIME,
   DR_REASON_NOTDEFINED,
   DR_REASON_OUTSERVICE
} kxdynrte_reason_t;

static char kxdynrte_reason_s [] [21] = {
   "DR_REASON_FIRSTTIME",
   "DR_REASON_NOTDEFINED",
   "DR_REASON_OUTSERVICE"
};

/*
** The group is the type of outbound ISC request that is being executed
**
** The possible types are as follows:
**
** DR_GROUP_DPL
**     A Distributed Program Link (DPL) request
**
** DR_GROUP_VSAM
**     A Function Ship of a VSAM file request
**
** DR_GROUP_TSQ
**     A Function Ship of a Temporary Storage Queue request
**
** DR_GROUP_TDQ
**     A Function Ship of a Transient Data Queue request
**
** DR_GROUP_AP
**     Asynchronous Processing. A Function Ship of an EXEC CICS START request
**
** DR_GROUP_TR
**     A Transaction Route (TR) request
**
*/
typedef enum {
   DR_GROUP_DPL,
   DR_GROUP_VSAM,
   DR_GROUP_TSQ,
   DR_GROUP_TDQ,
   DR_GROUP_AP,
   DR_GROUP_TR
} kxdynrte_group_t;

static char kxdynrte_group_s [] [14] = {
   "DR_GROUP_DPL",
   "DR_GROUP_VSAM",
   "DR_GROUP_TSQ",
   "DR_GROUP_TDQ",
   "DR_GROUP_AP",
   "DR_GROUP_TR"
};


/*
** The command is the EXEC CICS command being executed.  For Transaction
** Route, the command is set to DR_COMMAND_NONE.
*/
typedef enum {
   DR_COMMAND_NONE,
   DR_COMMAND_LINK,
   DR_COMMAND_DELETE,
   DR_COMMAND_ENDBR,
   DR_COMMAND_READ,
   DR_COMMAND_READNEXT,
   DR_COMMAND_READPREV,
   DR_COMMAND_RESETBR,
   DR_COMMAND_REWRITE,
   DR_COMMAND_STARTBR,
   DR_COMMAND_UNLOCK,
   DR_COMMAND_WRITE,
   DR_COMMAND_DELETEQ,
   DR_COMMAND_WRITEQ,
   DR_COMMAND_READQ,
   DR_COMMAND_START,
   DR_COMMAND_CANCEL
} kxdynrte_command_t;

static char kxdynrte_command_s [] [20] = {
   "DR_COMMAND_NONE",
   "DR_COMMAND_LINK",
   "DR_COMMAND_DELETE",
   "DR_COMMAND_ENDBR",
   "DR_COMMAND_READ",
   "DR_COMMAND_READNEXT",
   "DR_COMMAND_READPREV",
   "DR_COMMAND_RESETBR",
   "DR_COMMAND_REWRITE",
   "DR_COMMAND_STARTBR",
   "DR_COMMAND_UNLOCK",
   "DR_COMMAND_WRITE",
   "DR_COMMAND_DELETEQ",
   "DR_COMMAND_WRITEQ",
   "DR_COMMAND_READQ",
   "DR_COMMAND_START",
   "DR_COMMAND_CANCEL"
};

/*
** Locally used defines
*/
#define DR_TRUE                   1
#define DR_FALSE                  0

/*
** The following defines are to populate the 'parms' field for the various
** groups. Always check that the option has been supplied to the routine
** before trying to use it.
**
** For example to check that the COMMAREA is available for use on a DPL
** request, code the following:
**
**    unsigned char *lptr;
**    if (drGetDplParms & DR_DPL_COMMAREA) {
**       if (!memcmp(drGetDplCommarea, ...
*/
#define DR_DPL_COMMAREA           0x01
#define DR_DPL_LENGTH             0x02
#define DR_DPL_DATALENGTH         0x04
#define DR_DPL_SYNCONRETURN       0x08
#define DR_DPL_TRANSID            0x10

#define DR_VSAM_FROM              0x01
#define DR_VSAM_LENGTH            0x02
#define DR_VSAM_RIDFLD            0x04
#define DR_VSAM_KEYLENGTH         0x08
#define DR_VSAM_RBA               0x10
#define DR_VSAM_RRN               0x20

#define DR_TSQ_FROM               0x01
#define DR_TSQ_LENGTH             0x02

#define DR_TDQ_FROM               0x01
#define DR_TDQ_LENGTH             0x02

#define DR_AP_TRANSID             0x01
#define DR_AP_FROM                0x02
#define DR_AP_LENGTH              0x04
#define DR_AP_REQID               0x08
#define DR_AP_TERMID              0x10
#define DR_AP_RTRANSID            0x20
#define DR_AP_RTERMID             0x40
#define DR_AP_QUEUE               0x80

#define DR_TR_TIOA                0x01
#define DR_TR_TIOALEN             0x02
#define DR_TR_TCTUA               0x04
#define DR_TR_TCTUALEN            0x08
#define DR_TR_COMMAREA            0x10
#define DR_TR_COMMAREALEN         0x20

/*
** Set of useful macros to get the fields within the structures. If the field
** is a pointer, a pointer to the field is returned. Otherwise, the value of
** the field is returned. This is the recommended method of accessing the
** structures as in future releases, the format of the structure may change.
*/
#define drGetReason               (dr->reason)
#define drGetGroup                (dr->group)
#define drGetCommand              (dr->command)
#define drGetCount                (dr->count)
#define drGetCsysid               (dr->csysid)
#define drGetLsysid               (dr->lsysid)
#define drGetRsysid               (dr->rsysid)

#define drGetDplParms             (dr->drUnion.dpl.parms)
#define drGetDplProgram           (dr->drUnion.dpl.program)
#define drGetDplCommarea          (dr->drUnion.dpl.commarea)
#define drGetDplLength            (dr->drUnion.dpl.length)
#define drGetDplDatalength        (dr->drUnion.dpl.datalength)
#define drGetDplTransid           (dr->drUnion.dpl.transid)

#define drGetVsamParms            (dr->drUnion.vsam.parms)
#define drGetVsamFile             (dr->drUnion.vsam.file)
#define drGetVsamFrom             (dr->drUnion.vsam.from)
#define drGetVsamLength           (dr->drUnion.vsam.length)
#define drGetVsamRidfld           (dr->drUnion.vsam.ridfld)
#define drGetVsamKeylength        (dr->drUnion.vsam.keylength)

#define drGetTsqParms             (dr->drUnion.tsq.parms)
#define drGetTsqQueue             (dr->drUnion.tsq.queue)
#define drGetTsqFrom              (dr->drUnion.tsq.from)
#define drGetTsqLength            (dr->drUnion.tsq.length)

#define drGetTdqParms             (dr->drUnion.tdq.parms)
#define drGetTdqQueue             (dr->drUnion.tdq.queue)
#define drGetTdqFrom              (dr->drUnion.tdq.from)
#define drGetTdqLength            (dr->drUnion.tdq.length)

#define drGetApParms              (dr->drUnion.ap.parms)
#define drGetApTransid            (dr->drUnion.ap.transid)
#define drGetApFrom               (dr->drUnion.ap.from)
#define drGetApLength             (dr->drUnion.ap.length)
#define drGetApReqid              (dr->drUnion.ap.reqid)
#define drGetApTermid             (dr->drUnion.ap.termid)
#define drGetApRtransid           (dr->drUnion.ap.rtransid)
#define drGetApRtermid            (dr->drUnion.ap.rtermid)
#define drGetApQueue              (dr->drUnion.ap.queue)

#define drGetTrParms              (dr->drUnion.tr.parms)
#define drGetTrTermid             (dr->drUnion.tr.termid)
#define drGetTrTransid            (dr->drUnion.tr.transid)
#define drGetTrTioa               (dr->drUnion.tr.tioa)
#define drGetTrTioalen            (dr->drUnion.tr.tioalen)
#define drGetTrTctua              (dr->drUnion.tr.tctua)
#define drGetTrTctualen           (dr->drUnion.tr.tctualen)
#define drGetTrCommarea           (dr->drUnion.tr.commarea)
#define drGetTrCommarealen        (dr->drUnion.tr.commarealen)
#define drGetTrTerminationCode    (dr->drUnion.tr.terminationcode)

/*
** Use the drSetRsysid macro to set the returned sysid. The drSet function is
** called so that the exit can pass in a null terminated string. The rsysid
** field must be space filled.
**
** For example, to route all requests to S2 if routes to S1 have failed
** execute the following:
**
**    if (drGetCount > 2)                      - So that if the ALLOCATE to S2
**       return(DR_RETURN_ERROR);              - fails, the exit doesn't loop
**    if (drGetReason == DR_REASON_FIRSTTIME)
**       drSetRsysid("S1");
**    else
**       drSetRsysid("S2");
**    return(DR_RETURN_OK);
*/
#define drSetRsysid(a)            drSet(dr->rsysid, a, 4)

/*
** Structure passed to the Data-Dependent Dynamic Routing Exit.
*/
typedef struct kxdynrte_s {

   kxdynrte_reason_t  reason;
   kxdynrte_group_t   group;
   kxdynrte_command_t command;
   int                count;
   char               csysid[4];  /* SYSID specified on the command       */
   char               lsysid[4];  /* Local SYSID taken from the SIT Table */
   char               rsysid[4];  /* SYSID to ALLOCATE on                 */

   union {
      /*
      ** The structure for Distributed Program Link (DPL)
      */
      struct {
         int  parms;
         char program[8];
         char *commarea;
         int  length;
         int  datalength;
         char transid[4];
      } dpl;

      /*
      ** The structure for Function Shipping VSAM files
      */
      struct {
         int  parms;
         char file[8];
         char *from;
         int  length;
         char *ridfld;
         int  keylength;
      } vsam;

      /*
      ** The structure for Function Shipping Temporary Storage Queues
      */
      struct {
         int  parms;
         char queue[LEN_TSQ];
         char *from;
         int  length;
      } tsq;

      /*
      ** The structure for Function Shipping Transient Data Queues
      */
      struct {
         int  parms;
         char queue[4];
         char *from;
         int  length;
      } tdq;

      /*
      ** The structure for Asynchronous Processing
      */
      struct {
         int  parms;
         char transid[4];
         char *from;
         int  length;
         char reqid[8];
         char termid[4];
         char rtransid[4];
         char rtermid[4];
         char queue[LEN_TSQ];
      } ap;

      /*
      ** The structure for Transaction Routing
      */
      struct {
         int  parms;
         char termid[4];
         char transid[4];
         char *tioa;
         int  tioalen;
         char *tctua;
         int  tctualen;
         char *commarea;
         int  commarealen;
         int  terminationcode;
      } tr;

   } drUnion;

} kxdynrte_t;

kxdynrte_return_t kxdynrte();
kxdynrte_end_return_t kxdynrte_end(kxdynrte_t *);

#endif
