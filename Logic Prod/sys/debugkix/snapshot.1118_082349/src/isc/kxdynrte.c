/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/****************************************************************************
**
** FUNCTION:    kxdynrte
**
** DESCRIPTION: User exit module called when an LU6.2 ALLOCATE command is
**              about to be executed. The exit allows the programmer to
**              change the system that the request will be routed to. This
**              exit may be used to:
**
**              1. Reroute requests to an alternative region when the
**                 primary region is unavailable
**
**              2. Distribute workload between multiple remote regions.
**
**              3. Accomplish other customer specific tasks
**
**              The associated header file 'kxdynrte.h' contains a detailed
**              list of some of the input parameters defined below. Look in
**              that file for a complete description of the use of the
**              structure and the possible values it may contain.
**
** INPUTS:      dr        - pointer to the structure passed from UniKix
**
** OUTPUTS:     none
**
** RETURN:      DR_RETURN_OK    - route the request
**              DR_RETURN_ERROR - return SYSIDERR
**
******************************************************************************/
#include <stdio.h>
#include "./kxdynrte.h"

/*
** The routine drFindField is used to return a pointer to a 3270 data stream
** field. For instance, a user may type information into a field on a BMS map.
** This routine will return a pointer to that field in the data stream so that
** the data can be compared and the transaction can be routed to an alternate
** region.
**
** The parameters are:
**     pdrptr  - the 3270 data stream
**     plength - the length of the data stream
**     prow    - the row of the field, offset from 1
**     pcolumn - the column of the field, offset from 1
**
** For example, to route to region REG1 when a user types in the data GREEN
** into a field on the top line of the screen in the 16th column to run
** transaction TRN1, execute the following code segment.
**
**     if (drGetGroup == DR_GROUP_TR) {
**        if (!memcmp(drGetTrTransid, "TRN1", 4)) {
**           lptr = drFindField(drGetTrTioa, drGetTrTioalen, 1, 16);
**           if (lptr != 0) {
**              if (!memcmp(lptr, "GREEN", 5)) {
**                 drSetRsysid("REG1");
**              }
**           }
**        }
**     }
**
** NOTE: The data that the user types in is mixed case and the drFindField
** function does not alter the case. Always copy the data into a temporary
** variable before upper or lower casing the data.
*/
char *drFindField(
#ifdef __STDC__
   char *pdrptr,
   int plength,
   int prow,
   int pcolumn
#endif
);

/*
** The kxdynrte_debug function is used when debugging a dynamic routing exit.
** Uncomment the code around the calls to kxdynrte_debug in the code below and
** recompile the transaction processor. Every time the exit is called, a list
** of the parameters in the structure is displayed along with the returned
** sysid and return code when exiting.
*/
void drDebug(
#ifdef __STDC__
   kxdynrte_t *dr,
   kxdynrte_return_t exitrc,
   int entering
#endif
);

kxdynrte_return_t
#ifdef __STDC__
kxdynrte(kxdynrte_t *dr)
#else
kxdynrte(dr)
kxdynrte_t *dr;
#endif
{
   kxdynrte_return_t rc = DR_RETURN_OK;

   /*
   ** For debug purposes, uncomment the following code
   */
   /*
   drDebug(dr, rc, DR_TRUE);
   */

   /*
   ** Care must be taken to avoid causing loops when running the dynamic
   ** routing exit. The conditions that UniKix uses to stop executing the
   ** exit are that:
   **
   ** 1. The LU6.2 ALLOCATE command for a route request executed successfully
   **
   ** 2. The exit returns the error DR_RETURN_ERROR
   **
   ** 3. The Returned Sysid field contains blanks.
   **
   ** UniKix will continue to execute the exit until one of these conditions
   ** is not true.
   **
   ** To avoid excessive looping, the count field can be used to return
   ** an error to UniKix if the exit has been called a specified number of
   ** times.
   **
   ** The default action of the Dynamic Routing exit is to return with a
   ** DR_RETURN_OK return code. This causes UniKix to try to route to the
   ** default remote region. If the route fails the exit will not be
   ** reexecuted because the Returned Sysid is set to its default of blanks.
   */

   /*
   ** For debug purposes, uncomment the following code
   */
   /*
   drDebug(dr, rc, DR_FALSE);
   */

   return(rc);

} /* end of kxdynrte */


/***********************************************************************
**
** FunctionName: drDebug
**
** Description:  Print out debug information from the structure passed
**               to the dynamic routing exit.
**
** INPUTS:       dr        pointer to the dynamic routing structure
**               exitrc    the return code from the exit
**               entering  whether UniKix is entering or exiting the exit
**
** OUTPUTS:
**
************************************************************************/
void
#ifdef __STDC__
drDebug(kxdynrte_t *dr, kxdynrte_return_t exitrc, int entering)
#else
drDebug(dr, exitrc, entering)
kxdynrte_t *dr;
kxdynrte_return_t exitrc;
int entering;
#endif
{
   if (entering) {
      printf("\nEntering Dynamic Routing debug\n");
      printf("%14.14s : %s\n", "Reason", kxdynrte_reason_s[drGetReason]);
      printf("%14.14s : %s\n", "Group", kxdynrte_group_s[drGetGroup]);
      printf("%14.14s : %s\n", "Command", kxdynrte_command_s[drGetCommand]);
      printf("%14.14s : %d\n", "Count", drGetCount);
      printf("%14.14s : %4.4s\n", "Command Sysid", drGetCsysid);
      printf("%14.14s : %4.4s\n", "Local Sysid", drGetLsysid);

      switch (drGetGroup) {

         case DR_GROUP_DPL:
            printf("%14.14s : %8.8s\n", "Program", drGetDplProgram);

            if (drGetDplParms & DR_DPL_COMMAREA) {
               printf("%14.14s : %p\n", "Commarea", drGetDplCommarea);
            }
            if (drGetDplParms & DR_DPL_LENGTH) {
               printf("%14.14s : %d\n", "Length", drGetDplLength);
            }
            if (drGetDplParms & DR_DPL_DATALENGTH) {
               printf("%14.14s : %d\n", "Datalength", drGetDplDatalength);
            }
            if (drGetDplParms & DR_DPL_SYNCONRETURN) {
               printf("%14.14s :\n", "Synconreturn");
            }
            if (drGetDplParms & DR_DPL_TRANSID) {
               printf("%14.14s : %4.4s\n", "Transid", drGetDplTransid);
            }
            break;

         case DR_GROUP_VSAM:
            printf("%14.14s : %8.8s\n", "File", drGetVsamFile);

            if (drGetVsamParms & DR_VSAM_FROM) {
               printf("%14.14s : %p\n", "From", drGetVsamFrom);
            }
            if (drGetVsamParms & DR_VSAM_LENGTH) {
               printf("%14.14s : %d\n", "Length", drGetVsamLength);
            }
            if (drGetVsamParms & DR_VSAM_RIDFLD) {
               printf("%14.14s : %p\n", "Ridfld", drGetVsamRidfld);
            }
            if (drGetVsamParms & DR_VSAM_KEYLENGTH) {
               printf("%14.14s : %d\n", "Keylength", drGetVsamKeylength);
            }
            if (drGetVsamParms & DR_VSAM_RBA) {
               printf("%14.14s :\n", "Rba");
            }
            if (drGetVsamParms & DR_VSAM_RRN) {
               printf("%14.14s :\n", "Rrn");
            }
            break;

         case DR_GROUP_TSQ:
            printf("%14.14s : %8.8s\n", "Queue", drGetTsqQueue);

            if (drGetTsqParms & DR_TSQ_FROM) {
               printf("%14.14s : %p\n", "From", drGetTsqFrom);
            }
            if (drGetTsqParms & DR_TSQ_LENGTH) {
               printf("%14.14s : %d\n", "Length", drGetTsqLength);
            }
            break;

         case DR_GROUP_TDQ:
            printf("%14.14s : %4.4s\n", "Queue", drGetTdqQueue);

            if (drGetTdqParms & DR_TDQ_FROM) {
               printf("%14.14s : %p\n", "From", drGetTdqFrom);
            }
            if (drGetTdqParms & DR_TDQ_LENGTH) {
               printf("%14.14s : %d\n", "Length", drGetTdqLength);
            }
            break;

         case DR_GROUP_AP:
            if (drGetApParms & DR_AP_TRANSID) {
               printf("%14.14s : %4.4s\n", "Transid", drGetApTransid);
            }
            if (drGetApParms & DR_AP_FROM) {
               printf("%14.14s : %p\n", "From", drGetApFrom);
            }
            if (drGetApParms & DR_AP_LENGTH) {
               printf("%14.14s : %d\n", "Length", drGetApLength);
            }
            if (drGetApParms & DR_AP_REQID) {
               printf("%14.14s : %8.8s\n", "Reqid", drGetApReqid);
            }
            if (drGetApParms & DR_AP_TERMID) {
               printf("%14.14s : %4.4s\n", "Termid", drGetApTermid);
            }
            if (drGetApParms & DR_AP_RTRANSID) {
               printf("%14.14s : %4.4s\n", "Rtransid", drGetApRtransid);
            }
            if (drGetApParms & DR_AP_RTERMID) {
               printf("%14.14s : %4.4s\n", "Rtermid", drGetApRtermid);
            }
            if (drGetApParms & DR_AP_QUEUE) {
               printf("%14.14s : %8.8s\n", "Queue", drGetApQueue);
            }
            break;

         case DR_GROUP_TR:
            printf("%14.14s : %4.4s\n", "Termid", drGetTrTermid);
            printf("%14.14s : %4.4s\n", "Transid", drGetTrTransid);

            if (drGetTrParms & DR_TR_TIOA) {
               printf("%14.14s : %p\n", "Tioa", drGetTrTioa);
            }
            if (drGetTrParms & DR_TR_TIOALEN) {
               printf("%14.14s : %d\n", "Tioalen", drGetTrTioalen);
            }
            if (drGetTrParms & DR_TR_TCTUA) {
               printf("%14.14s : %p\n", "Tctua", drGetTrTctua);
            }
            if (drGetTrParms & DR_TR_TCTUALEN) {
               printf("%14.14s : %d\n", "Tctualen", drGetTrTctualen);
            }
            if (drGetTrParms & DR_TR_COMMAREA) {
               printf("%14.14s : %p\n", "Commarea", drGetTrCommarea);
            }
            if (drGetTrParms & DR_TR_COMMAREALEN) {
               printf("%14.14s : %d\n", "Commarealen", drGetTrCommarealen);
            }
            break;
      }

   } else {
      printf("Exiting  Dynamic Routing debug\n");
      printf("%14.14s : %s\n", "Return Code", kxdynrte_return_s[exitrc]);
      printf("%14.14s : %4.4s\n", "Returned Sysid", drGetRsysid);
   }
}

/*
** User exit function being invoked after a TR request has terminated.
*/
kxdynrte_end_return_t
#ifdef __STDC__
kxdynrte_end(kxdynrte_t *dr)
#else
kxdynrte_end(dr)
kxdynrte_t *dr;
#endif
{
  kxdynrte_end_return_t rc = DR_END_NOP;

  return(rc);
}
