/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:24:50  $ */
/* $Modtime:   23 Apr 2013 13:24:50  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   kximsexit.c  $ $Revision:   1.3  $";
#endif

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/user/trans/PVCS/kximsexit.c_v  $
 *
 *    Rev 1.3   23 Apr 2013 13:24:50   unikix
 * Update Dell copyright
 *
 *    Rev 1.2   18 Jan 2013 08:56:44   unikix
 * Dell rebranding
 *
 *    Rev 1.1   18 Nov 2010 14:12:24   dd134127
 * Fix compile errors on non-Linux platforms.
 *
 *    Rev 1.0   22 Jun 2010 16:50:34   gr134150
 * Initial revision.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>             /*defines getenv, malloc*/
#include <errno.h>
#include <unistd.h>

static char hkxImsMTObuffer[4096];

/*
 * MODIFY:
 *
 * The following MOD name must be modified based on application definition.
 *
 */
#define KXIMS_DFSMO1 "DFSMO1  "

/* Example of expected input message structure.
 *
 *     01  MO-MTO.
 *         03  MO-LL                    PIC S999    COMP  VALUE +119.
 *         03  MO-ZZ                    PIC S99     COMP  VALUE ZEROS.
 *         03  FILLER                   PIC X(19)   VALUE 'UNRECOVERABLE ERROR'.
 *         03  MO-PGM                   PIC X(9).
 *         03  MO-RTN                   PIC X(9).
 *         03  MO-REG                   PIC X(10).
 *         03  MO-INF-ERROR.
 *             05  MO-RET-CODE          PIC XXX.
 *             05  MO-SEG               PIC X(9).
 *             05  MO-KEY               PIC X(21).
 *         03  FILLER                   PIC X(35)   VALUE SPACES.
 */

/*
 * MODIFY:
 *
 * The following message structure must be modified based on the specific application input formatting definition.
 *
 * The following is an example of how a structure similar to the above MO-MTO should be remapped into.
 *
 */
struct kxImsMTO_DFSMO1 {
   char mto_ll[2];
   char mto_zz[2];
   char mto_err1[19];
   char mto_pgm[9];
   char mto_rtn[9];
   char mto_reg[10];
   char mto_retcode[3];
   char mto_seg[9];
   char mto_key[21];
   char mto_err2[35];
};

static struct kxImsMTO_DFSMO1 hkxImsMTOArea;

char *kxImsFormatMsgToMTO(char *pmodname, char *parea, unsigned short plen)
{
   int lret = -1;
   if(plen != sizeof(struct kxImsMTO_DFSMO1)) { /* received an unexpected structure size */
      return(NULL);
   }
   if(strncmp(pmodname, KXIMS_DFSMO1, 8)) { /* received an invalid MOD name */
      return(NULL);
   }
   lret = 0;
   memcpy(&hkxImsMTOArea, parea, sizeof(struct kxImsMTO_DFSMO1));

   /*
    * MODIFY:
    *
    * The following formatted output must be modified based on the specific application output formatting definition.
    * Not to exceed a system imposed limit of 220 bytes.
    *
    */
   sprintf(hkxImsMTObuffer, "MTO: %19.19s PGM=%9.9s RTN=%9.9s REG=%10.10s RET-CODE=%3.3s SEGMENT=%9.9s KEY=%21.21s %35.35s\n",
           hkxImsMTOArea.mto_err1,
           hkxImsMTOArea.mto_pgm,
           hkxImsMTOArea.mto_rtn,
           hkxImsMTOArea.mto_reg,
           hkxImsMTOArea.mto_retcode,
           hkxImsMTOArea.mto_seg,
           hkxImsMTOArea.mto_key,
           hkxImsMTOArea.mto_err2);

   return(hkxImsMTObuffer);
}

