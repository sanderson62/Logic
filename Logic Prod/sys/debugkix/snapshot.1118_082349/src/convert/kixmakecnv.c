/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:24:40  $ */
/* $Modtime:   23 Apr 2013 13:24:40  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   kixmakecnv.c  $ $Revision:   1.3  $";
#endif

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/user/convert/PVCS/kixmakecnv.c_v  $
 *
 *    Rev 1.3   23 Apr 2013 13:24:40   unikix
 * Update Dell copyright
 *
 *    Rev 1.2   18 Jan 2013 08:56:28   unikix
 * Dell rebranding
 *
 *    Rev 1.1   13 Jan 2009 15:25:58   dd134127
 * B7000903
 * 64-bit port
 *
 *    Rev 1.0   31 May 2007 13:40:04   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:50:42   unikix
 * New 8.0 archive
 *
 *    Rev 1.0.1.0   15 Oct 2003 09:51:56   dv133961
 * B4825517
 * Remove warning messages generated during kixinstall
 *
 *    Rev 1.0   19 Sep 2001 16:47:36   unikix
 * Initial 7.2
 *
 *    Rev 1.4   25 Apr 2001 13:51:24   calumm
 * D007705
 * Use new conversion tables.
 *
 *    Rev 1.3   21 Aug 2000 16:59:26   randyh
 * Defect# 6781 Correct quote problem in the COBOL translator
 *
 *    Rev 1.2   19 May 2000 08:22:50   daved
 * Remove references to UniKix.
 *
 *    Rev 1.1   03 May 2000 14:38:56   daved
 * D004051
 * Put DBCS changes into TRANS
 *
 *    Rev 1.0   30 Jan 2000 11:47:48   unikix
 * Initial 7.0
 *
 *    Rev 1.1   18 Aug 1999 10:06:12   daved
 * D004051
 * Support for DBCS conversion files
 *
 *    Rev 1.0   15 Mar 1999 19:59:54   unikix
 * Initial 6.0
 *
 *    Rev 1.0   05/22/97 17:33:18   unikix
 * Initial 5.1
 *
 *    Rev 1.4   02/26/97 10:59:20   daved
 * Merge 1.2.1.0 and 1.3
 *
 *    Rev 1.3   02/21/97 16:16:48   prabha
 * B002009
 * Changes to use getenv instead of kxgetenv.
 *
 *    Rev 1.2.1.0   02/25/97 09:57:42   nialb
 * B002017
 * case #2017
 * handle aid support for foreign char sets
 *
 *    Rev 1.2   10/24/96 10:37:28   daved
 * DCAsec
 * Undo revision 1.1 changes.
 *
 *    Rev 1.0   04/10/96 14:13:14   unikix
 * Initial 5.0
 *
 *    Rev 1.2   08/18/95 13:32:24   rick
 * removed initialization from xmyname...caused problem in building unikixi
 *
 *    Rev 1.1   08/16/95 16:35:40   rick
 * sequent port - added definition for 'xmyname'
 *
 *    Rev 1.0   12/28/94 15:18:20   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.2   11/11/94 08:05:08   alan
 * Minimize include reference
 *
 *    Rev 1.1   11/03/94 10:02:04   alan
 * A few corrections; delete debug stuff
 *
 *    Rev 1.0   10/26/94 10:04:56   alan
 * Initial revision.
*/

/*
kixmakecnv.c exported routines:
        main
kixmakecnv.c exported variables:
kixmakecnv.c imports:
*/

/* ------------- unix includes ------------- */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/msg.h>
#include <stdlib.h>
#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#include <errno.h>
#include <string.h>
#include <memory.h>
#include <malloc.h>
#include <stddef.h>
#include <unistd.h>

/* ------------ Product includes ------------ */

#define PATHMAX 129
#define PRODNAME "UNIKIX"

#include "kxcnv.h"

extern unsigned char Lwr2upr[256];
extern unsigned char Bit2ebcd[256];
extern unsigned char User_Asc2ebcd[256];
extern unsigned char User_Ebcd2asc[256];
extern unsigned char System_Asc2ebcd[256];
extern unsigned char System_Ebcd2asc[256];

static struct kxcnvtbl_rec hcnvrec;
#define SPACES  "                              "

char xmyname[20];

int main ()
{
   char    *lenv;
   int     lerrno;
   int     lfildes;
   char    lpath[PATHMAX];
   int     lrc;
   int     loop_ctr;

   struct l_standard_aid_values {
      unsigned char  ebcdic;
      unsigned char  ascii;
      char *cobol_variable;
   };

#define AID_STRUCT_DELIM "Z"

   static struct l_standard_aid_values l_std_aid_val[] = {
      {0x60, 0x2d, "DFHNULL"},
      {0x7d, 0x27, "DFHENTER"},
      {0x6d, 0x5f, "DFHCLEAR"},
      {0x6a, 0x7c, "DFHCLRP"},
      {0x7e, 0x3d, "DFHPEN"},
      {0xe6, 0x57, "DFHOPID"},
      {0xe7, 0x58, "DFHMSRE"},
      {0x88, 0x68, "DFHSTRF"},
      {0x7f, 0x22, "DFHTRIG"},
      {0x6c, 0x25, "DFHPA1"},
      {0x6e, 0x3E, "DFHPA2"},
      {0x6b, 0x2c, "DFHPA3"},
      {0xf1, 0x31, "DFHPF1"},
      {0xf2, 0x32, "DFHPF2"},
      {0xf3, 0x33, "DFHPF3"},
      {0xf4, 0x34, "DFHPF4"},
      {0xf5, 0x35, "DFHPF5"},
      {0xf6, 0x36, "DFHPF6"},
      {0xf7, 0x37, "DFHPF7"},
      {0xf8, 0x38, "DFHPF8"},
      {0xf9, 0x39, "DFHPF9"},
      {0x7a, 0x3a, "DFHPF10"},
      {0x7b, 0x23, "DFHPF11"},
      {0x7c, 0x40, "DFHPF12"},
      {0xc1, 0x41, "DFHPF13"},
      {0xc2, 0x42, "DFHPF14"},
      {0xc3, 0x43, "DFHPF15"},
      {0xc4, 0x44, "DFHPF16"},
      {0xc5, 0x45, "DFHPF17"},
      {0xc6, 0x46, "DFHPF18"},
      {0xc7, 0x47, "DFHPF19"},
      {0xc8, 0x48, "DFHPF20"},
      {0xc9, 0x49, "DFHPF21"},
      {0x4a, 0x5b, "DFHPF22"},
      {0x4b, 0x2e, "DFHPF23"},
      {0x4c, 0x3c, "DFHPF24"},
      {0x00, 0x00, AID_STRUCT_DELIM}
   };

   strcpy(xmyname, "kixmakecnv");
   memcpy(hcnvrec.cnv_name, "cnvtbl  ", 8);
   hcnvrec.cnv_magic1 = 0x12345678;
   memcpy(hcnvrec.xlwr2upr, Lwr2upr, 256);
   memcpy(hcnvrec.xbit2ebcd, Bit2ebcd, 64);
   memcpy(hcnvrec.xuser_asc2ebcd, User_Asc2ebcd, 256);
   memcpy(hcnvrec.xuser_ebcd2asc, User_Ebcd2asc, 256);
   memcpy(hcnvrec.xsystem_asc2ebcd, System_Asc2ebcd, 256);
   memcpy(hcnvrec.xsystem_ebcd2asc, System_Ebcd2asc, 256);
   hcnvrec.cnv_magic2 = (unsigned)0x87654321;

   lenv = getenv(PRODNAME);
   sprintf(lpath, "%s%s", lenv, "/lib/cnvtbl");
   lfildes = open(lpath, O_RDWR | O_CREAT, 0644);
   if (lfildes == -1) {
      lerrno = errno;
      printf("errno %d from open of %s\n", lerrno, lpath);
      if (lerrno == ENOENT) {
         printf("The path to the file does not exist\n");
      }
      exit(-1);
   }
   lrc = write(lfildes, &hcnvrec, offsetof(struct kxcnvtbl_rec, xnls_asc2ebcd[0]));
   if (lrc != -1) {
      lrc = write(lfildes, &hcnvrec.cnv_magic2, sizeof(int));
   }
   if (lrc == -1) {
      lerrno = errno;
      printf("errno %d from write of %s\n", lerrno, lpath);
      exit(-1);
   }
   lrc = close(lfildes);
   if (lrc == -1) {
      lerrno = errno;
      printf("errno %d from close of %s\n", lerrno, lpath);
   }

   for (loop_ctr = 0;
         * (l_std_aid_val[loop_ctr].cobol_variable) != 'Z';
         loop_ctr++) {
      if (System_Ebcd2asc[l_std_aid_val[loop_ctr].ebcdic] !=
            l_std_aid_val[loop_ctr].ascii) {
         printf("Warning: you must change the variable %s in $UNIKIX/copy/DFHAID to a '%c' (ascii 0x%x)\n", l_std_aid_val[loop_ctr].cobol_variable,
                l_std_aid_val[loop_ctr].ascii,
                l_std_aid_val[loop_ctr].ascii);
      }
   }

   return(0);
}
