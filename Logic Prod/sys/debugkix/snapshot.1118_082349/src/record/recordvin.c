/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:24:46  $ */
/* $Modtime:   23 Apr 2013 13:24:46  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   recordvin.c  $ $Revision:   1.4  $";
#endif

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/user/record/PVCS/recordvin.c_v  $
 *
 *    Rev 1.4   23 Apr 2013 13:24:46   unikix
 * Update Dell copyright
 *
 *    Rev 1.3   18 Jan 2013 08:56:38   unikix
 * Dell rebranding
 *
 *    Rev 1.2   07 Oct 2009 14:08:24   dd134127
 * Merge 1.0.1.0 and 1.1
 *
 *    Rev 1.1   13 Jan 2009 15:26:02   dd134127
 * B7000903
 * 64-bit port
 *
 *    Rev 1.0.1.0   25 Sep 2009 16:36:20   tl134147
 * B7001341 - initial Windows SUA checkin
 *
 *    Rev 1.0   31 May 2007 13:40:08   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:51:08   unikix
 * New 8.0 archive
 *
 *    Rev 1.3   15 Oct 2003 09:57:54   dv133961
 * B4825517
 * Remove warning messages generated during kixinstall
 *
 *    Rev 1.2   17 Jun 2002 16:21:06   rh134138
 * BugTraq# 4700087 # Misc Large File issues: user record readers & writers
 *
 *    Rev 1.1   26 Sep 2001 11:25:54   unikix
 * Rebrand80
 *
 *    Rev 1.1   08 May 2001 14:27:40   daved
 * Correct misspellings, and clean up compile warnings.
 *
 *    Rev 1.0   30 Jan 2000 11:47:58   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 20:00:04   unikix
 * Initial 6.0
 *
 *    Rev 1.0   05/22/97 17:33:58   unikix
 * Initial 5.1
 *
 *    Rev 1.0   04/10/96 14:14:10   unikix
 * Initial 5.0
 *
 *    Rev 1.1   11/17/95 13:36:42   daved
 * B001234
 * "%5.5d" format string gives 6 (not 5) characters when the number is negative
 * (for example "-00002").  Change the format string to "%05d", which only gives
 * 5 characters ("-0002").
 *
 *    Rev 1.0   12/28/94 15:18:42   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.0.1.0   11/18/93 16:37:04   unikix
 * UniKix 4.1 baseline (from V400m)
 *
 *    Rev 1.0   06/10/93 11:59:58   unikix
 * UniKix 3.1.2 version
 *
 *    Rev 1.0   05/21/93 16:14:40   unikix
 * UniKix 3.1.2 version
 */

/*
** Name:            RECORDV.in
**                  reads variable length record from the sequential file.
**
**                    1.0      (91/06/03)
**
** Parameters:      1: name of this program
**                  2: name of file to be read
**                  3: maximum logical record length
**                  4: "F" for fixed or "V" for variable length file.
**
** Return:          The return codes and data is returned thru standard
**                  output.  If the read is successful the number of bytes
**                  read in is placed in the first 6 bytes (5 bytes for the
**                  number and a trailing null), followed by the data.
**                  End of file is indicated by a zero record length.
**                  An error is indicated by a record length less than zero.
**
** External:        <ERRNO.H> list of errors for file operations.
**                  open UNIX close function.
**
*/

/*
recordvin.c exported routines:
        main
recordvin.c has no exported variables
recordvin.c imports:
        atoi
        close
        errno
        exit
        free
        malloc
        memcpy
        open
        read
        sprintf
        write
*/

/* ------------- unix includes ------------- */
#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#include <malloc.h>
#include <memory.h>

/* ------------- local defines ------------- */

#define OKCD       0                    /*completed ok (EOF) code */
#define OPNOK      0                    /*file was opened         */
#define STDOUTDES  1                    /*standard output file des*/
#define ERRSZ      6                    /*size of error record, 6 bytes */
#define MEMERR    -9999                 /*return code for memory error  */
#define LENERR    -9998                 /*return code for record len error */
#define OPNERR    -2                    /*return code for open error    */
#define RDERR     -3                    /*return code for read error    */
#define BUFSIZE   4096                  /*max size of pipe record       */


int main (/* ARGSUSED */argc, argv)
int argc;
char *argv[];
{
   int      lrecl;                            /*logical record length*/
   int      lrkp;                             /*key offset*/
   int      lkylth;                           /*key length*/
   char     lrecfmt;                          /*record format F=fixed V=var.*/
   char     lseq_fileid[129];                 /*file id of seq file to be read*/
   char     lerr[ERRSZ];                      /*error sting to send back */
   char    *lrecbuff;                         /*pointer to record buffer*/
   int      lfildes;                          /*input file descriptor*/
   int      loutdes;                          /*standard output file descriptor*/
   unsigned lbufl;                            /*number of bytes in buffer */
   int      lreclth;                          /*number of bytes in a record*/
   int      lnread;                           /*number of bytes actually read*/
   int      li;                               /*number of bytes written out  */
   int      lj;                               /*number of byte to write      */

   /* Get parameters from argv.                   */
   /* First one is program name and is ignored.   */
   /* Second one path name input file.            */
   /* Third one is string of logical file length. */
   /* 4th one is record format.                   */
   memcpy(lseq_fileid, argv[1], 129);
   lrecl  = atoi(argv[2]);
   memcpy((char *)&lrecfmt, argv[3], 1);
   lkylth = atoi(argv[4]);
   lrkp   = atoi(argv[5]);

   loutdes = STDOUTDES;   /* Set file descriptor to standard output */

   /* Create memory for buffer.              */
   lbufl = (unsigned)lrecl + 6;
   if((lrecbuff = malloc(lbufl)) == 0) {
      sprintf(lerr, "%05d", MEMERR);
      write(loutdes, lerr, ERRSZ);
      exit(0);
   }

   /* Open input file.                      */
#ifdef __INTERIX
   if((lfildes = open(lseq_fileid, O_RDONLY)) < 0)
#else
   if((lfildes = open64(lseq_fileid, O_RDONLY)) < 0)
#endif
   {
      sprintf(lerr, "%05d", OPNERR);
      write(loutdes, lerr, ERRSZ);
      free(lrecbuff);
      exit(errno);
   }

   /* Write startup ok to pipe.           */
   sprintf(lerr, "%05d", OPNOK);
   write(loutdes, lerr, ERRSZ);

   /* Start read on input file.           */
   /* Read while file is not eof.         */

   while ((lnread = read(lfildes, &lreclth, 4)) > 0) {
      if ((lreclth > lrecl) || (lreclth < lrkp + lkylth)) {
         sprintf(lerr, "%05d", LENERR);
         write(loutdes, lerr, ERRSZ);
         free(lrecbuff);
         exit(0);
      }

      lnread = read(lfildes, &lrecbuff[6], (unsigned)lreclth);
      /* If rcd read is shorter than it says   */
      /* or rcd len is greater than the max    */
      /* len, make len native to flag an error */
      if (lnread != lreclth) {
         sprintf(lrecbuff, "%05d", RDERR);
      } else {
         sprintf(lrecbuff, "%05d", lnread);
      }
      /* Can only write a 4096 block at a time.*/
      /* On first block we must make room for  */
      /* the six byte record size.             */
      li = (lnread < BUFSIZE - 6) ? lnread : BUFSIZE - 6;
      /* Always write out one block.           */
      write(loutdes, lrecbuff, (unsigned)li + 6);
      /* Subtract out bytes already written out*/
      lnread -= li;
      /* If record is larger than 4090 then    */
      /* continue writing to pipe in 4096      */
      /* blocks until all has been written out.*/
      while (lnread > 0) {
         /* can only write a 4096 block at a time.*/
         lj = (lnread < BUFSIZE) ? lnread : BUFSIZE;
         /* Write out data to pipe offset by 6 for*/
         /* the record length in the beginning and*/
         /* the number of bytes already written.  */
         write(loutdes, lrecbuff + 6 + li, (unsigned)lj);
         li += lj;
         lnread -= lj;
      }
   }

   /* End is indicated by a number less than*/
   /* one.  Zero is EOF, a negative number  */
   /* is an error.                          */

   if(lnread < 0) {
      /* Read error                            */
      sprintf(lerr, "%05d", RDERR);
      write(loutdes, lerr, ERRSZ);
      free(lrecbuff);
      exit(errno);
   } else {
      /* Write out EOF indicator.              */
      sprintf(lerr, "%05d", OKCD);
      write(loutdes, lerr, ERRSZ);
   }

   close(lfildes);         /* Close input file.                     */
   free(lrecbuff);         /* Free buffer memory.                   */

   return(0);
}
