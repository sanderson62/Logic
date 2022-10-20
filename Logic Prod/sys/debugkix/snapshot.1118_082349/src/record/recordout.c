/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:24:44  $ */
/* $Modtime:   23 Apr 2013 13:24:44  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   recordout.c  $ $Revision:   1.4  $";
#endif

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/user/record/PVCS/recordout.c_v  $
 *
 *    Rev 1.4   23 Apr 2013 13:24:44   unikix
 * Update Dell copyright
 *
 *    Rev 1.3   18 Jan 2013 08:56:38   unikix
 * Dell rebranding
 *
 *    Rev 1.2   07 Oct 2009 14:07:20   dd134127
 * Merge 1.0.1.0 and 1.1
 *
 *    Rev 1.1   13 Jan 2009 15:26:00   dd134127
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
 *    Rev 1.3   15 Oct 2003 09:57:20   dv133961
 * B4825517
 * Remove warning messages generated during kixinstall
 *
 *    Rev 1.2   17 Jun 2002 16:23:44   rh134138
 * BugTraq# 4700087 # Misc Large File issues: user record readers & writers
 *
 *    Rev 1.1   26 Sep 2001 11:25:54   unikix
 * Rebrand80
 *
 *    Rev 1.1   08 May 2001 14:24:38   daved
 * Correct misspellings, and clean up compile warnings.
 *
 *    Rev 1.0   30 Jan 2000 11:47:58   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 20:00:02   unikix
 * Initial 6.0
 *
 *    Rev 1.0   05/22/97 17:33:56   unikix
 * Initial 5.1
 *
 *    Rev 1.0   04/10/96 14:14:08   unikix
 * Initial 5.0
 *
 *    Rev 1.0   12/28/94 15:18:42   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.0.1.0   11/18/93 16:36:56   unikix
 * UniKix 4.1 baseline (from V400m)
 *
 *    Rev 1.0   06/10/93 11:59:52   unikix
 * UniKix 3.1.2 version
 *
 *    Rev 1.0   05/21/93 16:14:38   unikix
 * UniKix 3.1.2 version
 */

/*
** Name:            RECORD.out
**                  writes a fixed length record to a sequential file.
**
**                    1.0      (90/12/17)
**
** Parameters:      1: name of this program
**                  2: name of file to be written
**                  3: logical record length "99999"
**                  4: "F" for fixed or "V" for variable length file.
**
** Return:          The return codes and data is returned thru standard
**                  back.  The data is sent via standard output. The length
**                  is placed in the first 6 bytes (5 bytes for the
**                  number and a trailing null), followed by the data.
**                  End of file is indicated by a zero record length.
**                  The pipe is one-way so an error can not be reported
**                  back to the parent procedure.  If there is a error the
**                  procedure will abort and the parent will time out.
**
** External:        <ERRNO.H> list of errors for file operations.
**                  open UNIX close function.
**
*/

/*
recordout.c exported routines:
        main
recordout.c has no exported variables
recordout.c imports:
        atoi
        close
        creat
        errno
        exit
        fprintf
        free
         malloc
        memcpy
        read
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

#define TRUE       1
#define OKCD       0
#define OPNOK      0
#define STDOUTDES  1
#define STDINDES   0
#define ERRSZ      6
#define MEMERR    -1
#define OPNERR    -2
#define RDERR     -3
#define BLKSIZE    4096


int main (/* ARGSUSED */argc, argv)
int argc;
char *argv[];
{
   int      lrecl;                            /*logical record length*/
   char     lsrecl[6];                      /*logical record length*/
   char     lrecfmt;                          /*record format F=fixed V=var.*/
   char     lseq_fileid[129];               /*file id of seq file to be read*/
   char    *lrecbuff;                         /*pointer to record buffer      */
   int      lfildes;                          /*output file descriptor*/
   int      lindes;                           /*standard input  file descriptor*/
   int      lcurr_len;                        /*current record length          */
   unsigned lbufl;                            /*number of bytes in buffer */
   int      lnread;                           /*number of bytes actually read   */
   int      li;                               /*total number of bytes read in   */
   int      lj;                               /*first time flag                 */


   /* Get parameters for argv. */
   memcpy(lseq_fileid, argv[1], 129);
   lrecl = atoi(argv[2]);
   memcpy((char *)&lrecfmt, argv[3], 1);

   lindes = STDINDES;    /* Set file descriptor to standard input. */

   /* Create memory for buffer. Size = logical record length + 6.  */
   lbufl = (unsigned)lrecl + 6;
   if((lrecbuff = malloc(lbufl)) == 0) {
      exit(errno); /* Abort with system error number.   */
   }

   /* Open output file in create mode (delete if exist).          */
#ifdef __INTERIX
   if((lfildes = creat(lseq_fileid, 0644)) < 0)
#else
   if((lfildes = creat64(lseq_fileid, 0644)) < 0)
#endif
   {
      exit(errno); /* Abort with system error number.   */
   }

   /* Read pipe until error, no more data, or record size = zero. */
   /* CONSTCOND */
   for(;;) {
      li = 0;
      while ((lnread = read(lindes, lrecbuff + li, (unsigned)(6 - li))) != (6 - li)) {
         if (lnread < 1) {
            /* This is a read error.                               */
            if (errno == 0) {
               errno = RDERR;
            }
            exit(errno); /* Abort with system error number.   */
         }
         li += lnread;
      }

      /* The first 6 bytes contain the record size format = "99999\0" */
      memcpy(lsrecl, lrecbuff, 6);

      /* If record size is less then one this is EOF.                 */
      if((lcurr_len = atoi(lsrecl)) < 1) {
         break;
      }

      /* The pipe can only transfer 4096 bytes at a time.  So if the  */
      /*     record is larger we must loop until the complete record  */
      /*     has been read in.                                        */

      lj = 0;

      while(lcurr_len > 0) {
         lj = (lj == 0) ? BLKSIZE - 6 : BLKSIZE;
         li = (lcurr_len < lj) ? lcurr_len : lj;
         lnread = read(lindes, lrecbuff, (unsigned)(li));
         if (lnread < 1) { /* Check for read error.                 */
            if (errno == 0) { /* No data in pipe.                    */
               errno = RDERR;
            }
            exit(errno); /* Abort with system error number.   */
         }
         /* Now write the output file and check for errors.    */
         if(write(lfildes, lrecbuff, (unsigned)lnread) < 0) {
            exit(errno); /* Abort with system error number.   */
         }
         lcurr_len -= lnread;
      }
   }

   close(lfildes); /* Close output file                                    */
   free(lrecbuff); /* Free buffer memory                                   */

   return(0);
}
