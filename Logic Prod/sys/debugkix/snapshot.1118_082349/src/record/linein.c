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
static char sccsid[] = "@(#) $Workfile:   linein.c  $ $Revision:   1.4  $";
#endif

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/user/record/PVCS/linein.c_v  $
 *
 *    Rev 1.4   23 Apr 2013 13:24:44   unikix
 * Update Dell copyright
 *
 *    Rev 1.3   18 Jan 2013 08:56:36   unikix
 * Dell rebranding
 *
 *    Rev 1.2   07 Oct 2009 14:03:02   dd134127
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
 *    Rev 1.0   17 Nov 2003 13:51:02   unikix
 * New 8.0 archive
 *
 *    Rev 1.3   15 Oct 2003 09:55:04   dv133961
 * B4825517
 * Remove warning messages generated during kixinstall
 *
 *    Rev 1.2   17 Jun 2002 16:22:52   rh134138
 * BugTraq# 4700087 # Misc Large File issues: user record readers & writers
 *
 *    Rev 1.1   26 Sep 2001 11:25:54   unikix
 * Rebrand80
 *
 *    Rev 1.1   08 May 2001 14:08:10   daved
 * Correct misspellings, and clean up compile warnings.
 *
 *    Rev 1.0   30 Jan 2000 11:47:56   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 20:00:02   unikix
 * Initial 6.0
 *
 *    Rev 1.0   05/22/97 17:33:46   unikix
 * Initial 5.1
 *
 *    Rev 1.0   04/10/96 14:13:46   unikix
 * Initial 5.0
 *
 *    Rev 1.1   11/17/95 13:28:26   daved
 * B001234
 * "%5.5d" format string gives 6 (not 5) characters when the number is negative
 * (for example "-00002").  Change the format string to "%05d", which only gives
 * 5 characters ("-0002").
 *
 *    Rev 1.0   12/28/94 15:18:32   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.0.1.0   11/18/93 16:35:16   unikix
 * UniKix 4.1 baseline (from V400m)
 *
 *    Rev 1.0   06/10/93 11:59:12   unikix
 * UniKix 3.1.2 version
 *
 *    Rev 1.0   05/21/93 16:14:26   unikix
 * UniKix 3.1.2 version
 */

/*
** Name:            line.in
**                  reads the record from a text sequential file.
**
**                    1.0      (90/12/17)
**
** Parameters:      1: name of this program
**                  2: name of file to be read
**                  3: logical record length
**                  4: "F" for fixed or "V" for variable length file.
**
** Return:          The return codes and data is returned thru standard
**                  output.  If the read is successful the number of bytes
**                  read in is placed in the first 6 bytes (5 bytes for the
**                  number and a trailing null), followed by the data.
**                  If a fixed length file the end is padded with spaces.
**                  End of file is indicated by a zero record length.
**                  An error is indicated by a record length less than zero.
**
** External:        <ERRNO.H> list of errors for file operations.
**                  open UNIX close function.
**
*/

/*
linein.c exported routines:
        main
linein.c has no exported variables
linein.c imports:
        atoi
        errno
        exit
        fclose
        fgets
        fopen
        free
        malloc
         memcpy
        memset
        sprintf
        strlen
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
#include <memory.h>
#include <malloc.h>

/* ------------- local defines ------------- */

#define OKCD       0
#define OPNOK      0
#define STDOUTDES  1
#define ERRSZ      6
#define MEMERR    -9999
#define LENERR    -9998
#define OPNERR    -2
#define RDERR     -3
#define FIXED     'F'

int main (/* ARGSUSED */argc, argv)
int argc;
char *argv[];
{
   int      lrecl;                            /*logical record length*/
   int      lkylth;                           /*key length*/
   int      lrkp;                             /*relative key position*/
   char     lrecfmt;                          /*record format F=fixed V=var.*/
   char     lseq_fileid[129];                 /*file id of seq file to be read*/
   char     lerr[ERRSZ];                      /*error sting to send back*/
   char    *lrecbuff;                         /*pointer to record buffer*/
   int      loutdes;                          /*standard output file descriptor*/
   unsigned lbufl;                            /*number of bytes in buffer*/
   int      lnread;                           /*number of bytes actually read*/
   int      li;                               /*temp number*/
   FILE    *lstream;                          /*input file stream*/


   /* Get parameters for argv */
   memcpy(lseq_fileid, argv[1], 129);
   lrecl  = atoi(argv[2]);
   memcpy((char *)&lrecfmt, argv[3], 1);
   lkylth = atoi(argv[4]);
   lrkp   = atoi(argv[5]);

   loutdes = STDOUTDES; /* Set file descriptor to standard output */

   /* Reserve memory for record buffer, size = logical record length plus  */
   /*    six for record size plus one for carriage return and one for null */

   lbufl = (unsigned)lrecl + 6 + 2; /* fgets adds a null so we need 2 more */
   if((lrecbuff = malloc(lbufl)) == 0) {
      /* memory error write back error number */
      sprintf(lerr, "%05d", MEMERR);
      write(loutdes, lerr, ERRSZ);
      exit(0); /* Abort */
   }

   /* Open text file in read mode */
#ifdef __INTERIX
   if((lstream = fopen(lseq_fileid, "r")) == 0)
#else
   if((lstream = fopen64(lseq_fileid, "r")) == 0)
#endif
   {
      /* open error write back error number and free memory */
      sprintf(lerr, "%05d", OPNERR);
      write(loutdes, lerr, ERRSZ);
      free(lrecbuff);
      exit(errno); /* Abort */
   }

   /* Write back that open was ok, parent is expecting a return code */
   sprintf(lerr, "%05d", OPNOK);
   write(loutdes, lerr, ERRSZ);

   /* Now read thru text file until EOF  or error */
   while(fgets(&lrecbuff[6], lrecl + 2, lstream) != 0) {
      lnread = (strlen(&lrecbuff[6])) - 1;

      if ((lnread == lrecl) && (lrecbuff[lnread + 6] != '\n')) {
         sprintf(lerr, "%05d", LENERR);
         write(loutdes, lerr, ERRSZ);
         free(lrecbuff);
         exit(0);
      }

      /* If a FIXED file, then pad with spaces */
      if ((lrecfmt == FIXED) && ((li = lrecl - lnread) > 0)) {
         memset(&lrecbuff[lnread + 6], ' ', li);
         lnread = lrecl;
      }
      /* If a variable file and record len is less than key len + offset */
      else if ((lrecfmt != FIXED) && (lnread < lkylth + lrkp)) {
         sprintf(lerr, "%05d", LENERR);
         write(loutdes, lerr, ERRSZ);
         free(lrecbuff);
         exit(0);
      }

      /* Format the actual record length into a string.  */
      /* String format = "99999\0"                       */
      memset(&lrecbuff[6 + lnread], 0 , 1);
      sprintf(lrecbuff, "%05d", lnread);

      /* Write data down the pipe */
      write(loutdes, lrecbuff, (unsigned)lnread + 6);
   }

   /* Check to see if we quit because of an error */
   if ((li = (ferror(lstream))) != 0) {
      /* Format error number and write to pipe */
      sprintf(lerr, "%05d", li * (-1));
      write(loutdes, lerr, ERRSZ);
      /* Free memory from buffer and abort */
      free(lrecbuff);
      exit(li);
   } else {
      /* No error, format EOF code and write to pipe */
      sprintf(lerr, "%05d", OKCD);
      write(loutdes, lerr, ERRSZ);
   }

   fclose(lstream); /* Close input record */
   free(lrecbuff);  /* Free memory from buffer */

   return(0);
}

