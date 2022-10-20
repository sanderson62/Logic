/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   07 Oct 2009 14:05:04  $ */
/* $Modtime:   07 Oct 2009 14:03:44  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   lineout.c  $ $Revision:   1.2  $";
#endif

/*
 * $Log:   /home/dd134127/unikix/TPE11.2.0/unikixsrc/user/record/PVCS/lineout.c_v  $
 * 
 *    Rev 1.2   07 Oct 2009 14:05:04   dd134127
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
 *    Rev 1.0   17 Nov 2003 13:51:04   unikix
 * New 8.0 archive
 * 
 *    Rev 1.3   15 Oct 2003 09:56:12   dv133961
 * B4825517
 * Remove warning messages generated during kixinstall
 * 
 *    Rev 1.2   17 Jun 2002 16:22:00   rh134138
 * BugTraq# 4700087 # Misc Large File issues: user record readers & writers
 * 
 *    Rev 1.1   26 Sep 2001 11:25:54   unikix
 * Rebrand80
 * 
 *    Rev 1.1   08 May 2001 14:11:58   daved
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
 *    Rev 1.0   04/10/96 14:13:48   unikix
 * Initial 5.0
 * 
 *    Rev 1.0   12/28/94 15:18:32   unikix
 * Initial revision (from V410ad)
 * 
 *    Rev 1.0.1.0   11/18/93 16:35:24   unikix
 * UniKix 4.1 baseline (from V400m)
 * 
 *    Rev 1.0   06/10/93 11:59:18   unikix
 * UniKix 3.1.2 version
 * 
 *    Rev 1.0   05/21/93 16:14:28   unikix
 * UniKix 3.1.2 version
 */

/*
** Name:            line.out
**                  writes a record to a text sequential file.
**
**                    1.0      (90/12/17)
**
** Parameters:      1: name of this program
**                  2: name of file to be written
**                  3: logical record length "99999\0"
**                  4: "F" for fixed or "V" for variable length file.
**
** Return:          The return codes and data is returned thru standard
**                  output.  The data is sent via standard output. The length
**                  is placed in the first 6 bytes (5 bytes for the
**                  number and a trailing null), followed by the data.
**                  For fixed length records blanks are added to output.
**                  End of file is indicated by a zero record length.
**                  If an error occurs the program exits with the error number,
**                  and the parent will time out.
**
** External:        <ERRNO.H> list of errors for file operations.
**                  open UNIX close function.
**
*/

/*
lineout.c exported routines:
        main
lineout.c has no exported variables
lineout.c imports:
        atoi
        errno
        exit
        fclose
        fopen
        fputs
        free
         malloc
        memcpy
        memset
        read
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
#define STDINDES   0
#define ERRSZ      6
#define RDERR     -3
#define FIXED     'F'



int main (/* ARGSUSED */argc,argv)
int argc;
char *argv[];
{
int      lrecl;                            /*logical record length*/
char     lsrecl[6];                      /*logical record length*/
char     lrecfmt;                          /*record format F=fixed V=var.*/
char     lseq_fileid[129];               /*file id of seq file to be read*/
char    *lrecbuff;                         /*pointer to record buffer      */
int      lindes;                           /*standard input  file descriptor*/
int      lcurr_len;                        /*current record length          */
unsigned lbufl;                            /*number of bytes in buffer */
int      lnread;                           /*number of bytes actually read   */
FILE    *lstream;                          /*output file stream */
int      li;                               /*temp numeric variable */

    /* Get the parameters from argv */
    memcpy(lseq_fileid, argv[1], 129);
    lrecl = atoi(argv[2]);
    memcpy((char *)&lrecfmt,argv[3], 1);

    lindes = STDINDES;  /* Set file descriptor to standard input. */

    /* Reserve memory for record buffer, size = logical length + 6 */
    lbufl = (unsigned)lrecl + 6 + 1;
    if((lrecbuff = malloc(lbufl)) == 0)
        {
        exit(errno); /* abort with system error number  */
        }

    /* Open output file in create mode (delete if already exist). */
#ifdef __INTERIX
    if((lstream = fopen(lseq_fileid, "w+")) == 0)
#else
    if((lstream = fopen64(lseq_fileid, "w+")) == 0)
#endif
        {
        exit(errno); /* abort with system error number  */
        }

    /* Read the pipe while there is no errors and not EOF */
    /* CONSTCOND */
    for (;;)
        {
         /* read pipe to first get actual record length */
        li = 0;
        while ((lnread = read(lindes,lrecbuff+li,(unsigned)(6 - li))) != (6 - li))
            {
	    if (lnread < 1)  /* Check if error */
		{
		if (errno == 0)
		  {
		  errno = RDERR;
		  }
		exit(errno); /* abort with system error number  */
		}
            li += lnread;
            }

        /* convert actual record size (format = "99999\0") to integer */
        memcpy(lsrecl,lrecbuff,6);
        if((lcurr_len = atoi(lsrecl)) < 1) /* If less the one then EOF */
            {
            break;  /* end of file */
            }

        /* Read record from pipe. */
        lnread = read(lindes,lrecbuff,(unsigned)(lcurr_len));
        if (lnread < 1)  /* Check for error */
            {
            if (errno == 0)
              {
              errno = RDERR;
              }
            exit(errno); /* abort with system error number  */
            }

        li = lnread;
        while (li < lcurr_len)
            {
	    lnread = read(lindes,lrecbuff+li,(unsigned)(lcurr_len - li));
	    if (lnread < 1)  /* Check for error */
		{
		if (errno == 0)
		  {
		  errno = RDERR;
		  }
		exit(errno); /* abort with system error number  */
		}
	    li += lnread;
            }

        if (lrecfmt == FIXED) /* FIXED RECORDS */
            {         /* remove trailing blanks from fixed records */
            for (li = lcurr_len - 1; (lrecbuff[li] == ' '); li--);
            lcurr_len = li + 1;
            }

        memset(&lrecbuff[lcurr_len], 0x0a, 1); /*       add line feed */
        memset(&lrecbuff[lcurr_len+1], 0,       1);  /* add null to end string */

        /* Write out record and check for error */
        if(fputs(lrecbuff, lstream) < 1)
            {
            exit(errno); /* abort with system error number  */
            }
        }

    fclose(lstream); /* close text file */
    free(lrecbuff);  /* free buffer memory */

    return(0);
}

