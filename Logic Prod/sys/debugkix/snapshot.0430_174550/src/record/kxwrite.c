/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   09 Jun 2009 10:46:58  $ */
/* $Modtime:   09 Jun 2009 10:56:32  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   kxwrite.c  $ $Revision:   1.1  $";
#endif

/*
 * $Log:   /ENG/mf134128/trans/TPE112e.mac/unikixsrc/user/record/PVCS/kxwrite.c_v  $
 * 
 *    Rev 1.1   09 Jun 2009 10:46:58   mf134128
 * b7001206
 * Return ssize_t rather than int for 64 bit compatability.
 * 
 *    Rev 1.0   31 May 2007 13:40:08   unikix
 * Initial TPE11.0.0a
 * 
 *    Rev 1.0   17 Nov 2003 13:51:02   unikix
 * New 8.0 archive
 * 
 *    Rev 1.0   19 Sep 2001 16:47:46   unikix
 * Initial 7.2
 * 
 *    Rev 1.0   30 Jan 2000 11:47:56   unikix
 * Initial 7.0
 * 
 *    Rev 1.1   09 Aug 1999 09:28:42   daved
 * B004029
 * Correct some typos in the comments.
 * 
 *    Rev 1.0   15 Mar 1999 20:00:02   unikix
 * Initial 6.0
 * 
 *    Rev 1.0   05/22/97 17:33:44   unikix
 * Initial 5.1
 * 
 *    Rev 1.0   04/10/96 14:13:46   unikix
 * Initial 5.0
 * 
 *    Rev 1.0   12/28/94 15:18:30   unikix
 * Initial revision (from V410ad)
 * 
 *    Rev 1.0.1.0   11/18/93 16:35:08   unikix
 * UniKix 4.1 baseline (from V400m)
 * 
 *    Rev 1.0   06/10/93 11:59:00   unikix
 * UniKix 3.1.2 version
 * 
 *    Rev 1.0   05/21/93 16:14:22   unikix
 * UniKix 3.1.2 version
 */

/*
** Name:            RECORDV.out
**                  writes a variable length record to a sequential file.
**
**                  1.0  (91/06/04)
**
** Parameters:      1: record length (call by value)
**                  2: record (call by reference)
**
** Beware:	    When compiling this routine, the programmer must be aware
**		    of the byte order of the machine.  If compiling on an 
**		    Intel 386/486 machine, you must compile using the following
**		    option:
**		           cc -DINTEL -c kxwrite.c
**
** Return:          The function is called by COBOL program to open the pipe
**                  line and write variable records to it.  It passes in the
**                  record length and record from the COBOL program to 
**                  create a sequential file.  The function returns a positive
**                  non-zero errno if any error occurred.  It returns 0 if 
**                  the write was successful.
**
** External:        <ERRNO.H> list of errors for file operations.
**                  open UNIX close function.
**
*/

/* ------------- unix includes ------------- */

#include <sys/types.h>
#ifdef __STDC__
#include <stdlib.h>
#include <unistd.h>
#endif
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>
#ifndef DPX
#include <malloc.h>
#include <memory.h>
#endif

/* ------------- local defines ------------- */

#ifdef INTEL
#define COBWORD(a) ((((a)&0xff)<<24)    | \
		    (((a)&0xff00)<<8)   | \
		    (((a)&0xff0000)>>8) | \
		    ((((unsigned)(a)&0xff000000))>>24))
#else
#define COBWORD(a) (a)
#endif

#define STDOUTDES  1

ssize_t kxwrite (rcd_len,rcd_area)
int  *rcd_len;
char *rcd_area;
{
int      loutdes;                        /*output file descriptor         */
int      lnread;                         /*no of bytes written            */
char     lcurr_len[6];                   /*current record length          */
int	 lrcd_len;

    loutdes = STDOUTDES;       /* Set file descriptor to standard output. */
    lrcd_len = COBWORD(*rcd_len); 

    sprintf(lcurr_len, "%5.5d", lrcd_len);
    lnread = write(loutdes, lcurr_len, (unsigned)(6)); 
    switch (lnread)
        {
        case -1 : return(errno);         /* write error   */
        default : break;                 /* normal return */
        }
    if (lrcd_len > 0)
        {
        lnread = write(loutdes,rcd_area,(unsigned)lrcd_len);  /* write rcd */
        switch (lnread)
            {
            case -1 : return(errno);     /* write error   */
            default : return(0);         /* normal return */
            }
       }
    return(0);
}
