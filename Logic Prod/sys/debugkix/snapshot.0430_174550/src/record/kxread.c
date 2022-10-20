/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   08 Jun 2009 13:30:20  $ */
/* $Modtime:   08 Jun 2009 13:32:20  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   kxread.c  $ $Revision:   1.1  $";
#endif

/*
 * $Log:   /ENG/mf134128/trans/TPE112e.mac/unikixsrc/user/record/PVCS/kxread.c_v  $
 * 
 *    Rev 1.1   08 Jun 2009 13:30:20   mf134128
 * b7001206
 * Change return code from int to ssize_t for 64 bit compatability.
 * 
 *    Rev 1.0   31 May 2007 13:40:08   unikix
 * Initial TPE11.0.0a
 * 
 *    Rev 1.0   17 Nov 2003 13:51:02   unikix
 * New 8.0 archive
 * 
 *    Rev 1.2   15 Oct 2003 09:58:58   dv133961
 * B4825517
 * Remove warning messages generated during kixinstall
 * 
 *    Rev 1.1   26 Sep 2001 11:25:52   unikix
 * Rebrand80
 * 
 *    Rev 1.1   08 May 2001 14:04:32   daved
 * Correct misspellings.
 * 
 *    Rev 1.0   30 Jan 2000 11:47:56   unikix
 * Initial 7.0
 * 
 *    Rev 1.0   15 Mar 1999 20:00:00   unikix
 * Initial 6.0
 * 
 *    Rev 1.0   05/22/97 17:33:42   unikix
 * Initial 5.1
 * 
 *    Rev 1.0   04/10/96 14:13:38   unikix
 * Initial 5.0
 * 
 *    Rev 1.0   12/28/94 15:18:26   unikix
 * Initial revision (from V410ad)
 * 
 *    Rev 1.0.1.0   11/18/93 16:34:38   unikix
 * UniKix 4.1 baseline (from V400m)
 * 
 *    Rev 1.0   06/10/93 11:58:48   unikix
 * UniKix 3.1.2 version
 * 
 *    Rev 1.0   05/21/93 16:14:16   unikix
 * UniKix 3.1.2 version
 */

/*
** Name:            kxread.c
**                  read variable length record file from a sequential file
**
**                  1.0  (91/06/04)
**
** Parameters:      1: record length (call by reference)
**                  2: record (call by reference)
**
** Beware:	    When compiling this routine, the programmer must be aware
**		    of the byte order of the machine.  If compiling on an 
**		    Intel 386/486 machine, you must compile using the following
**		    option:
**		           cc -DINTEL -c kxread.c
**
** Return:          The function is called by COBOL program to open the pipe
**                  line and read variable records from it.  It passes the
**                  record length and record back to the COBOL program to 
**                  create a sequential file in Micro Focus format.  The
**                  function returns a positive non-zero errno if any error
**                  occurred.  It returns 0 if the read was successful.  On
**                  seeing EOF, it returns -1.
**                  procedure will abort and the parent will time out.
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

#define STDINDES   0

ssize_t kxread (rcd_len, rcd_area)
int  *rcd_len;
char *rcd_area;
{
int      lindes;          /*standard input  file descriptor*/
int      lnread;          /*number of bytes actually read  */
int	 lrcd_len;
int      li;

    lindes = STDINDES;    /* Set file descriptor to standard input. */

    li = 0;
    while ((lnread = read(lindes,rcd_area+li,(unsigned)(6-li))) != (6-li))
        {
	switch (lnread)
	    {
	    case -1 : return(errno);               /* read error    */
	    case 0  : return(-1);                  /* end of file   */
	    default : break;                       /* normal return */
	    }
        li += lnread;
        }
    if ((lrcd_len = atoi(rcd_area)) < 1)           /* end of file   */
        {
        return(-1);
        }
    *rcd_len = COBWORD(lrcd_len);
    lnread = read(lindes,rcd_area,(unsigned)(lrcd_len));/* read rcd */
    if (lnread == -1) return(errno);               /* read error    */
    if (lnread == 0) return(-1);                   /* end of file   */
    if (lnread == lrcd_len) return(0);             /* normal return */
    li = lnread;
    while (li < lrcd_len)
        {
	lnread = read(lindes,rcd_area + li,(unsigned)(lrcd_len - li));
	if (lnread == -1) return(errno);
	if (lnread == 0) return(-1);
	if (lnread ==(lrcd_len - li)) return(0); 
        li += lnread;
        }
    return 0;
}
