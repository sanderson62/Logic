/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/* $Date:   23 Apr 2013 13:23:46  $ */
/* $Modtime:   23 Apr 2013 13:23:46  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   kx_start_db.c  $ $Revision:   1.6  $";
#endif

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/trans/mfintf/PVCS/kx_start_db.c_v  $
 *
 *    Rev 1.6   23 Apr 2013 13:23:46   unikix
 * Update Dell copyright
 *
 *    Rev 1.5   18 Jan 2013 08:55:14   unikix
 * Dell rebranding
 *
 *    Rev 1.4   07 Oct 2009 11:18:02   dd134127
 * Merge 1.0.1.0 and 1.3
 *
 *    Rev 1.3   23 Jul 2009 14:02:06   dd134127
 * B4939902.B7000903
 * Provide prototypes for all called functions.
 *
 *    Rev 1.2   06 Jul 2009 09:48:24   rh134138
 * B4939902 Post EIP removal of printfs -- actually are unneccessary.
 *
 *    Rev 1.1   06 Jul 2009 07:50:48   rh134138
 * Bug# 4939902 C debugger CEDF option causes a transaction to go into an infinite CPU loop
 *
 *    Rev 1.0.1.0   25 Sep 2009 16:36:10   tl134147
 * B7001341 - initial Windows SUA checkin
 *
 *    Rev 1.0   31 May 2007 13:39:38   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:48:08   unikix
 * New 8.0 archive
 *
 *    Rev 1.0   19 Sep 2001 16:46:18   unikix
 * Initial 7.2
 *
 *    Rev 1.0   30 Jan 2000 11:46:20   unikix
 * Initial 7.0
 *
 *    Rev 1.1   26 May 1999 13:08:52   randyh
 * Case# 3889 Provide Symbolic Debugger support for 'C' on NT
 *
 *    Rev 1.0   17 May 1999 09:14:26   randyh
 * Initial revision.
 *
 *    Rev 1.0   15 Mar 1999 19:58:54   unikix
 * Initial 6.0
 *
 */
/*----------------------------------------------------------------------*/
/*      This function is always compiled in -g mode for debugging       */
/*----------------------------------------------------------------------*/

#include <stdio.h>
#include <unistd.h>

extern int x_dbg_waittime2;

void kx_start_db (void)
{
#ifdef WIN_NATIVE
   kix_dbgpid(getpid());
#else

   /*
   The debugger will attach here at the 'sleep' statement.  To proceed
   use the 'continue' after setting breakpoints in the application code.
   There may be a slight pause to let the sleep finish
   */
   sleep(x_dbg_waittime2);
#endif
   return;
}
