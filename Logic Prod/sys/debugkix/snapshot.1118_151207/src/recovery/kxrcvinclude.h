/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   31 May 2007 13:40:08  $ */
/* $Modtime:   31 May 2007 10:32:14  $ */

/* $Workfile:   kxrcvinclude.h  $ $Revision:   1.0  $ */

/*
 * $Log:   /builds/source/TPE11.0.0a/unikixsrc/user/recovery/PVCS/kxrcvinclude.h_v  $
 * 
 *    Rev 1.0   31 May 2007 13:40:08   unikix
 * Initial TPE11.0.0a
 * 
 *    Rev 1.0   17 Nov 2003 13:51:10   unikix
 * New 8.0 archive
 * 
 *    Rev 1.0   19 Sep 2001 16:47:50   unikix
 * Initial 7.2
 * 
 *    Rev 1.0   30 Jan 2000 11:47:58   unikix
 * Initial 7.0
 * 
 *    Rev 1.1   05 Aug 1999 12:19:46   unikix
 * Correct ; in previous revison to , 
 * 
 *    Rev 1.0   27 Jul 1999 15:55:36   kupendra
 * Initial revision.
 */

void kxesds_backout_user_exit(
#ifdef __STDC__
         char *record_buffer,
         int   record_length,
         char *record_dataset,
         int  *return_code
#endif
);


