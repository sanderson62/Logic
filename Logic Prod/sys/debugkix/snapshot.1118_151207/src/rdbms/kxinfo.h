/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   31 May 2007 13:39:56  $ */
/* $Modtime:   31 May 2007 10:32:10  $ */

/* $Workfile:   kxinfo.h  $ $Revision:   1.0  $ */

/*
 * $Log:   /builds/source/TPE11.0.0a/unikixsrc/trans/txserver/PVCS/kxinfo.h_v  $
 * 
 *    Rev 1.0   31 May 2007 13:39:56   unikix
 * Initial TPE11.0.0a
 * 
 *    Rev 1.0   17 Nov 2003 13:50:00   unikix
 * New 8.0 archive
 * 
 *    Rev 1.0   19 Sep 2001 16:47:18   unikix
 * Initial 7.2
 * 
 *    Rev 1.0   30 Jan 2000 11:47:10   unikix
 * Initial 7.0
 * 
 *    Rev 1.0   15 Mar 1999 19:59:48   unikix
 * Initial 6.0
 * 
 *    Rev 1.1   07/18/97 11:01:16   prabha
 * D002240
 * Changes to kxsysinfo for supporting new SIT table.
 * 
 *    Rev 1.0   05/22/97 16:41:22   unikix
 * Initial 5.1
 * 
 *    Rev 1.0   04/10/96 14:31:18   unikix
 * Initial 5.0
 * 
 *    Rev 1.1   03/25/96 16:05:16   prabha
 * DTTYINFO
 * Added ttystr structure for function kxttyinfo
 * 
 *    Rev 1.0   12/28/94 15:23:48   unikix
 * Initial revision (from V410ad)
 * 
 *    Rev 1.0.1.1   07/12/94 14:22:56   prabha
 * Added field for returning transaction index number
 * 
 *    Rev 1.0.1.0   11/18/93 17:59:32   unikix
 * UniKix 4.1 baseline (from V400m)
 * 
 *    Rev 1.0   07/26/93 08:34:24   prabha
 * Initial revision.
 * 
 *    Rev 1.0   07/23/93 12:22:54   unikix
 * UniKix 4.0 version
 */

#ifndef _KXINFO_H
#define _KXINFO_H

/*
*     This file does not require other files
*/

/* Structure for passing parameter information from tct table */

struct lextpar { 
	char 	cur_usrnam [8];
	char    cur_opsec  [8];
	char	cur_opcls  [3];
	char	cur_trmid  [4];
	char    cur_luname [8];
	char    cur_opid   [3];
	char    cur_trancd [4];
	} lextpar;

struct lsyspar {
	char	cur_sysname [8];
	/* Case 2240 */
	/* Added entries for RDBMS related variables */
	char	cur_svrname [8];
	char	cur_dbname  [8];
	char	cur_usrname [8];
	char	cur_usrpass [8];
	char	cur_tranidx [3];
	} lsyspar;

struct lmsgstr {
	char	cur_errno [4];
	char	cur_func  [20];
	char	cur_msg	  [60];
	} lmsgstr;

struct lttystr {
	char	cur_termpid [6];
	char	cur_ttyname [20];
	char	cur_termenv [14];
	} lttystr;

#endif
