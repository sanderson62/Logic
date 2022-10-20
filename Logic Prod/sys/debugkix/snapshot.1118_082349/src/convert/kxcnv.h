/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:24:42  $ */
/* $Modtime:   23 Apr 2013 13:24:42  $ */

/* $Workfile:   kxcnv.h  $ $Revision:   1.2  $ */

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/user/public/PVCS/kxcnv.h_v  $
 *
 *    Rev 1.2   23 Apr 2013 13:24:42   unikix
 * Update Dell copyright
 *
 *    Rev 1.1   18 Jan 2013 08:56:36   unikix
 * Dell rebranding
 *
 *    Rev 1.0   31 May 2007 13:40:06   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:51:00   unikix
 * New 8.0 archive
 *
 *    Rev 1.1   26 Sep 2001 09:41:02   calumm
 * B008438
 * Fixed mispelling of 3270 in comment
 *
 *    Rev 1.0   19 Sep 2001 16:47:44   unikix
 * Initial 7.2
 *
 *    Rev 1.2   25 Apr 2001 13:57:06   calumm
 * D007705
 * New conversion tables.
 *
 *    Rev 1.1   03 May 2000 13:47:46   daved
 * D003968.D004051
 * Put DBCS changes into TRANS
 *
 *    Rev 1.0   30 Jan 2000 11:47:54   unikix
 * Initial 7.0
 *
 *    Rev 1.2   09 Nov 1999 12:22:10   shwetank
 * B003968
 * Corrected typo in previous install
 *
 *    Rev 1.1   18 Aug 1999 10:07:32   daved
 * D003968
 * Support for DBCS conversion files
 *
 *    Rev 1.0   15 Mar 1999 20:00:00   unikix
 * Initial 6.0
 *
 *    Rev 1.0   05/22/97 17:35:16   unikix
 * Initial 5.1
 *
 *    Rev 1.0   04/10/96 14:13:26   unikix
 * Initial 5.0
 *
 *    Rev 1.0   12/28/94 15:18:24   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.0   10/26/94 09:14:24   alan
 * Initial revision.
 */

#ifndef _KXCNV_H
#define _KXCNV_H

/*
**      format of conversion table
**      $UNIKIX/lib/cnvtbl
*/

struct kxcnvtbl_rec {
   char        cnv_name[8];
   int         cnv_magic1;
   unsigned char xlwr2upr[256];
   unsigned char xbit2ebcd[64];
   unsigned char xuser_asc2ebcd[256];
   unsigned char xuser_ebcd2asc[256];
   unsigned char xsystem_asc2ebcd[256];
   unsigned char xsystem_ebcd2asc[256];
   unsigned short xnls_asc2ebcd[65536];
   unsigned short xnls_ebcd2asc[65536];
   int         cnv_magic2;
};

/*
**      pseudo functions for ASCII/EBCDIC conversion
*/

extern struct kxcnvtbl_rec *xcnv;

#define lwr2upr(c)              (xcnv->xlwr2upr[(unsigned char)(c)])

/*
 * The 6 bit conversion table.
 * See table C-1 in the 3270 Data Stream Programmer's Reference (GA23-0059-07).
 */
#define bit2ebcd(i)             (xcnv->xbit2ebcd[((int)(i))&0x3F])

/*
 * System data (3270 attributes, orders, SBA values, etc) conversion macros.
 */
#define asc2ebcd(c)             (xcnv->xsystem_asc2ebcd[(unsigned char)(c)])
#define ebcd2asc(c)             (xcnv->xsystem_ebcd2asc[(unsigned char)(c)])

/*
 * Single byte user data conversion macros.
 */
#define user_asc2ebcd(c)        (xcnv->xuser_asc2ebcd[(unsigned char)(c)])
#define user_ebcd2asc(c)        (xcnv->xuser_ebcd2asc[(unsigned char)(c)])

/*
 * Double byte user data conversion macros.
 */
#define nls_asc2ebcd(c)         (xcnv->xnls_asc2ebcd[(unsigned short)(c)])
#define nls_ebcd2asc(c)         (xcnv->xnls_ebcd2asc[(unsigned short)(c)])

#endif
