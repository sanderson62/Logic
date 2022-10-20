/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:17:14  $ */
/* $Modtime:   23 Apr 2013 13:17:14  $ */

/* $Workfile:   cics_eib.h  $ $Revision:   1.3  $ */

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/devtools/compilers/clt/PVCS/cics_eib.h_v  $
 *
 *    Rev 1.3   23 Apr 2013 13:17:14   unikix
 * Update Dell copyright
 *
 *    Rev 1.2   18 Jan 2013 08:46:14   unikix
 * Dell rebranding
 *
 *    Rev 1.1   27 Nov 2008 07:17:54   sm133957
 * D000974
 * Synclevel2 DTP Implementation
 *
 *    Rev 1.0   31 May 2007 13:36:22   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:31:22   unikix
 * New 8.0 archive
 *
 *    Rev 1.1   14 May 2002 13:17:40   mf134128
 * b4653514
 * Make EIBTASKN char[4] as per IBM documentation.
 *
 *    Rev 1.0   25 Sep 2001 17:59:12   unikix
 * Initial 7.2
 *
 *    Rev 1.2   08 May 2001 13:31:38   daved
 * Correct misspellings.
 *
 *    Rev 1.1   19 May 2000 08:12:32   daved
 * Remove references to UniKix.
 *
 *    Rev 1.0   30 Jan 2000 11:41:06   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 19:53:28   unikix
 * Initial 6.0
 *
 *    Rev 1.0   13 May 1998 15:25:12   randyh
 * Initial revision.
 *
 *    Rev 1.0   05/22/97 16:40:58   unikix
 * Initial 5.1
 *
 *    Rev 1.0   04/10/96 14:30:58   unikix
 * Initial 5.0
 *
 *    Rev 1.0   12/28/94 15:23:34   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.1.1.0   11/18/93 17:57:34   unikix
 * UniKix 4.1 baseline (from V400m)
 *
 *    Rev 1.1   10/01/93 15:50:42   alex
 * eibrcode must be unsigned char to avoid problems
 *
 *    Rev 1.0   06/10/93 12:23:44   unikix
 * UniKix 3.1.2 version
 */

#ifndef _KXEIB_C_H
#define _KXEIB_C_H
typedef unsigned char  byte;

struct cics_eib {
   /*exec interface block        */
   char     eibtime[4];     /*time                        */
   char     eibdate[4];     /*date                        */
   char     eibtrnid[4];    /*transaction id              */
   char    eibtaskn[4];    /*task number                 */
   char     eibtrmid[4];    /*terminal identifier         */
   short    dfheigdi;       /*reserved - goto depending on*/
   short    eibcposn;       /*cursor position             */
   short    eibcalen;       /*commarea length             */
   char     eibaid;         /*attention identifier        */
   char     filler1;
   char     eibfn[2];       /*exec cics function code     */
   char     filler2[2];
   byte     eibrcode[6];    /*response code               */
   char     filler3[2];
   char     eibds[8];       /*dataset name                */
   char     eibreqid[8];    /*request identifier          */
   char     eibrsrce[8];    /*resource name               */
   char     eibsync;        /*sync point required         */
   char     eibfree;        /*terminal free required      */
   char     eibrecv;        /*data receive required       */
   char     eibsend;        /*data send required          */
   char     eibatt;         /*attach data exists          */
   char     eibeoc;         /*data received complete      */
   char     eibfmh;         /*data received has fmh       */
   char     eibcompl;       /*data complete               */
   char     eibsig;         /*signal received             */
   char     eibconf;        /*confirm received            */
   char     eiberr;         /*error received              */
   char     eibrldbk;       /*rollback response received  */
   char     eiberrcd[4];    /* error code received        */
   char     eibsynrb;       /*sync rollback required      */
   char     eibnodat;       /*no data received            */
   char     filler5[2];
   int      eibresp;
   int      eibresp2;
   short    dfheigdj;       /*reserved                    */
   short    dfheigdk;       /*reserved                    */
};
typedef struct cics_eib DFHEIBLK;

/* Values which can be held by EIBERRCD */

#define EIBERRCD_SYNC                   (0x08240000)
#define EIBERRCD_ABEND                  (0x08640000)
#define EIBERRCD_ABEND_SVC              (0x08640001)
#define EIBERRCD_ABEND_TIMER            (0x08640002)
#define EIBERRCD_ERROR                  (0x08890000)
#define EIBERRCD_MISMATCH               (0x10086034)
#define EIBERRCD_PIP_ILLEGAL            (0x10086031)
#define EIBERRCD_PIP_INVALID            (0x10086032)
#define EIBERRCD_SECURITY_INVALID       (0x080f6051)
#define EIBERRCD_SYNC_NOT_SUPPORTED     (0x10086041)
#define EIBERRCD_UNKNOWN_TPN            (0x10086021)
#define EIBERRCD_CONN_FAIL              (0xA0000100)
#define EIBERRCD_CONN_FAIL_RETRY        (0xA0000100)
#define EIBERRCD_TPN_NOT_AVAIL          (0x084b6031)
#define EIBERRCD_TPN_FAIL               (0x084c0000)

/* Values which can be held by boolean fields in the EIB */

#define EIBTRUE         (0xFF)
#define EIBFALSE        (0x00)

#endif
