/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

#ifndef _KXUSERTM_H
#define _KXUSERTM_H

/**********************************************************************/
/* $Date:   23 Apr 2013 13:20:02  $ */
/* $Modtime:   23 Apr 2013 13:20:02  $ */
/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/lib/public/PVCS/kxusertm.h_v  $
 *
 *    Rev 1.2   23 Apr 2013 13:20:02   unikix
 * Update Dell copyright
 *
 *    Rev 1.1   18 Jan 2013 08:50:10   unikix
 * Dell rebranding
 *
 *    Rev 1.0   31 May 2007 13:37:52   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:35:38   unikix
 * New 8.0 archive
 *
 *    Rev 1.0   25 Apr 2003 14:27:42   jt134157
 * Initial revision.
 */

#include <stddef.h>
#include "xa.h"

/*
        The KXRMENTRY defines a configured Resource Manager.
        The TM uses a table of KXRMENTRY types to manage the RMs.
*/
typedef struct _tagkxrmentry {

   struct xa_switch_t     *xaswitch;                       /* = configured vendor switch */
   size_t                  eyecatcher;                     /* = "ACEFACE" */
   int                     flags;                          /* = r/t flags */
   XID                     xid;                            /* = XID for this branch */
   char                    openinfo[MAXINFOSIZE];          /* = configured open info */
   char                    closeinfo[MAXINFOSIZE];         /* = configured close info */
   char                    name[RMNAMESZ];                 /* = configured name */
} KXRMENTRY;

/* definition of our eyecatcher */
#define KXRMEYECATCHER          0xaceface                       /* ref. quadrophenia */
/*
        The GlobalRMTable defines all the configured RMs for this thread of control.
*/
struct kxGlobalRMTable_t {
   size_t                  total;                          /* = total # of KXRMENTRY */
   KXRMENTRY              *tables;                         /* = configured Resource Managers */
   char                   *stamp;                          /* = generation stamp */
};

#endif /* _KXUSERTM_H */
