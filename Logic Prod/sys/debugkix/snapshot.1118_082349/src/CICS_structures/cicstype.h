/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:17:14  $ */
/* $Modtime:   23 Apr 2013 13:17:14  $ */

/* $Workfile:   cicstype.h  $ $Revision:   1.2  $ */

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/devtools/compilers/clt/PVCS/cicstype.h_v  $
 *
 *    Rev 1.2   23 Apr 2013 13:17:14   unikix
 * Update Dell copyright
 *
 *    Rev 1.1   18 Jan 2013 08:46:14   unikix
 * Dell rebranding
 *
 *    Rev 1.0   31 May 2007 13:36:22   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.0   17 Nov 2003 13:31:24   unikix
 * New 8.0 archive
 *
 *    Rev 1.0   19 Sep 2001 16:37:02   unikix
 * Initial 7.2
 *
 *    Rev 1.0   30 Jan 2000 11:41:06   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 19:53:28   unikix
 * Initial 6.0
 *
 *    Rev 1.0   21 Aug 1998 08:29:50   daved
 * Initial revision.
 */

#if !defined(CICSTYPE_H) /* multiple inclusion defence */
#define CICSTYPE_H


/*
 * OBJECT: typedefs - External CICS/6000 typedefs.
 */

typedef char            cics_char_t;    /* default character             */
typedef unsigned char   cics_ubyte_t;   /* unsigned 8-bit                */
typedef signed short    cics_sshort_t;  /* signed 16-bit                 */
typedef unsigned short  cics_ushort_t;  /* unsigned 16-bit               */
typedef signed long     cics_slong_t;   /* signed 32-bit                 */
typedef unsigned long   cics_ulong_t;   /* unsigned 32-bit               */
typedef int             cics_bool_t;    /* boolean type                  */

typedef struct {
   cics_ulong_t     High;
   cics_ulong_t     Low;
}  cics_uxlong_t;                       /* unsigned 64-bit               */

typedef struct {
   cics_slong_t     High;
   cics_ulong_t     Low;
}  cics_sxlong_t;                       /* signed 64-bit                 */

/*
 * OBJECT: typedefs - Common CICS typedefs.
 */

typedef unsigned long   u_long_t;       /* unsigned 32-bit               */
typedef signed long     long_t;         /* signed 32-bit                 */
typedef unsigned short  u_short_t;      /* unsigned 16-bit               */
typedef int             bool_t;         /* boolean type                  */
typedef unsigned int    u_int_t;        /* unsigned natural machine size */
typedef unsigned char   u_byte_t;       /* unsigned 8-bit                */
typedef signed char     byte_t;         /* signed 8-bit                  */

typedef struct {
   u_long_t    High;
   u_long_t    Low;
} u_xlong_t;                            /* unsigned 64-bit               */

#endif /* CICSTYPE_H */

