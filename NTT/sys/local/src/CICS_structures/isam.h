/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/*   This header file is to  N E V E R  be distributed !!!!!
                             ---------
     It is to only be used to create objects using the cisam interface
     and is to be curently kept only in this directory.
*/

/***************************************************************************
 *
 *                         INFORMIX SOFTWARE, INC.
 *
 *                            PROPRIETARY DATA
 *
 *      THIS DOCUMENT CONTAINS TRADE SECRET DATA WHICH IS THE PROPERTY OF
 *      INFORMIX SOFTWARE, INC.  THIS DOCUMENT IS SUBMITTED TO RECIPIENT IN
 *      CONFIDENCE.  INFORMATION CONTAINED HEREIN MAY NOT BE USED, COPIED OR
 *      DISCLOSED IN WHOLE OR IN PART EXCEPT AS PERMITTED BY WRITTEN AGREEMENT
 *      SIGNED BY AN OFFICER OF INFORMIX SOFTWARE, INC.
 *
 *      THIS MATERIAL IS ALSO COPYRIGHTED AS AN UNPUBLISHED WORK UNDER
 *      SECTIONS 104 AND 408 OF TITLE 17 OF THE UNITED STATES CODE.
 *      UNAUTHORIZED USE, COPYING OR OTHER REPRODUCTION IS PROHIBITED BY LAW.
 *
 *
 *  Title:      isam.h
 *  Sccsid:     @(#)isam.h      9.11    2/23/94  14:31:41
 *  Description:
 *              Header file for programs using C-ISAM.
 *
 ***************************************************************************
 */

/*
 *       C-ISAM version 4.10
 *  Indexed Sequential Access Method
 *  Relational Database Systems, Inc.
 */

#ifndef ISAM_INCL               /* avoid multiple include problems */
#define ISAM_INCL

#define CHARTYPE        0
#define DECIMALTYPE     0
#define CHARSIZE        1

#define INTTYPE         1
#define INTSIZE         2

#define LONGTYPE        2
#define LONGSIZE        4

#define DOUBLETYPE      3
#ifndef NOFLOAT
#define DOUBLESIZE      (sizeof(double))
#endif /* NOFLOAT */

#ifndef NOFLOAT
#define FLOATTYPE       4
#define FLOATSIZE       (sizeof(float))
#endif /* NOFLOAT */

#define USERCOLL(x)     ((x))

#define COLLATE1        0x10
#define COLLATE2        0x20
#define COLLATE3        0x30
#define COLLATE4        0x40
#define COLLATE5        0x50
#define COLLATE6        0x60
#define COLLATE7        0x70

#define NCHARTYPE       7       /* NLS Native Language CHARacter TYPE */

#define MAXTYPE         5
#define ISDESC          0x80    /* add to make descending type  */
#define TYPEMASK        0x7F    /* type mask                    */

#define BYTEMASK  0xFF          /* mask for one byte            */
#define BYTESHFT  8             /* shift for one byte           */

#define ISFIRST         0       /* position to first record     */
#define ISLAST          1       /* position to last record      */
#define ISNEXT          2       /* position to next record      */
#define ISPREV          3       /* position to previous record  */
#define ISCURR          4       /* position to current record   */
#define ISEQUAL         5       /* position to equal value      */
#define ISGREAT         6       /* position to greater value    */
#define ISGTEQ          7       /* position to >= value         */

/* isread lock modes */
#define ISLOCK          0x100   /* record lock                  */
#define ISSKIPLOCK      0x200   /* skip record even if locked   */
#define ISWAIT          0x400   /* wait for record lock         */
#define ISLCKW          0x500   /* ISLOCK + ISWAIT              */

/* isstart lock modes */
#define ISKEEPLOCK      0x800   /* keep rec lock in autolk mode */

/* isopen, isbuild lock modes */
#define ISAUTOLOCK      0x200   /* automatic record lock        */
#define ISMANULOCK      0x400   /* manual record lock           */
#define ISEXCLLOCK      0x800   /* exclusive isam file lock     */

/* isopen, isbuild file types */
#define ISINPUT         0       /* open for input only          */
#define ISOUTPUT        1       /* open for output only         */
#define ISINOUT         2       /* open for input and output    */
#define ISTRANS         4       /* open for transaction proc    */
#define ISNOLOG         8       /* no loggin for this file      */
#define ISVARLEN        0x10    /* variable length records      */
#define ISFIXLEN        0x0     /* (non-flag) fixed length records only */

/* audit trail mode parameters */
#define AUDSETNAME      0       /* set new audit trail name     */
#define AUDGETNAME      1       /* get audit trail name         */
#define AUDSTART        2       /* start audit trail            */
#define AUDSTOP         3       /* stop audit trail             */
#define AUDINFO         4       /* audit trail running ?        */

/*
 * Define MAXKEYSIZE 240 and NPARTS 16 for AF251
 */
#define MAXKEYSIZE      120     /* max number of bytes in key   */
#define NPARTS          8       /* max number of key parts      */

struct keypart {
   short kp_start;             /* starting byte of key part    */
   short kp_leng;              /* length in bytes              */
   short kp_type;              /* type of key part             */
};

struct keydesc {
   short k_flags;              /* flags                        */
   short k_nparts;             /* number of parts in key       */
   struct keypart
         k_part[NPARTS];         /* each key part                */
   /* the following is for internal use only   */
   short k_len;                /* length of whole key          */
   long k_rootnode;            /* pointer to rootnode          */
};
#define k_start   k_part[0].kp_start
#define k_leng    k_part[0].kp_leng
#define k_type    k_part[0].kp_type

#define ISNODUPS  000           /* no duplicates allowed        */
#define ISDUPS    001           /* duplicates allowed           */
#define DCOMPRESS 002           /* duplicate compression        */
#define LCOMPRESS 004           /* leading compression          */
#define TCOMPRESS 010           /* trailing compression         */
#define COMPRESS  016           /* all compression              */
#define ISCLUSTER 020           /* index is a cluster one       */

struct dictinfo {
   short di_nkeys;             /* number of keys defined (msb set for VARLEN)*/
   short di_recsize;           /* (maximum) data record size   */
   short di_idxsize;           /* index record size            */
   long di_nrecords;           /* number of records in file    */
};

#define EDUPL     100           /* duplicate record     */
#define ENOTOPEN  101           /* file not open        */
#define EBADARG   102           /* illegal argument     */
#undef EBADKEY
#define EBADKEY   103           /* illegal key desc     */
#define ETOOMANY  104           /* too many files open  */
#define EBADFILE  105           /* bad isam file format */
#define ENOTEXCL  106           /* non-exclusive access */
#define ELOCKED   107           /* record locked        */
#define EKEXISTS  108           /* key already exists   */
#define EPRIMKEY  109           /* is primary key       */
#define EENDFILE  110           /* end/begin of file    */
#define ENOREC    111           /* no record found      */
#define ENOCURR   112           /* no current record    */
#define EFLOCKED  113           /* file locked          */
#define EFNAME    114           /* file name too long   */
#define ENOLOK    115           /* can't create lock file */
#define EBADMEM   116           /* can't alloc memory   */
#define EBADCOLL  117           /* bad custom collating */
#define ELOGREAD  118           /* cannot read log rec  */
#define EBADLOG   119           /* bad log record       */
#define ELOGOPEN  120           /* cannot open log file */
#define ELOGWRIT  121           /* cannot write log rec */
#define ENOTRANS  122           /* no transaction       */
#define ENOSHMEM  123           /* no shared memory     */
#define ENOBEGIN  124           /* no begin work yet    */
#define ENONFS    125           /* can't use nfs        */
#define EBADROWID 126           /* reserved for future use */
#define ENOPRIM   127           /* no primary key       */
#define ENOLOG    128           /* no logging           */
#define EUSER     129           /* reserved for future use */
#define ENODBS    130           /* reserved for future use */
#define ENOFREE   131           /* no free disk space   */
#define EROWSIZE  132           /* row size too big     */
#define EAUDIT    133           /* audit trail exists   */
#define ENOLOCKS  134           /* no more locks        */
#define ENOPARTN  135           /* reserved for future use */
#define ENOEXTN   136           /* reserved for future use */
#define EOVCHUNK  137           /* reserved for future use */
#define EOVDBS    138           /* reserved for future use */
#define EOVLOG    139           /* reserved for future use */
#define EGBLSECT  140           /* global section disallowing access - VMS */
#define EOVPARTN  141           /* reserved for future use */
#define EOVPPAGE  142           /* reserved for future use */
#define EDEADLOK  143           /* reserved for future use */
#define EKLOCKED  144           /* reserved for future use */
#define ENOMIRROR 145           /* reserved for future use */
#define EDISKMODE 146           /* reserved for future use */
#define EARCHIVE  147           /* reserved for future use */
#define ENEMPTY   148           /* reserved for future use */
#define EDEADDEM  149           /* reserved for future use */
#define EDEMO     150           /* demo limits have been exceeded */
#define EBADVCLEN 151           /* reserved for future use */
#define EBADRMSG  152           /* reserved for future use */
#define ENOMANU   153           /* must be in ISMANULOCK mode */
#define EDEADTIME 154           /* lock timeout expired */
#define EPMCHKBAD 155           /* primary and mirror chunk bad */
#define EBADSHMEM 156           /* can't attach to shared memory*/
#define EINTERUPT 157           /* interrupted isam call */
#define ENOSMI    158           /* operation disallowed on SMI pseudo table */
#define ENLS_LANG 159           /* Collation sequence invaild */
#define EB_BUSY   160           /* reserved for future use */
#define EB_NOOPEN 161           /* reserved for future use */
#define EB_NOBS   162           /* reserved for future use */
#define EB_PAGE   163           /* reserved for future use */
#define EB_STAMP  164           /* reserved for future use */
#define EB_NOCOL  165           /* reserved for future use */
#define EB_FULL   166           /* reserved for future use */
#define EB_PSIZE  167           /* reserved for future use */
#define EB_ARCH   168           /* reserved for future use */
#define EB_CHKNLOG 169          /* reserved for future use */
#define EB_IUBS   170           /* reserved for future use */
#define EBADFORMAT 171          /* locking or NODESIZE change */

/* Dismountable media blobs errors */
#define EB_SFULL  180           /* reserved for future use */
#define EB_NOSUBSYS  181        /* reserved for future use */
#define EB_DUPBS  182           /* reserved for future use */
/* Shared Memory errors */
#define ES_PROCDEFS     21584   /* can't open config file */
#define ES_IILLVAL      21586   /* illegal config file value */
#define ES_ICONFIG      21595   /* bad config parameter */
#define ES_ILLUSRS      21596   /* illegal number of users */
#define ES_ILLLCKS      21597   /* illegal number of locks */
#define ES_ILLFILE      21598   /* illegal number of files */
#define ES_ILLBUFF      21599   /* illegal number of buffs */
#define ES_SHMGET       25501   /* shmget error */
#define ES_SHMCTL       25502   /* shmctl error */
#define ES_SEMGET       25503   /* semget error */
#define ES_SEMCTL       25504   /* semctl error */

/*
 * For system call errors
 *   iserrno = errno (system error code 1-99)
 *   iserrio = IO_call + IO_file
 *              IO_call  = what system call
 *              IO_file  = which file caused error
 */

#define IO_OPEN   0x10          /* open()       */
#define IO_CREA   0x20          /* creat()      */
#define IO_SEEK   0x30          /* lseek()      */
#define IO_READ   0x40          /* read()       */
#define IO_WRIT   0x50          /* write()      */
#define IO_LOCK   0x60          /* locking()    */
#define IO_IOCTL  0x70          /* ioctl()      */

#define IO_IDX    0x01          /* index file   */
#define IO_DAT    0x02          /* data file    */
#define IO_AUD    0x03          /* audit file   */
#define IO_LOK    0x04          /* lock file    */
#define IO_SEM    0x05          /* semaphore file */

/*
 * NOSHARE was needed as an attribute for global variables on VMS systems
 * It has been left here to make sure that it is defined for the
 * plethera of scattered references.
 */
#define NOSHARE

extern int iserrno;             /* isam error return code       */
extern int iserrio;             /* system call error code       */
extern unsigned int isrecnum;   /* record number of last call   */
extern int isreclen;            /* actual record length, or     */
/* minimum (isbuild, isindexinfo) */
/* or maximum (isopen )         */
extern char isstat1;            /* cobol status characters      */
extern char isstat2;
extern char isstat3;
extern char isstat4;
extern char *isversnumber;      /* C-ISAM version number        */
extern char *iscopyright;       /* RDS copyright                */
extern char *isserial;          /* C-ISAM software serial number */
extern int  issingleuser;       /* set for single user access   */
extern int  is_nerr;            /* highest C-ISAM error code    */
extern char *is_errlist[];      /* C-ISAM error messages        */
extern char *islanginfo();      /* NLS language used            */
/*  error message usage:
 *      if (iserrno >= 100 && iserrno < is_nerr)
 *          printf("ISAM error %d: %s\n", iserrno, is_errlist[iserrno-100]);
 */

struct audhead {
   char au_type[2];            /* audit record type aa,dd,rr,ww*/
   char au_time[4];            /* audit date-time              */
   char au_procid[2];          /* process id number            */
   char au_userid[2];          /* user id number               */
   char au_recnum[4];          /* record number                */
   char au_reclen[2];          /* audit record length beyond header */
};
#define AUDHEADSIZE   14        /* num of bytes in audit header */
#define VAUDHEADSIZE  16        /* VARLEN num of bytes in audit header  */

/*
** prototypes for file manipulation functions
*/
int    isaddindex(int isfd, struct keydesc *keydesc);
int    isaudit(int isfd, char *filename, int mode);
int    isbegin(void);
int    isbuild(char *filename, int reclen, struct keydesc *keydesc, int mode);
int    isclose(int isfd);
int    iscluster(int isfd, struct keydesc *keydesc);
int    iscommit(void);
int    isdelcurr(int isfd);
int    isdelete(int isfd, unsigned char *record);
int    isdelindex(int isfd, struct keydesc *keydesc);
int    isdelrec(int isfd, long recnum);
int    iserase(char *filename);
int    isflush(int isfd);
int    isindexinfo(int isfd, struct keydesc *buffer, int number);
char  *islanginfo(char *filename);
int    islock(int isfd);
int    islogclose(void);
int    islogopen(char *logname);
int    isnlsversion(char *filename);
int    isopen(char *filename, int mode);
int    isread(int isfd, unsigned char *record, int mode);
int    isrelease(int isfd);
int    isrename(char *oldname, char *newname);
int    isrewcurr(int isfd, unsigned char *record);
int    isrewrec(int isfd, long recnum, char *record);
int    isrewrite(int isfd, unsigned char *record);
int    isrollback(void);
int    issetunique(int isfd, long uniqueid);
int    isstart(int isfd, struct keydesc *keydesc,
               int length, char *record, int mode);
int    isuniqueid(int isfd, long *uniqueid);
int    isunlock(int isfd);
int    iswrcurr(int isfd, unsigned char *record);
int    iswrite(int isfd, unsigned char *record);

#endif /* ISAM_INCL */
