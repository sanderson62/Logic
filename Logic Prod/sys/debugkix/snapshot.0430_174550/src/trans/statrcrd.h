/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   26 Jan 2011 09:38:54  $ */
/* $Modtime:   21 Jan 2011 16:21:26  $ */

/* $Workfile:   statrcrd.h  $ $Revision:   1.7  $ */

/*
 * $Log:   /ENG/mf134128/trans/TPE1131.mac/unikixsrc/lib/public/PVCS/statrcrd.h_v  $
 * 
 *    Rev 1.7   26 Jan 2011 09:38:54   mf134128
 * b7001570
 * Add support for CHANNEL/CONTAINERS
 * 
 *    Rev 1.6   07 May 2010 12:03:26   rh134138
 * 7001569  Add support for EXEC CICS WEB commands
 *          options as an HTTP client
 * 
 *    Rev 1.5   16 Jul 2009 14:46:40   dd134127
 * Merge 1.3.1.1 and 1.4
 * 
 *    Rev 1.4   12 Nov 2008 10:55:22   dd134127
 * B7000903
 * 64-bit port
 * 
 *    Rev 1.3.1.1   18 May 2009 16:29:30   mm136118
 * B7000222
 * Maintain API usage statistics for all CICS commands; EIP rework
 * 
 *    Rev 1.3.1.0   11 May 2009 16:05:24   mm136118
 * B7000222
 * Maintain API usage statistics for all CICS commands
 * 
 *    Rev 1.3   13 Feb 2008 10:51:12   dd134127
 * B7000594
 * statrcrd.h should not include files that are not shipped to customers.
 * 
 *    Rev 1.2   13 Jul 2007 09:17:10   rs142317
 * 7000206
 * EIP feedback - INQUIRE TSQNAME is a separate op code
 * 
 *    Rev 1.1   11 Jun 2007 10:40:40   rs142317
 * 7000206
 * Support 16 byte TSQ names
 * 
 *    Rev 1.0   31 May 2007 13:37:54   unikix
 * Initial TPE11.0.0a
 * 
 *    Rev 1.5   13 Nov 2006 12:40:10   dd134127
 * Merge 1.3.1.1 and 1.4
 * 
 *    Rev 1.4   26 Aug 2005 18:06:54   ss134153
 * B6249492(81)
 * Added constant for NETNAME's length
 * 
 *    Rev 1.3.1.1   04 Aug 2005 10:01:28   jt134157
 * D6305728
 * Update MAX_REQUESTS for new web/document APIs
 * 
 *    Rev 1.3.1.0   31 May 2005 15:53:58   dd134127
 * Merge 1.0.3.0 and 1.3
 * 
 *    Rev 1.3   11 Mar 2005 11:10:44   mf134128
 * b4800524
 * Extend accounting record to log cpu & cics time
 * 
 *    Rev 1.2   20 Jan 2005 09:15:50   dd134127
 * Merge 1.0.1.0 and 1.1
 * 
 *    Rev 1.1   23 Dec 2003 01:08:50   ds134155
 * 
 * B4774439
 * Changes for QUERY SECURITY and SIGNON
 * 
 *    Rev 1.0.3.0   04 Jan 2005 10:14:38   jt134157
 * D6213791
 * Web API POC.
 * 
 *    Rev 1.0.1.0   07 Oct 2004 07:42:08   dv133961
 * B5046780
 * Increment the CICS API counts even if there is an error
 * 
 *    Rev 1.5   01 May 2003 12:43:52   dd134127
 * Merge 1.2.1.0 and 1.4
 * 
 *    Rev 1.4   02 Apr 2003 09:20:06   dd134127
 * Merge 1.1.2.0 and 1.3
 * 
 *    Rev 1.3   17 Jan 2003 13:51:18   dd134127
 * Merge 1.1.1.0 and 1.2
 * 
 *    Rev 1.2.1.0   07 Apr 2003 02:54:42   dv133961
 * B4816649
 * CICS API Statistics count is incorrect (ABEND, SYNCPT, INQ_TRANS, SET_TRANS)
 * 
 *    Rev 1.2   14 Nov 2002 13:46:42   rh134138
 * BugTraq #4769270 Add support for INQUIRE TSQUEUE commands in kixclt
 * 
 *    Rev 1.1.2.0   04 Feb 2003 06:24:48   dv133961
 * B4706018
 * Incorrect counts for ABEND and SYNCPOINT
 * 
 *    Rev 1.1.1.0   13 Jan 2003 02:56:30   dv133961
 * B4699621
 * API - Use counts statistics incomplete
 * 
 *    Rev 1.1   13 May 2002 16:43:24   rh134138
 * BugTraq# 4651326 Changes for the SPI/API RDO project
 * 
 *    Rev 1.0   19 Sep 2001 16:40:20   unikix
 * Initial 7.2
 * 
 *    Rev 1.4   22 Mar 2001 10:09:28   randyh
 * Defect# 7442 kixdump -Sc does not show EXEC CICS LINK counts
 * 
 *    Rev 1.3   19 May 2000 08:18:06   daved
 * Remove references to UniKix.
 * 
 *    Rev 1.2   05 Mar 2000 12:57:24   rick
 * D004226
 * Txn class changes - MAX_REQUESTS changed to 98
 * 
 *    Rev 1.1   05 Mar 2000 11:26:20   calumm
 * B004275
 * Changed MAX_REQUESTS to 97
 * 
 *    Rev 1.0   30 Jan 2000 11:42:30   unikix
 * Initial 7.0
 * 
 *    Rev 1.1   22 Oct 1999 12:12:54   randyh
 * Case# 4134 Implement CHANGE/VERIFY PASSWORD a.p.i.
 * 
 *    Rev 1.0   15 Mar 1999 19:55:08   unikix
 * Initial 6.0
 * 
 *    Rev 1.0   05/22/97 17:04:50   unikix
 * Initial 5.1
 * 
 *    Rev 1.0   04/10/96 14:32:40   unikix
 * Initial 5.0
 * 
 *    Rev 1.0   12/28/94 15:24:34   unikix
 * Initial revision (from V410ad)
 * 
 *    Rev 1.2.1.2   11/18/94 15:30:40   alan
 * Add some new CICS function names
 * 
 *    Rev 1.2.1.1   01/17/94 17:36:58   som
 * Move struct jct_rcd from ../bcommand/journal.c into this file
 * 
 *    Rev 1.2.1.0   11/18/93 18:05:40   unikix
 * UniKix 4.1 baseline (from V400m)
 * 
 *    Rev 1.2   09/09/93 11:22:08   shwetank
 * Batch Accounting
 * 
 *    Rev 1.1   07/29/93 15:48:24   shwetank
 * UniKix Accounting
 * 
 *    Rev 1.0   06/10/93 12:27:52   unikix
 * UniKix 3.1.2 version
 */

/* ************** */
#ifndef _STATRCRD_H
#define _STATRCRD_H

#define LEN_ABEND 4
#define MLEN     8
#define LEN_UNIT 8
#define LEN_VTAM_NAME 20
#define LEN_ACCT_DATE 8
#define LEN_TIME 8
#define LEN_SYSTEM 8
#define LEN_RELEASE 8
#define LEN_TRANSID 4
#define LEN_TERMID 4
#define LEN_NETNAME 8
#define LEN_START_QUEUE 16
#define LEN_OPID 3
#define LEN_APPNM 8
#define LEN_PGMNM 8

/* Bug 4699621 - MAX_REQUESTS is now changed to 1 more than number of entries */
/* This applies to the function codes #defined in trans/public/kxreqfuncd.h,
   the hindex arrays (2 versions) in trans/applintf/cvparmltu.c, and
   the lreq_table array in trans/applintf/kxfndreq.c.  */
#define MAX_REQUESTS 142
#define LEN_USERNAME 8

/*STATRCRD.H   for  C programs*/

#define JRNL_TRANSID		1
#define JRNL_USER		2
#define JRNL_STRT_FILE		5
#define JRNL_STRT_FROM_DEAD_KIX	6
#define JRNL_STRT_KIX_NORM	7
#define JRNL_END_FILE		15
#define JRNL_END_KIX		16
#define FLIP_NORMAL             1
#define FLIP_SHUTDOWN           2
#define JCT_RCD_TYPE		20

#define KIXJRNL			"kixjournal"

#define DEF_JRNL_FILESZ_K 20480
#define DEF_JRNL_BUFSZ 10240

struct acntg_hdr_type {
   unsigned short       uk_reclen;            /* Length of record (includes
					         Accounting header */
   unsigned short       uk_rcd_type;          /* Record Type ( 1 - transid, 							               2 - user) */
   char 		uk_machine_fmt; /* Machine format (1 - reverse byte,  */
   char			filler1;
   char  		uk_sysname[LEN_SYSTEM]; /* This field is always
						    "UniKix " */
   char                 uk_release[LEN_RELEASE];/* Bytes: 0,1 - release no.
								(e.g. 04) */
				                /* 2,3 - update no. (e.g. 01,02)
					           4,5 - maintenance no.
						       (e.g. 01,02,...)
						   6,7 - reserved */
   char                 uk_reserved[8];       /* Reserved */
   char                 uk_machine[MLEN];     /* Machine Name */
   char                 uk_date[LEN_ACCT_DATE];    /* Date - mmddyyyy */
   char 		uk_time[LEN_TIME];    /* Time - hhmmssdd (hours,
								  minutes,
							          seconds,
							          hundredths
								  of secs)*/
   short                uk_jrnl_id;           /* Journal identifier */
   					      /*    0 - straight byte) */
   };

struct acntg_body_type {
   char                 uk_transid[LEN_TRANSID]; /* Transaction id */
   char                 uk_termid[LEN_TERMID];   /* Terminal id */
   char                 uk_userid[LEN_USERNAME]; /* User id */
   char  		uk_start_time[12];       /* Time the transid is attached 						   to the transaction processor
						*/
   char      		uk_finish_time[12]; 	/* Time the transaction proces-
						   sor finished with the transid						*/
   char  		uk_elapsed_time[12];    /* Elapsed time for which the
						   user task was dispatched */
   char  		uk_response_time[12];   /* Difference of time between 
						   when the transaction was 
						   attached to the transaction 
						   processor and when the 
						   transaction sent its first 
						   output message to the
						   terminal */
   char			uk_sys_cpu_time[9];	/* system cpu time for tx */
   char 		uk_user_cpu_time[9];	/* user cpu time for tx */
   char			uk_cics_time[12];	/* time spent in cics calls */
   int			uk_seq_no;		/* Sequence number of 
						   transaction */
   char                 uk_trans_type;           /* Transaction Type:
						      0 - Attached from terminal 							  input

						      1 - Attached by Automatic 							  Transaction Initiation							  (ATI) without data

						      2 - Attached by ATI with
							  data

						      3 - Attached by Transient
							  Data trigger level

						      4 - Attached by user 
							  request

						      5 - Attached from terminal							  TCTTE transid 
						*/
   char			uk_opid[LEN_OPID];	/* Operator identification at
						   task creation */

   char 		uk_perf_rcdtype;	/* Performance record type 
						   For UniKix, this field
						   will always be 'T' */
   char 		uk_unit;		/* Unit for elapsed time:
						     1 - 16 millisecond units
						     2 - millisecond units
						     3 - microseconds */
   char                 uk_prognm[LEN_PGMNM];   /* Name of the first program
						   invoked at attach time */
   char                 uk_name[LEN_VTAM_NAME]; /* Fully qualified name by
						   which the originating 
						   system is known to the VTAM
						   network */
   char 		uk_unit_work[LEN_UNIT]; /* Name by which the unit of
						   work is known within the 
						   originating system */
   char                 uk_org_abend[LEN_ABEND];/* Original Abend code */
   char 		uk_cur_abend[LEN_ABEND];/* Current Abend Code */
   char			filler2[2];
   int 			uk_trm_msgs_sent;       /* Number of messages sent
						   to the principal terminal
						   facility */
   int                  uk_trm_chars_sent;      /* Number of characters sent
						   to the principal terminal 
						   facility */
   int 			uk_trm_msgs_rcvd;	/* Number of messages received 
						   from the principal terminal
						   facility */
   int 			uk_trm_chars_rcvd;	/* Number of characters received						   from the principal terminal 
						   facility */
   int 			uk_trm_storage;         /* Amount of Terminal Storage
						   (TIOA) allocated to the 
						   terminal */
   int			uk_getmains;		/* Number of user storage							   GETMAIN requests issued
						   by user task */
   int			uk_usr_storage;		/* Amount of User Storage
						   allocated to the user task */   int			uk_gets;		/* Number of GET requests issued						   by the user task */
   int                  uk_puts;		/* Number of PUT requests issued						   by user task */
   int 			uk_browses;		/* Number of browse requests
						   issued by user task */
   int 			uk_adds;	        /* Number of ADD requests 
						   issued by user task */
   int			uk_deletes; 		/* Number of delete requests 
						   issued by user task */
   int			uk_file_controls;	/* Total number of file control
						   requests issued by the user
						   task */ 
   int			uk_file_accesses;	/* Number of file access-method 						   interfaces issued by the user						   task */
   int 			uk_td_gets;		/* Number of transient data GET							   requests issued by the user
						   task */
   int 			uk_td_puts;		/* Number of transient data PUT
						   requests issued by the user
						   task */
   int 			uk_td_purges;		/* Number of transient data 
						   DELETE requests issued by the						   user task */
   int 			uk_tds;			/* Total number of transient 
						   data requests issued by the
						   user task */
   int 			uk_ts_gets;             /* Number of temporary storage
						   GET requests issued by the 
						   user task */
   int 			uk_ts_puts_a;		/* Number of auxiliary temporary 						   storage PUTS issued by the
						   user task */
   int			uk_ts_puts_m;           /* Number of main temporary
						   storage PUTS issued by the
						   user task */
   int			uk_tss;			/* Total number of temporary 	  						   storage requests issued
						   by the user task */
   int 			uk_bms_maps;		/* Number of BMS MAP requests
						   issued by the user task */
   int			uk_bms_ins;		/* Number of BMS IN requests
						   issued by the user task */
   int 			uk_bms_outs;		/* Number of BMS OUT requests
						   issued by the user task */
   int			uk_bmss;		/* Total number of BMS requests
						   issued by the user task */
   int			uk_links;		/* Number of program link
						   requests issued by user
						   task */
   int			uk_xctls;		/* Number of XCTL requests
						   issued by the user task */
   int			uk_loads; 		/* Number of LOAD requests
						   issued by the user task */
   int			uk_jrnl_outs;		/* Number of journal output
						   requests during the user 
						   task */
   int			uk_start_initiates;     /* Number of interval control
						   START or INITIATE requests
						   during the user task */
   int 			uk_syncpoints;		/* Number of SYNCPOINT requests
						   issued during the user 
						   task */
   char			uk_uk_sysid[LEN_APPNM];	/* Application Name */

   int			uk_btch_rcd_type;	/* Record Type
							1 - Input Transaction
							2 - Batch Transaction
						*/
   int			uk_btch_open_inputs;    /* Number of OPEN INPUT requests						   from batch jobs */
   int			uk_btch_open_outputs;   /* Number of OPEN OUTPUT
						   requests from batch jobs */
   int			uk_btch_open_ios;	/* Number of OPEN I/O requests
						   from batch jobs */
   int			uk_btch_open_extends;   /* Number of OPEN EXTEND 
						   requests from batch jobs */
   int			uk_btch_closes;		/* Number of CLOSE requests
						   from batch jobs */
   int        		uk_btch_close_locks;	/* Number of CLOSE WITH LOCK
						   requests from batch jobs */
   int 			uk_btch_reads;		/* Number of READ requests
						   from batch jobs */
   int			uk_btch_read_previous;  /* Number of READ PREVIOUS
						   requests from batch jobs */
   int			uk_btch_read_random;    /* Number of READ random 
						   requests from batch jobs */
   int			uk_btch_writes;		/* Number of WRITE requests
						   from batch jobs */
   int 			uk_btch_rewrites;	/* Number of REWRITE requests
						   from batch jobs */
   int			uk_btch_starts_equal;	/* Number of START equal to 
						   full length prime key
						   requests from batch jobs */
   int			uk_btch_starts_any;     /* Number of START equal to
						   any key/record number
						   requests from batch jobs */
   int			uk_btch_starts_greater; /* Number of START greater than
						   requests from batch jobs */
   int 			uk_btch_starts_notless; /* Number of START not less than						   requests from batch jobs */
   int			uk_btch_starts_less;    /* Number of START less than
						   requests from batch jobs */
   int 			uk_btch_deletes;	/* Number of DELETE requests
						   from batch jobs */
   int			uk_btch_commits;  	/* Number of COMMIT requests
						   from batch jobs */
   int			uk_btch_rollbacks;      /* Number of ROLLBACK requests
						   from batch jobs */
   char			uk_rfu[600];		/* Reserved for future use */

   };

struct acntg_info {
   struct acntg_hdr_type acntg_hdr;
   struct acntg_body_type acntg_body;
   };

struct statrcrd {
   unsigned long   	sr_noios; /* no. of physical I/O's */
   unsigned long	sr_wqcount; /* no. of times on wait queue */
   unsigned long  	sr_nordbf; /* no.  of read buffers */
   unsigned long	sr_noread; /* no. of physical reads */
   unsigned long	sr_nosoftwrites; /* no. of logical writes */
   unsigned long	sr_nowrites; /* no. of physical writes */
   unsigned long 	sr_nob4imag; /* no. of before images */
   unsigned long 	sr_nofuncs[MAX_REQUESTS]; /* no. of times each func is called */
   unsigned long	sr_iowcount; /* i/o wait count */
   unsigned long	sr_wbwcount; /* wait for buffer wait count */
   unsigned long	sr_rcwcount; /* recovery messages wait count */
   unsigned long	sr_smwcount; /* semaphore wait wait count */
   unsigned long	sr_idlcount; /* processor idle wait count */
   double		sr_iowtime;  /* i/o wait time */
   double		sr_wbwtime;  /* wait for buffer wait time */
   double		sr_rcwtime;  /* recovery messages wait time */
   double		sr_smwtime;  /* semaphore wait wait time */
   double		sr_idltime;  /* idle wait time */
   double		sr_sys_cpu_time; /* system cpu time used */
   double		sr_app_cpu_time; /* application cpu time used */
   double               sr_start_time; /* Transaction start time */
   double		sr_finish_time; /* Transaction finish time */
   double		sr_response_time; /* Transaction response time */
   double		sr_elapsed_time; /* Elapsed time for transaction */
   double		sr_cics_time; /* time spent in CICS calls */
   struct acntg_info    sr_acntg_info; /* Accounting Information */
   };

struct jct_rcd {
    unsigned short jct_rcd_len;
    unsigned short rcd_typ;
    unsigned int tot_len;
    /*short filler;*/
    short sys_id;
    char  usr_id[2];
    char  rcd_blck[2];
    unsigned short sys_len;
    char  flag1;
    char  flag2;
    char  flag3;
    char  taskno[3];
    char  rcd_time[4];
    char  tranid[4];
    char  termid[4];
    short preflen;
} ;



#endif
