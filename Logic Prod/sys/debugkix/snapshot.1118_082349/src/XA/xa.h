/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

#ifndef XA_H
#define XA_H

/**********************************************************************/
/* $Date:   23 Apr 2013 13:20:04  $ */
/* $Modtime:   23 Apr 2013 13:20:04  $ */
/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/lib/public/PVCS/xa.h_v  $
 *
 *    Rev 1.2   23 Apr 2013 13:20:04   unikix
 * Update Dell copyright
 *
 *    Rev 1.1   18 Jan 2013 08:50:12   unikix
 * Dell rebranding
 *
 *    Rev 1.0   31 May 2007 13:37:54   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.2   16 Jun 2004 14:18:36   jt134157
 * D5059505
 * Add support for ax_* API.
 *
 *    Rev 1.1   30 Jan 2004 12:44:52   jt134157
 * D4979498
 * Add NULLXID constant.
 *
 *    Rev 1.0   17 Nov 2003 13:35:44   unikix
 * New 8.0 archive
 *
 *    Rev 1.0   25 Apr 2003 14:26:46   jt134157
 * Initial revision.
 */


/*
 * Transaction branch identification: XID
 */
#ifndef XIDDATASIZE
#define XIDDATASIZE     128             /* size in bytes */
#define MAXGTRIDSIZE    64              /* maximum size in bytes of gtrid */
#define MAXBQUALSIZE    64              /* maximum size in bytes of bqual */
struct xid_t {
   long formatID;                  /* format identifier */
   long gtrid_length;              /* value not to exceed 64 */
   long bqual_length;              /* value not to exceed 64 */
   char data[XIDDATASIZE];
};
typedef struct xid_t XID;
/*
 * A value of -1 in formatID means that the XID is null.
 */
#define NULLXID -1

#endif

#ifndef TX_H
/*
 * XA Options
 */
typedef long TRANSACTION_TIMEOUT;       /* type of transaction timeouts */
#endif

/*
 * Structure for optional XA information
 */
struct xactl_t {
   long flags;                     /* valid element flags */
   TRANSACTION_TIMEOUT timeout;    /* timeout value */
};
typedef struct xactl_t XACTL;
#define XAOPTS_NOFLAGS 0x00000000L      /* no optional value */
#define XAOPTS_TIMEOUT 0x00000001L      /* timeout value present */

/*
 * Declarations of routines by which RMs call TMs:
 */

#if defined(__cplusplus)
extern "C" {
#endif

extern int ax_reg(int, XID *, long);
extern int ax_unreg(int, long);

/*
 * XA Switch Data Structure
 */
#ifndef RMNAMESZ
#define RMNAMESZ 32             /* length of resource manager name, */
/* including the null terminator */
#define MAXINFOSIZE 256         /* maximum size in bytes of xa_info strings, */
/* including the null terminator */
struct xa_switch_t {
   char name[RMNAMESZ];          /* name of resource manager */
   long flags;                   /* resource manager specific options */
   long version;                 /* 0 = XA compatible, 1 = XA+ compatible */
   int (*xa_open_entry)(char *, int, long);              /* xa_open function pointer */
   int (*xa_close_entry)(char *, int, long);             /* xa_close function pointer*/
   int (*xa_start_entry)(XID *, int, long);              /* xa_start function pointer */
   int (*xa_end_entry)(XID *, int, long);                /* xa_end function pointer */
   int (*xa_rollback_entry)(XID *, int, long);           /* xa_rollback function pointer */
   int (*xa_prepare_entry)(XID *, int, long);            /* xa_prepare function pointer */
   int (*xa_commit_entry)(XID *, int, long);             /* xa_commit function pointer */
   int (*xa_recover_entry)(XID *, long, int, long);      /* xa_recover function pointer*/
   int (*xa_forget_entry)(XID *, int, long);             /* xa_forget function pointer */
   int (*xa_complete_entry)(int *, int *, int, long);    /* xa_complete function pointer */
   /* XA+ extensions (= version 1 format) */
   int (*xa_ready_entry)(XID *, int, long);              /* xa_ready function pointer */
   int (*xa_done_entry)(XID *, int, long);               /* xa_done function pointer */
   int (*xa_wait_recovery_entry)(int, long);             /* xa_wait_recovery function pointer */
   int (*xa_wait_entry)(int, long);                      /* xa_wait function pointer */
   int (*xa_start_2_entry)(XID *, int, XACTL *, long);   /* xa_start_2 function pointer */
   struct ax_switch_t **xa_tmswitch;                     /* Location of TM switch pointer */
};
#endif

/*
 * AX Switch Data Structure
 */
struct ax_switch_t {
   long flags;                                     /* transaction manager flags */
   long version;                                   /* must be 0 */
   int (*ax_reg_entry)(int, XID *, long);          /* ax_reg function pointer */
   int (*ax_unreg_entry)(int, long);               /* ax_unreg function pointer */
   int (*ax_start_entry)(int, XID *, long);        /* ax_start function pointer */
   int (*ax_end_entry)(int, XID *, long);          /* ax_end function pointer */
   int (*ax_rollback_entry)(int, XID *, long);     /* ax_rollback function pointer */
   int (*ax_prepare_entry)(int, XID *, long);      /* ax_prepare function pointer */
   int (*ax_commit_entry)(int, XID *, long);       /* ax_commit function pointer */
   int (*ax_recover_entry)(int, XID *, long, long); /* ax_recover function pointer */
   int (*ax_add_branch_entry)(int, XID *, long);   /* ax_add_branch function pointer */
   int (*ax_forget_branch_entry)(int, XID *, long); /* ax_forget_branch function pointer */
   int (*ax_set_branch_info_entry)(int, XID *, char *, long, long); /* ax_set_branch_info function pointer */
   int (*ax_get_branch_info_entry)(int, XID *, char *, long *, long); /* ax_get_branch_info function pointer */
   int (*ax_ready_entry)(int, XID *, long);        /* ax_ready function pointer */
   int (*ax_done_entry)(int, XID *, long);         /* ax_done function pointer */
   int (*ax_reg_2_entry)(int, XID *, XACTL *, long); /* ax_reg_2 function pointer */
   int (*ax_start_2_entry)(int, XID *, XACTL *, long); /* ax_start_2 function pointer */
};
#if defined(__cplusplus)
}
#endif


/*
 * Flag definitions for the RM switch
 */
#ifndef TMNOFLAGS
#define TMNOFLAGS       0x00000000L     /* no resource manager features
                                         * selected
                                         */
#define TMREGISTER      0x00000001L     /* resource manager dynamically
                                         * registers
                                         */
#define TMNOMIGRATE     0x00000002L     /* resource manager does not support
                                         * association migration
                                         */
#define TMUSEASYNC      0x00000004L     /* resource manager supports
                                         * asynchronous operations
                                         */
#define TMUSECHAIN      0x00000008L     /* resource manager supports
                                         * transaction chaining
                                         */
#define TMUSEOPTS       0x00000010L     /* resource manager supports
                                         * xa_start_2
                                         */
#define TMUSE2PHASE     0x00000020L     /* The RM might force upgrading
                                         * one-phase commit to two-phase commit
                                         */
#define TMSWITCHOK      0x00000040L     /* resource manager has provided
                                         * location for address of
                                         * transaction manager switch
                                         */
#define TMNOROLLALLOWED 0x00000080L     /* tx_rollback is not permitted
                                         * in subordinates
                                         */
#define TMNOCOMALLOWED  0x00000100L     /* tx_commit is not permitted
                                         * in subordinates
                                         */
#define TMUSETHREADS    0x00000200L     /* resource manager can use threads
                                         * as thread of control
                                         */
#endif

/*
 * Flag definitions for the TM switch
 */
#define TMSUPPORTSTHREADS       0x00000001L     /* The TM is prepared to use
                                                 * threads as thread of control
                                                 */
#define TMSUBORDINATE           0x00000002L     /* The subordinate set of ax_()
                                                 * functions can be called
                                                 */
/*
 * Flag definitions for xa_ and ax_ routines
 */
/* use TMNOFLAGS, defined above, when not specifying other flags */
#ifndef TMASYNC
#define TMASYNC         0x80000000L     /* perform routine asynchronously */
#define TMONEPHASE      0x40000000L     /* caller is using one-phase commit
                                         * optimisation
                                         */
#define TMFAIL          0x20000000L     /* dissociates caller and marks
                                         * transaction branch rollback-only
                                         */
#define TMNOWAIT        0x10000000L     /* return if blocking condition exists */
#define TMRESUME        0x08000000L     /* caller is resuming association
                                         * with suspended transaction branch
                                         */
#define TMSUCCESS       0x04000000L     /* dissociate caller from transaction
                                         * branch
                                         */
#define TMSUSPEND       0x02000000L     /* caller is suspending, not ending,
                                         * association
                                         */
#define TMSTARTRSCAN    0x01000000L     /* start a recovery scan */
#define TMENDRSCAN      0x00800000L     /* end a recovery scan */
#define TMMULTIPLE      0x00400000L     /* wait for any asynchronous operation */
#define TMJOIN          0x00200000L     /* caller is joining existing
                                         * transaction branch
                                         */
#define TMMIGRATE       0x00100000L     /* caller intends to perform migration */

#define TMRECOVER       0x00080000L     /* call is in recovery mode */
#define TMCHAINED       0x00040000L     /* call is in transaction chaining mode */
#define TMDEFERRED      0x00020000L     /* start is pending acceptance by
                                         * the application program
                                         */
#endif

/*
 * Maximum values for ax_* functions
 */
#define TMMAXBLOBLEN    1024    /* maximum blob_len for ax_set_branch_info() */
#define TMMAXBLOBTOT    8192    /* maximum total blob data created using
                                 * ax_set_branch_info() for all branches
                                 * created at this node for a given transaction
                                 */
/*
 * ax_() return codes (transaction manager reports to resource manager)
 */
#ifndef TM_OK
#define TM_RBBASE       100             /* the inclusive lower bound
                                         * of the rollback codes */
#define TM_RBROLLBACK   TM_RBBASE       /* the rollback was caused by
                                         * an unspecified reason */
#define TM_RBCOMMFAIL   TM_RBBASE+1     /* the rollback was caused by
                                         * a communication failure */
#define TM_RBDEADLOCK   TM_RBBASE+2     /* a deadlock was detected */
#define TM_RBINTEGRITY  TM_RBBASE+3     /* a condition that violates
                                         * the integrity of the resources
                                         * was detected */
#define TM_RBOTHER      TM_RBBASE+4     /* the resource manager rolled back
                                         * the transaction branch for a reason
                                         * not on this list */
#define TM_RBPROTO      TM_RBBASE+5     /* a protocol error occurred in the
                                         * resource manager */
#define TM_RBTIMEOUT    TM_RBBASE+6     /* a transaction branch took too long */
#define TM_RBTRANSIENT  TM_RBBASE+7     /* may retry the transaction branch */
#define TM_RBEND        TM_RBTRANSIENT  /* the inclusive upper bound
                                         * of the rollback codes */
#define TM_DEFERRED     11              /* the commit decision has not been made */
#define TM_RETRY_COMMFAIL 10            /* ax_commit could not be completed due
                                         * to communication failure */
#define TM_NOMIGRATE    9               /* resumption must occur where suspension occurred */
#define TM_HEURHAZ      8               /* the transaction branch may have been
                                         * heuristically completed */
#define TM_HEURCOM      7               /* the transaction branch may have been
                                         * heuristically committed */
#define TM_HEURRB       6               /* the transaction branch may have been
                                         * heuristically rolled back */
#define TM_HEURMIX      5               /* the transaction branch may have been
                                         * heuristically committed and rolled back */
#define TM_RDONLY       3               /* the transaction branch was read-only and
                                         * has been committed */
#define TM_JOIN         2       /* caller is joining existing transaction
                                 * branch
                                 */
#define TM_RESUME       1       /* caller is resuming association with
                                 * suspended transaction branch
                                 */
#define TM_OK           0       /* normal execution */
#define TMER_TMERR      -1      /* an error occurred in the
                                 * transaction manager
                                 */
#define TMER_INVAL      -2      /* invalid arguments were given */
#define TMER_PROTO      -3      /* routine invoked in an improper context */
#define TMER_NOTA       -4      /* the XID is not valid */
#define TMER_DUPID      -8      /* the XID already exits */
#endif
/*
 * xa_() return codes (resource manager reports to transaction manager)
 */
#define XA_RBBASE       100             /* The inclusive lower bound of the
                                         * rollback codes
                                         */
#define XA_RBROLLBACK   XA_RBBASE       /* The rollback was caused by an
                                         * unspecified reason
                                         */
#define XA_RBCOMMFAIL   XA_RBBASE+1     /* The rollback was caused by a
                                         * communication failure
                                         */
#define XA_RBDEADLOCK   XA_RBBASE+2     /* A deadlock was detected */
#define XA_RBINTEGRITY  XA_RBBASE+3     /* A condition that violates the integrity
                                         * of the resources was detected
                                         */
#define XA_RBOTHER      XA_RBBASE+4     /* The resource manager rolled back the
                                         * transaction branch for a reason not
                                         * on this list
                                         */
#define XA_RBPROTO      XA_RBBASE+5     /* A protocol error occurred in the
                                         * resource manager
                                         */
#define XA_RBTIMEOUT    XA_RBBASE+6     /* A transaction branch took too long */
#define XA_RBTRANSIENT  XA_RBBASE+7     /* May retry the transaction branch */
#define XA_RBEND        XA_RBTRANSIENT  /* The inclusive upper bound of the
                                         * rollback codes
                                         */

#define XA_TWOPHASE     13      /* Use two-phase commit */
#define XA_PROMOTED     12      /* AP promoted to initiator */
#define XA_DEFERRED     11      /* the commit decision has not been made */
#define XA_RETRY_COMMFAIL 10    /* the xa_commit could not be completed
                                 * due to communication failure */
#define XA_NOMIGRATE    9       /* resumption must occur where
                                 * suspension occurred
                                 */
#define XA_HEURHAZ      8       /* the transaction branch may have
                                 * been heuristically completed
                                 */
#define XA_HEURCOM      7       /* the transaction branch has been
                                 * heuristically committed
                                 */
#define XA_HEURRB       6       /* the transaction branch has been
                                 * heuristically rolled back
                                 */
#define XA_HEURMIX      5       /* the transaction branch has been
                                 * heuristically committed and rolled back
                                 */
#define XA_RETRY        4       /* routine returned with no effect and
                                 * may be re-issued
                                 */
#define XA_RDONLY       3       /* the transaction branch was read-only and
                                 * has been committed
                                 */
#define XA_OK           0       /* normal execution */
#define XAER_ASYNC      -2      /* asynchronous operation already outstanding */
#define XAER_RMERR      -3      /* a resource manager error occurred in the
                                 * transaction branch
                                 */
#define XAER_NOTA       -4      /* the XID is not valid */
#define XAER_INVAL      -5      /* invalid arguments were given */
#define XAER_PROTO      -6      /* routine invoked in an improper context */
#define XAER_RMFAIL     -7      /* resource manager unavailable */
#define XAER_DUPID      -8      /* the XID already exists */
#define XAER_OUTSIDE    -9      /* resource manager doing work outside */
/* global transaction */

#endif /* ifndef XA_H */
/*
 * End of xa.h header
 */
