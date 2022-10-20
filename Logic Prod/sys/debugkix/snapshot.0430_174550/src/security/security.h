/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   06 Nov 2008 09:02:16  $ */
/* $Modtime:   04 Nov 2008 15:23:40  $ */

/* $Workfile:   security.h  $ $Revision:   1.2  $ */

/*
 * $Log:   /ENG/rh134138/TPE11.0.3/unikixsrc/lib/public/PVCS/security.h_v  $
 * 
 *    Rev 1.2   06 Nov 2008 09:02:16   rh134138
 * Bug# 7000921 INQ/SET TRANCLASS access permission check uses
 *              KIX_COMMAND,TCLASS instead of KIX_COMMAND,TRANCLASS
 * 
 *    Rev 1.1   07 Jun 2007 09:58:16   dd134127
 * Bug# 6378594 Add CICS-specific user attributes and
 *      LASTUSETIME to MTP's ESM interface
 * 
 *    Rev 1.0   31 May 2007 13:37:54   unikix
 * Initial TPE11.0.0a
 * 
 *    Rev 1.7   20 Jun 2006 13:08:12   rh134138
 * Bug# 6378594 Add CICS-specific user attributes and
 *      LASTUSETIME to MTP's ESM interface
 * Post-EIP-rework
 * 
 *    Rev 1.6   25 May 2006 14:30:14   rh134138
 * Bug# 6378594 Add CICS-specific user attributes and
 *      LASTUSETIME to MTP's ESM interface
 * 
 *    Rev 1.5.1.0   02 Nov 2006 14:26:28   mf134128
 * b6387367
 * Support EXEC CICS START USERID
 * 
 *    Rev 1.5   09 Dec 2005 09:34:14   rh134138
 * Bug# 6358065 MTP must report thru CESN login the difference between
 *              'AccountExpiration' and 'PasswordExpiration'
 * 
 *    Rev 1.4   11 Oct 2004 09:50:14   rh134138
 * BugTraq# 5094094 QUERY SECURITY missing essential error recognition code (runtime & translate)
 * (refix)
 * 
 *    Rev 1.3   31 Aug 2004 06:55:44   rh134138
 * BugTraq# 5094094 QUERY SECURITY is error handling deficient
 * 
 *    Rev 1.2   14 Jan 2004 08:54:20   ds134155
 * D4774439
 * Added prototype for kxsec_rescheck_q for QUERY SECURITY
 * 
 *    Rev 1.1   23 Dec 2003 01:08:42   ds134155
 * B4774439
 * Changes for QUERY SECURITY and SIGNON
 * 
 *    Rev 1.0   17 Nov 2003 13:35:42   unikix
 * New 8.0 archive
 * 
 *    Rev 1.2   14 Nov 2002 12:58:14   ks134152
 * 4769271
 * Added support for INQUIRE TSQUEUE
 * 
 *    Rev 1.1   12 Apr 2002 14:19:24   ss134151
 * 4627972
 * Add KIX_COMMANDS resource names for use in ESM permission checking.
 * 
 *    Rev 1.0   19 Sep 2001 16:40:18   unikix
 * Initial 7.2
 * 
 *    Rev 1.1   19 May 2000 08:16:14   daved
 * Remove references to UniKix.
 * 
 *    Rev 1.0   30 Jan 2000 11:42:30   unikix
 * Initial 7.0
 * 
 *    Rev 1.0   15 Mar 1999 19:55:08   unikix
 * Initial 6.0
 * 
 *    Rev 1.1   03 Sep 1998 15:33:36   steve
 * B003328
 * Cleaned up dependency on & moved macros to kxconst.h, to support user exit code.
 * 
 *    Rev 1.0   05/22/97 17:03:54   unikix
 * Initial 5.1
 * 
 *    Rev 1.0   07/26/96 14:11:44   steve
 * Initial revision.
 * 
 *    Rev 1.0   07/01/96 08:45:30   unikix
 * UniKix 5.0 version
 */

/*------------	declarations for security manager constants ---------------*/
#define ESM_OPNAME 20
#define ESM_OPCLASS 3
#define ESM_OPIDENT 3

struct ESMoprInfo {
    char          ESM_opname[ESM_OPNAME];/*operator name*/
    unsigned char ESM_opclass[ESM_OPCLASS];/*operator class*/
    char          ESM_opident[ESM_OPIDENT];/*operator identification*/
};

struct ESMpwInfo {
    long long ESM_lastTimeLogdOn; /* ABSTIME of last logged in time */
    long long ESM_lastTimeChgPW;  /* ABSTIME of last time password changed */
    long long ESM_expiryTime;     /* ABSTIME of when password will expire */
    short     ESM_daysLeft;       /* Count of days from now until PW expires */
};

enum asset_type {	/* asset-types for kxsec_rescheck */
   	KIX_FILES = 0,
   	KIX_PROGRAMS,
	KIX_JOURNALS,
	KIX_COMMANDS,
	KIX_START_TRANS,
	KIX_ATTACH_TRANS,
	KIX_TD_QUEUE,
	KIX_TS_QUEUE,
	KIX_TERMINALS,
	UNIX_APPLS,
	KIX_SURROGATE
};

enum access_type {	/* access-types for kxsec_rescheck */
   	ReadAccess = 0,
	WriteAccess,
	UpdateAccess,
	ExecuteAccess,
	CreateAccess,
	DeleteAccess,
        ControlAccess,
        AlterAccess
};

/* following are command resource names used for KIX_COMMANDS
   security access checking (as per CICS documentation) */

#define  CONNECTION_COMMAND     "CONNECTION"
#define  DESTINATION_COMMAND    "TDQUEUE"
#define  FILE_COMMAND           "FILE"
#define  PROGRAM_COMMAND        "PROGRAM"
#define  SECURITY_COMMAND       "SECURITY"
#define  SHUTDOWN_COMMAND       "SHUTDOWN"
#define  SYSTEM_COMMAND         "SYSTEM"
#define  TASK_COMMAND           "TASK"
#define  TERMINAL_COMMAND       "TERMINAL"
#define  TRANCLASS_COMMAND      "TRANCLASS"
#define  TRANSACTION_COMMAND    "TRANSACTION"
#define  TEMPQUEUE_COMMAND      "TSQUEUE"

#define  LOGIN_SUCCESS            0
#define  LOGIN_PW_FAILURE         1
#define  LOGIN_ROLEID_NOT_FOUND   2
#define  LOGIN_PASSWORD_EXPIRED   3
#define  LOGIN_INVALID_PW_FORMAT  4
#define  LOGIN_PW_SUSPENDED       5
#define  LOGIN_USERID_NOT_IN_ROLE 6
#define  LOGIN_USERID_NOT_FOUND   7
#define  LOGIN_ESM_FAILED         8
#define  LOGIN_MIN_PW_DUR_FAIL    9
#define  LOGIN_MUST_CHG_PW       10
#define  LOGIN_ACCT_EXPIRED      11


#ifndef _KXCONST_H
typedef char  boolean;

#ifndef TRUE
#define  TRUE        1
#endif

#ifndef FALSE
#define  FALSE       0
#endif
#endif



/*-------------------------------------------------------------------------*/

/*-------------	declarations for security manager functions ---------------*/

boolean kxsec_rescheck(		/*	resource access check 	*/
#ifdef __STDC__
		       enum asset_type asset_type_req, 
		       char *asset_name, int asset_name_length, 
		       char *username, int username_length, 
		       enum access_type access_type_req
#endif
			);

boolean kxsec_rescheck_q(       /* resource access check for query security */
#ifdef __STDC__
                       enum asset_type asset_type_req,
                       char *asset_name, int asset_name_length,
                       char *asset_class, int asset_class_length,
                       char *username, int username_length,
                       boolean log_this,
                       enum access_type access_type_req
#endif
                        );

void kxsec_docache(		/*	enable caching of asset check results */
#ifdef __STDC__
			void
#endif
			);

void kxsec_clearcache(		/*	clear current results cache contents */
#ifdef __STDC__
			void
#endif
			);

void kxsec_dontcache(		/*	disable caching of asset check results */
#ifdef __STDC__
			void
#endif
			);

int kxsec_login(		/*	user login check	*/
#ifdef __STDC__
		    char *username, int username_length, 
		    char *rolename, int rolename_length, 
		    char *password, int password_length, 
		    char *newpw, int newpw_length,
		    struct ESMoprInfo *esmi
#endif
		    );

int kxsec_pw_manag(		/*	user password check	*/
#ifdef __STDC__
		    char *username, int username_length, 
		    char *password, int password_length, 
		    char *newpw, int newpw_length,
		    struct ESMpwInfo *esmi
#endif
		    );

boolean kxsec_logout(		/*	user logout check	*/
#ifdef __STDC__
		    char *username, int username_length 
#endif
		    );

boolean kxsec_mgrstatus(		/*	external security manager status */
#ifdef __STDC__
			void
#endif
			);

