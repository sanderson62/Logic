/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:24:46  $ */
/* $Modtime:   23 Apr 2013 13:24:46  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   kxsec_exits.c  $ $Revision:   1.5  $";
#endif

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/user/security/PVCS/kxsec_exits.c_v  $
 *
 *    Rev 1.5   23 Apr 2013 13:24:46   unikix
 * Update Dell copyright
 *
 *    Rev 1.4   18 Jan 2013 08:56:40   unikix
 * Dell rebranding
 *
 *    Rev 1.3   19 Oct 2011 10:37:38   ss134151
 * D7002011
 * Add kxsec_getEntryInfo function prototype
 *
 *    Rev 1.2   10 Jul 2007 14:34:40   dd134127
 * B6378594
 * Add esmi to the non-STDC arguments of kxsec_login and kxsec_pw_manag.
 *
 *    Rev 1.1   07 Jun 2007 10:55:14   dd134127
 * Bug# 6378594 Add CICS-specific user attributes and
 *      LASTUSETIME to MTP's ESM interface
 *
 *    Rev 1.0   31 May 2007 13:40:08   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.4   20 Jun 2006 14:36:00   rh134138
 * Bug# 6378594 Add CICS-specific user attributes and
 *      LASTUSETIME to MTP's ESM interface
 * Post-EIP-rework
 *
 *    Rev 1.3   26 May 2006 09:20:24   rh134138
 * Bug# 6378594 Add CICS-specific user attributes and
 *      LASTUSETIME to MTP's ESM interface
 *
 *    Rev 1.2   30 Aug 2004 17:19:14   ss134151
 * B4980533
 * Improve 'kxsec_rescheck_q' parameter comments re. precedence of 'resource_class'
 *  over 'resource_type_req' that user's code must be aware of.
 *
 *    Rev 1.1   13 Jan 2004 11:34:58   ds134155
 * D4774439
 * Changes for QUERY SECURITY and SIGNON
 *
 *    Rev 1.0   17 Nov 2003 13:51:12   unikix
 * New 8.0 archive
 *
 *    Rev 1.0   19 Sep 2001 16:47:50   unikix
 * Initial 7.2
 *
 *    Rev 1.1   19 May 2000 08:40:28   daved
 * Remove references to UniKix.
 *
 *    Rev 1.0   30 Jan 2000 11:48:00   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 20:00:08   unikix
 * Initial 6.0
 *
 *    Rev 1.0   03 Sep 1998 15:20:04   steve
 * Initial revision.
 *
 *    Rev 1.0   05/22/97 17:05:02   unikix
 * Initial 5.1
 *
 *    Rev 1.2   07/31/96 13:29:18   unikix
 * Had to change name on non-ansi compiles on previous install
 *
 *    Rev 1.1   07/31/96 11:52:26   unikix
 * Corrected the argument types in the prototype
 *
 *    Rev 1.0   07/26/96 14:06:54   steve
 * Initial revision.
 *
 */

/*
kxsec_exits.c exported routines:
        kxsec_login
        kxsec_logout
        kxsec_rescheck
        kxsec_docache
        kxsec_dontcache
        kxsec_clearcache
        kxsec_getEntryInfo

kxsec_exits.c exported variables:

kxsec_exits.c imports:

*/

/* ------------- unix includes ------------- */

#include <sys/types.h>
#ifdef __STDC__
#include <unistd.h>
#endif
#ifdef __STDC__
#include <stdlib.h>
#endif

/* ------------ Product includes ------------ */

#include "security.h"

/* ------------- local defines ------------- */


/* --------- external definitions ---------- */


/* ---------- static definitions ----------- */


/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_login
%f
 *
 * DESCRIPTION:
 *      This function is called to log on the indicated userid.
 *
%d
 * INPUTS:
 *   username           - the name of the user being logged on
 *   username_length    - the number of characters in that user name
 *   rolename           - the name of the user being logged on
 *   rolename_length    - the number of characters in that role name
 *   password           - the password for that user
 *   password_length    - the number of characters in that password
 *   newpw              - the new password for that user, if supplied
 *   newpw_length       - the number of characters in that new password, zero
 *                              if not supplied
 *   ESMoprInfo pointer - Pointer to callers space to put the ESM Information
 *
 * OUTPUTS:
 *   Return Codes:
 *     LOGIN_SUCCESS            - User is successfully logged on and password
 *                                updated if new password was supplied
 *     LOGIN_USERID_NOT_FOUND   - User not found on ESM
 *     LOGIN_PW_FAILURE         - Failed password check on ESM.
 *     LOGIN_ROLEID_NOT_FOUND   - Role not found in ESM
 *     LOGIN_PASSWORD_EXPIRED   - User's password has expired
 *     LOGIN_INVALID_PW_FORMAT  - Invalid password format
 *     LOGIN_PW_SUSPENDED       - Password has been suspended
 *     LOGIN_USERID_NOT_IN_ROLE - User does not belong in role
 *     LOGIN_ESM_FAILED         - Unknown problem in ESM
 *
 * CAVEATS:
 *
%F
 ************************************************************************/

int kxsec_login(
#ifdef __STDC__
   char *username, int username_length,
   char *rolename, int rolename_length,
   char *password, int password_length,
   char *newpw, int newpw_length,
   struct ESMoprInfo *esmi)
#else
   username, username_length, rolename, rolename_length,
   password, password_length, newpw, newpw_length, esmi)
char *username;
int username_length;
char *rolename;
int rolename_length;
char *password;
int password_length;
char *newpw;
int newpw_length;
struct ESMoprInfo *esmi;
#endif
{
   return LOGIN_SUCCESS;
}

/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_pw_manag
%f
 *
 * DESCRIPTION:
 *      This function is called to check or change PW.
 *
%d
 * INPUTS:
 *   username           - the name of the user verifying pw
 *   username_length    - the number of characters in that user name
 *   password           - the password for that user
 *   password_length    - the number of characters in that password
 *   newpw              - the new password for that user, if supplied
 *   newpw_length       - the number of characters in that new password, zero
 *                              if not supplied
 *   ESMpwInfo pointer  - Pointer to callers space to put the ESM Information
 *
 * OUTPUTS:
 *   Return Codes:
 *     LOGIN_SUCCESS            - Password successfully verified and password
 *                                updated if new password was supplied
 *     LOGIN_USERID_NOT_FOUND   - User not found on ESM
 *     LOGIN_PW_FAILURE         - Failed password check on ESM.
 *     LOGIN_PASSWORD_EXPIRED   - User's password has expired
 *     LOGIN_INVALID_PW_FORMAT  - Invalid password format
 *     LOGIN_PW_SUSPENDED       - Password has been suspended
 *     LOGIN_USERID_NOT_IN_ROLE - User does not belong in role
 *     LOGIN_ESM_FAILED         - Unknown problem in ESM
 *
 * CAVEATS:
 *
%F
 ************************************************************************/

int kxsec_pw_manag(
#ifdef __STDC__
   char *username, int username_length,
   char *password, int password_length,
   char *newpw, int newpw_length,
   struct ESMpwInfo *esmi)
#else
   username, username_length,
   password, password_length, newpw, newpw_length, esmi)
char *username;
int username_length;
char *password;
int password_length;
char *newpw;
int newpw_length;
struct ESMpwInfo *esmi;
#endif
{
   return LOGIN_SUCCESS;
}

/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_logout
%f
 *
 * DESCRIPTION:
 *      This function is called to log off the indicated userid.
 *
%d
 * INPUTS:
 *   username           - the name of the user to be logged off
 *   username_length    - the number of characters in that user name
 *
 * OUTPUTS:
 *   Return Codes:
 *     TRUE             - User is successfully logged off
 *     FALSE            - User was not successfully logged off
 *
 * CAVEATS:
 *
%F
 ************************************************************************/

boolean kxsec_logout(
#ifdef __STDC__
   char *username, int username_length)
#else
   username, username_length)
char *username;
int username_length;
#endif
{
   return TRUE;
}

/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_docache
%f
 *
 * DESCRIPTION:
 *      This function is called to enable caching of results from asset
 *      checking done by this process. This eliminates multiple calls to
 *      an external security manager for the same asset / permission.
 *
%d
 * INPUTS:
 *   None
 *
 * OUTPUTS:
 *   None
 *
 * CAVEATS:
 *
%F
 ************************************************************************/
void kxsec_docache()
{
   return;
}

/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_clearcache
%f
 *
 * DESCRIPTION:
 *      This function is called to clear the cache of results from asset
 *      checking done by this process. This is done whenever the cached results
 *      must be refreshed from the external security manager (eg. end of tx).
 *
%d
 * INPUTS:
 *   None
 *
 * OUTPUTS:
 *   None
 *
 * CAVEATS:
 *
%F
 ************************************************************************/
void kxsec_clearcache()
{
   return;
}

/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_dontcache
%f
 *
 * DESCRIPTION:
 *      This function is called to disable caching of results from asset
 *      checking by this process. This call is only needed if caching is
 *      explicitly enabled by calling kxsec_docache(), since the default is
 *      that caching is disabled.
 *
%d
 * INPUTS:
 *   None
 *
 * OUTPUTS:
 *   None
 *
 * CAVEATS:
 *
%F
 ************************************************************************/
void kxsec_dontcache()
{
   return;
}

/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_rescheck
%f
 *
 * DESCRIPTION:
 *      This function is called to check for permission to an asset by
 *      the indicated userid.
 *
%d
 * INPUTS:
 *   asset_type         - the asset type, e.g. KIX_FILES
 *   asset_name         - the name of the asset, e.g. the dataset name
 *   asset_name_length  - the number of characters in that asset name
 *   username           - the name of the user for whom access is being checked
 *   username_length    - the number of characters in that user name
 *   access_type        - the permission being requested, e.g. WriteAccess
 *
 * OUTPUTS:
 *   Return Codes:
 *     TRUE             - User is granted the requested permission to the asset
 *     FALSE            - User is denied the requested permission to the asset
 *
 * CAVEATS:
 *
%F
 ************************************************************************/
boolean kxsec_rescheck(
#ifdef __STDC__
   enum asset_type asset_type_req,
   char *asset_name, int asset_name_length,
   char *username, int username_length,
   enum access_type access_type_req)
#else
   asset_type_req, asset_name, asset_name_length,
   username, username_length, access_type_req)
enum asset_type asset_type_req;
char    *asset_name;
int     asset_name_length;
char    *username;
int     username_length;
enum access_type access_type_req;
#endif

{
   return TRUE;
}

/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_rescheck_q
%f
 *
 * DESCRIPTION:
 *      This function is called to check for permission to an resource by
 *      the indicated userid.
 *
%d
 * INPUTS:
 *   resource_type_req    - the CICS resource type, e.g. KIX_FILES
 *   resource_name        - the name of the resource, e.g. dataset name
 *   resource_name_length - the number of characters in that resource name
 *   resource_class       - user defined resource type name
 *   resource_class_length - length of user defined resource class
 *   username           - the name of the user for whom access is being checked
 *   username_length    - the number of characters in that user name
 *   log_this           - TRUE if logging, FALSE if not.
 *   access_type_req    - the permission being requested, e.g. WriteAccess
 *
 * OUTPUTS:
 *   Return Codes:
 *     TRUE             - User is granted requested permission to the resource
 *     FALSE            - User is denied requested permission to the resource or
 *                        there was a problem.
 *
 * CAVEATS:
 *   Note; resource_type_req should be used only if resource_class is null or
 *         resource_class_length is zero.
 *
%F
 ************************************************************************/
boolean
kxsec_rescheck_q(
#ifdef __STDC__
   enum asset_type resource_type_req,
   char *resource_name, int resource_name_length,
   char *resource_class, int resource_class_length,
   char *username, int username_length,
   boolean log_this,
   enum access_type access_type_req)
#else
   resource_type_req,
   resource_name, resource_name_length,
   resource_class, resource_class_length,
   username, username_length,
   log_this,
   access_type_req)
enum asset_type resource_type_req;
char           *resource_name;
int             resource_name_length;
char           *resource_class;
int             resource_class_length;
char           *username;
int             username_length;
boolean         log_this;
enum access_type access_type_req;
#endif
{
   return TRUE;
}


/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_mgrstatus
%f
 *
 * DESCRIPTION:
 *      This function is called to determine whether the external security
 *      manager is operational or not
 *
%d
 * INPUTS:
 *   None
 *
 * OUTPUTS:
 *   Return Codes:
 *     TRUE             - External security manager is operational and ready
 *     FALSE            - External security manager is not currently operational
 *
 * CAVEATS:
 *
%F
 ************************************************************************/

boolean kxsec_mgrstatus()
{
   return FALSE;
}


/*************************************************************************
%F
%d
%f
 * Name of Function: kxsec_getEntryInfo
%f
 *
 * DESCRIPTION:
 *      This function is called to get the 'info' field of the requested entry
 *      by the requesting userid.
 *
%d
 * INPUTS:
 *   entry_class                - name of the entry class (eg. "userid")
 *   entry_class_length         - length of entry class
 *   entry_name                 - name of the entry, eg. user name
 *   entry_name_length          - number of characters in that entry name
 *
 * OUTPUTS:
 *   Return Codes:
 *     0                        - User has access to the entry, info returned
 *     non-zero                 - User can't access the entry, or other error
 *   entry_info                 - the 'description' field for that entry,
 *                                    256 bytes in length (blank-filled)
 *
 * CAVEATS:
 *
%F
 ************************************************************************/
int
kxsec_getEntryInfo(
   char *entry_class, int entry_class_length,
   char *entry_name, int entry_name_length,
   char entry_info[256])
{
   return 1; /* no info to return */
}

