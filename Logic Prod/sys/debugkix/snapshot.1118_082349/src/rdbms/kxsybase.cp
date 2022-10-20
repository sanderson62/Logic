/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007-2013 Dell Inc.                                  */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


#include <stdio.h>
#include <stdarg.h>
#include <errno.h>

void kx_sql_errmsg_rtn();

EXEC SQL INCLUDE sqlca;

/* Sybase required includes */
#include <sybhesql.h>
#include <sybtesql.h>

EXEC SQL BEGIN DECLARE SECTION;
   char userid[9];
   char userpass[31];
   char servername[13];
EXEC SQL END DECLARE SECTION;

#include "kxinfo.h"

struct lsyspar kix_sys_info;
struct lextpar kix_tct_info;
struct lmsgstr kix_msg_info;

static int kxbg_flag = 0;

extern void kxsysinfo();
extern void kxsetmsg();

/************************************************************
KXSYBLGN()
*************************************************************/
int KXSYBLGN()
{
   char *lchar;
   strcpy(kix_msg_info.cur_func, "KXSYBLGN");

   EXEC SQL WHENEVER SQLWARNING GOTO sql_warn;
   EXEC SQL WHENEVER NOT FOUND GOTO sql_notfound;
   EXEC SQL WHENEVER SQLERROR GOTO sql_abort;
   /*
   *---------------------------------------------------------------*
   * This call update the  sit information
   * user can use this information to set the userid .
   * In this example, databasename is used for the connection
   *---------------------------------------------------------------*
   */

   kxsysinfo(&kix_sys_info);
   memset (userid, '\0', sizeof(userid));
   memset (userpass, '\0', sizeof(userpass));
   memset (servername, '\0', sizeof(servername));
   memcpy(userid, kix_sys_info.cur_usrname, 8);
   memcpy(userpass, kix_sys_info.cur_usrpass, 30);
   memcpy(servername, kix_sys_info.cur_svrname, 12);

   lchar = strchr(userid, ' ');
   if (lchar) {
      *lchar = 0;
   }

   lchar = strchr(userpass, ' ');
   if (lchar) {
      *lchar = 0;
   }

   lchar = strchr(servername, ' ');
   if (lchar) {
      *lchar = 0;
   }

   EXEC SQL CONNECT :userid  IDENTIFIED BY :userpass
                   USING :servername;

   EXEC SQL BEGIN TRANSACTION;

   kxbg_flag = 1;

   return(0); /* Good return code */

sql_abort:
   kx_sql_errmsg_rtn();
   return(-1); /* This is a fatal and need to abort */

sql_warn:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_notfound:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */
} /* End of KXSYBLGN function */

/************************************************************
KXSYBLGF()
*************************************************************/
int KXSYBLGF()
{
   strcpy(kix_msg_info.cur_func, "KXSYBLGF");

   EXEC SQL WHENEVER SQLWARNING GOTO sql_warn;
   EXEC SQL WHENEVER NOT FOUND GOTO sql_notfound;
   EXEC SQL WHENEVER SQLERROR GOTO sql_error;

   if (kxbg_flag != 1 ) {
      EXEC SQL BEGIN TRANSACTION;
   }
   EXEC SQL ROLLBACK WORK;
   EXEC SQL DISCONNECT CURRENT;
   kxbg_flag = 0;

   return(0); /* Good return code */

sql_error:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_warn:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_notfound:
   kx_sql_errmsg_rtn();
   return(1);  /* This is a fatal and need to abort */

} /* End of KXSYBLGF function */


/************************************************************
KXSYBBTRN()
*************************************************************/
int KXSYBBTRN()
{
   strcpy(kix_msg_info.cur_func, "KXSYBBTRN");

   EXEC SQL WHENEVER SQLWARNING GOTO sql_warn;
   EXEC SQL WHENEVER NOT FOUND GOTO sql_notfound;
   EXEC SQL WHENEVER SQLERROR GOTO sql_abort;
   /*
   *---------------------------------------------------------------*
   * This call update the  tct information
   * user can use this information to inquire the current user info.
   *---------------------------------------------------------------*
   */

   /*
   kxtctinfo(&kix_tct_info);
   */

   if (kxbg_flag != 1 ) {
      EXEC SQL BEGIN TRANSACTION;
   }
   kxbg_flag = 1;
   return(0); /* Good return code */

sql_abort:
   kx_sql_errmsg_rtn();
   return(-1); /* This is a fatal and need to abort */

sql_warn:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_notfound:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

} /* End of KXSYBBTRN function */


/************************************************************
KXSYBETRN()
*************************************************************/
int KXSYBETRN()
{
   EXEC SQL WHENEVER SQLWARNING GOTO sql_warn;
   EXEC SQL WHENEVER NOT FOUND GOTO sql_notfound;
   EXEC SQL WHENEVER SQLERROR GOTO sql_error;
   strcpy(kix_msg_info.cur_func, "KXSYBETRN");

   if (kxbg_flag != 1 ) {
      EXEC SQL BEGIN TRANSACTION;
   }
   EXEC SQL COMMIT WORK;

   EXEC SQL BEGIN TRANSACTION;
   kxbg_flag = 1;

   return(0); /* Good return code */

sql_error:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_warn:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_notfound:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

} /* End of KXSYBETRN function */


/************************************************************
KXSYBSAVE()
*************************************************************/
int KXSYBSAVE()
{
   strcpy(kix_msg_info.cur_func, "KXSYBSAVE");

   EXEC SQL WHENEVER SQLWARNING GOTO sql_warn;
   EXEC SQL WHENEVER NOT FOUND GOTO sql_notfound;
   EXEC SQL WHENEVER SQLERROR GOTO sql_error;
   if (kxbg_flag != 1 ) {
      EXEC SQL BEGIN TRANSACTION;
   }
   kxbg_flag = 0;
   EXEC SQL COMMIT WORK;
   EXEC SQL BEGIN TRANSACTION;
   kxbg_flag = 1;
   return(0); /* Good return code */

sql_error:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_warn:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_notfound:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

} /* End of KXSYBSAVE function */


/************************************************************
KXSYBUNDO()
*************************************************************/
int KXSYBUNDO()
{
   strcpy(kix_msg_info.cur_func, "KXSYBUNDO");

   EXEC SQL WHENEVER SQLWARNING GOTO sql_warn;
   EXEC SQL WHENEVER NOT FOUND GOTO sql_notfound;
   EXEC SQL WHENEVER SQLERROR GOTO sql_error;
   if (kxbg_flag != 1 ) {
      EXEC SQL BEGIN TRANSACTION;
   }
   kxbg_flag = 0;
   EXEC SQL ROLLBACK WORK;
   EXEC SQL BEGIN TRANSACTION;
   kxbg_flag = 1;
   return(0); /* Good return code */

sql_error:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_warn:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

sql_notfound:
   kx_sql_errmsg_rtn();
   return(1); /* This is a fatal and need to abort */

} /* End of KXSYBUNDO function */

/*
*---------------------------------------------------------------
* kx_sql_errmsg_rtn to process error messages
*---------------------------------------------------------------
*/
void kx_sql_errmsg_rtn()
{
   sprintf(kix_msg_info.cur_errno, "1098\n"); /* Set errno to 1098 */
   sprintf(kix_msg_info.cur_msg,
           "Database Error. Sqlcode = %d\n", sqlca.sqlcode);
   kxsetmsg(&kix_msg_info);

   /* Additional Diagnostic messages */
#ifdef TRACEMSG
   printf("errmsg = %s\n", sqlca.sqlerrm.sqlerrmc);
#endif
}
