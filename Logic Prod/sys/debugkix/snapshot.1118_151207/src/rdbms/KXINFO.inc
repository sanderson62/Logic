 /******************************************************************/
 /*                                                                */
 /* Copyright (c) 2007 by Clerity Solutions, Inc.                  */
 /* All rights reserved.                                           */
 /*                                                                */
 /******************************************************************/

 DCL 01  KIX_TCT_INFO,
         05  KIX_USRNAM          CHAR(8),
         05  KIX_OPSEC           CHAR(8),
         05  KIX_OPCLS           CHAR(3),
         05  KIX_TRMID           CHAR(4),
         05  KIX_LUNAME          CHAR(8),
         05  KIX_OPID            CHAR(3),
         05  KIX_TRANCD          CHAR(4);
 
 DCL 01  KIX_SYS_INFO,
         05  KIX_SITNAME         CHAR(8),
	 05  KIX_SIT_SRV_NAME    CHAR(8),
	 05  KIX_SIT_DB_NAME     CHAR(8),
	 05  KIX_SIT_USR_NAME    CHAR(8),
	 05  KIX_SIT_USR_PASS    CHAR(8),
         05  KIX_TRANIDX         CHAR(3);
 
 DCL 01  KIX_MSG_INFO,
         05  KIX_MSGNO           CHAR(4) INITIAL('1098'),
         05  KIX_MSG_ROUTINE     CHAR(20),
         05  KIX_MSG_STR         CHAR(60),
         /* terminate KIX_MSG_STR with a null byte for C */
         05  FILLER              CHAR(1) INITIAL(LOW(1));
