/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <dlfcn.h>
#ifdef __INTERIX
 #define _int64 long long
 #include <interix/interix.h>
 #include <errno.h>
#endif

#ifdef INTEL
#define COBMQHCONN(a) ((((a)&0xff)<<24)    | \
                    (((a)&0xff00)<<8)   | \
                    (((a)&0xff0000)>>8) | \
                    ((((unsigned)(a)&0xff000000))>>24))
#else
#define COBMQHCONN(a) (a)
#endif
/* #include <cmqc.h> can't include due to redefinition of MQ API */
/*------------ data names copied from cmqc.h --------*/

#if defined(BIT64_PLATFORM)
 typedef int MQLONG;
#else
 typedef long MQLONG;
#endif

#define MQENTRY
#define MQPOINTER *
#define MQCC_FAILED 2
#define MQCC_OK      0
#define MQHC_UNUSABLE_HCONN     (-1)

typedef char MQCHAR;
typedef MQCHAR MQPOINTER PMQCHAR;
typedef MQLONG MQPOINTER PMQLONG;
typedef MQLONG MQHCONN;
typedef MQHCONN MQPOINTER PMQHCONN;
typedef MQLONG MQHOBJ;
typedef MQHOBJ MQPOINTER PMQHOBJ;
typedef void MQPOINTER PMQVOID;
typedef struct tagMQCNO MQCNO;
typedef MQCNO  MQPOINTER PMQCNO;

/* 
 * The following C prototypes describe the Cobol calling interface
 * to the MQ library functions for Cobol. 
 *
 * The C calling interface prototypes are defined in cmqc.h which we
 * can't include in this module. Basically everything is passed by 
 * reference when the caller is Cobol and when the caller is C, some 
 * of the values are passed by value. In the comments for the prototypes
 * the appearance of BY VALUE FOR C indicates that the same function,
 * when called by C, will be passing the parameter BY VALUE. 
 * The actual interface routines do the necessary casting to be 
 * consistent with the language API.
 */

 /****************************************************************/
 /*  MQBACK Function -- Back Out Changes                         */
 /****************************************************************/

   void MQENTRY MQBACK (
     PMQHCONN pHconn,     /* I: Connection handle - BY VALUE FOR C */
     PMQLONG  pCompCode,  /* O: Completion code */
     PMQLONG  pReason);   /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQBEGIN Function -- Begin Unit of Work                      */
 /****************************************************************/

   void MQENTRY MQBEGIN (
     PMQHCONN pHconn,         /* I: Connection handle - BY VALUE FOR C */
     PMQVOID  pBeginOptions,  /* IO: Options that control the action of
                               MQBEGIN */
     PMQLONG  pCompCode,      /* O: Completion code */
     PMQLONG  pReason);       /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQCLOSE Function -- Close Object                            */
 /****************************************************************/

   void MQENTRY MQCLOSE (
     PMQHCONN pHconn,     /* I: Connection handle - BY VALUE FOR C */
     PMQHOBJ  pHobj,      /* IO: Object handle */
     PMQLONG  pOptions,   /* I: Options that control the action of
                             MQCLOSE - BY VALUE FOR C */
     PMQLONG  pCompCode,  /* O: Completion code */
     PMQLONG  pReason);   /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQCMIT Function -- Commit Changes                           */
 /****************************************************************/

   void MQENTRY MQCMIT (
     PMQHCONN pHconn,     /* I: Connection handle - BY VALUE FOR C */
     PMQLONG  pCompCode,  /* O: Completion code */
     PMQLONG  pReason);   /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQCONN Function -- Connect Queue Manager                    */
 /****************************************************************/

 void MQENTRY MQCONN (
   PMQCHAR   pQMgrName,  /* I: Name of queue manager */
   PMQHCONN  pHconn,     /* O: Connection handle */
   PMQLONG   pCompCode,  /* O: Completion code */
   PMQLONG   pReason);   /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQCONNX Function -- Connect Queue Manager (Extended)        */
 /****************************************************************/

 void MQENTRY MQCONNX (
   PMQCHAR   pQMgrName,     /* I: Name of queue manager */
   PMQCNO    pConnectOpts,  /* IO: Options that control the action of MQCONNX */
   PMQHCONN  pHconn,        /* O: Connection handle */
   PMQLONG   pCompCode,     /* O: Completion code */
   PMQLONG   pReason);      /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQDISC Function -- Disconnect Queue Manager                 */
 /****************************************************************/

 void MQENTRY MQDISC (
   PMQHCONN  pHconn,     /* IO: Connection handle */
   PMQLONG   pCompCode,  /* O: Completion code */
   PMQLONG   pReason);   /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQGET Function -- Get Message                               */
 /****************************************************************/

   void MQENTRY MQGET (
     PMQHCONN pHconn,        /* I: Connection handle - BY VALUE FOR C */
     PMQHOBJ  pHobj,         /* I: Object handle - BY VALUE FOR C */
     PMQVOID  pMsgDesc,      /* IO: Message descriptor */
     PMQVOID  pGetMsgOpts,   /* IO: Options that control the action of
                                MQGET */
     PMQLONG  pBufferLength, /* IL: Length in bytes of the Buffer area - BY VALUE FOR C */
     PMQVOID  pBuffer,       /* OB: Area to contain the message data */
     PMQLONG  pDataLength,   /* O: Length of the message */
     PMQLONG  pCompCode,     /* OR: Completion code */
     PMQLONG  pReason);      /* OC: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQINQ Function -- Inquire Object Attributes                 */
 /****************************************************************/

   void MQENTRY MQINQ (
     PMQHCONN pHconn,          /* I: Connection handle - BY VALUE FOR C */
     PMQHOBJ  pHobj,           /* I: Object handle - BY VALUE FOR C */
     PMQLONG  pSelectorCount,  /* I: Count of selectors - BY VALUE FOR C */
     PMQLONG  pSelectors,      /* I: Array of attribute selectors */
     PMQLONG  pIntAttrCount,   /* I: Count of integer attributes - BY VALUE FOR C */
     PMQLONG  pIntAttrs,       /* O: Array of integer attributes */
     PMQLONG  pCharAttrLength, /* IL: Length of character attributes
                                  buffer - BY VALUE FOR C */
     PMQCHAR  pCharAttrs,      /* OB: Character attributes */
     PMQLONG  pCompCode,       /* OC: Completion code */
     PMQLONG  pReason);        /* OR: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQOPEN Function -- Open Object                              */
 /****************************************************************/

   void MQENTRY MQOPEN (
     PMQHCONN pHconn,     /* I: Connection handle - BY VALUE FOR C */
     PMQVOID  pObjDesc,   /* IO: Object descriptor */
     PMQLONG  pOptions,   /* I: Options that control the action of
                             MQOPEN - BY VALUE FOR C */
     PMQHOBJ  pHobj,      /* O: Object handle */
     PMQLONG  pCompCode,  /* O: Completion code */
     PMQLONG  pReason);   /* O: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQPUT Function -- Put Message                               */
 /****************************************************************/

   void MQENTRY MQPUT (
     PMQHCONN pHconn,        /* I: Connection handle - BY VALUE FOR C */
     PMQHOBJ  pHobj,         /* I: Object handle - BY VALUE FOR C */
     PMQVOID  pMsgDesc,      /* IO: Message descriptor */
     PMQVOID  pPutMsgOpts,   /* IO: Options that control the action of
                                MQPUT */
     PMQLONG  pBufferLength, /* IL: Length of the message in Buffer - BY VALUE FOR C */
     PMQVOID  pBuffer,       /* IB: Message data */
     PMQLONG  pCompCode,     /* OC: Completion code */
     PMQLONG  pReason);      /* OR: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQPUT1 Function -- Put One Message                          */
 /****************************************************************/

   void MQENTRY MQPUT1 (
     PMQHCONN pHconn,        /* I: Connection handle - BY VALUE FOR C */
     PMQVOID  pObjDesc,      /* IO: Object descriptor */
     PMQVOID  pMsgDesc,      /* IO: Message descriptor */
     PMQVOID  pPutMsgOpts,   /* IO: Options that control the action of
                                MQPUT1 */
     PMQLONG  pBufferLength, /* IL: Length of the message in Buffer - BY VALUE FOR C */
     PMQVOID  pBuffer,       /* IB: Message data */
     PMQLONG  pCompCode,     /* OC: Completion code */
     PMQLONG  pReason);      /* OR: Reason code qualifying CompCode */

 /****************************************************************/
 /*  MQSET Function -- Set Object Attributes                     */
 /****************************************************************/

   void MQENTRY MQSET (
     PMQHCONN pHconn,          /* I: Connection handle - BY VALUE FOR C */
     PMQHOBJ  pHobj,           /* I: Object handle - BY VALUE FOR C */
     PMQLONG  pSelectorCount,  /* I: Count of selectors - BY VALUE FOR C */
     PMQLONG  pSelectors,      /* I: Array of attribute selectors */
     PMQLONG  pIntAttrCount,   /* I: Count of integer attributes - BY VALUE FOR C */
     PMQLONG  pIntAttrs,       /* I: Array of integer attributes */
     PMQLONG  pCharAttrLength, /* IL: Length of character attributes
                                  buffer - BY VALUE FOR C */
     PMQCHAR  pCharAttrs,      /* IB: Character attributes */
     PMQLONG  pCompCode,       /* OC: Completion code */
     PMQLONG  pReason);        /* OR: Reason code qualifying CompCode */

/* define return codes to userexit */
#define KXSUCCESS  0   /*  Continue with the transaction */
#define KXABORT   -1   /*  Abort transaction server      */
#define KXFAILURE  1   /*  Transaction setup failure     */
/* 
 * define size of kxsetmsg buffer.
 * error msg 1098 uses a %60.60s for text
 */
#define SETMSG_BUFSIZE 60
#define MAXPATH 256
#ifdef __INTERIX
typedef void *HINSTANCE;
typedef void *HMODULE;
typedef void *HWND;
typedef char *LPCSTR;
typedef unsigned long DWORD;
typedef int (__stdcall *FARPROC)();

void * __stdcall LoadLibraryA( LPCSTR lpLibFileName);
FARPROC __stdcall GetProcAddress ( HMODULE hModule, LPCSTR lpProcName);
int __stdcall FreeLibrary ( HMODULE hLibModule);
DWORD __stdcall GetLastError();
#define LoadLibrary  LoadLibraryA
#define GWL_HINSTANCE       (-6)
#endif

/* this is the safestored MQ connection handle */
static MQHCONN hConn = MQHC_UNUSABLE_HCONN;
static int CInterfaceRequested = 0;
static int CInterfaceDetected = 0;
/* define implementation interface */
typedef struct _KXMQCImplPtrs {
	void (*mqback)(MQHCONN, PMQLONG, PMQLONG);
	void (*mqbegin)(MQHCONN, PMQVOID, PMQLONG, PMQLONG);
	void (*mqclose)(MQHCONN, PMQHOBJ, MQLONG, PMQLONG, PMQLONG);
	void (*mqcmit)(MQHCONN, PMQLONG, PMQLONG);
	void (*mqconn)(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG);
	void (*mqconnx)(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG);
	void (*mqdisc)(PMQHCONN, PMQLONG, PMQLONG);
	void (*mqget)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG);
	void (*mqinq)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG);
	void (*mqopen)(MQHCONN, PMQVOID, MQLONG, PMQHOBJ, PMQLONG, PMQLONG);
	void (*mqput)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG);
	void (*mqput1)(MQHCONN, PMQVOID, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG);
	void (*mqset)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG);
} KXMQCIMPL;
typedef struct _KXMQCobolImplPtrs {
	void (*mqback)(PMQHCONN, PMQLONG, PMQLONG);
	void (*mqbegin)(PMQHCONN, PMQVOID, PMQLONG, PMQLONG);
	void (*mqclose)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG);
	void (*mqcmit)(PMQHCONN, PMQLONG, PMQLONG);
	void (*mqconn)(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG);
	void (*mqconnx)(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG);
	void (*mqdisc)(PMQHCONN, PMQLONG, PMQLONG);
	void (*mqget)(PMQHCONN, PMQHOBJ, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG);
	void (*mqinq)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQCHAR, PMQLONG, PMQLONG);
	void (*mqopen)(PMQHCONN, PMQVOID, PMQLONG, PMQHOBJ, PMQLONG, PMQLONG);
	void (*mqput)(PMQHCONN, PMQHOBJ, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG);
	void (*mqput1)(PMQHCONN, PMQVOID, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG);
	void (*mqset)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQCHAR, PMQLONG, PMQLONG);
} KXMQCOBOLIMPL;

/* C language implementation */
static KXMQCIMPL hMQ_C_impl = { 0 };
/* COBOL language implementation */
static KXMQCOBOLIMPL hMQ_Cobol_impl = { 0 };

static char *errorString;

/*
 * Helper function to prime an interface with
 * function pointers from an implementation.
 */
static void hCInterfaceInit(KXMQCIMPL *impl, void *so)
{
#ifdef __INTERIX
	impl->mqback = (void(*)(MQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQBACK");
	impl->mqbegin = (void(*)(MQHCONN, PMQVOID, PMQLONG, PMQLONG))GetProcAddress(so, "MQBEGIN");
	impl->mqclose = (void(*)(MQHCONN, PMQHOBJ, MQLONG, PMQLONG, PMQLONG))GetProcAddress(so, "MQCLOSE");
	impl->mqcmit = (void(*)(MQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQCMIT");
	impl->mqconn = (void(*)(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQCONN");
	impl->mqconnx = (void(*)(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQCONNX");
	impl->mqdisc = (void(*)(PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQDISC");
	impl->mqget = (void(*)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG))GetProcAddress(so, "MQGET");
	impl->mqinq = (void(*)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG))GetProcAddress(so, "MQINQ");
	impl->mqopen = (void(*)(MQHCONN, PMQVOID, MQLONG, PMQHOBJ, PMQLONG, PMQLONG))GetProcAddress(so, "MQOPEN");
	impl->mqput = (void(*)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG))GetProcAddress(so, "MQPUT");
	impl->mqput1 = (void(*)(MQHCONN, PMQVOID, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG))GetProcAddress(so, "MQPUT1");
	impl->mqset = (void(*)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG))GetProcAddress(so, "MQSET");
#else
        impl->mqback = (void(*)(MQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQBACK");
        impl->mqbegin = (void(*)(MQHCONN, PMQVOID, PMQLONG, PMQLONG))dlsym(so, "MQBEGIN");
        impl->mqclose = (void(*)(MQHCONN, PMQHOBJ, MQLONG, PMQLONG, PMQLONG))dlsym(so, "MQCLOSE");
        impl->mqcmit = (void(*)(MQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQCMIT");
        impl->mqconn = (void(*)(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQCONN");
        impl->mqconnx = (void(*)(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQCONNX");
        impl->mqdisc = (void(*)(PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQDISC");
        impl->mqget = (void(*)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG))dlsym(so, "MQGET");
        impl->mqinq = (void(*)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG))dlsym(so, "MQINQ");
        impl->mqopen = (void(*)(MQHCONN, PMQVOID, MQLONG, PMQHOBJ, PMQLONG, PMQLONG))dlsym(so, "MQOPEN");
        impl->mqput = (void(*)(MQHCONN, MQHOBJ, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG))dlsym(so, "MQPUT");
        impl->mqput1 = (void(*)(MQHCONN, PMQVOID, PMQVOID, PMQVOID, MQLONG, PMQVOID, PMQLONG, PMQLONG))dlsym(so, "MQPUT1");
        impl->mqset = (void(*)(MQHCONN, MQHOBJ, MQLONG, PMQLONG, MQLONG, PMQLONG, MQLONG, PMQCHAR, PMQLONG, PMQLONG))dlsym(so, "MQSET");
#endif
}
static void hCobolInterfaceInit(KXMQCOBOLIMPL *impl, void *so)
{
#ifdef __INTERIX
	impl->mqback = (void(*)(PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQBACK");
	impl->mqbegin = (void(*)(PMQHCONN, PMQVOID, PMQLONG, PMQLONG))GetProcAddress(so, "MQBEGIN");
	impl->mqclose = (void(*)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG))GetProcAddress(so, "MQCLOSE");
	impl->mqcmit = (void(*)(PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQCMIT");
	impl->mqconn = (void(*)(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQCONN");
	impl->mqconnx = (void(*)(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQCONNX");
	impl->mqdisc = (void(*)(PMQHCONN, PMQLONG, PMQLONG))GetProcAddress(so, "MQDISC");
	impl->mqget = (void(*)(PMQHCONN, PMQHOBJ, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG))GetProcAddress(so, "MQGET");
	impl->mqinq = (void(*)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQCHAR, PMQLONG, PMQLONG))GetProcAddress(so, "MQINQ");
	impl->mqopen = (void(*)(PMQHCONN, PMQVOID, PMQLONG, PMQHOBJ, PMQLONG, PMQLONG))GetProcAddress(so, "MQOPEN");
	impl->mqput = (void(*)(PMQHCONN, PMQHOBJ, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG))GetProcAddress(so, "MQPUT");
	impl->mqput1 = (void(*)(PMQHCONN, PMQVOID, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG))GetProcAddress(so, "MQPUT1");
	impl->mqset = (void(*)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQCHAR, PMQLONG, PMQLONG))GetProcAddress(so, "MQSET");
#else
        impl->mqback = (void(*)(PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQBACK");
        impl->mqbegin = (void(*)(PMQHCONN, PMQVOID, PMQLONG, PMQLONG))dlsym(so, "MQBEGIN");
        impl->mqclose = (void(*)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG))dlsym(so, "MQCLOSE");
        impl->mqcmit = (void(*)(PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQCMIT");
        impl->mqconn = (void(*)(PMQCHAR, PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQCONN");
        impl->mqconnx = (void(*)(PMQCHAR, PMQCNO, PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQCONNX");
        impl->mqdisc = (void(*)(PMQHCONN, PMQLONG, PMQLONG))dlsym(so, "MQDISC");
        impl->mqget = (void(*)(PMQHCONN, PMQHOBJ, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG, PMQLONG))dlsym(so, "MQGET");
        impl->mqinq = (void(*)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQCHAR, PMQLONG, PMQLONG))dlsym(so, "MQINQ");
        impl->mqopen = (void(*)(PMQHCONN, PMQVOID, PMQLONG, PMQHOBJ, PMQLONG, PMQLONG))dlsym(so, "MQOPEN");
        impl->mqput = (void(*)(PMQHCONN, PMQHOBJ, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG))dlsym(so, "MQPUT");
        impl->mqput1 = (void(*)(PMQHCONN, PMQVOID, PMQVOID, PMQVOID, PMQLONG, PMQVOID, PMQLONG, PMQLONG))dlsym(so, "MQPUT1");
        impl->mqset = (void(*)(PMQHCONN, PMQHOBJ, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQLONG, PMQCHAR, PMQLONG, PMQLONG))dlsym(so, "MQSET");
#endif
}

/*
 * Helper function to issue error message 1098.
 */
#ifdef UNIKIX
#include "kxinfo.h"
#else
static struct lmsgstr {
 char cur_errno [4];
 char cur_func  [20];
 char cur_msg   [60];
} lmsgstr;
#endif
static void hsetmsg(char *msgtext)
{
	extern void kxsetmsg(struct lmsgstr *);
	struct lmsgstr msg;
	/* set errno, function name & text */
	strncpy(msg.cur_errno, "1098", sizeof(msg.cur_errno));
	strncpy(msg.cur_func, "KXMQSERIES", sizeof(msg.cur_func));
	strncpy(msg.cur_msg, msgtext, sizeof(msg.cur_msg));
	/* issue this error message */
	kxsetmsg(&msg);
}

/*
 * Call to pre-define that MQ C library interface needs to be
 * used for intercepted MQ entry points. If this routine is not
 * called, then the MQ Cobol library interface will be initially
 * selected. This call is necessary to fully support the MQ C API
 * when running on an Intel platform.
 */
void kxmq_setc()
{
	CInterfaceRequested = 1;
	return;
}

/*
 * Call from user exit to allocate any MQ specifics.
 * The kixinstall activity passed on the MQ library
 * bindings (either 'Client' or 'Server') to this
 * module via the MQSERIES constant. This module has
 * therefore been compiled with that knowledge and
 * allocates the associated shared libraries as such.
 * The MQI implementations for Cobol & C are then
 * obtained and stored in a language specific interface
 * block. These pointers are used to forward an application
 * MQI call onto the correct MQ implementation.
 * This enables UniKix to support Cobol and any other 
 * language that defaults to the C binding.
 */
int KXMQSALLOC()
{
	int ret = KXSUCCESS;
/* only want to do this in an MQ environment */
#ifdef MQSERIES	
	/*
	 *  Attach the MQ libraries and get address of 
	 *  intercepted functions.
	 *  The constant MQSERIES is defined by kixinstall and
	 *  can be set to either "Server" or "Client" based
	 *  upon the user selection. We use this to determine
	 *  which binding the user has requested and load
	 *  the associated language implementation.
	 */
	{
		/* kixinstall defined binding */
		char *Server="Server";	
		char *Client="Client";
		/* language implementations */
		void *soC;
		void *soCobol;
		/* implementation names */
		char *libC;
		char *libCobol;
		/* loader flags */
		int ldflags;
		/* kxsetmsg buffer */
		char errmsg[SETMSG_BUFSIZE];
		/* path for $MQSERIES - needed for Windows port */
		char *MQ_UxPath;
		char MQ_WinPath[MAXPATH];
		int lplen = 0;
#ifdef RS6000
		ldflags = RTLD_LAZY | RTLD_GLOBAL | RTLD_MEMBER;
#else
		ldflags = RTLD_LAZY | RTLD_GLOBAL;
#endif
#ifdef __INTERIX
		/* need to specify the pathname for Windows */
		MQ_UxPath = (char *)getenv("MQSERIES"); /* get unix path */
		lplen = strlen(MQ_UxPath); /* get length of path */
		if (unixpath2win(MQ_UxPath,0,MQ_WinPath,lplen) != 0)
		{
			sprintf(errmsg, "Unable to convert MQSERIES pathname, errno = %d\n", errno);
			hsetmsg(errmsg);
			return KXABORT;
		}
		libC = (char *) malloc(MAXPATH);
		libCobol = (char *) malloc(MAXPATH);
		strcpy(libC, MQ_WinPath);
		strcpy(libCobol, MQ_WinPath);
#endif
		/* use the bindings specified by kixinstall */
		if (strcmp(Server, MQSERIES) == 0) {
			/* server bindings defined */
#ifdef RS6000
			libC = "libmqm.a(libmqm.o)";
			libCobol = "libmqmcb.a(libmqmcobol.o)";
#elif defined __INTERIX
			strcat(libC,"\\bin\\mqm"); /* add the library name */
			strcat(libCobol, "\\bin\\mqmcb32");
#else
			libC = "libmqm.so";
			libCobol = "libmqmcb.so";
#endif
		} else {
			/* client bindings defined */
#ifdef RS6000
			libC = "libmqic.a(mqic.o)";
			libCobol = "libmqicb.a(libmqicb.o)";
#elif defined __INTERIX
			strcat(libC, "\\bin\\mqic32");
			strcat(libCobol, "\\bin\\mqiccb32");
#else
			libC = "libmqic.so";
			libCobol = "libmqicb.so";
#endif			
		}
		/* populate C language implementation interface */	
#ifdef __INTERIX
		soC = LoadLibraryA(libC);
#else
		soC = dlopen(libC,  ldflags);
#endif
		if (soC != NULL) {
			hCInterfaceInit(&hMQ_C_impl, soC);
		} else {
			sprintf(errmsg, "Unable to open mq library %s", libC); 
			hsetmsg(errmsg);
			errorString = dlerror();
			if (errorString != NULL) {
				sprintf(errmsg, "<%s>", errorString);
				hsetmsg(errmsg);
			}
			ret = KXABORT;
		}
		/* populate Cobol language implementation interface */
#ifdef __INTERIX
		soCobol = LoadLibraryA(libCobol);
#else
		soCobol = dlopen(libCobol, ldflags);
#endif
		if (soCobol != NULL) {
			hCobolInterfaceInit(&hMQ_Cobol_impl, soCobol);
#ifdef __INTERIX
			kxInitMQBridge(MQBACK,
						   MQBEGIN,
						   MQCLOSE,
						   MQCMIT,
						   MQCONN,
						   MQCONNX,
						   MQDISC,
						   MQGET,
						   MQINQ,
						   MQOPEN,
						   MQPUT,
						   MQPUT1,
						   MQSET);
#endif
		} else {
			sprintf(errmsg, "Unable to open mq library %s", libCobol); 
			hsetmsg(errmsg);
			errorString = dlerror();
			if (errorString != NULL) {
				sprintf(errmsg, "<%s>", errorString);
				hsetmsg(errmsg);
			}
			ret = KXABORT;
		}
#ifdef __INTERIX
		free(libC);
		free(libCobol);
#endif
	} 
#endif /* MQSERIES */
	return ret;
}

/*
 * Call from the user exit to backout this MQ UOW.
 * This is only active when "Transactional MQ" has
 * been selected. Because we have the connection
 * handle from the MQCONN, we can safely pass this
 * on to the MQBACK C language implementation.
 */
int KXMQSUNDO()
{
#ifdef MQSERIES
	/* If the handle is still valid... */
	if (hConn != MQHC_UNUSABLE_HCONN) {
		MQLONG lCompCode;
		MQLONG lReasonCode;
		MQHCONN lTempHconn;
		
		if (CInterfaceRequested) {
			lTempHconn = hConn;
		} else {
			lTempHconn = COBMQHCONN(hConn);
		}

		/* use C binding */
		hMQ_C_impl.mqback(lTempHconn, &lCompCode, &lReasonCode);
		if (lReasonCode) {
			char errmsg[SETMSG_BUFSIZE];
			sprintf(errmsg, "MQBACK failed with CompCode [%d], ReasonCode [%d]\n",
				lCompCode,lReasonCode);
			hsetmsg(errmsg);
			return(lReasonCode);
		}
	}
#endif /*MQSERIES */
	return 0;
}

/*
 * Call from the user exit to commit this MQ UOW.
 * This is only active when "Transactional MQ" has
 * been selected. Because we have the connection
 * handle from the MQCONN, we can safely pass this
 * on to the MQCMIT C language implementation.
 */
int KXMQSSAVE()
{
#ifdef MQSERIES
	/* If the handle is still valid... */
	if (hConn != MQHC_UNUSABLE_HCONN) {
		MQLONG lCompCode;
		MQLONG lReasonCode;
		MQHCONN lTempHconn;
		
		if (CInterfaceRequested) {
			lTempHconn = hConn;
		} else {
			lTempHconn = COBMQHCONN(hConn);
		}

		hMQ_C_impl.mqcmit(lTempHconn, &lCompCode, &lReasonCode);

		if (lReasonCode) {
			char errmsg[SETMSG_BUFSIZE];
			sprintf(errmsg, "MQCMIT failed with CompCode [%d], ReasonCode [%d]\n",
				lCompCode,lReasonCode);
			hsetmsg(errmsg);
			return(lReasonCode);
		}
	}
#endif /*MQSERIES */
	return 0;
}

/*
 * Call from the user exit indicating the end of this TX.
 * Typically, the application has closed down all its MQ
 * resources and issued an MQDISC. If this is so, our local
 * connection handle has been reset. If the handle is still
 * valid, then MQ is directed to disconnect the handle.
 */
int KXMQSETRN()
{
#ifdef MQSERIES
	/* 
	 * If the handle is still valid, then
	 * this TX has ended without disconnecting
	 * the connection. Take care of disconnecting here.
	 */
	if (hConn != MQHC_UNUSABLE_HCONN) {
		MQLONG lCompCode;
		MQLONG lReasonCode;
		MQHCONN lTempHconn;
		
		if (CInterfaceRequested) {
			lTempHconn = hConn;
		} else {
			lTempHconn = COBMQHCONN(hConn);
		}
		
		CInterfaceRequested = 0;
		CInterfaceDetected = 0;

		hMQ_C_impl.mqdisc(&lTempHconn, &lCompCode, &lReasonCode);

		hConn = MQHC_UNUSABLE_HCONN;
		
		if (lReasonCode) {
			char errmsg[SETMSG_BUFSIZE];
			sprintf(errmsg, "MQDISC failed with CompCode [%d], ReasonCode [%d]\n",
				lCompCode,lReasonCode);
			hsetmsg(errmsg);
			return(lReasonCode);
		} 
	}
#endif
	return 0;
}

/*
 *    Intercept these MQ entry points so we can manage
 *    the MQ connection. 
 *    MQ has a specific interface for the COBOL language, so
 *    we must attempt to determine which language the caller
 *    is using. The approach here is to check the passed connection
 *    handle. If it is not the same as the handle we safestored
 *    following the MQCONN, then we assume the handle is actually a
 *    reference to the handle and this caller is COBOL. The basis for
 *    this is that the 'C' binding uses pass-by-value, so the handle
 *    is passed to several functions. COBOL always uses pass-by-reference,
 *    so the handle is never passed directly. 
 *
 *    Functions MQCONN, MQCONNX, and MQDISC have the same semantics for
 *    both C and COBOL. For these functions, it is not necessary to 
 *    differentiate between C and Cobol unless we are executing on an 
 *    Intel platform because of reverse byte issues between the C and
 *    COBOL libraries. A function, kxmq_setc, is provided for C callers 
 *    to explicitely request that the C interface be used. The kxmq_setc 
 *    function should be called prior to any call to MQCONN or MQCONNX. 
 *    If the call is not made, then return codes and condition codes for 
 *    MQCONN, MQCONNX, and possibly MQDISC will not be in the correct 
 *    format when executing on an Intel platform. 
 *
 *    Transaction bracket related calls (MQBACK, MQBEGIN & MQCMIT) are
 *    forwarded on to the MQ runtime. MQ is responsible for handling
 *    these requests and rejecting any that invalidate the configured
 *    environment (for example, MQCMIT in an XA environment is rejected
 *    by MQ with an MQRC_ENVIRONMENT_ERROR response). This technique
 *    allows applications to control MQ resources explicitly if so desired,
 *    while also ensuring correct transactional semantics when required. 
 *
 *    Note that only a single MQ queue manager is supported. This is
 *    enforced in an XA environment (during XA startup, not here), but
 *    not in a non-XA environment. A second call to MQCONN/MQCONNX will
 *    destroy our copy of the existing connection handle. If this must
 *    be handled, additional checking is required BEFORE the connect call is
 *    forwarded on to MQ.
 */
#ifdef MQSERIES

/*********************************************************************/
/*  MQBACK Function -- Back Out Changes                              */
/*********************************************************************/
void MQBACK (
   PMQHCONN pHconn,     /* Connection handle - BY VALUE FOR C */
   PMQLONG  pCompCode,  /* Completion code */
   PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqback(Hconn, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqback(pHconn, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQBEGIN Function -- Begin Unit of Work                           */
/*********************************************************************/
void MQBEGIN (
   PMQHCONN pHconn,         /* Connection handle - BY VALUE FOR C */
   PMQVOID  pBeginOptions,  /* Options that control the action of
                               MQBEGIN */
   PMQLONG  pCompCode,      /* Completion code */
   PMQLONG  pReason)        /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqbegin(Hconn, pBeginOptions, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqbegin(pHconn, pBeginOptions, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQCLOSE Function -- Close Object                                 */
/*********************************************************************/
void MQCLOSE (
   PMQHCONN pHconn,     /* Connection handle - BY VALUE FOR C */
   PMQHOBJ  pHobj,      /* Object handle */
   PMQLONG  pOptions,   /* Options that control the action of MQCLOSE - BY VALUE FOR C */
   PMQLONG  pCompCode,  /* Completion code */
   PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqclose(Hconn, pHobj, (MQLONG) pOptions, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqclose(pHconn, pHobj, pOptions, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQCMIT Function -- Commit Changes                                */
/*********************************************************************/
void MQCMIT (
   PMQHCONN pHconn,     /* Connection handle - BY VALUE FOR C */
   PMQLONG  pCompCode,  /* Completion code */
   PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqcmit(Hconn, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqcmit(pHconn, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQCONN Function -- Connect Queue Manager                         */
/*********************************************************************/
void MQCONN (
   PMQCHAR   pQMgrName,  /* Name of queue manager */
   PMQHCONN  pHconn,     /* Connection handle */
   PMQLONG   pCompCode,  /* Completion code */
   PMQLONG   pReason)    /* Reason code qualifying CompCode */ 
{
	if (CInterfaceRequested) {
		hMQ_C_impl.mqconn(pQMgrName, pHconn, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqconn(pQMgrName, pHconn, pCompCode, pReason);
	}
	if (*pCompCode == MQCC_OK) {
		/* keep a local copy of the handle */
		hConn = *pHconn;
	}
}

/*********************************************************************/
/*  MQCONNX Function -- Connect Queue Manager (Extended)             */
/*********************************************************************/
void MQCONNX (
   PMQCHAR   pQMgrName,     /* Name of queue manager */
   PMQCNO    pConnectOpts,  /* Options that control the action of
                               MQCONNX */
   PMQHCONN  pHconn,        /* Connection handle */
   PMQLONG   pCompCode,     /* Completion code */
   PMQLONG   pReason)       /* Reason code qualifying CompCode */
{
	if (CInterfaceRequested) {
		hMQ_C_impl.mqconnx(pQMgrName, pConnectOpts, pHconn, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqconnx(pQMgrName, pConnectOpts, pHconn, pCompCode, pReason);
	}
	if (*pCompCode == MQCC_OK) {
		/* keep a local copy of the handle */
		hConn = *pHconn;
	}
}
/*********************************************************************/
/*  MQDISC Function -- Disconnect Queue Manager                      */
/*********************************************************************/
void MQDISC (
   PMQHCONN  pHconn,     /* Connection handle */
   PMQLONG   pCompCode,  /* Completion code */
   PMQLONG   pReason)    /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;
	
	if (CInterfaceRequested) {
		Hconn = *pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN (*pHconn);
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqdisc(&Hconn, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqdisc(pHconn, pCompCode, pReason);
	}

	CInterfaceRequested = 0;
	CInterfaceDetected = 0;

	if (*pCompCode == MQCC_OK) {
		/* update our local copy of the handle */
		hConn = MQHC_UNUSABLE_HCONN;
	}
}

/*********************************************************************/
/*  MQGET Function -- Get Message                                    */
/*********************************************************************/
void MQGET (
   PMQHCONN pHconn,        /* Connection handle - BY VALUE FOR C */
   PMQHOBJ  pHobj,         /* Object handle - BY VALUE FOR C */
   PMQVOID  pMsgDesc,      /* Message descriptor */
   PMQVOID  pGetMsgOpts,   /* Options that control the action of
                              MQGET */
   PMQLONG  pBufferLength, /* Length in bytes of the Buffer area - BY VALUE FOR C */
   PMQVOID  pBuffer,       /* Area to contain the message data */
   PMQLONG  pDataLength,   /* Length of the message */
   PMQLONG  pCompCode,     /* Completion code */
   PMQLONG  pReason)       /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqget(Hconn, (MQHOBJ) pHobj, pMsgDesc, pGetMsgOpts, (MQLONG) pBufferLength, pBuffer, pDataLength, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqget(pHconn, pHobj, pMsgDesc, pGetMsgOpts, pBufferLength, pBuffer, pDataLength, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQINQ Function -- Inquire Object Attributes                      */
/*********************************************************************/
void MQINQ (
   PMQHCONN pHconn,          /* Connection handle - BY VALUE FOR C */
   PMQHOBJ  pHobj,           /* Object handle - BY VALUE FOR C */
   PMQLONG  pSelectorCount,  /* Count of selectors - BY VALUE FOR C */
   PMQLONG  pSelectors,      /* Array of attribute selectors */
   PMQLONG  pIntAttrCount,   /* Count of integer attributes - BY VALUE FOR C */
   PMQLONG  pIntAttrs,       /* Array of integer attributes */
   PMQLONG  pCharAttrLength, /* Length of character attributes buffer - BY VALUE FOR C */
   PMQCHAR  pCharAttrs,      /* Character attributes */
   PMQLONG  pCompCode,       /* Completion code */
   PMQLONG  pReason)         /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqinq(Hconn, (MQHOBJ) pHobj, (MQLONG) pSelectorCount, pSelectors, (MQLONG) pIntAttrCount, pIntAttrs, (MQLONG) pCharAttrLength, pCharAttrs, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqinq(pHconn, pHobj, pSelectorCount, pSelectors, pIntAttrCount, pIntAttrs, pCharAttrLength, pCharAttrs, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQOPEN Function -- Open Object                                   */
/*********************************************************************/
void MQOPEN (
   PMQHCONN pHconn,     /* Connection handle  - BY VALUE FOR C */
   PMQVOID  pObjDesc,   /* Object descriptor */
   PMQLONG  pOptions,   /* Options that control the action of MQOPEN - BY VALUE FOR C */
   PMQHOBJ  pHobj,      /* Object handle */
   PMQLONG  pCompCode,  /* Completion code */
   PMQLONG  pReason)    /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqopen(Hconn, pObjDesc, (MQLONG) pOptions, pHobj, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqopen(pHconn, pObjDesc, pOptions, pHobj, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQPUT Function -- Put Message                                    */
/*********************************************************************/
void MQPUT (
   PMQHCONN pHconn,        /* Connection handle - BY VALUE FOR C */
   PMQHOBJ  pHobj,         /* Object handle  - BY VALUE FOR C */
   PMQVOID  pMsgDesc,      /* Message descriptor */
   PMQVOID  pPutMsgOpts,   /* Options that control the action of
                              MQPUT */
   PMQLONG  pBufferLength, /* Length of the message in Buffer - BY VALUE FOR C */
   PMQVOID  pBuffer,       /* Message data */
   PMQLONG  pCompCode,     /* Completion code */
   PMQLONG  pReason)       /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqput(Hconn, (MQHOBJ) pHobj, pMsgDesc, pPutMsgOpts, (MQLONG) pBufferLength, pBuffer, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqput(pHconn, pHobj, pMsgDesc, pPutMsgOpts, pBufferLength, pBuffer, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQPUT1 Function -- Put One Message                               */
/*********************************************************************/
void MQPUT1 (
   PMQHCONN pHconn,        /* Connection handle - BY VALUE FOR C */
   PMQVOID  pObjDesc,      /* Object descriptor */
   PMQVOID  pMsgDesc,      /* Message descriptor */
   PMQVOID  pPutMsgOpts,   /* Options that control the action of
                              MQPUT1 */
   PMQLONG  pBufferLength, /* Length of the message in Buffer - BY VALUE FOR C */
   PMQVOID  pBuffer,       /* Message data */
   PMQLONG  pCompCode,     /* Completion code */
   PMQLONG  pReason)       /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqput1(Hconn, pObjDesc, pMsgDesc, pPutMsgOpts, (MQLONG) pBufferLength, pBuffer, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqput1(pHconn, pObjDesc, pMsgDesc, pPutMsgOpts, pBufferLength, pBuffer, pCompCode, pReason);
	}
}

/*********************************************************************/
/*  MQSET Function -- Set Object Attributes                          */
/*********************************************************************/
void MQSET (
   PMQHCONN pHconn,          /* Connection handle - BY VALUE FOR C */
   PMQHOBJ  pHobj,           /* Object handle - BY VALUE FOR C */
   PMQLONG  pSelectorCount,  /* Count of selectors - BY VALUE FOR C */
   PMQLONG  pSelectors,      /* Array of attribute selectors */
   PMQLONG  pIntAttrCount,   /* Count of integer attributes - BY VALUE FOR C */
   PMQLONG  pIntAttrs,       /* Array of integer attributes */
   PMQLONG  pCharAttrLength, /* Length of character attributes buffer - BY VALUE FOR C */
   PMQCHAR  pCharAttrs,      /* Character attributes */
   PMQLONG  pCompCode,       /* Completion code */
   PMQLONG  pReason)         /* Reason code qualifying CompCode */
{
	MQHCONN Hconn;

	if (CInterfaceRequested) {
		Hconn = (MQHCONN) pHconn;
	} else if (CInterfaceDetected) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
	} else if ((MQHCONN) pHconn == hConn) {
		Hconn = COBMQHCONN ((MQHCONN) pHconn);
		CInterfaceDetected = 1;
	}
	if (CInterfaceRequested || CInterfaceDetected) {
		hMQ_C_impl.mqset(Hconn, (MQHOBJ) pHobj, (MQLONG) pSelectorCount, pSelectors, (MQLONG) pIntAttrCount, pIntAttrs, (MQLONG) pCharAttrLength, pCharAttrs, pCompCode, pReason);
	} else {
		hMQ_Cobol_impl.mqset(pHconn, pHobj, pSelectorCount, pSelectors, pIntAttrCount, pIntAttrs, pCharAttrLength, pCharAttrs, pCompCode, pReason);
	}
}
#endif /* MQSERIES */
