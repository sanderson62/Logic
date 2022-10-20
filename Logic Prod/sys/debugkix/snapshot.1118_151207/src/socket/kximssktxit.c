/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/***************** UNIX INCLUDES *******************/

#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

/***************** Product Definitions **************/

extern void kxprtf(const char *pstr, ...);
                        /* This function takes the same parameters
                           as the standard 'C' library's printf function.
                           It prints the message in the log file.
                           ($KIXSYS/unikixmain.log)
                        */
static int h_kximsdebug = 0;
#define kxImsPrintDbg   if(h_kximsdebug)      kxprtf
#define kxImsHexDump    if(h_kximsdebug)      hexdump

/****************************************************************************
**
** FUNCTION:    kxusrexit_in_socket
**
** DESCRIPTION: User exit module called when a IMS Connect is made. 
**              This exit reads the initial socket data and returns
**              the details of the message to the transaction processor. 
**              The default format of the initial socket data is represented
**              by struct kxImsConnectMsg. Do not change such structure, if
**              a different message format is expected from and to the client 
**              you must make the appropriate modification to this user exit
**              file in order to remap the client input message into a 
**              struct kxImsConnectMsg to pass to the transaction processor
**              and convert the message received from the transaction processor 
**              into the message structure the client is expecting.
**                
**           ***************************************************************
**           * THE PURPOSE OF THIS USER EXIT IS TO ALLOW USERS TO REMAP   *
**           * THIS DEFAULT FORMAT OF THE IMS CONNECT MESSAGE             *
**           ***************************************************************
**
**           Although the user exit gives complete flexibility to the 
**           user for defining his/her own initial message format, the 
**           user exit must abide by the constraints laid out as comments 
**           in the default code. The user exit returns certain parameters 
**           to the transaction processor. These parameters collectively
**           allow the transaction processor to schedule the execution of
**           the required transaction and pass to it the desired data.
**
**
** INPUTS:      psocket_fd  - File Descriptor (fd) of the socket to read from.
**
** OUTPUTS:     ptransid    - Transaction that needs to be executed
**		pdata       - Data Area to be passed
**		pdata_len   - Length of message to be passed
**		pstart_type - Set to KC
**		pinterval   - Reserved
**
**	        This integer function returns a value of (-1) on error,
**							 (0)  on success.
******************************************************************************/

#define LEN_TRANID	4

#define MAX_SKT_BUFSIZE  4096  /* Max 32000 */

#ifdef INTEL
#define COBHALF(a) ((((a)&0xff)<<8) | \
                    ((((unsigned)(a)&0xff00))>>8))
#define COBWORD(a) ((((a)&0xff)<<24)    | \
                    (((a)&0xff00)<<8)   | \
                    (((a)&0xff0000)>>8) | \
                    ((((unsigned)(a)&0xff000000))>>24))
#else
#define COBHALF(a) (a)
#define COBWORD(a) (a)
#endif

struct kxImsIrmHdr {
  char    llll[4];
  char    irm_len[2];
  char    irm_arch;
  char    irm_f0;
  char    irm_id[8];
  char    irm_res[4];
  char    irm_f5;
  char    irm_timer;
  char    irm_soct;
  char    irm_es;
  char    irm_clientid[8];
  char    irm_f1;
  char    irm_f2;
  char    irm_f3;
  char    irm_f4;
  char    irm_trncod[8];
  char    irm_imsdestid[8];
  char    irm_lterm[8];
  char    irm_racf_userid[8];
  char    irm_racf_grname[8];
  char    irm_racf_pw[8];
  char    LL[2];
  char    zz[2];
};

static char hrecv_buffer[32000];

struct kxImsConnectMsg { /* Non-IMS Connector for Java Message Structure - Type 2 without fields IRM_APPL_NM and IRM_REROUT_NM */
  struct kxImsIrmHdr irm;
  char               data[MAX_SKT_BUFSIZE];
};

static struct kxImsConnectMsg iRecv;

static void initialize_output_parameters (char ptransid[],
	       char pread_buffer[],
	       char pdata_buffer[],
	       int *pdata_len,
	       char pinterval[]);

int kxusrexit_in_socket( 
		int    psocket_fd,	/* Input parameter */ 
		char   ptransid[4],	/* 4-character transaction-id */
		char **pdata,		/* data area */
		int   *pdata_len,	/* length of data area */
		char   pstart_type[2],	/* 2-character start type set to KC */
		char   pinterval[6])	/* reserved */

{
	int     length_received;
	int     ltot_bytes;
        int     lbytes_to_read;
        int32_t lsize_llll;
        int     lfirst_time = 1;

        kxImsPrintDbg("uims[kxusrexit_in_socket]: begins, socket fd %d\n",psocket_fd);
	memcpy (ptransid, "/SKT", 4);
	*pdata = (void*)&iRecv;
	*pdata_len = sizeof(iRecv);
	memcpy (pstart_type, "KC", 2);
	memset (pinterval, '0', 6);

        memset(&iRecv, 0, sizeof(iRecv));
        memset(&hrecv_buffer, 0, sizeof(hrecv_buffer));

        ltot_bytes = 0;
        lbytes_to_read = sizeof(struct kxImsIrmHdr); /* we start expecting at least the header */
        kxImsPrintDbg("uims[kxusrexit_in_socket]: expecting minimum %d bytes\n",lbytes_to_read);
        while(ltot_bytes < lbytes_to_read) {
          length_received = 0;
	  length_received = recv (psocket_fd, &hrecv_buffer[ltot_bytes], (sizeof(hrecv_buffer)-ltot_bytes), 0);
	  if (length_received < 0) {
            kxprtf("uims[kxusrexit_in_socket]]: error reported by recv(), errno = %d\n",errno);
	    return(-1);
	  }
	  if (length_received == 0) { /* possibly, the client has gone away or closed the connection */
            kxprtf("uims[kxusrexit_in_socket]]: error client disconnected\n");
	    return(-2);
	  }
          kxImsPrintDbg("uims[kxusrexit_in_socket]: received %d bytes\n",length_received);
          kxImsHexDump(&hrecv_buffer[ltot_bytes],length_received);
          ltot_bytes += length_received;
          if((ltot_bytes > 3) && lfirst_time) { /* we received at minimum the LLLL let's update the bytes to read */
            lfirst_time = 0;
            memcpy(&lsize_llll, &hrecv_buffer[0], 4);
            lbytes_to_read = COBWORD(lsize_llll);
            kxImsPrintDbg("uims[kxusrexit_in_socket]: bytes to read modified to %d bytes\n",lbytes_to_read);
          }
        }
	*pdata_len = ltot_bytes;

        if(ltot_bytes > sizeof(struct kxImsConnectMsg)) {
          kxprtf("uims[kxusrexit_in_socket]]: error received %d bytes, max expected %d\n",ltot_bytes,sizeof(struct kxImsConnectMsg));
	  return(-2);
        }
        memcpy(&iRecv,&hrecv_buffer[0],ltot_bytes);
        kxImsPrintDbg("uims[kxusrexit_in_socket]: ends, area received from client, len = %d, socket fd = %d\n",ltot_bytes, psocket_fd);
        kxImsHexDump(&iRecv,ltot_bytes);
	return(0);
}
	    

/****************************************************************************
**
** FUNCTION:    kxsktxit
**
** DESCRIPTION: User exit module called when a socket request has been 
**              received.  This exit is used to check whether the request
**		should be executed.  Return a value of 1 in pswitch for
**		the request to be executed and a different value for the 
**		request to be rejected.
**		
**
** INPUTS:      ptrans    - transaction.
**              pdata     - data area sent.
**              pdatalen  - length of data area.
**              paction   - type of socket request "KC"
**              ptime     - reserved
**              paddrfam  - network address family, 2 means TCP/IP.
**              pport     - port number of requestors port.
**		paddr     - internet address of requestors host.
**		preserved - reserved.
**		pterm     - terminal associated with the request (always null).
**
** OUTPUTS:     pswitch   - a value of 1 allows the transaction to execute.
**
******************************************************************************/

void kxsktxit(

char           *ptrans,        /* 4 character string pointer  */
char           *pdata,         /* character string pointer to data area */ 
int             pdatalen,      /* pdata is not a null-terminated string.
				  The length of data is passed through pdatalen.
			       */
char           *paction,       /* 2 character string pointer  */
char           *ptime,         /* 6 character string pointer  */
short           paddrfam,      /* halfword binary value       */
in_port_t       pport,         /* halfword binary value       */
in_addr_t       paddr,         /* fullword binary value       */
char           *pswitch,       /* 1 character pointer         */
char            preserved,     /* 1 character string          */
char           *pterm)         /* 4 character string pointer  */

{

    kxImsPrintDbg("uims[kxsktxit]:ptrans  :%s\n", ptrans);
    kxImsPrintDbg("uims[kxsktxit]:paction :%s\n", paction);
    kxImsPrintDbg("uims[kxsktxit]:ptime   :%s\n", ptime);
    kxImsPrintDbg("uims[kxsktxit]:paddrfam:%d\n", paddrfam);
    kxImsPrintDbg("uims[kxsktxit]:pport   :%u\n", pport);
    kxImsPrintDbg("uims[kxsktxit]:paddr   :%d.%d.%d.%d\n", (paddr >> 24) & 0xFF,
	   (paddr >> 16) & 0xFF, (paddr >>  8) & 0xFF, (paddr) & 0xFF);
    kxImsPrintDbg("uims[kxsktxit]:pterm   :%s\n", pterm);

    /*
    ** The default action of the Socket Security Exit is to return a
    ** value of 1 in the pswitch field which allows all socket requests
    ** to continue.
    */
    *pswitch = 0x01;

    return;

} /* end of kxsktxit */

/****************************************************************************
**
** FUNCTION:    kxusrexit_out_socket
**
** DESCRIPTION: User exit module called when a transaction wants to reply 
**              to client connected via IMS Connect. 
**		This exit sends the transaction socket data to the client.
**	   	The default format of the socket data is represented
**              by struct kxImsConnectMsg. Do not change such structure, if
**              a different message format is expected from and to the client 
**              you must make the appropriate modification to this user exit
**              file in order to remap the client input message into a 
**              struct kxImsConnectMsg to pass to the transaction processor
**              and convert the message received from the transaction processor 
**              into the message structure the client is expecting.
**		
**           ***************************************************************
**	     *	THE PURPOSE OF THIS USER EXIT IS TO ALLOW USERS TO REMAP   *
**	     *	THIS DEFAULT FORMAT OF THE IMS CONNECT MESSAGE             *
**           ***************************************************************
**
**		Although the user exit gives complete flexibility to the 
**		user for defining his/her own initial message format, the 
**		user exit must abide by the constraints laid out as comments 
**		in the default code. The user exit returns certain parameters 
**		to the transaction processor. These parameters collectively
**		allow the transaction processor to schedule the execution of
**		the required transaction and pass to it the desired data.
**
**
** INPUTS:      psocket_fd  - File Descriptor (fd) of the socket to read from.
**              pdata       - Data Area to be sent to the client
**		pdata_len   - Length of message to be sent to the client
**
** OUTPUTS:     This integer function returns a value of (-1) on error,
**							 (0)  on success.
******************************************************************************/
int kxusrexit_out_socket( 
		int    psocket_fd,	/* Input parameter */ 
		char  *pdata,		/* data area */
		int    pdata_len)	/* length of data area */
{
	int   lret;
	int   ltot_bytes;
        char *lbytes_to_send_p;

        kxImsPrintDbg("uims[kxusrexit_out_socket]: begins, area to client, len = %d, socket fd = %d\n", pdata_len, psocket_fd);

        ltot_bytes       = pdata_len;
        lbytes_to_send_p = pdata;
        while(ltot_bytes > 0) {
          kxImsPrintDbg("uims[kxusrexit_out_socket]: sending %d bytes\n", ltot_bytes);
          kxImsHexDump(lbytes_to_send_p, ltot_bytes);
	  lret = send (psocket_fd, lbytes_to_send_p, ltot_bytes, 0);
          if(lret <= 0) {
            kxprtf("uims[kxusrexit_out_socket]]: error reported by send(), bytes to send %d, ret = %d, errno = %d\n", ltot_bytes, lret, errno);
            return(-1);
          }
          ltot_bytes       -= lret;
          lbytes_to_send_p += lret;
        }
        kxImsPrintDbg("uims[kxusrexit_out_socket]: ends\n");
        return(0);
}

int kxusrexit_in_socket_2( 
		int    psocket_fd,	/* Input parameter */ 
		char  *pdata,		/* data area */
		int    pdata_len)	/* length of data area */
{
	int                     length_received;
	int                     ltot_bytes;
        int                     lbytes_to_read;
        int32_t                 lsize_llll;
        unsigned short          lsize_LL;
        unsigned short          lsize_LL2;
        int                     lfirst_time = 1;
        struct kxImsConnectMsg *l_iRecv_p;

        kxImsPrintDbg("uims[kxusrexit_in_socket_2]: begins, socket fd %d\n",psocket_fd);

        memset(&hrecv_buffer, 0, sizeof(hrecv_buffer));
        if(pdata_len <= 0) {
          kxprtf("uims[kxusrexit_in_socket_2]]: error invalid input data len %d\n",pdata_len);
	  return(-1);
        }
        if(pdata_len > sizeof(hrecv_buffer)) {
          kxprtf("uims[kxusrexit_in_socket_2]]: error input data len %d exceedes max input buffer size %d\n",pdata_len,sizeof(hrecv_buffer));
	  return(-2);
        }

        ltot_bytes = 0;
        lbytes_to_read = sizeof(struct kxImsIrmHdr); /* we start expecting at least the header */
        kxImsPrintDbg("uims[kxusrexit_in_socket_2]: expecting minimum %d bytes\n",lbytes_to_read);
        while(ltot_bytes < lbytes_to_read) {
          length_received = 0;
	  length_received = recv (psocket_fd, &hrecv_buffer[ltot_bytes], (sizeof(hrecv_buffer)-ltot_bytes), 0);
	  if (length_received < 0) {
            kxprtf("uims[kxusrexit_in_socket_2]]: error reported by recv(), errno = %d\n",errno);
	    return(-3);
	  }
	  if (length_received == 0) { /* possibly, the client has gone away or closed the connection */
            kxImsPrintDbg("uims[kxusrexit_in_socket_2]]: client disconnected\n");
	    return(0);
	  }
          kxImsPrintDbg("uims[kxusrexit_in_socket_2]: received %d bytes\n",length_received);
          kxImsHexDump(&hrecv_buffer[ltot_bytes],length_received);
          ltot_bytes += length_received;
          if((ltot_bytes > 3) && lfirst_time) { /* we received at minimum the LL,zz let's update the bytes to read */
            lfirst_time = 0;
            memcpy(&lsize_llll, &hrecv_buffer[0], 4);
            lbytes_to_read = COBWORD(lsize_llll);
            kxImsPrintDbg("uims[kxusrexit_in_socket_2]: bytes to read modified to %d bytes\n",lbytes_to_read);
          }
        }

        if(ltot_bytes > pdata_len) {
          kxprtf("uims[kxusrexit_in_socket_2]]: error received %d bytes, max expected %d\n",ltot_bytes,pdata_len);
	  return(-4);
        }
        l_iRecv_p = (struct kxImsConnectMsg *)&hrecv_buffer[0];
        kxImsPrintDbg("uims[kxusrexit_in_socket_2]: ends, area received from client, len = %d, socket fd = %d\n",ltot_bytes, psocket_fd);
        kxImsHexDump(l_iRecv_p,ltot_bytes);

        memcpy(&lsize_LL, &l_iRecv_p->irm.LL[0], 2);
        lsize_LL2      = COBHALF(lsize_LL);
        memcpy(pdata,&l_iRecv_p->irm.LL[0],lsize_LL2);
        kxImsPrintDbg("uims[kxusrexit_in_socket_2]: area sent to program, len = %d\n",lsize_LL2);
        kxImsHexDump(pdata,lsize_LL2);
	return(lsize_LL2);
}
