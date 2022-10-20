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

extern void kxerror(int kxerrno, int printflag, char *kxroutine);
extern void kxerror1(int kxerrno, int printflag, char *kxroutine, int errcode1);

#define E_PRINT		1
#define E_PRINTF	2

#define E_2327		2327
#define E_2328		2328
#define E_2329		2329
#define E_2330		2330
#define E_2331		2331

#define LEN_TRANID	4


/****************************************************************************
**
** FUNCTION:    kxusrexit_in_socket
**
** DESCRIPTION: User exit module called when a socket request is made. 
**		This exit reads the initial socket data and returns
**		the details of the message to the transaction processor. 
**	   	The default format of the initial socket data is:-
**		
**		<4-byte tranid>,<35-byte data>,<2-byte start-type>,<6-byte
**							            time value>
**
**		The following constraints apply for the default format:-
**		
**		1. The valid values for start-type are "TD" for Transient Data
**		   and "IC" for Interval Control. The default value is "KC"
**		   which denotes immediate execution of the transaction.
**		
**		2. The 6-byte time value is only used if a start-type 
**		   of "IC" is specified. The time value, represented in 
**		   HHMMSS is the time that needs to expire before the 
**		   transaction is scheduled for execution.
**
**		3. The commas preceding the fields are mandatory, i.e. 
**		   if start-type has to be specified but no data needs to be
**		   specified, the message would look like 
**	
**		   xxxx,,TD       
**
**           ***************************************************************
**	     *	THE PURPOSE OF THIS USER EXIT IS TO ALLOW USERS TO CHANGE  *
**	     *	THIS DEFAULT FORMAT OF THE INITIAL SOCKET MESSAGE.         *
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
**
** OUTPUTS:     ptransid    - Transaction that needs to be executed
**		pdata       - Data Area for CLIENT-IN-DATA
**		pdata_len   - Length of data to be passed to CLIENT-IN-DATA.
**		pstart_type - Start type for transaction ("TD" or "IC")
**		pinterval   - Time, in HHMMSS format, after which 
**			      the transaction is scheduled for execution
**
**	        This integer function returns a value of (-1) on error,
**							 (0)  on success.
******************************************************************************/


#define CLIENT_DATA_LENGTH    35 /* This is the length of the data passed into 
				    the initial socket message record's 
				    CLIENT-IN-DATA field.

				    This value MUST be less than 32733

				    Within this constraint, the user is 
				    welcome to change this value */


#define SOCKET_RECEIVE_LENGTH 52 /* This is the maximum length of the default 
				    message, if all parameters are specified.
				    Actually, it is 50 but since the max was
				    52 before, we have retained the higher 
				    value for compatibility with previous 
				    releases.

				    The constraints for this value are 
				    governed by the limits imposed by the 
				    Operating System configuration on maximum 
				    length that can be received from a 
				    socket connection. 

				    Within those constraints, the user is
				    welcome to change this value */

static char data_buffer[CLIENT_DATA_LENGTH];
static char read_buffer[SOCKET_RECEIVE_LENGTH];

static void initialize_output_parameters (char ptransid[],
	       char pread_buffer[],
	       char pdata_buffer[],
	       int *pdata_len,
	       char pinterval[]);

int kxusrexit_in_socket( 
		int    psocket_fd,	/* Input parameter */ 
		char   ptransid[4],	/* 4-character transaction-id */
		char **pdata,		/* data area for client data */
		int   *pdata_len,	/* length of data area */
		char   pstart_type[2],	/* 2-character start type, valid values
					   are "TD" and "IC" */
		char   pinterval[6])	/* 6-char time value, HHMMSS format */

{
	/* Housekeeping variables required for parsing */

	int length_received;
	int len_transid;
	int len_data;
	int len_start_type;
	int len_interval;
	int i;

	char *first_comma;
	char *second_comma;
	char *third_comma;

	initialize_output_parameters (ptransid, read_buffer, data_buffer, 
					pdata_len, pinterval);

	/* Since the default initial socket message has no way of telling the
	   receiver how much data is being sent (i.e. there is no leading 
	   length indicator in the data nor there is any trailing pattern 
	   to look for), we attempt to receive all the data in one go. If 
	   the length of data being received can be determined, 'recv' should 
	   be coded to be in a loop till all data is received or an error 
	   condition occurs.
	*/

	length_received = recv (psocket_fd, read_buffer, sizeof(read_buffer),
				 0);
	if (length_received < 0)
	{
	    kxerror1(E_2327,E_PRINT+E_PRINTF, "unikixtran", errno);
	    return(-1);
	}

	if (length_received == 0) /* possibly, the client has gone away or */
	{ 	                  /* closed the connection */
	    return(-2);
	}
	*pdata = data_buffer;

	first_comma = memchr (read_buffer, ',', length_received);

	if (first_comma == 0)  /*only transid specified */
	{
	    len_transid = length_received;
	}
	else
	{
	    len_transid = first_comma - read_buffer;
	}

	/* Validate transid */
	if ((len_transid > LEN_TRANID) || (len_transid == 0))
	{
	    kxerror(E_2328,E_PRINT+E_PRINTF, "unikixtran");
	    return(-1);
	}
	
	memcpy (ptransid, read_buffer, len_transid);

	if (first_comma == 0) /* No more data */
	{
	    return(0);
	}

	second_comma = memchr (first_comma + 1, ',', length_received - 
						     len_transid - 1);

	if (second_comma == 0) /* Nothing specified after data */
	{
	    len_data = length_received - len_transid - 1; /* 1-byte comma */
	}
	else
	{
	    len_data = second_comma - first_comma - 1;
	}
	
	/* Validate data length */
	if (len_data > CLIENT_DATA_LENGTH)
	{
	    kxerror1(E_2329,E_PRINT, "unikixtran", len_data);
	    return(-1);
	}

        if (len_data > 0)
	{
	    memcpy (data_buffer, first_comma + 1, len_data);
	}


	/* If the CICS/COBOL programs invoked via socket messages expect 
	   the data length to be fixed, *pdata_len should be set to that 
	   fixed value. (e.g. 35 in situations where the default
	   socket message format is being used.

	   However, if the CICS/COBOL programs invoked via socket messages
	   expect varying data lengths, *pdata_len should be set to 
	   len_data which really reflects the length of data received.  

	   *pdata_len = len_data;
	*/

	*pdata_len = CLIENT_DATA_LENGTH;

	if (second_comma == 0) /* No more data */
	{
	    return(0);
	}

	third_comma = memchr (second_comma + 1, ',', length_received -
						     len_transid - 1 -
						     len_data - 1);

	if (third_comma == 0) /* nothing specified after start-type */
	{
	    len_start_type = length_received - len_transid - len_data 
				- 1 - 1; /* 1-byte commas after transid */
	}				 /* and data */
	else
	{
	    len_start_type = third_comma - second_comma - 1;
	}
	
	/* Validate start-type */
	if (len_start_type == 0)
	{
	    return(0);
	}
	
	if (((len_start_type > 2)) ||
	    (memcmp (second_comma + 1, "TD", 2) &&
	     memcmp (second_comma + 1, "IC", 2)))
	{
	    kxerror(E_2330,E_PRINT+E_PRINTF, "unikixtran");
	    return(-1);
	}

	memcpy (pstart_type, second_comma + 1, len_start_type);
			    
	if (third_comma == 0) /* No more data */
	{
	    return(0);
	}

        len_interval = length_received - len_transid - len_data - 
			len_start_type - 1 - 1 - 1;

	/* Validate interval */
	if (len_interval == 0)
	{
	    return(0);
	}

	if (len_interval > 6)
	{
	    kxerror(E_2331,E_PRINT+E_PRINTF, "unikixtran");
	    return(-1);
	}

	for (i = 0; i < 6; i++)
	{
	    if ((*(third_comma + 1 + i) > '9') ||
		(*(third_comma + 1 + i) < '0'))
	    {
		kxerror(E_2331,E_PRINT+E_PRINTF, "unikixtran");
		return(-1);
	    }
	}

	memcpy (pinterval, third_comma + 1, len_interval);
	return(0);
}
	    

static void initialize_output_parameters (char ptransid[],
	       char pread_buffer[],
	       char pdata_buffer[],
	       int *pdata_len,
	       char pinterval[])
{
	memset (ptransid,     ' ', LEN_TRANID); /* Transids are space padded */

	memset (pread_buffer, 0, SOCKET_RECEIVE_LENGTH);
	memset (pdata_buffer, 0, CLIENT_DATA_LENGTH);

	memset (pinterval,    '0', 6); /* interval expects ASCII text between
					  0 and 9 */

	/* Since the default initial socket message expects 35 bytes of 
	   data, *pdata_len is initialized to 35. If the CICS/COBOL programs 
	   expect varying lengths of data, *pdata_len should be initialized
	   to 0. The user exit, as delivered with the software, assumes the 
	   default initial socket message format. This allows for backward
	   compatibility, i.e. application programs written for the default
	   message format would still work.
	*/

	*pdata_len = CLIENT_DATA_LENGTH;

	return;
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
**              paction   - type of socket request "KC", "IC" or "TD".
**              ptime     - interval control time in the form "HHMMSS".
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
char           *pdata,         /* character string pointer to data area; 
				  length was fixed at 40 before, but now it can
				  be changed by the user exit function 
				  kxusrexit_in_socket() provided within this
				  module;
			       */

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

    /*
    ** When testing, remove the comments around the following code
    */
    /*
    printf("kxsktxit:ptrans  :%s\n", ptrans);
    printf("kxsktxit:pdata   :%*.*s\n", pdatalen, pdatalen, pdata);
    printf("kxsktxit:paction :%s\n", paction);
    printf("kxsktxit:ptime   :%s\n", ptime);
    printf("kxsktxit:paddrfam:%d\n", paddrfam);
    printf("kxsktxit:pport   :%u\n", pport);
    printf("kxsktxit:paddr   :%d.%d.%d.%d\n", (paddr >> 24) & 0xFF,
	   (paddr >> 16) & 0xFF, (paddr >>  8) & 0xFF, (paddr) & 0xFF);
    printf("kxsktxit:pterm   :%s\n", pterm);
    */

    /*
    ** The default action of the Socket Security Exit is to return a
    ** value of 1 in the pswitch field which allows all socket requests
    ** to continue.
    */
    *pswitch = 0x01;

    return;

} /* end of kxsktxit */

int kxusrexit_out_socket( /* dummy function actual definiton in kximssktxit.c */
                int    psocket_fd,      /* Input parameter */
                char  *pdata,           /* data area */
                int    pdata_len)       /* length of data area */
{
  return(-1);
}

int kxusrexit_in_socket_2( /* dummy function actual definiton in kximssktxit.c */
                int    psocket_fd,      /* Input parameter */
                char  *pdata,           /* data area */
                int    pdata_len)       /* length of data area */
{
  return(-1);
}
