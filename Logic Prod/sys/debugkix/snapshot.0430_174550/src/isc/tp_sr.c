#ifndef lint
static char tp_srSid[] = "@(#)tp_sr.c	1.6\t28 Apr 1993";
#endif /* lint */

/*
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT
 * NOTICE AND SHOULD NOT BE CONSIDERED AS A COMMITMENT BY BRIXTON
 * SYSTEMS.
 */

/*****************************************************************************
*
* Prog Name:    tp_sr.c
*
* Description:
*
*	BrxCPIC Sample Program - send and receive.  This TP is partnered
*	with tp_rs.c.
*
* Usage:
*
*       tp_sr [-h <server> -l <local_lu> -r <partner_lu> -m <mode_name>
*              -p <remote_tp> -c -t <trace_flag>]
*
*       where,
*
*       -h <server>     identifies the LU62 Server host.  Default is the
*                       local host.
*
*       -l <local_lu>   local LU name
*
*       -r <partner_lu> Partner LU name
*
*       -m <mode_name>  Mode name
*
*       -p <remote_tp>  remote TP name
*
*	-c		Send and Confirm (default is Send and Flush)
*
*       -t <trace_flag> trace options word.
*
* Creation Date: 03/15/92
*
* Change Log:
*
*****************************************************************************/

#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <sys/param.h>
#include <sys/types.h>
#include <netinet/in.h>
 
#include "brxlu62.h"
#include "kxgetmem.h"


/************************************************************************
 *
 * Program constants and globals
 *
 ************************************************************************
 */
#define MAX_SND_MSG_SIZE    4096    /* MAX size of transmitted messages     */
#define MAX_RCV_MSG_SIZE    4096    /* MAX size of received messages        */

typedef enum {
    RECEIVER = 0,
    SENDER,
    DEALLOCATED
} prog_state_e;

char		*Prog_name;
char 		*Host = ""; 	/* LU62 Server host */

char		*LocalLUName   = "LOCLUC";

char		*TPName        = "*";
char		*PartnerLUName = "LOCLUCP";
char		*ModeName      = "SNALU62";
char 		*UserId        = "username";
char 		*Password      = "password";
char 		*Profile       = "group";

lu62_processing_mode_e 	Processing_Mode	= PM_BLOCKING;
lu62_return_control_e 	Return_Control	= RC_WHEN_SESSION_ALLOCATED;
lu62_conv_type_e 	Conv_Type 	= CONVERSATION_MAPPED;
lu62_sync_level_e 	Sync_Level	= SYNC_LEVEL_NONE;
lu62_security_e 	Security	= SECURITY_NONE;

bit32		Trace_flag;

/* send and receive buffers are malloc'ed during initialization */
static char     *rbuffer;
static char     *sbuffer;

typedef struct ll_record {
    bit16 len;
    bit16 seq_num;
    char  data[81];
} ll_record_t;  
 
static ll_record_t LL_buf1 = {0, 0,
"1234567890123456789012345678901234567890123456789012345678901234567890123456789\n"};
#define LL_BUF1_LEN     84

static ll_record_t LL_buf2 = {0, 0,
"ABCDEFGHIJKLMNOPQRSTUVWXYZ\n"};
#define LL_BUF2_LEN     31


/*-----------------------------------------------------------------------
 * session
 *-----------------------------------------------------------------------
 */
void
session()
{
    prog_state_e prog_state;
    ll_record_t *recv_buf, *send_buf;
    int send_len;
    int rc;
    bit16 send_seq_num = 0;

    static lu62_open_req_t     open_req      = {0,};
    static lu62_close_req_t    close_req     = {0,};
    static lu62_allocate_t     alloc_req     = {0,};
    static lu62_confirm_t      cfm_req       = {0,};
    static lu62_confirmed_t    cfmd_req      = {0,};
    static lu62_deallocate_t   deall_req     = {0,};
    static lu62_flush_t        flush_req     = {0,};
    static lu62_send_data_t    send_data_req = {0,};
    static lu62_receive_t      recv_data_req = {0,};

    rbuffer = (char *)malloc(MAX_RCV_MSG_SIZE);
    sbuffer = (char *)malloc(MAX_SND_MSG_SIZE);

    /* SENDER set up */

    prog_state = SENDER;

    /* convert bit16 fields to network order */
    LL_buf1.len = htons(LL_BUF1_LEN);
    LL_buf2.len = htons(LL_BUF2_LEN);

    bcopy(&LL_buf1, sbuffer, LL_BUF1_LEN);
    bcopy(&LL_buf2, sbuffer + LL_BUF1_LEN, LL_BUF2_LEN);
    send_len = LL_BUF1_LEN + LL_BUF2_LEN;
    send_buf = (ll_record_t *)sbuffer;
 
    rc = open_lu(&open_req); 
    if (rc == LU62_ERROR)   
        exit(1);           

    /*  establish LU port context for upcoming verbs */
    close_req.port_id = open_req.port_id;
    alloc_req.port_id = open_req.port_id;

    rc = allocate_conv(&alloc_req);
    if (rc == LU62_ERROR)
        exit(1);

    /* display attributes */
    rc = get_attributes(alloc_req.conv_id);
    if (rc == LU62_ERROR)
        exit(1);

    /* display tp_properties */
    rc = get_tp_properties(alloc_req.conv_id);
    if (rc == LU62_ERROR)
        exit(1);

    /* establish conversation context for upcoming verbs */
    cfm_req.conv_id       = alloc_req.conv_id;
    cfmd_req.conv_id      = alloc_req.conv_id;
    deall_req.conv_id     = alloc_req.conv_id;
    flush_req.conv_id     = alloc_req.conv_id;
    send_data_req.conv_id = alloc_req.conv_id;
    recv_data_req.conv_id = alloc_req.conv_id;

    /* RECEIVER set up */
    recv_buf = (ll_record_t *)rbuffer;

    while (rc == LU62_OK 
       &&  prog_state != DEALLOCATED) {
        switch (prog_state) {

        case RECEIVER:
            bzero(rbuffer, MAX_RCV_MSG_SIZE);
            recv_data_req.data = (bit8 *)recv_buf;
            recv_data_req.length = MAX_RCV_MSG_SIZE;
            rc = receive_ll(&recv_data_req);

            if (rc == LU62_OK) {

                switch (recv_data_req.what_received) {
                case WR_CONFIRM:
                    printf("CONFIRM_RECEIVED\n");
                    rc = confirmed_conv(&cfmd_req);
                    break;
                case WR_SEND:
                    printf("SEND_RECEIVED\n");
                    prog_state = SENDER;
                    break;
                case WR_CONFIRM_SEND:
                    printf("CONFIRM_SEND_RECEIVED\n");
                    rc = confirmed_conv(&cfmd_req);
                    prog_state = SENDER;
                    break;
                case WR_CONFIRM_DEALLOCATE:
                    printf("CONFIRM_DEALLOC_RECEIVED\n");
                    rc = confirmed_conv(&cfmd_req);
                    prog_state = DEALLOCATED;
                    break;
                }
            }
            break;

        case SENDER:
            send_seq_num += 1;
            send_buf->seq_num = htons(send_seq_num);
            send_data_req.data = (bit8 *)send_buf;
            send_data_req.length = send_len;
            rc = send_data_conv(&send_data_req);
            /***********************************************/  
            /* for DTP processing ..do another send        */
            /***********************************************/  
            if (send_seq_num == 1)
                rc = send_data_conv(&send_data_req);

            if (rc == LU62_OK)
                if (Sync_Level == SYNC_LEVEL_CONFIRM) 
                    rc = confirm_conv(&cfm_req);
            prog_state = RECEIVER;
            break;

        case DEALLOCATED:
            break;
        }
    }

    close_lu(&close_req);
}


/*-----------------------------------------------------------------------
 * usage
 *-----------------------------------------------------------------------
 */
usage()
{
    printf("usage: tp_sr [options]\n");
    printf("options are:\n");
    printf("    -h<server          = %s>\n", Host);
    printf("    -l<lu_name         = %s>\n", LocalLUName);
    printf("    -r<partner_lu_name = %s>\n", PartnerLUName);
    printf("    -m<mode_name       = %s>\n", ModeName);
    printf("    -p<tp_name         = %s>\n", TPName);
    printf("    -c (send and confirm)\n");
    printf("    -t (trace) = %x\n", Trace_flag);
    exit(1);
}


/*-----------------------------------------------------------------------
 * main
 *-----------------------------------------------------------------------
 */
main(argc, argv)
int  argc;
char *argv[];
{
    extern char *optarg;
    extern int optind;
    int c;

    int errflg = 0;

    char *cp;
    long strtol();

    signal(SIGPIPE, SIG_IGN);

    /* default is no tracing */
    Trace_flag = 0;

    /*
     * Process command line args
     * minimal error checking!
     */
    Prog_name = argv[0];
    for (; cp = strchr(Prog_name, '/'); Prog_name = ++cp);

    while((c = getopt(argc, argv, "h:l:r:m:p:ct:")) != EOF)
        switch(c) {
 
        case 'h':      /* server host */
            Host = optarg;
            break;
        case 'l':
            LocalLUName = optarg;
            break;
        case 'r':
            PartnerLUName = optarg;
            break;
        case 'm':
            ModeName = optarg;
            break;
        case 'p':
            TPName = optarg;
            break;
        case 'c':
            Sync_Level = SYNC_LEVEL_CONFIRM;
            break;
        case 't':      /* API tracing options */
            Trace_flag = strtol(optarg, (char **) NULL, 0);
            break;
        case '?':      /* help: doesn't work too well with csh */
            errflg++;
            break;
        default:       /* the rest are errors */
            errflg++;
            break;
        }
 
    if (errflg) {
        usage();
    }

    lu62_set_trace_flag(Trace_flag);

    session();

    exit(0);
}

