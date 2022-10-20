#ifndef lint
static char tp_rsSid[] = "@(#)tp_rs.c	1.8\t12 Jun 1993";
#endif /* lint */

/*
 * THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT
 * NOTICE AND SHOULD NOT BE CONSIDERED AS A COMMITMENT BY BRIXTON
 * SYSTEMS.
 */

/*****************************************************************************
*
* Prog Name:    tp_rs.c
*
* Description:
*
*	BrxCPIC Sample Program - receive and send.  This TP is partnered
*	with tp_sr.c.
*
* Usage:
*
*       tp_rs [-h <server> -l <local_lu> -p <local_tp> -t <trace_flag>]
*
*       where,
*
*       -h <server>     identifies the LU62 Server host.  Default is the
*                       local host.
*
*       -l <local_lu>   local LU name
*
*       -p <local_tp>   TP name to register for incoming attach
*
*       -r <res_id>	resource id to use as own TP instance in accept
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

char		*LocalLUName   = "LOCLUB";

char		*TPName        = "TPB";
char		*PartnerLUName;
char		*ModeName;
char 		*UserId;
char 		*Password;

bit32		OwnTPInstance  = 0;

lu62_processing_mode_e 	Processing_Mode	= PM_BLOCKING;
lu62_return_control_e 	Return_Control	= RC_WHEN_SESSION_ALLOCATED;
lu62_conv_type_e 	Conv_Type;
lu62_sync_level_e 	Sync_Level;
lu62_security_e 	Security;

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

static ll_record_t LL_buf2 = {31, 0,
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
    static lu62_register_tp_t  register_req  = {0,};
    static lu62_accept_t       accept_req    = {0,};
    static lu62_get_attributes_t getattr_req = {0,};
    static lu62_confirm_t      cfm_req       = {0,};
    static lu62_confirmed_t    cfmd_req      = {0,};
    static lu62_deallocate_t   deall_req     = {0,};
    static lu62_flush_t        flush_req     = {0,};
    static lu62_send_data_t    send_data_req = {0,};
    static lu62_receive_t      recv_data_req = {0,};

    rbuffer = (char *)malloc(MAX_RCV_MSG_SIZE);
    sbuffer = (char *)malloc(MAX_SND_MSG_SIZE);

    rc = open_lu(&open_req);
    if (rc == LU62_ERROR)
        exit(1);

    /* establish LU port context for upcoming verbs */
    close_req.port_id     = open_req.port_id;
    accept_req.port_id    = open_req.port_id;

    if (OwnTPInstance) {
        accept_req.own_tp_instance = OwnTPInstance;
    }
    else {
        register_req.port_id  = open_req.port_id;
        rc = register_tp(&register_req, TPName);
        if (rc == LU62_ERROR)
            exit(1);
    }

    rc = accept_conv(&accept_req);
    if (rc == LU62_ERROR)
        exit(1);

    /* display conversation type */
    rc = get_conv_type(accept_req.conv_id);
    if (rc == LU62_ERROR)
        exit(1);

    /* display attributes */
    rc = get_attributes(accept_req.conv_id);
    if (rc == LU62_ERROR)
        exit(1);

    /* display tp_properties */
    rc = get_tp_properties(accept_req.conv_id);
    if (rc == LU62_ERROR)
        exit(1);

    /* determine sync level */
    getattr_req.conv_id = accept_req.conv_id;
    rc = lu62_get_attributes(&getattr_req);
    if (rc == LU62_ERROR)
        exit(1);
    Sync_Level = getattr_req.sync_level;

    /* establish conversation context for upcoming verbs */
    cfm_req.conv_id       = accept_req.conv_id;
    cfmd_req.conv_id      = accept_req.conv_id;
    deall_req.conv_id     = accept_req.conv_id;
    flush_req.conv_id     = accept_req.conv_id;
    send_data_req.conv_id = accept_req.conv_id;
    recv_data_req.conv_id = accept_req.conv_id;

    /* RECEIVER set up */
    prog_state = RECEIVER;
    recv_buf = (ll_record_t *)rbuffer;

    /* SENDER set up */

    /* convert bit16 fields to network order */
    LL_buf1.len = htons(LL_BUF1_LEN);
    LL_buf2.len = htons(LL_BUF2_LEN);

    bcopy(&LL_buf1, sbuffer, LL_BUF1_LEN);
    bcopy(&LL_buf2, sbuffer + LL_BUF1_LEN, LL_BUF2_LEN);
    send_len = LL_BUF1_LEN + LL_BUF2_LEN;
    send_buf = (ll_record_t *)sbuffer;

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
    printf("usage: tp_rs [options]\n");
    printf("options are:\n");
    printf("    -h<server = %s>\n", Host);
    printf("    -l<lu_name = %s>\n", LocalLUName);
    printf("    -p<tp_name = %s>\n", TPName);
    printf("    -r<res_id  = %d>\n", OwnTPInstance);
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

    while((c = getopt(argc, argv, "h:l:p:r:t:")) != EOF)
        switch(c) {
 
        case 'h':      /* server host */
            Host = optarg;
            break;
        case 'l':
            LocalLUName = optarg;
            break;
        case 'p':
            TPName = optarg;
            break;
        case 'r':
            OwnTPInstance = strtol(optarg, (char **) NULL, 0);
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

