/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/****************************************************************************
**
** FUNCTION:    kxAutoInstTermID_exit
**
** DESCRIPTION: User exit module called when a TN3270 auto-installed terminal
**              is assigned, to set its 'terminal ID'
**
** INPUTS:      pluname   - incoming session's 8-char. LU name (9 char null terminated string)
**              pdevtype  - incoming session's device type
**              pipaddr   - incoming session's IP address
**              pipport   - incoming session's IP port number
**
** OUTPUTS:     ptermID   - 4-char terminal id to use for this session (NOT null terminated)
**                          The field is primed with a default value
**
** To build this user exit use the following make command
**     make -f makefile.xit
**
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include <arpa/inet.h>

void kxAutoInstTermID_exit(char *pluname, char *devtype, int pipaddr, int pipport, char ptermID[4])

{
   /*
   struct in_addr lin_addr;

   lin_addr.s_addr = pipaddr;
   printf ("kxAutoInstTermID_exit: Entry - lu name %s, devtype %s, IP address:port %s:%d, terminal id %4.4s\n",
                pluname, devtype, inet_ntoa( lin_addr ), pipport, ptermID );
   */

   /*
   ** The default action of the auto-installed terminal ID exit is to do nothing.
   */

   /*
   printf ("kxAutoInstTermID_exit: Exit - terminal id %4.4s\n", ptermID);
   fflush( stdout );
   */

   return;

} /* end of kxAutoInstTermID_exit */
