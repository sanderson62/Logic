/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/****************************************************************************
**
** FUNCTION:   kxusrxlt
**
** DESCRIPTION:General User exit module to translate characters to the
**         desired code set.  The default is that data will be converted
**         from ASCII to EBCDIC on transmission and from EBCDIC to ASCII on
**         receipt.  For TCTUA and CommArea translation, the standard
**         translation tables are used. For the TIOA, the code attempts
**         to use the user-specified translation table. If no translation
**         table is specified by the user, the standard translation table
**         is used for TIOA conversion.
**
**         UniKix calls this exit for Transaction Routing so that data
**         sent to or received from the remote system is converted to the
**         correct code set.  This routine can also be used with user programs,
**         such as ones incorporating APPC conversations, to convert the data
**         being sent from program to program.
**
**         Transaction Routing internally calls this routine using values of
**         3, 4 and 5 for pno to distinguish between the TCT User Area, the
**         Terminal Input/Output Area and the Communications Area,
**         respectively.  These will normally follow the default code path so
**         caution should be exercised if this path is changed.
**
**         To call this routine from a user program, set pfrom and pto to
**         point to the data buffer, plen set to the length of the data to
**         convert and pdirection set to INBOUND or OUTBOUND.  The parameters
**         pno, pdata and pcode can be set to any value so that the user can
**         distinguish which code path to follow.  However, be careful not to
**         set pno to a value that conflicts with Transaction Routing.
**
**         The function 'kxcvt' can be called to access the standard
**         Conversion Vector Table routines.  Using this call, table
**         entries of type 'US' with sequence numbers of 'D' can be used to
**         convert the data with compare templates if necessary.
**
**         The default path assumes that the areas being dealt with in all
**         cases are display usage Cobol data with separate signs.
**
** INPUTS: pfrom      - address of area to translate from
**         pto        - address of area to translate to
**         plen       - length data to be translated in characters
**         pno        - parameter number 3 for USERAREA, 4 for TIOA and 5
**                      for COMMAREA
**         pdirection - INBOUND or OUTBOUND
**         pdata      - For Transaction Routing this is the 4 character
**                      transaction identifier (upper case)
**         pcode      - code page of partner (from TCT - System Entries Table).
**
** OUTPUTS:void
**
**
**
******************************************************************************/

#include <string.h>
#include "kxcnv.h"

extern int kxConvertTIOAmsg();

#define INBOUND  0
#define OUTBOUND 1
static void htranslate();

int kxusrxlt(pfrom, pto, plen, pno, pdirection, pdata, pcode)
char *pfrom;
char *pto;
int   plen;
int   pno;
int   pdirection;
char *pdata;
char *pcode;
{
   char presource[9];

   memset(presource, 0, 9);

   /*
   ** When testing, remove the comments around the following code
   */
   /*
   kxprtd("kxusrxlt:pfrom:%x\n", pfrom);
   kxprtd("kxusrxlt:pto:%x\n", pto);
   kxprtd("kxusrxlt:plen:%d\n", plen);
   kxprtd("kxusrxlt:pno:%d\n", pno);
   kxprtd("kxusrxlt:pdirection:%d\n", pdirection);
   kxprtd("kxusrxlt:pdata:%4.4s\n", pdata);
   kxprtd("kxusrxlt:pcode:%5.5s\n", pcode);
   */

   /*
   ** For different data passed in, you can include a test:
   **
   ** if (!memcmp(pdata, "ABCD")) {
   **     .
   **     .
   **     .
   ** }
   */

   /*
   ** Test the 'pno' value.  For a different translation, change the function
   ** call.
   */
   switch(pno) {

         /*
         ** To call the standard CVT table routines code:
         **
         ** kxcvt(resource_name,        8 character string
         **       pfrom,                character pointer
         **       pto,                  character pointer
         **       plen,                 integer
         **       pdirection)           integer
         **
         ** where resource_name is the name of the resource in the CVT table
         **
         ** for example:
         **
         ** sprintf(presource, "%s%d", pdata, pno);
         ** kxcvt(presource, pfrom, pto, plen, pdirection);
         **
         */

      case 3:  /* TCTUA    */
         /*
         ** Check the code page of the remote system.
         */
         if ((!strncmp(pcode, "     ", 5)) ||
               (pcode[0] == '\0')) {
            /*
            ** If the remote system is ASCII, do not convert the TCTUA.
            */
            memcpy(pto, pfrom, plen);

         } else {
            /*
            ** If the TCTUA does not contain display usage data or it
            ** does not need to be converted, remove the call to htranslate
            ** and uncomment the following memcpy statement.
            */
            /*
            memcpy(pto, pfrom, plen);
            */

            /*
            ** By default, it is assumed that the TCTUA contains display
            ** data and it needs to be converted.  This statement is executed.
            */
            htranslate(pfrom, pto, plen, pdirection);

            /*
            ** If the TCTUA contains pure DBCS characters, kxDBCStranslate
            ** will need to be called instead of htranslate.
            ** htranslate does a single-byte conversion.
            ** kxDBCStranslate does an equivalent double-byte conversion.
            ** Usage of kxDBCStranslate:
            ** void function; takes 3 input parameters:
            **          pto - the buffer to be translated
            **          plen - length of the buffer, must be
            **                 an even number
            **          conversion type - 'E' for ascii to ebcdic
            **                            'A' for ebcdic to ascii
            **
            ** Note that the pto parameter in the sample commented code below
            ** needs to be filled from pfrom, the original buffer.
            */
            /*
            memcpy(pto, pfrom, plen);
            if (pdirection == OUTBOUND)
                kxDBCStranslate(pto, plen, 'E');
            else
                kxDBCStranslate(pto, plen, 'A');
            */

            /*
            ** If the TCTUA contains a combination of display and binary
            ** data or a mix of single-byte and double-byte display data,
            ** set up a CVT entry and call the kxcvt routine as
            ** explained, above.
            */
         }

         break;

      case 4:  /* TIOA     */
         /*
         ** Check the code page of the remote system.
         */
         if ((!strncmp(pcode, "     ", 5)) ||
               (pcode[0] == '\0')) {
            /*
            ** If the remote system is ASCII, do not convert the TIOA.
            */
            memcpy(pto, pfrom, plen);

         } else {
            /*
            ** The TIOA is converted to EBCDIC.
            ** kxConvertTIOAmsg handles any DBCS translation.
            */
            memcpy(pto, pfrom, plen);
            if (pdirection == OUTBOUND) {
               kxConvertTIOAmsg(pto, plen, 'E');
            } else {
               kxConvertTIOAmsg(pto, plen, 'A');
            }
         }

         break;

      case 5:  /* COMMAREA */
         /*
         ** Check the code page of the remote system.
         */
         if ((!strncmp(pcode, "     ", 5)) ||
               (pcode[0] == '\0')) {
            /*
            ** If the remote system is ASCII, do not convert the COMMAREA.
            */
            memcpy(pto, pfrom, plen);

         } else {
            /*
            ** If the CommArea does not contain display usage data or it does
            ** not need to be converted, remove the call to htranslate and
            ** uncomment the following memcpy statement.
            */
            /*
            memcpy(pto, pfrom, plen);
            */

            /*
            ** By default, it is assumed that the CommArea contains display
            ** data and it needs to be converted.  This statement is executed.
            */
            htranslate(pfrom, pto, plen, pdirection);

            /*
            ** If the CommArea contains pure DBCS characters, kxDBCStranslate
            ** will need to be called instead of htranslate.
            ** htranslate does a single-byte conversion.
            ** kxDBCStranslate does an equivalent double-byte conversion.
            ** Usage of kxDBCStranslate:
            ** void function; takes 3 input parameters:
            **          pto - the buffer to be translated
            **          plen - length of the buffer, must be
            **                 an even number
            **          conversion type - 'E' for ascii to ebcdic
            **                            'A' for ebcdic to ascii
            **
            ** Note that the pto parameter in the sample commented code below
            ** needs to be filled from pfrom, the original buffer.
            */
            /*
            memcpy(pto, pfrom, plen);
            if (pdirection == OUTBOUND)
                kxDBCStranslate(pto, plen, 'E');
            else
                kxDBCStranslate(pto, plen, 'A');
            */

            /*
            ** If the CommArea contains a combination of display and binary
            ** data or a mix of single-byte and double-byte display data,
            ** set up a CVT entry and call the kxcvt routine as
            ** explained, above.
            */
         }

         break;

      default: /* USER     */

         htranslate(pfrom, pto, plen, pdirection);
         break;

   }
   return(0);
}



static void htranslate(pfrom, pto, plen, pdirection)
char *pfrom;
char *pto;
int   plen;
int   pdirection;
{

   /*
   ** The default conversion is between ASCII code page ISO 8859-1 and
   ** EBCDIC code page IBM-1047.
   */
   register int ii;

   if (pdirection == OUTBOUND) {

      for(ii = 0; ii < plen; ii++) {
         *(pto + ii) = user_asc2ebcd(*(pfrom + ii));
      }

   } else {

      for(ii = 0; ii < plen; ii++) {
         *(pto + ii) = user_ebcd2asc(*(pfrom + ii));
      }

   }

}

