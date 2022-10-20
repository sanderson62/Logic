/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/****************************************************************************
**
** FUNCTION:    kxcvtxlt
**
** DESCRIPTION: User exit module for ISC functions to translate
**              fields to the desired code set.  This exit is called if
**              the template entry in the CVT table type is set to a number 
**              from 50 to 80.
**
**              For fields that contain a mix of single-byte and 
**              double-byte characters, single-byte segments should be 
**              translated using user_ebcd2asc (for ebcdic to ascii) and 
**              user_asc2ebcd (for ascii to ebcdic). These macros are defined 
**              in $UNIKIX/src/convert/kxcnv.h  
**              See the sample commented code below for an example of the 
**              usage of these macros. Double-byte segments should be 
**              translated using the kxDBCStranslate function. 
**              See the sample commented code below for an example of the 
**              usage of this function. 
**      
** INPUTS:      prtype    - resource type: US, FC, PC, IC, TS or TD
**              pname     - resource name up to 8 characters
**              pseqno    - sequence number: K, D, or 1-254
**              pdata     - data area to be converted up to approx. 32k
**              length    - length of data to be converted
**              direction - direction, outbound(1) or inbound(0)
**              pcode     - code page of partner system ("00037" for EBCDIC)
**              petype    - template entry type ("50" to "80")
**
** OUTPUTS:     void
**
**
******************************************************************************/

/*
**  NOTE: The 'length' parameter contains either the value specified on the 
**        length field in the CVT template entry or the length of data left
**        to convert in the record, whichever is the shorter.
*/

#include <string.h>
#include "kxcnv.h"

extern void kxDBCStranslate();

#define INBOUND  0
#define OUTBOUND 1

int kxcvtxlt(prtype, pname, pseqno, pdata, length, direction, pcode, petype)
char *prtype;
char *pname;
char *pseqno;
char *pdata;
int   length;
int   direction;
char *pcode;  
char *petype;
{
    int ii;

    /*
    ** The selection of the user conversion routines can be done on a
    ** single resource or system-wide basis.
    **
    ** single resource:
    **
    ** if((!strncmp(prtype, "FC", 2)) &&
    **    (!strncmp(pname,  "DATAFILE", 8)) &&
    **    (!strncmp(pseqno, "D  "))) {
    **
    **     if(!strncmp(petype, "51", 2)) {
    **
    **         conversion routine...
    **
    **     }
    **
    ** }
    **
    ** system-wide:
    **
    ** if(!strncmp(petype, "51", 2)) {
    **
    **     conversion routine...
    **
    ** }
    */

    /*
    ** This example routine will convert data between ASCII and EBCDIC for
    ** template entry type '51'.  For other entry types, the data will be
    ** left unconverted. The example assumes that the data comprises 
    ** single-byte characters only. 
    **
    ** user_asc2ebcd and user_ebcd2asc macros use the user-specified table
    ** for conversion between ASCII and EBCDIC. If no ASCII<->EBCDIC table
    ** is specified by the user, the standard ASCII<->EBCDIC table
    ** is used. 
    **
    ** The asc2ebcd and ebcd2asc macros are deprecated, the user versions
    ** should be used instead.
    */

    if(!strncmp(petype, "51", 2)) {

        if(direction == OUTBOUND) {

	    /* change this 'for' loop for different translation */
            for(ii=0; ii<length; ii++) {

                *(pdata+ii) = (char)user_asc2ebcd(*(pdata+ii));

            } /* end of for */

    	} else {

	    /* change this for loop for different translation */
            for(ii=0; ii<length; ii++) {

                *(pdata+ii) = (char)user_ebcd2asc(*(pdata+ii));

            } /* end of for */

	} /* end of else(direction == OUTBOUND) */

    } /* end of if(!strncmp(petype, "51", 2)) */

    /*
    ** This example routine will convert data between ASCII and EBCDIC for
    ** template entry type '52'.  For other entry types, the data will be
    ** left unconverted. The example assumes that the data comprises 
    ** double-byte characters only.
    */

    if(!strncmp(petype, "52", 2)) {

        if(direction == OUTBOUND) {

	    kxDBCStranslate(pdata, length, 'E');

    	} else {

	    kxDBCStranslate(pdata, length, 'A');

	} /* end of else(direction == OUTBOUND) */

    } /* end of if(!strncmp(petype, "52", 2)) */

    return 0;

} /* end of kxcvtxlt */
