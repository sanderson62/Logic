/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2007 by Clerity Solutions, Inc.                      */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

/****************************************************************************
**
** FUNCTION:    kxtnfxit
**
** DESCRIPTION: User exit module called when a terminal not found condition
**              arises.  This exit will be called if:
**
**                1.  During the processing of an EXEC CICS START command,
**                    the terminal specified by the TERMID option is not
**                    found in the TCT.
**
**                2.  During the execution of a transaction started either
**                    by an EXEC CICS START command or by Transient Data
**                    Automatic Transaction Initiation the required terminal
**                    is not found in the TCT.
**
** INPUTS:      pstrtcd   - start code: S, SD or QD.
**              ptasktype - task type: T (Transaction Route), F (Function Ship)
**                          or L (local transaction/terminal).
**              ptrans    - transaction.
**              pterm     - terminal.
**              ptermexec - if ptasktype is 'T' or 'L', terminal that executed
**                          the transaction which performed the start request.
**              pdfltTOR  - default TOR as defined in the SIT.
**              pnetname  - the partner LU alias of the system that originated
**                          the request, if ptasktype is not 'L'.
**              psysid    - the sysid of the system that originated the request,
**                          if ptasktype is not 'L'.
**              prnetname - the partner LU alias return field.
**              prsysid   - the sysid return field.
**
** OUTPUTS:     prnetname - return a value of 4 and in this field return the
**                          partner LU alias of the system the ATI request
**                          should be sent to.
**              prsysid   - return a value of 8 and in this field return the
**                          sysid of the system the ATI request should be sent
**                          to.
**
******************************************************************************/

/* Bug 4825517   */
#include <stdio.h>
#include <string.h>
#include <memory.h>

int kxtnfxit(pstrtcd, ptasktype, ptrans, pterm, ptermexec, pdfltTOR, pnetname,
             psysid, prnetname, prsysid)

/*
** All fields that are passed to this routine are space filled to their
** maximum length.
*/

char *pstrtcd;       /* 2 character string pointer */
char  ptasktype;     /* 1 byte character           */
char *ptrans;        /* 4 character string pointer */
char *pterm;         /* 4 character string pointer */
char *ptermexec;     /* 4 character string pointer */
char *pdfltTOR;      /* 4 character string pointer */
char *pnetname;      /* 8 character string pointer */
char *psysid;        /* 4 character string pointer */
char *prnetname;     /* 8 character string pointer */
char *prsysid;       /* 4 character string pointer */

{

    /*
    ** The default action of the Terminal Not Found exit is to use the 
    ** defaultTOR field as defined in the System Initialization Table:
    ** 
    **   1.  If the defaultTOR field is set, the contents of the field are
    **       copied to the contents of the prsysid field and a value of 8 is
    **       returned.  No TERMIDERR condition is raised and when executing a
    **       START or TDATI request, the ATI request is shipped over the
    **       connection defined in the DefaultTOR field.
    **
    **   2.  If the defaultTOR field is not set, a value of 0 is returned.
    **       When executing a START request, the TERMIDERR condition is raised,
    **       for a TDATI request, the start of the transaction will be 
    **       rescheduled when the terminal becomes available.
    */

    if ((memcmp(pdfltTOR, "    ", 4)) &&
	(strcmp(pdfltTOR, ""))) {

	memcpy(prsysid, pdfltTOR, 4);
	return(8);

    } else {

	return(0);

    }

    

    /*
    ** A more complex routine could do the following:
    **
    **
    **  if (memcmp(pterm, "AB", 2)) {
    **
    **      strcpy(prnetname, "CICS1");
    **
    **  } else {
    **
    **      strcpy(prnetname, "CICS2");
    **
    **  }
    **
    **  return(4);
    **
    */


} /* end of kxtnfxit */
