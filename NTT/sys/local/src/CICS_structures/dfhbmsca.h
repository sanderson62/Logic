/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   23 Apr 2013 13:17:14  $ */
/* $Modtime:   23 Apr 2013 13:17:14  $ */

/* $Workfile:   dfhbmsca.h  $ $Revision:   1.2  $ */

/*
 * $Log:   /builds/source/TPE12.1a.PVCS/unikixsrc/devtools/compilers/clt/PVCS/dfhbmsca.h_v  $
 *
 *    Rev 1.2   23 Apr 2013 13:17:14   unikix
 * Update Dell copyright
 *
 *    Rev 1.1   18 Jan 2013 08:46:14   unikix
 * Dell rebranding
 *
 *    Rev 1.0   31 May 2007 13:36:22   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.2   25 May 2005 06:19:10   rh134138
 * B6176319 Make the bms constants consistent with the COBOL and PLI versions
 * of the same constants
 *
 *    Rev 1.1   17 Jun 2004 14:00:10   rs142317
 * 4945801.5009507
 * Newline project cleanup
 *
 *    Rev 1.0   17 Nov 2003 13:31:26   unikix
 * New 8.0 archive
 *
 *    Rev 1.1   04 Jun 2002 16:16:14   ks134152
 * 4662767
 * Changes for OUTLINE. The values must be in ASCII.
 *
 *    Rev 1.0   19 Sep 2001 16:37:02   unikix
 * Initial 7.2
 *
 *    Rev 1.0   30 Jan 2000 11:41:08   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 19:53:28   unikix
 * Initial 6.0
 *
 *    Rev 1.0   13 May 1998 15:27:18   randyh
 * Initial revision.
 *
 *    Rev 1.0   05/22/97 17:15:22   unikix
 */

#if !defined(DFHBMS_H)
#define DFHBMS_H
/*
 *      BMS - control codes
 */
#define DFHBMUNP        0x20    /* Unprotected */
#define DFHBMUNN        0x26    /* Unprotected and numeric */
#define DFHBMPRO        0x2d    /* Protected */
#define DFHBMASK        0x30    /* Autoskip */
#define DFHBMBRY        0x48    /* Bright */
#define DFHPROTI        0x59    /* Protected, intensify, light pen detectable */
#define DFHBMASB        0x38    /* Autoskip and bright */
#define DFHBMDAR        0x3c    /* Dark */
#define DFHPROTN        0x25    /* Protected, nondisplay, nonprint, nondetect */
#define DFHBMFSE        0x41    /* MDT set */
#define DFHBMASF        0x31    /* Autoskip and MDT set */
#define DFHUNNUM        0x4a    /* Unprotected, numeric, MDT */
#define DFHBMPRF        0x2f    /* Protected and MDT set */
#define DFHUNIMD        0x49    /* Unprotected, intensify, detectable, MDT */
#define DFHUNINT        0x52    /* Unprotected, numeric, intensify,detect,MDT*/
#define DFHUNNOD        0x28    /* Unprotected,nondisp,nonprt,nondetect, MDT */
#define DFHUNNON        0x29    /* Unprot,num,nondisp,nonprt,nondetect, MDT */
#define DFHDFCOL        0x00    /* Default colour */
#define DFHBLUE         0x31    /* Blue */
#define DFHRED          0x32    /* Red */
#define DFHPINK         0x33    /* Pink */
#define DFHGREEN        0x34    /* Green */
#define DFHTURQ         0x35    /* Turquoise */
#define DFHYELLO        0x36    /* Yellow */
#define DFHNEUTR        0x37    /* Neutral */
#define DFH3270         0x7b    /* Base 3270 field attribute */
#define DFHALL          0x00    /* Reset all to defaults */
#define DFHBASE         0x00    /* Base programmed symbols */
#define DFHBLINK        0x31    /* Blink */
#define DFHBMDET        0x00    /* Field detected */
#define DFHBMEOF        0x80    /* Field erased */
#define DFHBMPEM        0x19    /* Printer end-of-message */

/* Newline, DFHBMPNL, is changed to an ASCII 0x0A */
#define DFHBMPNL        0x0a    /* Printer new-line */

#define DFHCOLOR        0x21    /* Colour */
#define DFHDFHI         0x00    /* Normal */
#define DFHDFT          0xff    /* Default */
#define DFHERROR        0x1a    /* Error code */
#define DFHHLT          0x20    /* Highlight */
#define DFHMENT         0x02    /* Mandatory enter */
#define DFHMET          0x03    /* Mandatory enter and trigger */
#define DFHMFE          0x06    /* Mandatory fill and mandatory enter */
#define DFHMFET         0x00    /* Mandatory fill and mand enter and trigger */
#define DFHMFIL         0x1c    /* Mandatory fill */
#define DFHMFT          0x09    /* Mandatory fill and trigger */
#define DFHMT           0x01    /* Trigger */
#define DFHPS           0x22    /* Programmed symbols */
#define DFHREVRS        0x32    /* Reverse video */
#define DFHSA           0x08    /* Set attribute (SA) order */
#define DFHUNDLN        0x34    /* Underscore */
#define DFHVAL          0x41    /* Validation */
#define DFHBMPSO        0x0e    /* Shift out value X'0E' */
#define DFHBMPSI        0x0f    /* Shift in value X'0F' */
#define DFHOUTLN        0xc2    /* Field outlining attribute code */
#define DFHBKTRN        0x46    /* Background transparency attribute code */
#define DFHDFFR         0x00    /* Default outline */
#define DFHUNDER        0x01    /* Underline */
#define DFHRIGHT        0x02    /* Right vertical line */
#define DFHOVER         0x9c    /* Overline */
#define DFHLEFT         0x97    /* Left vertical line */
#define DFHBOX          0x0f    /* Underline, right, left and overline */
#define DFHSOSI         0x01    /* SOSI = yes */
#define DFHTRANS        0xf0    /* Background transparency */
#define DFHOPAQ         0xff    /* No background transparency */
#endif
