/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/


/* $Date:   03 May 2012 06:47:02  $ */
/* $Modtime:   03 May 2012 06:24:16  $ */

#ifndef lint
#ifdef __STDC__
const
#endif
static char sccsid[] = "@(#) $Workfile:   cvlwrupr.c  $ $Revision:   1.1  $";
#endif

/*
 * $Log:   /CSO/sr104371/tpe1132_64/unikixsrc/user/convert/PVCS/cvlwrupr.c_v  $
 *
 *    Rev 1.1   03 May 2012 06:47:02   sr104371
 * B7002200
 * Improve diagnostics for mblen() failure in cvlwrupr()
 *
 *    Rev 1.0.2.2   23 Apr 2013 13:24:40   unikix
 * Update Dell copyright
 *
 *    Rev 1.0.2.1   25 Jan 2013 15:47:24   dd134127
 * Merge 1.0.1.2 and 1.0.2.0
 *
 *    Rev 1.0.2.0   18 Jan 2013 08:56:28   unikix
 * Dell rebranding
 *
 *    Rev 1.0.1.2   22 Jan 2013 16:03:48   ss134151
 * B7002436
 * EIP rework; less static variables, free() allocated memory once no longer needed
 *
 *    Rev 1.0.1.1   21 Jan 2013 18:45:36   ss134151
 * B7002436
 * Correct initial changes to optimize calls to setlocale
 *
 *    Rev 1.0.1.0   21 Dec 2012 14:34:00   ss134151
 * B7002436
 * Optimize calls to setlocale; only if current is not "default", needed for mblen
 *
 *    Rev 1.0   31 May 2007 13:40:04   unikix
 * Initial TPE11.0.0a
 *
 *    Rev 1.1   26 Feb 2004 14:55:58   mf134128
 * b4997186
 * return immediately if length < 1
 *
 *    Rev 1.0   17 Nov 2003 13:50:42   unikix
 * New 8.0 archive
 *
 *    Rev 1.4   15 Oct 2003 10:01:20   dv133961
 * B4825517
 * Remove warning messages generated during kixinstall
 *
 *    Rev 1.3   30 May 2002 12:22:48   dd134127
 * Check value of noBytes, not mblen.
 *
 *    Rev 1.2   30 May 2002 11:10:20   dd134127
 * define E_3657 and E_3658
 *
 *    Rev 1.1   29 May 2002 12:40:54   ss134153
 * D4662751
 * Changed lower to upper routine used in MTP to account for locale.
 * This allows for correct handling of double byte characters when
 * passed to the routine.
 *
 *    Rev 1.0   19 Sep 2001 16:47:36   unikix
 * Initial 7.2
 *
 *    Rev 1.0   30 Jan 2000 11:47:48   unikix
 * Initial 7.0
 *
 *    Rev 1.0   15 Mar 1999 19:59:54   unikix
 * Initial 6.0
 *
 *    Rev 1.0   05/22/97 17:33:14   unikix
 * Initial 5.1
 *
 *    Rev 1.0   04/10/96 14:13:02   unikix
 * Initial 5.0
 *
 *    Rev 1.0   12/28/94 15:18:12   unikix
 * Initial revision (from V410ad)
 *
 *    Rev 1.0.1.2   10/26/94 09:53:02   alan
 * Load conversion tables into memory
 *
 *    Rev 1.0.1.1   09/16/94 16:10:08   randyh
 * Move rev 1.1 to V410u
 *
 *    Rev 1.0.1.0   11/18/93 16:33:36   unikix
 * UniKix 4.1 baseline (from V400m)
 *
 *    Rev 1.1   07/06/94 17:01:22   randyh
 * Kaschewski: more unsigned char problems.
 *
 *    Rev 1.0   06/10/93 11:58:38   unikix
 * UniKix 3.1.2 version
 */

/*
cvlwrupr.c exported routines:
        cvlwrupr
cvlwrupr.c has no exported variables
cvlwrupr.c imports:
        toupper
        islower
        mblen
        setlocale
*/

/*
**Name       :  CVLWRUPR
**              This subroutine converts an array of characters to
**              upper case.
**
**              1.2         (91/02/15)
                Updated 2002/05/28
**
**Parameters :  PINCHAR: is the first character of the string to be
**              converted.
**              PLENGTH: is the number of characters to convert.
**
**Return     :  This procedure returns the string starting at PINCHAR
**              with PLENGTH character converted to upper case.
**
**External   :  TOUPPER(<char>). Converts a character from lower case
**              to upper case.
**
**              ISLOWER(<char>). Returns TRUE if character is lower case
**
**              MBLEN(<char *>). Returns number of bytes that comprise
**              the character pointed to.
**
**              SETLOCALE(<int category>, <const char * locale>):
**              Sets the locale or returns current locale depending
**              upon input parameters
**
**              CTYPE.H  Functions for the classification and conversion
**              of characters.
**
*/

/* ------------- unix includes ------------- */
#include <ctype.h>
#include <locale.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>


extern void kxprtd();
extern void kxerror();
extern void kxerror1();
extern void kxerror2();
extern void kxerrorstr();
extern void kxdump( char *mem, int len );
extern void kxdump_trace_tables( char type, char *msg );
extern int  kxChkTCTLocale();


#define E_3654  3654
#define E_3655  3655
#define E_3656  3656
#define E_3657  3657
#define E_3658  3658

#define E_PRINT 1


static char originalLocale[50] = ""; /* locale before entering routine */
static int origlocale_saved = 0;

static int localesChecked = 0; /* set to 1 once locale's compared (1st time) */
static int localesDiffer = 0; /* set to 1 if the two locales are different */

void cvlwrupr(unsigned char *pinchar, int plength)
{
   int   li;            /*loop index */
   char *retChar = NULL; /* locale from setlocale, used for islower ... */
   static int originalLocaleLen = 0; /* original locale's string length */
   int noBytes;
   int retInt;
   int lerr = 0;
   int tctlocaleDiffers = 0;

   if (plength < 1) { /* Nothing to do */
      return;
   }

   if (localesChecked == 0) { /* 1st time through? */
      localesChecked = 1; /* flag set, doing 1st time through stuff now */
      retChar = setlocale(LC_CTYPE, NULL);   /* save locale */
      if (retChar) {
         originalLocaleLen = strlen(retChar);
         strcpy(originalLocale, retChar);
         origlocale_saved = 1;
         retChar = setlocale(LC_CTYPE, "");  /* set locale to local environment */
         if (retChar) {
            if (strcmp(retChar, originalLocale) != 0) {
                  localesDiffer = 1; /* original / environment locales differ */
            }
         } else {
            kxerror(E_3655, E_PRINT, "cvlwrupr");
         }
      } else {
         kxerror(E_3654, E_PRINT, "cvlwrupr");
      }
   }
   tctlocaleDiffers = kxChkTCTLocale(originalLocale);
   if (tctlocaleDiffers) {
      /* the locale set in the tct will have already been set by kxChkTCTLocale */
   } else if (localesDiffer == 1) {
      retChar = setlocale(LC_CTYPE, ""); /* set locale to local environment */
      if (retChar == NULL) {
         kxerror(E_3655, E_PRINT, "cvlwrupr");
      }
   }

   if (MB_CUR_MAX > 1) { /* MBCS */
      errno = 0;
      retInt = mblen(NULL, 0); /* this flushes the multibyte buffer,
                                   supposedly!! */
      if (retInt == -1) {
         kxerror2(E_3656, E_PRINT, "cvlwrupr 1", retInt, errno);
      }
      for (li = 0; li < plength; li++) {
         errno = 0;
         noBytes = mblen((char *)pinchar + li, MB_CUR_MAX);
         if (noBytes == 1) { /* single byte */
            /* upcase single byte lower case chars */
            if (islower(*(pinchar + li))) {
               *(pinchar + li) = toupper(*(pinchar + li));
            }
         } else if (noBytes == 2) { /* double byte */
            li++; /* skip double byte chars */
         } else if (noBytes == 0) { /* 0x00 returns 0 */
         } else {
            if (noBytes == -1) {
               kxerror2(E_3656, E_PRINT, "cvlwrupr 2", noBytes, errno);
            } else if (noBytes > 2) {
               kxerror1(E_3657, E_PRINT, "cvlwrupr", noBytes);
            }
            if (!lerr) {
               lerr = 1;
               /*
                  uncomment next line to get a dump of the internal
                  trace table for this unikixtran
               */
               /*           kxdump_trace_tables ('C', NULL);      */
               if (origlocale_saved && (tctlocaleDiffers || localesDiffer) && retChar) {
                  kxprtd("Original locale was %s, current locale is %s, MB_CUR_MAX is %d\n",
                         originalLocale, retChar, MB_CUR_MAX);
               } else if (origlocale_saved && (!localesDiffer && !tctlocaleDiffers)) {
                  kxprtd("Original locale is %s, MB_CUR_MAX is %d\n",
                         originalLocale, MB_CUR_MAX);
               } else if (!origlocale_saved && retChar) {
                  kxprtd("Current locale is %s, MB_CUR_MAX is %d\n",
                         retChar, MB_CUR_MAX);
               } else {
                  kxprtd("MB_CUR_MAX is %d\n", MB_CUR_MAX);
               }
               kxdump((char *)pinchar, plength);
            }
            kxprtd("Error at byte from offset %d\n", li);
         }
      }
   } else { /* SBCS */
      for (li = 0; li < plength; li++) {
         /* upcase single byte lower case chars */
         if (islower(*(pinchar + li))) {
            *(pinchar + li) = toupper(*(pinchar + li));
         }
      }
   }

   if ((localesDiffer == 1) || (tctlocaleDiffers == 1)) {
      retChar = setlocale(LC_CTYPE, originalLocale); /* restore locale */
      if (!retChar) {
         kxerrorstr(E_3658, E_PRINT, "cvlwrupr", originalLocale);
      }
   }
}
