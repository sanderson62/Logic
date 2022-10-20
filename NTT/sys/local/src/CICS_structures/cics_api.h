/**********************************************************************/
/*                                                                    */
/* Copyright (c) 2016-2020 NTT DATA, Inc.                             */
/* All rights reserved.                                               */
/*                                                                    */
/**********************************************************************/

#ifndef C_CICSC_H
#define C_CICSC_H
#include <string.h>
#include <ctype.h>
#include <stdio.h>
#include "cics_rvb.h"

#include <setjmp.h>

#define bak2cics return(0)
/*****************
 * setjmp buffer *
 *****************/
#define CICS_ABEND   1

jmp_buf jmp_buffer;
int          jmp_rcode;             /* return code for setjmp abend logic */
#define KX_SETJMP   {jmp_rcode = setjmp(jmp_buffer); \
      if (jmp_rcode == CICS_ABEND) return;}
#define CHK_ERR_ABT {if (_dfheigdk) longjmp(jmp_buffer,CICS_ABEND);}

#define KIX_MOV_DFHEIV0(aa)                \
   {                                          \
      char ltmp[36];                           \
      if (aa[0] & 0x80000000)  {               \
         memcpy(&ltmp[1],&aa[1],34);            \
         ltmp[0] = 'z';                         \
         ltmp[35] = '\0';                       \
         sprintf(tmpDFHEIV0,"%-35.35s",ltmp);   \
         tmpDFHEIV0[0] = aa[0];                 \
      } else {                                 \
         sprintf(tmpDFHEIV0,"%-35.35s",aa);     \
      }                                        \
      KIX_MOV_STR(_dfheiv0,tmpDFHEIV0);        \
   }
static char tmpDFHEIV0[36];
#define KIX_MOV_STR(aa,bb) movenstr(aa,sizeof(aa),(char *)bb,sizeof(bb))
#define KIX_MOV_STRLEN(aa,bb,cc) movenstr(aa,cc,bb,cc)

struct cics_eib *tcb_eib_ptr;
struct cics_eib *dfheiptr;
static char bg_end_ws[2];

typedef short ints94;           /* PIC S9(4) COMP */
typedef short int92comp;        /* PIC 9(2) COMP */
typedef short int94comp;        /* PIC 9(4) COMP */
typedef short ints94comp;       /* PIC S9(4) COMP */
typedef short ints98comp;       /* PIC S9(4) COMP */
typedef int ints94comp3;        /* PIC 9(4) COMP 3 */
typedef short int94compsync;    /* PIC 9(4) COMP SYNC */
typedef short ints94compsync;   /* PIC 9(4) COMP SYNC */
typedef int ints99compsync;     /* PIC 9(9) COMP SYNC */
typedef int ints97comp3;        /* PIC 9(7) COMP 3 */
typedef short int9;             /* PIC 9 */

/* kxdfhei1.c exported routines */
#if defined(__cplusplus)
extern "C" {
#endif
extern void     kxdfhei1 (
   char    *peiv0,
   ...
);
extern char *kxsym2func (
   char *xxx
);
#if defined(__cplusplus)
}
#endif

static struct dfheiv_st {
   char dfheiv0[35],
        dfheiv1[8],
        dfheiv2[8],
        dfheiv3[8],
        dfheiv4[6],
        dfheiv5[4],
        dfheiv6[4],
        dfheiv7[2],
        dfheiv8[2],
        dfheiv9[1];
   ints94comp3 dfheiv10;
   ints94compsync dfheiv11,
                  dfheiv12,
                  dfheiv13,
                  dfheiv14,
                  dfheiv15;
   ints99compsync dfheiv16;



   char dfheiv17[4],
        dfheiv18[4],
        dfheiv19[4];
   char *dfheiv20,
        *dfheiv21,
        *dfheiv22,
        *dfheiv23,
        *dfheiv24;
   ints99compsync dfheiv25;
   ints99compsync dfheiv26;
   ints99compsync dfheiv27;
   ints99compsync dfheiv28;
   ints99compsync dfheiv29;
   ints99compsync dfheiv30;
   ints99compsync dfheiv31;
   ints99compsync dfheiv32;
   ints94compsync dfheiv33;
   ints94compsync dfheiv34;
   ints94compsync dfheiv35;
   ints97comp3 dfheiv97;
   ints94compsync dfheiv98;
   char filler[2],
        dfheiv99[8],
        dfheivl0[48],
        dfheivl1[48],
        dfheivl2[48],
        dfheivl3[48];
} dfheiv;


#define _dfheiv0  dfheiv.dfheiv0
#define _dfheiv1  dfheiv.dfheiv1
#define _dfheiv2  dfheiv.dfheiv2
#define _dfheiv3  dfheiv.dfheiv3
#define _dfheiv4  dfheiv.dfheiv4
#define _dfheiv5  dfheiv.dfheiv5
#define _dfheiv6  dfheiv.dfheiv6
#define _dfheiv7  dfheiv.dfheiv7
#define _dfheiv8  dfheiv.dfheiv8
#define _dfheiv9  dfheiv.dfheiv9
#define _dfheiv10 dfheiv.dfheiv10
#define _dfheiv11 dfheiv.dfheiv11
#define _dfheiv12 dfheiv.dfheiv12
#define _dfheiv13 dfheiv.dfheiv13
#define _dfheiv14 dfheiv.dfheiv14
#define _dfheiv15 dfheiv.dfheiv15
#define _dfheiv16 dfheiv.dfheiv16
#define _dfheiv17 dfheiv.dfheiv17
#define _dfheiv18 dfheiv.dfheiv18
#define _dfheiv19 dfheiv.dfheiv19
#define _dfheiv20 dfheiv.dfheiv20
#define _dfheiv21 dfheiv.dfheiv21
#define _dfheiv22 dfheiv.dfheiv22
#define _dfheiv23 dfheiv.dfheiv23
#define _dfheiv24 dfheiv.dfheiv24
#define _dfheiv25 dfheiv.dfheiv25
#define _dfheiv26 dfheiv.dfheiv26
#define _dfheiv27 dfheiv.dfheiv27
#define _dfheiv28 dfheiv.dfheiv28
#define _dfheiv29 dfheiv.dfheiv29
#define _dfheiv30 dfheiv.dfheiv30
#define _dfheiv31 dfheiv.dfheiv31
#define _dfheiv32 dfheiv.dfheiv32
#define _dfheiv33 dfheiv.dfheiv33
#define _dfheiv34 dfheiv.dfheiv34
#define _dfheiv35 dfheiv.dfheiv35
#define _dfheiv97 dfheiv.dfheiv97
#define _dfheiv98 dfheiv.dfheiv98
#define _dfheiv99 dfheiv.dfheiv99
#define _dfheivl0 dfheiv.dfheivl0
#define _dfheivl1 dfheiv.dfheivl1
#define _dfheivl2 dfheiv.dfheivl2
#define _dfheivl3 dfheiv.dfheivl3

#define _eibtime    tcb_eib_ptr->eibtime
#define _eibdate    tcb_eib_ptr->eibdate
#define _eibtrnid   tcb_eib_ptr->eibtrnid
#define _eibtaskn   tcb_eib_ptr->eibtaskn
#define _eibtrmid   tcb_eib_ptr->eibtrmid
#define _dfheigdi   tcb_eib_ptr->dfheigdi
#define _eibcposn   tcb_eib_ptr->eibcposn
#define _eibcalen   tcb_eib_ptr->eibcalen
#define _eibaid     tcb_eib_ptr->eibaid
#define _filler1    tcb_eib_ptr->filler1
#define _eibfn      tcb_eib_ptr->eibfn
#define _filler2    tcb_eib_ptr->filler2
#define _eibrcode0  tcb_eib_ptr->eibrcode[0]
#define _eibrcode1  tcb_eib_ptr->eibrcode[1]
#define _eibrcode2  tcb_eib_ptr->eibrcode[2]
#define _eibrcode3  tcb_eib_ptr->eibrcode[3]
#define _eibrcode4  tcb_eib_ptr->eibrcode[4]
#define _eibrcode5  tcb_eib_ptr->eibrcode[5]
#define _eibrcode   tcb_eib_ptr->eibrcode
#define _filler3    tcb_eib_ptr->filler3
#define _eibds      tcb_eib_ptr->eibds
#define _eibreqid   tcb_eib_ptr->eibreqid
#define _eibrsrce   tcb_eib_ptr->eibrsrce
#define _eibsync    tcb_eib_ptr->eibsync
#define _eibfree    tcb_eib_ptr->eibfree
#define _eibrecv    tcb_eib_ptr->eibrecv
#define _eibsend    tcb_eib_ptr->eibsend
#define _eibatt     tcb_eib_ptr->eibatt
#define _eibeoc     tcb_eib_ptr->eibeoc
#define _eibfmh     tcb_eib_ptr->eibfmh
#define _eibcompl   tcb_eib_ptr->eibcompl
#define _eibsig     tcb_eib_ptr->eibsig
#define _eibconf    tcb_eib_ptr->eibconf
#define _eiberr     tcb_eib_ptr->eiberr
#define _eibrldbk   tcb_eib_ptr->eibrldbk
#define _eiberrcd   tcb_eib_ptr->eiberrcd
#define _eibsynrb   tcb_eib_ptr->eibsynrb
#define _eibnodat   tcb_eib_ptr->eibnodat
#define _filler5    tcb_eib_ptr->filler5
#define _eibresp    tcb_eib_ptr->eibresp
#define _eibresp2   tcb_eib_ptr->eibresp2
#define _dfheigdj   tcb_eib_ptr->dfheigdj
#define _dfheigdk   tcb_eib_ptr->dfheigdk

#ifdef PRINTTRC
#  define printtrc(aa) kxprtd("%s\n",aa)
#else
#  define printtrc(aa)
#endif

static void movenstr(char *str1, size_t len, char *str2, size_t len2)
{
   int i = 0;

   for (i = 0; i < len && i < len2 && *str2; str1++, str2++, i++) {
      *str1 = *str2;
   }

   for (; i < len; str1++, i++) {
      *str1 = ' ';
   }
}
static void cics_gobk(void)
{
   KIX_MOV_DFHEIV0("9%                    \"  N#00009999");
   kxdfhei1(_dfheiv0, _dfheiv1);
   return;
}

static void cics_dfhbak(void)
{
   if (COBHALF(_dfheigdk)) {
      cics_gobk();
   }
   return;
}

static void cics_entr(char *pgm)
{
   KIX_MOV_DFHEIV0("9#                    $  N#00000000");
   memset(_dfheiv1, ' ', sizeof(_dfheiv1));
   memcpy(_dfheiv1, pgm, strlen(pgm));
   kxdfhei1(_dfheiv0, &bg_end_ws[0], &bg_end_ws[1], _dfheiv1);
   KIX_MOV_DFHEIV0("\"\"  E                 \"  N#00000001");
   kxdfhei1(_dfheiv0, &tcb_eib_ptr);
   cics_dfhbak();
   return;
}
#endif
