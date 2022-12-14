       TITLE 'CONVERT LIFE/70 AGENT LEVEL 50 TO FREEDOM COST CENTER'    00010000
                                                                        00020000
       IDENTIFICATION DIVISION.                                         00030000
       PROGRAM-ID.    FNB162.                                           00040000
       AUTHOR         DAN DRYDEN.                                       00050004
       DATE-WRITTEN   MAY, 1998.                                        00060004
                                                                        00070004
      ***************************************************************** 00080004
      *                         H I S T O R Y                         * 00090004
      ***************************************************************** 00100004
      * NAME  DATE      DESCRIPTION                                   * 00110004
      * ----  --------  --------------------------------------------- * 00120004
      * DANA  01/01/99  CR#199801150013 - FREEDOM SYSTEM INSTALL      * 00130004
      *                                                               * 00140004
      ***************************************************************** 00150004
                                                                        00160000
       DATA DIVISION.                                                   00170000
                                                                        00180000
       WORKING-STORAGE SECTION.                                         00190000
                                                                        00200000
       01  COST-CENTER-TABLE-VALUES.                                    00210000
CID  ***   05  PIC X(19) VALUE '      16221 02 6211'.                   00220002
           05  PIC X(19) VALUE '      16224 01 4300'.                   00230000
           05  PIC X(19) VALUE '00800 14476 01 4110'.                   00240000
           05  PIC X(19) VALUE '00821 14478 01 4130'.                   00250000
           05  PIC X(19) VALUE '00824 14675 01 4172'.                   00260000
           05  PIC X(19) VALUE '00825 14670 01 4173'.                   00270000
           05  PIC X(19) VALUE '00826 14680 01 4174'.                   00280000
           05  PIC X(19) VALUE '00828 14690 01 4175'.                   00290000
           05  PIC X(19) VALUE '00840 14477 01 4120'.                   00300000
           05  PIC X(19) VALUE '00990 14826 01 4160'.                   00310000
           05  PIC X(19) VALUE '01050 14401 01 4762'.                   00320000
           05  PIC X(19) VALUE '01501 14335 01 4732'.                   00330000
           05  PIC X(19) VALUE '01623 14415 01 4731'.                   00340000
           05  PIC X(19) VALUE '01780 14420 01 4732'.                   00350000
           05  PIC X(19) VALUE '06174 14545 01 4731'.                   00360000
           05  PIC X(19) VALUE '08544 14915 01 4140'.                   00370000
           05  PIC X(19) VALUE '08545 14566 01 4743'.                   00380000
           05  PIC X(19) VALUE '08719 14515 01 4731'.                   00390000
           05  PIC X(19) VALUE '08782 14540 01 4731'.                   00400000
           05  PIC X(19) VALUE '08802 14510 01 4731'.                   00410000
           05  PIC X(19) VALUE '11800 14230 01 4732'.                   00420000
           05  PIC X(19) VALUE '14190 14220 01 4732'.                   00430000
           05  PIC X(19) VALUE '14329 14495 01 4731'.                   00440000
           05  PIC X(19) VALUE '19627 14425 01 4732'.                   00450000
           05  PIC X(19) VALUE '21286 14490 01 4731'.                   00460000
           05  PIC X(19) VALUE '22714 14405 01 4732'.                   00470000
           05  PIC X(19) VALUE '29156 14575 01 4732'.                   00480000
           05  PIC X(19) VALUE '29158 14580 01 4732'.                   00490000
           05  PIC X(19) VALUE '33405 14240 01 4732'.                   00500000
           05  PIC X(19) VALUE '33721 14210 01 4732'.                   00510000
           05  PIC X(19) VALUE '33816 14345 01 4732'.                   00520000
           05  PIC X(19) VALUE '35132 14440 01 4732'.                   00530000
           05  PIC X(19) VALUE '37367 14315 01 4732'.                   00540000
           05  PIC X(19) VALUE '37368 14320 01 4733'.                   00550000
           05  PIC X(19) VALUE '39145 14435 01 4732'.                   00560000
           05  PIC X(19) VALUE '62839 14260 01 4733'.                   00570000
           05  PIC X(19) VALUE '62842 14461 01 4762'.                   00580000
           05  PIC X(19) VALUE '62844 14463 01 4762'.                   00590000
           05  PIC X(19) VALUE '62845 14464 01 4762'.                   00600000
           05  PIC X(19) VALUE '62846 14465 01 4762'.                   00610000
           05  PIC X(19) VALUE '64535 14455 01 4731'.                   00620000
           05  PIC X(19) VALUE '64550 14485 01 4731'.                   00630000
           05  PIC X(19) VALUE '65353 14456 01 4762'.                   00640000
           05  PIC X(19) VALUE '66366 14585 01 4731'.                   00650000
           05  PIC X(19) VALUE '68191 14471 01 4731'.                   00660000
           05  PIC X(19) VALUE '75075 14469 01 4762'.                   00670000
           05  PIC X(19) VALUE '76820 14489 01 4762'.                   00680000
           05  PIC X(19) VALUE '87000 14479 01 4762'.                   00690000
           05  PIC X(19) VALUE '92000 14950 01 4761'.                   00700000
           05  PIC X(19) VALUE '92001 14951 01 4761'.                   00710000
           05  PIC X(19) VALUE '92002 14952 01 4761'.                   00720000
           05  PIC X(19) VALUE '92003 14953 01 4761'.                   00730000
           05  PIC X(19) VALUE '92004 14954 01 4761'.                   00740000
           05  PIC X(19) VALUE '92006 14956 01 4761'.                   00750000
           05  PIC X(19) VALUE '92012 14957 01 4761'.                   00760000
           05  PIC X(19) VALUE '92034 14958 01 4761'.                   00770000
           05  PIC X(19) VALUE '92045 14960 01 4761'.                   00780000
           05  PIC X(19) VALUE '92046 14961 01 4761'.                   00790000
           05  PIC X(19) VALUE '92059 14959 01 4761'.                   00800000
           05  PIC X(19) VALUE '94000 14935 01 4742'.                   00810000
                                                                        00820000
       01  REDEFINES COST-CENTER-TABLE-VALUES.                          00830000
           05  COST-CENTER-TABLE   OCCURS 59 INDEXED BY CCT-INDEX.      00840003
               10  CCT-AGT-L50     PIC X(5).                            00850000
               10  FILLER          PIC X.                               00860000
               10  CCT-MSA-CENTER  PIC X(5).                            00870000
               10  FILLER          PIC X.                               00880000
               10  CCT-DIVISION    PIC X(2).                            00890000
               10  FILLER          PIC X.                               00900000
               10  CCT-CENTER      PIC X(4).                            00910000
                                                                        00920000
                                                                        00930000
       LINKAGE SECTION.                                                 00940000
                                                                        00950000
       01  AGENT-LEVEL-50        PIC X(5).                              00960000
       01  FREEDOM-COST-CENTER   PIC X(4).                              00970000
                                                                        00980000
                                                                        00990000
           EJECT                                                        01000000
      *                                                                 01010000
       PROCEDURE DIVISION USING AGENT-LEVEL-50                          01020000
                                FREEDOM-COST-CENTER.                    01030000
      *                                                                 01040000
           SET CCT-INDEX TO +1                                          01050000
                                                                        01060000
           SEARCH COST-CENTER-TABLE                                     01070000
             AT END MOVE SPACES TO FREEDOM-COST-CENTER                  01080000
             WHEN CCT-AGT-L50 (CCT-INDEX) = AGENT-LEVEL-50              01090000
                  MOVE CCT-CENTER (CCT-INDEX) TO FREEDOM-COST-CENTER    01100000
           END-SEARCH                                                   01110000
                                                                        01120000
           GOBACK.                                                      01130000
                                                                        01140000
