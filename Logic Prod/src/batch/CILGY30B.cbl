       IDENTIFICATION DIVISION.                                         00000010
       PROGRAM-ID.              CILGY30B.                               00000020
      *AUTHOR. MICHELLE RAPAICH.                                        00000030
      *DATE-COMPILED.                                                   00000040
      *REMARKS.                                                         00000050
      ***************************************************************** 00000060
      * 3/97 - ESPA CORRECTED RECORD LENGTH IN CERT-IN FD. WAS 1080,  * 00000070
      *        S/BE 1056.                                             * 00000094
      ***************************************************************** 00000095
       ENVIRONMENT DIVISION.                                            00000096
       INPUT-OUTPUT SECTION.                                            00000097
       FILE-CONTROL.                                                    00000098
                                                                        00000099
           SELECT CERT-IN          ASSIGN TO SYS010-FBA1-SYS010.        00000100
           SELECT REP-OUT          ASSIGN TO SYS011-FBA1-SYS011.        00000110
       DATA DIVISION.                                                   00000180
       FILE SECTION.                                                    00000190
                                                                        00000191
       FD  CERT-IN                                                      00000192
           RECORDING MODE IS F                                          00000193
           LABEL RECORDS ARE STANDARD                                   00000194
           BLOCK CONTAINS 0 RECORDS                                     00000195
           RECORD CONTAINS 1056 CHARACTERS.                             00000196
      *    COPY ECSCRTY1.                                               00000197
Y2KMOD     COPY ECSCRT01.                                               00000197
       FD  REP-OUT                                                      00000205
           RECORDING MODE IS F                                          00000206
           LABEL RECORDS ARE STANDARD                                   00000207
           BLOCK CONTAINS 0 RECORDS                                     00000210
           RECORD CONTAINS 133 CHARACTERS.                              00000220
       01  REP-OUT-REC       PIC X(133).                                00000230
                                                                        00000240
                                                                           CL*15
       WORKING-STORAGE SECTION.                                         00000300
       01  PRINT-LINES.                                                 00000550
           05  PRINT-LINE1.                                             00000560
               10  FILLER              PIC X(14)     VALUE              00000580
                                 'REPORT NO.  01'.                      00000590
               10  FILLER              PIC X(32)     VALUE SPACES.      00000591
               10  FILLER              PIC X(43)     VALUE              00000592
                   'CENTRAL STATES HEALTH AND LIFE OF OMAHA'.           00000593
               10  FILLER              PIC X(24)     VALUE SPACES.      00000594
               10  PL1-DATE.                                            00000595
                   15  PL1-MM          PIC X(02).                       00000596
                   15  PL1-SLASH1      PIC X(01)     VALUE '/'.         00000597
                   15  PL1-DD          PIC X(02).                       00000598
                   15  PL1-SLASH2      PIC X(01)     VALUE '/'.         00000599
                   15  PL1-YY          PIC X(02).                       00000600
               10  FILLER              PIC X(04)     VALUE SPACES.      00000610
               10  PL1-PAGE            PIC X(04)     VALUE 'PAGE'.      00000620
               10  FILLER              PIC X(01)     VALUE SPACES.      00000630
               10  PAGE-CNT            PIC Z9.                          00000640
           05  PRINT-LINE2.                                             00000692
               10  FILLER              PIC X(55)     VALUE SPACES.      00000693
               10  FILLER              PIC X(23)     VALUE              00000694
                      'CERTIFICATE ACTIVITY ON'.                        00000695
               10  FILLER              PIC X(55)     VALUE SPACES.      00000696
           05  PRINT-LINE4.                                             00000697
               10  FILLER              PIC X(42)     VALUE SPACES.      00000698
               10  TYPE-OUT            PIC X(50).                       00000699
               10  FILLER              PIC X(41)     VALUE SPACES.      00000701
           05  PRINT-LINE5.                                             00000697
               10  FILLER              PIC X(41)    VALUE SPACES.       00000698
               10  FILLER              PIC X(08)    VALUE 'PREMIUMS'.   00000698
               10  FILLER              PIC X(52)    VALUE SPACES.       00000698
               10  FILLER              PIC X(06)     VALUE 'CLAIMS'.    00000699
               10  FILLER              PIC X(26)     VALUE SPACES.      00000701
           05  PRINT-LINE6.                                             00000719
               10  FILLER          PIC X(05)     VALUE 'STATE'.         00000721
               10  FILLER          PIC X(14)     VALUE SPACES.          00000722
               10  FILLER          PIC X(10)     VALUE 'INDIV LIFE'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE 'INDIV A&H'.     00000730
               10  FILLER          PIC X(04)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'GROUP LIFE'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE 'GROUP A&H'.     00000730
               10  FILLER          PIC X(09)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'INDIV LIFE'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE 'INDIV A&H'.     00000730
               10  FILLER          PIC X(04)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'GROUP LIFE'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE 'GROUP A&H'.     00000730
               10  FILLER          PIC X(01)     VALUE SPACES.          00000730
           05  PRINT-LINE7.                                             00000719
               10  FILLER          PIC X(05)     VALUE '-----'.         00000721
               10  FILLER          PIC X(14)     VALUE SPACES.          00000722
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE '---------'.     00000730
               10  FILLER          PIC X(04)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE '---------'.     00000730
               10  FILLER          PIC X(09)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE '---------'.     00000730
               10  FILLER          PIC X(04)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(09)     VALUE '---------'.     00000730
               10  FILLER          PIC X(01)     VALUE SPACES.          00000730
           05  PRINT-LINE8.                                             00000840
               10  FILLER              PIC X(133)     VALUE SPACES.     00000850
           05  PRINT-LINE9.                                             00000950
               10  STATE-PRINT         PIC X(15).                       00000960
               10  FILLER              PIC X(01)     VALUE SPACES.      00000960
               10  IND-LIFE-PRINT      PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  IND-AH-PRINT      PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-LIFE-PRINT      PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-AH-PRINT      PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(06)     VALUE SPACES.      00000988
               10  IND-LIFE-CLAIM      PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  IND-AH-CLAIM      PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-LIFE-CLAIM      PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-AH-CLAIM      PIC --,---,--9.99.                 00000989
               10  FILLER            PIC X(01) VALUE SPACES.            00000989
           05  PRINT-LINE10.                                            00000995
               10  FILLER              PIC X(16)     VALUE SPACES.      00000960
               10  IND-LIFE-TOT-OUT    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  IND-AH-TOT-OUT    PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-LIFE-TOT-OUT    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-AH-TOT-OUT    PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(06)     VALUE SPACES.      00000988
               10  IND-LIFE-CLM-PRT    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  IND-AH-CLM-PRT    PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-LIFE-CLM-PRT    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  GRP-AH-CLM-PRT    PIC --,---,--9.99.                 00000989
               10  FILLER            PIC X(01) VALUE SPACES.            00000989
           05  PRINT-LINE11.                                            00001014
               10  FILLER              PIC X(91)     VALUE SPACES.      00001015
               10  FILLER              PIC X(11)     VALUE              00001016
                                       'SUBTOTAL - '.                   00001017
               10  PRINT-TOT-AMT       PIC Z,ZZZ,ZZ9.99.                00001019
               10  FILLER              PIC X(19)     VALUE SPACES.      00001020
           05  PRINT-LINE12.                                            00001021
               10  FILLER              PIC X(88)     VALUE SPACES.      00001022
               10  FILLER              PIC X(14)     VALUE              00001023
                                       'GRAND TOTAL - '.                00001024
               10  PRINT-GRAND-AMT     PIC Z,ZZZ,ZZ9.99.                00001025
               10  FILLER              PIC X(19)     VALUE SPACES.      00001026
           05  PRINT-LINE13.                                            00001027
               10  FILLER              PIC X(12)     VALUE SPACES.      00001028
               10  FILLER              PIC X(10)     VALUE              00001029
                                       'TOTAL FOR '.                    00001030
               10  PRINT-COMPANY       PIC X(04).                       00001031
               10  FILLER              PIC X(03)     VALUE              00001034
                                       ' - '.                           00001035
               10  PRINT-REC-CNT       PIC ZZZ,ZZ9.                     00001036
           05  PRINT-LINE14.                                            00001021
               10  FILLER              PIC X(52)     VALUE SPACES.      00001022
               10  FILLER              PIC X(29)     VALUE              00001023
                   'TERMS GREATER THAN 120 MONTHS'.                     00001024
               10  PRINT-GRAND-AMT     PIC Z,ZZZ,ZZ9.99.                00001025
               10  FILLER              PIC X(52)     VALUE SPACES.      00001026
           05  PRINT-TOTAL.                                             00001038
               10  FILLER              PIC X(12)     VALUE SPACES.      00001039
               10  FILLER              PIC X(17)     VALUE              00001040
                                       'TOTAL RECORDS -  '.             00001041
               10  PRINT-TOT-RECS      PIC ZZZ,ZZ9.                     00001042
               10  FILLER              PIC X(97)     VALUE SPACES.      00001044
           05  PRINT-LINE15.                                            00000950
               10  STATE-ISS-PRINT     PIC X(5).                        00000960
               10  FILLER              PIC X(01)     VALUE SPACES.      00000960
               10  PRINT-ISS-CNT-IND   PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  IND-LIFE-ISS-PRINT      PIC --,---,--9.99.           00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  PRINT-ISS-CNT-GRP   PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  GRP-LIFE-ISS-PRINT      PIC --,---,--9.99.           00000989
               10  FILLER              PIC X(05)     VALUE SPACES.      00000988
               10  PRINT-CNC-CNT-IND   PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  IND-AH-ISS-PRINT      PIC --,---,--9.99.             00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  PRINT-CNC-CNT-GRP   PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  GRP-AH-ISS-PRINT      PIC --,---,--9.99.             00000989
               10  FILLER              PIC X(05)     VALUE SPACES.      00000988
               10  NET-IND-CNT         PIC ---9.                        00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  NET-IND-PRINT       PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  NET-GRP-CNT         PIC ---9.                        00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  NET-GRP-PRINT       PIC --,---,--9.99.               00000989
           05  PRINT-LINE16.                                            00000697
               10  FILLER              PIC X(23)    VALUE SPACES.       00000698
               10  FILLER              PIC X(06)    VALUE 'ISSUED'.     00000698
               10  FILLER              PIC X(36)    VALUE SPACES.       00000698
               10  FILLER              PIC X(09)     VALUE 'CANCELLED'. 00000699
               10  FILLER              PIC X(39)     VALUE SPACES.      00000701
               10  FILLER              PIC X(03)     VALUE 'NET'.       00000699
               10  FILLER              PIC X(17)     VALUE SPACES.      00000701
           05  PRINT-LINE17.                                            00000719
               10  FILLER          PIC X(05)     VALUE 'STATE'.         00000721
               10  FILLER          PIC X(02)     VALUE SPACES.          00000722
               10  FILLER          PIC X(03)     VALUE 'CNT'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'INDIV LIFE'.    00000730
               10  FILLER          PIC X(02)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE 'CNT'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'GROUP LIFE'.    00000730
               10  FILLER          PIC X(06)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE 'CNT'.           00000740
               10  FILLER          PIC X(05)     VALUE SPACES.          00000730
               10  FILLER          PIC X(10)     VALUE 'INDIV LIFE'.    00000730
               10  FILLER          PIC X(02)     VALUE SPACES.          00000730
               10  FILLER          PIC X(03)     VALUE 'CNT'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'GROUP LIFE'.    00000730
               10  FILLER          PIC X(06)     VALUE SPACES.          00000730
               10  FILLER          PIC X(03)     VALUE 'CNT'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'INDIV LIFE'.    00000730
               10  FILLER          PIC X(02)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE 'CNT'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE 'GROUP LIFE'.    00000730
           05  PRINT-LINE18.                                            00000719
               10  FILLER          PIC X(05)     VALUE '-----'.         00000721
               10  FILLER          PIC X(02)     VALUE SPACES.          00000722
               10  FILLER          PIC X(03)     VALUE '---'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(02)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE '---'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(06)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE '---'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(02)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE '---'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(06)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE '---'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
               10  FILLER          PIC X(02)     VALUE SPACES.          00000740
               10  FILLER          PIC X(03)     VALUE '---'.           00000730
               10  FILLER          PIC X(05)     VALUE SPACES.          00000740
               10  FILLER          PIC X(10)     VALUE '----------'.    00000730
           05  PRINT-LINE19.                                            00000950
               10  FILLER              PIC X(06)     VALUE SPACES.      00000960
               10  IND-LIFE-TOT-PRT        PIC --,---,--9.99.           00000989
               10  FILLER              PIC X(15)     VALUE SPACES.      00000988
               10  GRP-LIFE-TOT-PRT        PIC --,---,--9.99.           00000989
               10  FILLER              PIC X(20)     VALUE SPACES.      00000988
               10  IND-AH-TOT-PRT        PIC --,---,--9.99.             00000989
               10  FILLER              PIC X(15)     VALUE SPACES.      00000988
               10  GRP-AH-TOT-PRT        PIC --,---,--9.99.             00000989
               10  FILLER              PIC X(15)     VALUE SPACES.      00000988
           05  PRINT-LINE20.                                            00000995
               10  FILLER              PIC X(06)     VALUE SPACES.      00000960
               10  ISS-CNT-IND         PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000960
               10  IND-LIFE-TOT-ISS    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000960
               10  ISS-CNT-GRP         PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  GRP-LIFE-TOT-ISS    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(05)     VALUE SPACES.      00000988
               10  CNC-CNT-IND         PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  IND-AH-TOT-ISS    PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  CNC-CNT-GRP         PIC ---9.                        00000988
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  GRP-AH-TOT-ISS    PIC --,---,--9.99.                 00000989
               10  FILLER              PIC X(05)     VALUE SPACES.      00000988
               10  NET-IND-ISS-CNT     PIC ---9.                        00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  NET-IND             PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  NET-GRP-ISS-CNT     PIC ---9.                        00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  NET-GRP             PIC --,---,--9.99.               00000989
           05  PRINT-LINE21.                                            00000995
               10  FILLER              PIC X(16)     VALUE SPACES.      00000960
               10  ALL-IND-LIFE-ISS    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(15)     VALUE SPACES.      00000988
               10  ALL-GRP-LIFE-ISS    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(20)     VALUE SPACES.      00000988
               10  ALL-IND-LIFE-CNC    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(15)     VALUE SPACES.      00000988
               10  ALL-IND-LIFE-CNC    PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(15)     VALUE SPACES.      00000988
           05  PRINT-LINE22.                                            00000950
               10  FILLER              PIC X(06)     VALUE SPACES.      00000960
               10  ISS-CNT-IND-ALL       PIC ---9.                      00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  IND-LIFE-ISS-PRT      PIC --,---,--9.99.             00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  ISS-CNT-GRP-ALL       PIC ---9.                      00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  GRP-LIFE-ISS-PRT      PIC --,---,--9.99.             00000989
               10  FILLER              PIC X(05)     VALUE SPACES.      00000988
               10  CNC-CNT-IND-ALL       PIC ---9.                      00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  IND-AH-ISS-PRT      PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  CNC-CNT-GRP-ALL       PIC ---9.                      00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  GRP-AH-ISS-PRT      PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(05)     VALUE SPACES.      00000988
               10  NET-IND-CNT-OUT       PIC ---9.                      00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  NET-IND-OUT         PIC --,---,--9.99.               00000989
               10  FILLER              PIC X(01)     VALUE SPACES.      00000988
               10  NET-GRP-CNT-OUT       PIC ---9.                      00000989
               10  FILLER              PIC X(02)     VALUE SPACES.      00000988
               10  NET-GRP-OUT         PIC --,---,--9.99.               00000989
                                                                        00000307
Y2KMOD 01  WS-DATES.                                                    00000307
Y2KMOD     05  WS-CR-DT                PIC 9(11).                       00000307
Y2KMOD     05  WS-CR-DT-R  REDEFINES WS-CR-DT.                          00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-CR-CN            PIC 99.                          00000307
Y2KMOD         10  WS-CR-YR            PIC 99.                          00000307
Y2KMOD         10  WS-CR-MO            PIC 99.                          00000307
Y2KMOD         10  WS-CR-DA            PIC 99.                          00000307
Y2KMOD     05  WS-CR-DT-X  REDEFINES WS-CR-DT.                          00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-CR-CNYR          PIC 9999.                        00000307
Y2KMOD         10  FILLER              PIC 9999.                        00000307
Y2KMOD                                                                  00000307
Y2KMOD     05  WS-ENTRY-DATE           PIC 9(11).                       00000307
Y2KMOD     05  WS-ENTRY-DATE-R  REDEFINES WS-ENTRY-DATE.                00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-ENTRY-CN         PIC 99.                          00000307
Y2KMOD         10  WS-ENTRY-YR         PIC 99.                          00000307
Y2KMOD         10  WS-ENTRY-MO         PIC 99.                          00000307
Y2KMOD         10  WS-ENTRY-DA         PIC 99.                          00000307
Y2KMOD                                                                  00000307
Y2KMOD     05  WS-LF-CANC-DT           PIC 9(11).                       00000307
Y2KMOD     05  WS-LF-CANC-DT-R  REDEFINES WS-LF-CANC-DT.                00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-LF-CNC-CN        PIC 99.                          00000307
Y2KMOD         10  WS-LF-CNC-YR        PIC 99.                          00000307
Y2KMOD         10  WS-LF-CNC-MO        PIC 99.                          00000307
Y2KMOD         10  WS-LF-CNC-DA        PIC 99.                          00000307
Y2KMOD     05  WS-LF-CANC-DT-X  REDEFINES WS-LF-CANC-DT.                00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-LF-CNC-CNYR      PIC 9999.                        00000307
Y2KMOD         10  FILLER              PIC 9999.                        00000307
Y2KMOD                                                                  00000307
Y2KMOD     05  WS-LF-CANCEL-EXIT-DT    PIC 9(11).                       00000307
Y2KMOD     05  WS-LF-CANCEL-EXIT-DT-R  REDEFINES                        00000307
Y2KMOD         WS-LF-CANCEL-EXIT-DT.                                    00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-LF-CEX-CN        PIC 99.                          00000307
Y2KMOD         10  WS-LF-CEX-YR        PIC 99.                          00000307
Y2KMOD         10  WS-LF-CEX-MO        PIC 99.                          00000307
Y2KMOD         10  WS-LF-CEX-DA        PIC 99.                          00000307
Y2KMOD                                                                  00000307
Y2KMOD     05  WS-AH-CANC-DT           PIC 9(11).                       00000307
Y2KMOD     05  WS-AH-CANC-DT-R  REDEFINES WS-AH-CANC-DT.                00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-AH-CNC-CN        PIC 99.                          00000307
Y2KMOD         10  WS-AH-CNC-YR        PIC 99.                          00000307
Y2KMOD         10  WS-AH-CNC-MO        PIC 99.                          00000307
Y2KMOD         10  WS-AH-CNC-DA        PIC 99.                          00000307
Y2KMOD                                                                  00000307
Y2KMOD     05  WS-AH-CANCEL-EXIT-DT    PIC 9(11).                       00000307
Y2KMOD     05  WS-AH-CANCEL-EXIT-DT-R  REDEFINES                        00000307
Y2KMOD         WS-AH-CANCEL-EXIT-DT.                                    00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-AH-CEX-CN        PIC 99.                          00000307
Y2KMOD         10  WS-AH-CEX-YR        PIC 99.                          00000307
Y2KMOD         10  WS-AH-CEX-MO        PIC 99.                          00000307
Y2KMOD         10  WS-AH-CEX-DA        PIC 99.                          00000307
Y2KMOD                                                                  00000307
Y2KMOD     05  WS-DIS-PAY-DT           PIC 9(11).                       00000307
Y2KMOD     05  WS-DIS-PAY-DT-R  REDEFINES WS-DIS-PAY-DT.                00000307
Y2KMOD         10  FILLER              PIC 999.                         00000307
Y2KMOD         10  WS-DIS-PAY-CN       PIC 99.                          00000307
Y2KMOD         10  WS-DIS-PAY-YR       PIC 99.                          00000307
Y2KMOD         10  WS-DIS-PAY-MO       PIC 99.                          00000307
Y2KMOD         10  WS-DIS-PAY-DA       PIC 99.                          00000307
Y2KMOD                                                                  00000307
                                                                        00000307
       01  WS-GENERAL-STORAGE.                                          00000307
           05 WK-STATE                 PIC X(15).                       00000310
           05 PREV-TYPE                PIC X(50).                       00000310
           05 WS-EOF                   PIC X(01)     VALUE 'N'.         00000310
           05 WK-TERM                  PIC S9(05)    VALUE ZEROS.       00000310
           05 TERM-WORK                PIC S9(05)    VALUE ZEROS.       00000310
           05 WS-DATE.                                                  00000320
              10 WS-MM                 PIC X(02).                       00000330
              10 WS-DD                 PIC X(02).                       00000340
              10 WS-YY                 PIC X(02).                       00000350
           05 SUB1                     PIC 99 VALUE ZERO.               00000351
           05 SUB2                     PIC 99 VALUE ZERO.               00000352
           05 SUB3                     PIC 99 VALUE ZERO.               00000352
           05 PAGE-NO                  PIC 999 VALUE 1.                 00000353
           05 FIRST-TIME               PIC X VALUE 'Y'.                 00000354
           05 FIRST-REC                PIC X VALUE 'Y'.                 00000354
           05 DONE                     PIC X VALUE 'N'.                 00000354
           05 LINE-CT                  PIC 99 VALUE ZERO.               00000356
           05 WK-AMT                   PIC S9(11)V99 COMP-3.            00000357
           05 IND-LIFE-TOT   PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 IND-AH-TOT     PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 GRP-LIFE-TOT   PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 GRP-AH-TOT     PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 IND-LIFE-CLM-OUT    PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 CANCEL-AMT          PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 IND-AH-CLM-OUT      PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 GRP-LIFE-CLM-OUT    PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 GRP-AH-CLM-OUT      PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 DOLLAR-AMT          PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 TEX-FACT-1          PIC S9(07)V99 COMP-3 VALUE ZEROS.     00000357
           05 TEX-FACT-2          PIC S9(03)    COMP-3 VALUE ZEROS.     00000357
           05 TEX-FACT-3          PIC S9(03)    COMP-3 VALUE ZEROS.     00000357
           05 CALC-TOTAL          PIC X         VALUE 'N'.              00000357
           05 END-OF-RPT          PIC X         VALUE 'N'.              00000357
           05 IND-LIFE-ALL   PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 IND-AH-ALL     PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 GRP-LIFE-ALL   PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 GRP-AH-ALL     PIC S9(11)V99 COMP-3 VALUE ZEROS.          00000357
           05 IND-LIFE-CLM-ALL    PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 IND-AH-CLM-ALL      PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 GRP-LIFE-CLM-ALL    PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 GRP-AH-CLM-ALL      PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 TOT-LIFE-IND        PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 TOT-AH-GRP          PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 TOT-LIFE-GRP        PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 TOT-AH-IND          PIC S9(11)V99 COMP-3 VALUE ZEROS.     00000357
           05 ALL-CNT-ISS-IND     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
           05 ALL-CNT-CNC-IND     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
           05 ALL-CNT-ISS-GRP     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
           05 ALL-CNT-CNC-GRP     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
           05 TOT-ISS-IND-CNT     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
           05 TOT-ISS-GRP-CNT     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
           05 TOT-CNC-IND-CNT     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
           05 TOT-CNC-GRP-CNT     PIC S9(09) COMP-3 VALUE ZEROS.        00000357
                                                                        00000357
       01  CID-ET-ARRAY.                                                00000393
           05 ET-STATE OCCURS 54 TIMES.                                 00000394
              10 ET-AMT OCCURS 24 TIMES PIC S9(11)V99 COMP-3.           00000395
              10 ET-NAME PIC X(15).                                     00000392
              10 ET-CLM OCCURS 12 TIMES PIC S9(11)V99 COMP-3.           00000395
              10 ET-CNT OCCURS 6 TIMES PIC S9(09) COMP-3.               00000395
              10 ET-CNC-CNT OCCURS 6 TIMES PIC S9(09) COMP-3.           00000395
       01  CID-ARRAY.                                                   00000393
           05 ET-STATE-IN OCCURS 54 TIMES.                              00000394
              10 ET-ISS-CNC OCCURS 24 TIMES PIC S9(11)V99 COMP-3.       00000395
              10 ET-NAME-IN PIC X(15).                                  00000392
                                                                        00000540
       01  CID-ET-OUT-ARRAY.                                            00000393
           05 ET-OUT-STATE OCCURS 54 TIMES.                             00000394
              10 ET-OUT-AMT OCCURS 4 TIMES PIC S9(11)V99 COMP-3.        00000395
              10 ET-OUT-NAME PIC X(15).                                 00000392
       01  CID-OT-OUT-ARRAY.                                            00000393
           05 OT-OUT-STATE OCCURS 54 TIMES.                             00000394
              10 OT-OUT-AMT OCCURS 4 TIMES PIC S9(11)V99 COMP-3.        00000395
              10 OT-OUT-NAME PIC X(15).                                 00000392
       01  CID-QA-OUT-ARRAY.                                            00000393
           05 QA-OUT-STATE OCCURS 54 TIMES.                             00000394
              10 QA-OUT-AMT OCCURS 4 TIMES PIC S9(11)V99 COMP-3.        00000395
              10 QA-OUT-NAME PIC X(15).                                 00000392
       01  CID-ALL-OUT-ARRAY.                                           00000393
           05 ALL-OUT-STATE OCCURS 54 TIMES.                            00000394
              10 ALL-OUT-AMT OCCURS 4 TIMES PIC S9(11)V99 COMP-3.       00000395
              10 ALL-OUT-NAME PIC X(15).                                00000392
              10 ALL-CLM OCCURS 12 TIMES PIC S9(11)V99 COMP-3.          00000395
       01  CID-ET-OUT-ISS-CNC.                                          00000393
           05 ET-OUT-ISS-STATE OCCURS 54 TIMES.                         00000394
              10 ET-OUT-ISS-CNC OCCURS 4 TIMES PIC S9(11)V99 COMP-3.    00000395
              10 ET-OUT-ISS-NAME PIC X(15).                             00000392
       01  CID-OT-OUT-ISS-CNC.                                          00000393
           05 OT-OUT-ISS-STATE OCCURS 54 TIMES.                         00000394
              10 OT-OUT-ISS-CNC OCCURS 4 TIMES PIC S9(11)V99 COMP-3.    00000395
              10 OT-OUT-ISS-NAME PIC X(15).                             00000392
       01  CID-QA-OUT-ISS-CNC.                                          00000393
           05 QA-OUT-ISS-STATE OCCURS 54 TIMES.                         00000394
              10 QA-OUT-ISS-CNC OCCURS 4 TIMES PIC S9(11)V99 COMP-3.    00000395
              10 QA-OUT-ISS-NAME PIC X(15).                             00000392
       01  CID-ALL-ISS-ARRAY.                                           00000393
           05 ALL-ISS-STATE OCCURS 54 TIMES.                            00000394
              10 ALL-ISS-AMT OCCURS 4 TIMES PIC S9(11)V99 COMP-3.       00000395
              10 ALL-ISS-NAME PIC X(15).                                00000392
       EJECT                                                               CL*15
       COPY ELCDATE.                                                       CL*15
       EJECT                                                               CL*15
      *COPY ZLCCALC.                                                       CL*15
Y2KMOD COPY ELCCALC.                                                       CL*15
                                                                        00001090
       PROCEDURE DIVISION.                                              00001091
                                                                        00001092
       A000-MAINLINE.                                                   00001093
           PERFORM B000-OPEN-FILES THRU B000-EXIT.                      00001094
           ACCEPT WS-DATE FROM SYSIN.                                   00001095
           PERFORM B010-PROCESSING THRU B010-EXIT                       00001097
               UNTIL WS-EOF = 'Y'.                                      00001098
           PERFORM B030-EOJ THRU B030-EXIT.                             00001099
           PERFORM B020-CLOSE-FILES THRU B020-EXIT.                     00001100
           GOBACK.                                                      00001110
       A000-EXIT.                                                       00001120
           EXIT.                                                        00001130
                                                                        00001140
       B000-OPEN-FILES.                                                 00001150
           OPEN INPUT CERT-IN.                                          00001160
           OPEN OUTPUT REP-OUT.                                         00001170
       B000-EXIT.                                                       00001206
           EXIT.                                                        00001207
                                                                        00001208
       B010-PROCESSING.                                                 00001210
           READ CERT-IN                                                 00001222
             AT END MOVE 'Y' TO WS-EOF.                                 00001230
           IF WS-EOF = 'Y'                                              00001231
             GO TO B010-EXIT.                                           00001232
           PERFORM B011-PROCESSING                                      00001233
             UNTIL SUB1 = 54.                                           00001234
Y2KMOD     PERFORM B011A-SET-WS-DATES.
           IF (CR-LF-TERM > 120) OR                                     00001237
              (CR-AH-TERM > 120)                                        00001238
              PERFORM B012-ASSIGN-VALUE.                                00001239
       B010-EXIT.                                                       00001439
           EXIT.                                                        00001440
                                                                        00001441
       B011-PROCESSING.                                                 00001442
              COMPUTE SUB1 = SUB1 + 1.                                  00001443
              MOVE ZEROS TO ET-AMT(SUB1, 1).                            00001445
              MOVE ZEROS TO ET-AMT(SUB1, 2).                            00001446
              MOVE ZEROS TO ET-AMT(SUB1, 3).                            00001447
              MOVE ZEROS TO ET-AMT(SUB1, 4).                            00001448
              MOVE ZEROS TO ET-AMT(SUB1, 5).                            00001449
              MOVE ZEROS TO ET-AMT(SUB1, 6).                            00001450
              MOVE ZEROS TO ET-AMT(SUB1, 7).                            00001451
              MOVE ZEROS TO ET-AMT(SUB1, 8).                            00001452
              MOVE ZEROS TO ET-AMT(SUB1, 9).                            00001452
              MOVE ZEROS TO ET-AMT(SUB1, 10).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 11).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 12).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 13).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 14).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 15).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 16).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 17).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 18).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 19).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 20).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 21).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 22).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 23).                           00001452
              MOVE ZEROS TO ET-AMT(SUB1, 24).                           00001452
              MOVE ZEROS TO ET-CLM(SUB1, 1).                            00001445
              MOVE ZEROS TO ET-CLM(SUB1, 2).                            00001446
              MOVE ZEROS TO ET-CLM(SUB1, 3).                            00001447
              MOVE ZEROS TO ET-CLM(SUB1, 4).                            00001448
              MOVE ZEROS TO ET-CLM(SUB1, 5).                            00001449
              MOVE ZEROS TO ET-CLM(SUB1, 6).                            00001450
              MOVE ZEROS TO ET-CLM(SUB1, 7).                            00001451
              MOVE ZEROS TO ET-CLM(SUB1, 8).                            00001452
              MOVE ZEROS TO ET-CLM(SUB1, 9).                            00001452
              MOVE ZEROS TO ET-CLM(SUB1, 10).                           00001452
              MOVE ZEROS TO ET-CLM(SUB1, 11).                           00001452
              MOVE ZEROS TO ET-CLM(SUB1, 12).                           00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 1).                        00001445
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 2).                        00001446
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 3).                        00001447
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 4).                        00001448
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 5).                        00001449
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 6).                        00001449
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 7).                        00001449
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 8).                        00001449
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 9).                        00001449
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 10).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 11).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 12).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 13).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 14).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 15).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 16).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 17).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 18).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 19).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 20).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 21).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 22).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 23).                       00001452
              MOVE ZEROS TO ET-ISS-CNC(SUB1, 24).                       00001452
              MOVE ZEROS TO ET-CNT(SUB1, 1).                            00001452
              MOVE ZEROS TO ET-CNT(SUB1, 2).                            00001452
              MOVE ZEROS TO ET-CNT(SUB1, 3).                            00001452
              MOVE ZEROS TO ET-CNT(SUB1, 4).                            00001452
              MOVE ZEROS TO ET-CNT(SUB1, 5).                            00001452
              MOVE ZEROS TO ET-CNT(SUB1, 6).                            00001452
              MOVE ZEROS TO ET-CNC-CNT(SUB1, 1).                        00001452
              MOVE ZEROS TO ET-CNC-CNT(SUB1, 2).                        00001452
              MOVE ZEROS TO ET-CNC-CNT(SUB1, 3).                        00001452
              MOVE ZEROS TO ET-CNC-CNT(SUB1, 4).                        00001452
              MOVE ZEROS TO ET-CNC-CNT(SUB1, 5).                        00001452
              MOVE ZEROS TO ET-CNC-CNT(SUB1, 6).                        00001452
       B011-EXIT.                                                       00001478
           EXIT.                                                        00001479
                                                                        00001480
Y2KMOD B011A-SET-WS-DATES.                                              00001480
Y2KMOD     MOVE CR-DT TO WS-CR-DT.                                      00001480
Y2KMOD     MOVE CR-ENTRY-DATE TO WS-ENTRY-DATE.                         00001480
Y2KMOD     MOVE CR-LF-CANC-DT TO WS-LF-CANC-DT.                         00001480
Y2KMOD     MOVE CR-LF-CANCEL-EXIT-DATE TO WS-LF-CANCEL-EXIT-DT.         00001480
Y2KMOD     MOVE CR-AH-CANC-DT TO WS-AH-CANC-DT.                         00001480
Y2KMOD     MOVE CR-AH-CANCEL-EXIT-DATE TO WS-AH-CANCEL-EXIT-DT.         00001480
Y2KMOD     MOVE CR-DIS-PAY-DT TO WS-DIS-PAY-DT.                         00001480
Y2KMOD                                                                  00001480
Y2KMOD B011A-EXIT.                                                      00001480
Y2KMOD     EXIT.                                                        00001480
Y2KMOD                                                                  00001480
       B012-ASSIGN-VALUE.                                               00001481
           MOVE 'N' TO CALC-TOTAL.                                      00001482
           IF CR-STATE = 'AA'                                           00001482
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 01 TO SUB1.                                          00001483
           IF CR-STATE = 'AE'                                           00001484
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 02 TO SUB1.                                          00001485
           IF CR-STATE = 'AK'                                           00001486
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 03 TO SUB1.                                          00001487
           IF CR-STATE = 'AL'                                           00001488
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 04 TO SUB1.                                          00001489
           IF CR-STATE = 'AP'                                           00001490
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 05 TO SUB1.                                          00001491
           IF CR-STATE = 'AR'                                           00001492
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 06 TO SUB1.                                          00001493
           IF CR-STATE = 'AZ'                                           00001494
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 07 TO SUB1.                                          00001495
           IF CR-STATE = 'CA'                                           00001496
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 08 TO SUB1.                                          00001497
           IF CR-STATE = 'CO'                                           00001498
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 09 TO SUB1.                                          00001499
           IF CR-STATE = 'CT'                                           00001500
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 10 TO SUB1.                                          00001501
           IF CR-STATE = 'DC'                                           00001502
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 11 TO SUB1.                                          00001503
           IF CR-STATE = 'DE'                                           00001504
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 12 TO SUB1.                                          00001505
           IF CR-STATE = 'FL'                                           00001506
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 13 TO SUB1.                                          00001507
           IF CR-STATE = 'GA'                                           00001508
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 14 TO SUB1.                                          00001509
           IF CR-STATE = 'HI'                                           00001510
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 15 TO SUB1.                                          00001511
           IF CR-STATE = 'IA'                                           00001512
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 16 TO SUB1.                                          00001513
           IF CR-STATE = 'ID'                                           00001514
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 17 TO SUB1.                                          00001515
           IF CR-STATE = 'IL'                                           00001516
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 18 TO SUB1.                                          00001517
           IF CR-STATE = 'IN'                                           00001518
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 19 TO SUB1.                                          00001519
           IF CR-STATE = 'KS'                                           00001520
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 20 TO SUB1.                                          00001521
           IF CR-STATE = 'KY'                                           00001522
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 21 TO SUB1.                                          00001523
           IF CR-STATE = 'LA'                                           00001524
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 22 TO SUB1.                                          00001525
           IF CR-STATE = 'MA'                                           00001526
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 23 TO SUB1.                                          00001527
           IF CR-STATE = 'MD'                                           00001528
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 24 TO SUB1.                                          00001529
           IF CR-STATE = 'ME'                                           00001530
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 25 TO SUB1.                                          00001531
           IF CR-STATE = 'MI'                                           00001532
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 26 TO SUB1.                                          00001533
           IF CR-STATE = 'MN'                                           00001534
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 27 TO SUB1.                                          00001535
           IF CR-STATE = 'MO'                                           00001536
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 28 TO SUB1.                                          00001537
           IF CR-STATE = 'MS'                                           00001538
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 29 TO SUB1.                                          00001539
           IF CR-STATE = 'MT'                                           00001540
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 30 TO SUB1.                                          00001541
           IF CR-STATE = 'NC'                                           00001542
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 31 TO SUB1.                                          00001543
           IF CR-STATE = 'ND'                                           00001544
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 32 TO SUB1.                                          00001545
           IF CR-STATE = 'NE'                                           00001546
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 33 TO SUB1.                                          00001547
           IF CR-STATE = 'NH'                                           00001548
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 34 TO SUB1.                                          00001549
           IF CR-STATE = 'NJ'                                           00001550
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 35 TO SUB1.                                          00001551
           IF CR-STATE = 'NM'                                           00001552
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 36 TO SUB1.                                          00001553
           IF CR-STATE = 'NV'                                           00001554
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 37 TO SUB1.                                          00001555
           IF CR-STATE = 'NY'                                           00001556
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 38 TO SUB1.                                          00001557
           IF CR-STATE = 'OH'                                           00001558
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 39 TO SUB1.                                          00001559
           IF CR-STATE = 'OK'                                           00001560
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 40 TO SUB1.                                          00001561
           IF CR-STATE = 'OR'                                           00001562
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 41 TO SUB1.                                          00001563
           IF CR-STATE = 'PA'                                           00001564
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 42 TO SUB1.                                          00001565
           IF CR-STATE = 'RI'                                           00001566
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 43 TO SUB1.                                          00001567
           IF CR-STATE = 'SC'                                           00001568
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 44 TO SUB1.                                          00001569
           IF CR-STATE = 'SD'                                           00001570
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 45 TO SUB1.                                          00001571
           IF CR-STATE = 'TN'                                           00001572
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 46 TO SUB1.                                          00001573
           IF CR-STATE = 'TX'                                           00001574
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 47 TO SUB1.                                          00001575
           IF CR-STATE = 'UT'                                           00001576
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 48 TO SUB1.                                          00001577
           IF CR-STATE = 'VA'                                           00001578
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 49 TO SUB1.                                          00001579
           IF CR-STATE = 'VT'                                           00001580
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 50 TO SUB1.                                          00001581
           IF CR-STATE = 'WA'                                           00001582
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 51 TO SUB1.                                          00001583
           IF CR-STATE = 'WI'                                           00001584
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 52 TO SUB1.                                          00001585
           IF CR-STATE = 'WV'                                           00001586
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 53 TO SUB1.                                          00001587
           IF CR-STATE = 'WY'                                           00001588
              MOVE CR-STATE TO WK-STATE                                 00001483
              MOVE 54 TO SUB1.                                          00001589
           IF SUB1 NOT EQUAL TO ZERO                                    00001590
              PERFORM B048-CHECK-LIFE.                                  00001591
       B012-EXIT.                                                       00001592
           EXIT.                                                        00001600
                                                                        00001611
       B020-CLOSE-FILES.                                                00001612
           CLOSE CERT-IN.                                               00001614
           CLOSE REP-OUT.                                               00001615
       B020-EXIT.                                                       00001624
           EXIT.                                                        00001625
                                                                        00001625
       B022-PRINT-TOTAL.                                                00001612
           MOVE IND-LIFE-TOT TO IND-LIFE-TOT-OUT.                       00001669
           MOVE GRP-LIFE-TOT TO GRP-LIFE-TOT-OUT.                       00001669
           MOVE IND-AH-TOT TO IND-AH-TOT-OUT.                           00001669
           MOVE GRP-AH-TOT TO GRP-AH-TOT-OUT.                           00001669
           MOVE IND-LIFE-CLM-OUT TO IND-LIFE-CLM-PRT.                   00001669
           MOVE GRP-LIFE-CLM-OUT TO GRP-LIFE-CLM-PRT.                   00001669
           MOVE IND-AH-CLM-OUT TO IND-AH-CLM-PRT.                       00001669
           MOVE GRP-AH-CLM-OUT TO GRP-AH-CLM-PRT.                       00001669
           WRITE REP-OUT-REC FROM PRINT-LINE10                          001397
                                              AFTER ADVANCING 3 LINES.  001397
           MOVE ZEROS TO IND-LIFE-TOT.                                  00001399
           MOVE ZEROS TO GRP-LIFE-TOT.                                  00001399
           MOVE ZEROS TO GRP-AH-TOT.                                    00001399
           MOVE ZEROS TO IND-AH-TOT.                                    00001399
           MOVE ZEROS TO IND-LIFE-CLM-OUT.                              00001669
           MOVE ZEROS TO GRP-LIFE-CLM-OUT.                              00001669
           MOVE ZEROS TO IND-AH-CLM-OUT.                                00001669
           MOVE ZEROS TO GRP-AH-CLM-OUT.                                00001669
       B022-EXIT.                                                       00001624
           EXIT.                                                        00001625
                                                                        00001370
       B023-PRINT-HEADINGS.                                             00001370
           MOVE PAGE-NO TO PAGE-CNT.                                    00001381
           MOVE 'N' TO FIRST-TIME.                                      00001382
           ADD 1 TO PAGE-NO.                                            00001390
           MOVE WS-MM  TO  PL1-MM.                                      00001391
           MOVE WS-DD  TO  PL1-DD.                                      00001392
           MOVE WS-YY  TO  PL1-YY.                                      00001393
           WRITE REP-OUT-REC FROM PRINT-LINE1 AFTER ADVANCING PAGE.     001397
           WRITE REP-OUT-REC FROM PRINT-LINE2 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE4 AFTER ADVANCING 1 LINE.   00001399
           MOVE TYPE-OUT TO PREV-TYPE.                                  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE14 AFTER ADVANCING 1 LINE.  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE8 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE5 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE6 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE7 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           MOVE 0 TO LINE-CT.                                           00001410
       B023-EXIT.                                                       00001420
           EXIT.                                                        00001430
                                                                        00001440
       B024-PRINT-HEADINGS.                                             00001370
           MOVE PAGE-NO TO PAGE-CNT.                                    00001381
           MOVE 'N' TO FIRST-TIME.                                      00001382
           ADD 1 TO PAGE-NO.                                            00001390
           MOVE WS-MM  TO  PL1-MM.                                      00001391
           MOVE WS-DD  TO  PL1-DD.                                      00001392
           MOVE WS-YY  TO  PL1-YY.                                      00001393
           WRITE REP-OUT-REC FROM PRINT-LINE1 AFTER ADVANCING PAGE.     001397
           WRITE REP-OUT-REC FROM PRINT-LINE2 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE4 AFTER ADVANCING 1 LINE.   00001399
           MOVE TYPE-OUT TO PREV-TYPE.                                  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE14 AFTER ADVANCING 1 LINE.  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE8 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE16 AFTER ADVANCING 1 LINE.  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE17 AFTER ADVANCING 1 LINE.  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE18 AFTER ADVANCING 1 LINE.  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           MOVE 0 TO LINE-CT.                                           00001410
       B024-EXIT.                                                       00001420
           EXIT.                                                        00001430
                                                                        00001430
       B025-PRINT-TOTAL.                                                00001612
           MOVE IND-LIFE-ALL TO IND-LIFE-TOT-OUT.                       00001669
           MOVE GRP-LIFE-ALL TO GRP-LIFE-TOT-OUT.                       00001669
           MOVE IND-AH-ALL TO IND-AH-TOT-OUT.                           00001669
           MOVE GRP-AH-ALL TO GRP-AH-TOT-OUT.                           00001669
           MOVE IND-LIFE-CLM-ALL TO IND-LIFE-CLM-PRT.                   00001669
           MOVE GRP-LIFE-CLM-ALL TO GRP-LIFE-CLM-PRT.                   00001669
           MOVE IND-AH-CLM-ALL TO IND-AH-CLM-PRT.                       00001669
           MOVE GRP-AH-CLM-ALL TO GRP-AH-CLM-PRT.                       00001669
           WRITE REP-OUT-REC FROM PRINT-LINE10                          001397
                                              AFTER ADVANCING 3 LINES.  001397
           MOVE ZEROS TO IND-LIFE-TOT-PRT.                              00001399
           MOVE ZEROS TO GRP-LIFE-TOT-PRT.                              00001399
           MOVE ZEROS TO GRP-AH-TOT-PRT.                                00001399
           MOVE ZEROS TO IND-AH-TOT-PRT.                                00001399
           MOVE ZEROS TO IND-LIFE-CLM-OUT.                              00001669
           MOVE ZEROS TO GRP-LIFE-CLM-OUT.                              00001669
           MOVE ZEROS TO IND-AH-CLM-OUT.                                00001669
           MOVE ZEROS TO GRP-AH-CLM-OUT.                                00001669
           MOVE ZEROS TO IND-LIFE-ALL.                                  00001399
           MOVE ZEROS TO GRP-LIFE-ALL.                                  00001399
           MOVE ZEROS TO GRP-AH-ALL.                                    00001399
           MOVE ZEROS TO IND-AH-ALL.                                    00001399
           MOVE ZEROS TO IND-LIFE-CLM-ALL.                              00001669
           MOVE ZEROS TO GRP-LIFE-CLM-ALL.                              00001669
           MOVE ZEROS TO IND-AH-CLM-ALL.                                00001669
           MOVE ZEROS TO GRP-AH-CLM-ALL.                                00001669
       B025-EXIT.                                                       00001624
           EXIT.                                                        00001625
                                                                        00001370
       B026-PRINT-TOTAL.                                                00001612
           COMPUTE NET-IND = IND-LIFE-TOT - IND-AH-TOT.                 00001669
           COMPUTE NET-GRP = GRP-LIFE-TOT - GRP-AH-TOT.                 00001669
           MOVE IND-LIFE-TOT TO IND-LIFE-TOT-ISS.                       00001669
           MOVE GRP-LIFE-TOT TO GRP-LIFE-TOT-ISS.                       00001669
           MOVE IND-AH-TOT TO IND-AH-TOT-ISS.                           00001669
           MOVE GRP-AH-TOT TO GRP-AH-TOT-ISS.                           00001669
           WRITE REP-OUT-REC FROM PRINT-LINE20                          001397
                                              AFTER ADVANCING 3 LINES.  001397
           MOVE ZEROS TO IND-LIFE-TOT.                                  00001399
           MOVE ZEROS TO GRP-LIFE-TOT.                                  00001399
           MOVE ZEROS TO GRP-AH-TOT.                                    00001399
           MOVE ZEROS TO IND-AH-TOT.                                    00001399
           MOVE ZEROS TO NET-IND-ISS-CNT.                               00001399
           MOVE ZEROS TO NET-GRP-ISS-CNT.                               00001399
           MOVE ZEROS TO TOT-ISS-IND-CNT.                               00001669
           MOVE ZEROS TO TOT-ISS-GRP-CNT.                               00001669
           MOVE ZEROS TO TOT-CNC-IND-CNT.                               00001669
           MOVE ZEROS TO TOT-CNC-GRP-CNT.                               00001669
       B026-EXIT.                                                       00001624
           EXIT.                                                        00001625
                                                                        00001626
       B028-PRINT-TOTAL.                                                00001612
           COMPUTE NET-IND-CNT-OUT = TOT-ISS-IND-CNT - TOT-CNC-IND-CNT. 00001669
           COMPUTE NET-GRP-CNT-OUT = TOT-ISS-GRP-CNT - TOT-CNC-GRP-CNT. 00001669
           COMPUTE NET-IND-OUT = TOT-LIFE-IND - TOT-AH-IND.             00001669
           COMPUTE NET-GRP-OUT = TOT-LIFE-GRP - TOT-AH-GRP.             00001669
           MOVE TOT-LIFE-IND TO IND-LIFE-ISS-PRT.                       00001669
           MOVE TOT-LIFE-GRP TO GRP-LIFE-ISS-PRT.                       00001669
           MOVE TOT-AH-IND TO IND-AH-ISS-PRT.                           00001669
           MOVE TOT-AH-GRP TO GRP-AH-ISS-PRT.                           00001669
           WRITE REP-OUT-REC FROM PRINT-LINE22                          001397
                                              AFTER ADVANCING 3 LINES.  001397
           MOVE ZEROS TO IND-LIFE-TOT.                                  00001399
           MOVE ZEROS TO GRP-LIFE-TOT.                                  00001399
           MOVE ZEROS TO GRP-AH-TOT.                                    00001399
           MOVE ZEROS TO IND-AH-TOT.                                    00001399
           MOVE ZEROS TO TOT-ISS-IND-CNT.                               00001669
           MOVE ZEROS TO TOT-ISS-GRP-CNT.                               00001669
           MOVE ZEROS TO TOT-CNC-IND-CNT.                               00001669
           MOVE ZEROS TO TOT-CNC-GRP-CNT.                               00001669
       B028-EXIT.                                                       00001624
           EXIT.                                                        00001625
                                                                        00001625
       B030-EOJ.                                                        00001627
           IF FIRST-TIME = 'Y'                                          00001632
              PERFORM B033-PRINT-HEADINGS.                              00001632
           MOVE 0 TO SUB1.                                              00001632
           PERFORM B034-ZERO-OUT                                        00001634
             UNTIL SUB1 = 54.                                           00001635
           MOVE 0 TO SUB1.                                              00001632
           PERFORM B035-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-TOT NOT = 0 OR                                  00001669
               IND-AH-TOT NOT = 0 OR                                    00001669
               GRP-AH-TOT NOT = 0 OR                                    00001669
               GRP-LIFE-TOT NOT = 0)                                    00001669
           PERFORM B022-PRINT-TOTAL.                                    00001669
           MOVE 0 TO SUB1.                                              00001634
           PERFORM B036-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-TOT NOT = 0 OR                                  00001669
               IND-AH-TOT NOT = 0 OR                                    00001669
               GRP-AH-TOT NOT = 0 OR                                    00001669
               GRP-LIFE-TOT NOT = 0)                                    00001669
           PERFORM B022-PRINT-TOTAL.                                    00001669
           MOVE 0 TO SUB1.                                              00001634
           PERFORM B038-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-TOT NOT = 0 OR                                  00001669
               IND-AH-TOT NOT = 0 OR                                    00001669
               GRP-AH-TOT NOT = 0 OR                                    00001669
               GRP-LIFE-TOT NOT = 0)                                    00001669
           PERFORM B022-PRINT-TOTAL.                                    00001669
           MOVE 0 TO SUB1.                                              00001634
           PERFORM B039-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-PRINT NOT = 0 OR                                00001669
               IND-AH-PRINT NOT = 0 OR                                  00001669
               GRP-AH-PRINT NOT = 0 OR                                  00001669
               GRP-LIFE-PRINT NOT = 0)                                  00001669
           PERFORM B025-PRINT-TOTAL.                                    00001669
           MOVE 0 TO SUB1.                                              00001634
           PERFORM B042-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-TOT NOT = 0 OR                                  00001669
               IND-AH-TOT NOT = 0 OR                                    00001669
               GRP-AH-TOT NOT = 0 OR                                    00001669
               GRP-LIFE-TOT NOT = 0)                                    00001669
           PERFORM B026-PRINT-TOTAL.                                    00001669
           MOVE 0 TO SUB1.                                              00001634
           PERFORM B043-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-TOT NOT = 0 OR                                  00001669
               IND-AH-TOT NOT = 0 OR                                    00001669
               GRP-AH-TOT NOT = 0 OR                                    00001669
               GRP-LIFE-TOT NOT = 0)                                    00001669
           PERFORM B026-PRINT-TOTAL.                                    00001669
           MOVE 0 TO SUB1.                                              00001634
           PERFORM B044-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-TOT NOT = 0 OR                                  00001669
               IND-AH-TOT NOT = 0 OR                                    00001669
               GRP-AH-TOT NOT = 0 OR                                    00001669
               GRP-LIFE-TOT NOT = 0)                                    00001669
           PERFORM B026-PRINT-TOTAL.                                    00001669
           MOVE 0 TO SUB1.                                              00001634
           PERFORM B045-PROCESSING                                      00001634
             UNTIL SUB1 = 54.                                           00001635
           IF (IND-LIFE-PRINT NOT = 0 OR                                00001669
               IND-AH-PRINT NOT = 0 OR                                  00001669
               GRP-AH-PRINT NOT = 0 OR                                  00001669
               GRP-LIFE-PRINT NOT = 0)                                  00001669
           PERFORM B028-PRINT-TOTAL.                                    00001669
       B030-EXIT.                                                       00001660
           EXIT.                                                        00001661
                                                                        00001370
       B033-PRINT-HEADINGS.                                             00001370
           MOVE 0 TO SUB1.                                              00001632
           PERFORM B330-CHECK                                           00001399
            UNTIL SUB1 = 54.                                            00001399
           MOVE ZEROS TO IND-LIFE-PRINT.                                00001399
           MOVE ZEROS TO IND-AH-PRINT.                                  00001399
           MOVE ZEROS TO GRP-LIFE-PRINT.                                00001399
           MOVE ZEROS TO GRP-AH-PRINT.                                  00001399
           MOVE ZEROS TO IND-LIFE-CLAIM.                                00001399
           MOVE ZEROS TO IND-AH-CLAIM.                                  00001399
           MOVE ZEROS TO GRP-LIFE-CLAIM.                                00001399
           MOVE ZEROS TO GRP-AH-CLAIM.                                  00001399
           MOVE PAGE-NO TO PAGE-CNT.                                    00001381
           MOVE ZEROS TO IND-LIFE-ISS-PRINT.                            00001399
           MOVE ZEROS TO IND-AH-ISS-PRINT.                              00001399
           MOVE ZEROS TO GRP-LIFE-ISS-PRINT.                            00001399
           MOVE ZEROS TO GRP-AH-ISS-PRINT.                              00001399
           MOVE 'N' TO FIRST-TIME.                                      00001382
           ADD 1 TO PAGE-NO.                                            00001390
           MOVE WS-MM  TO  PL1-MM.                                      00001391
           MOVE WS-DD  TO  PL1-DD.                                      00001392
           MOVE WS-YY  TO  PL1-YY.                                      00001393
           WRITE REP-OUT-REC FROM PRINT-LINE1 AFTER ADVANCING 1 LINE.   00001397
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE2 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           MOVE 0 TO LINE-CT.                                           00001410
       B033-EXIT.                                                       00001420
           EXIT.                                                        00001430
                                                                        00001440
       B034-ZERO-OUT.                                                   00001442
              COMPUTE SUB1 = SUB1 + 1.                                  00001443
              MOVE ZEROS TO ET-OUT-AMT(SUB1, 1).                        00001445
              MOVE ZEROS TO ET-OUT-AMT(SUB1, 2).                        00001446
              MOVE ZEROS TO ET-OUT-AMT(SUB1, 3).                        00001447
              MOVE ZEROS TO ET-OUT-AMT(SUB1, 4).                        00001448
              MOVE ZEROS TO OT-OUT-AMT(SUB1, 1).                        00001445
              MOVE ZEROS TO OT-OUT-AMT(SUB1, 2).                        00001446
              MOVE ZEROS TO OT-OUT-AMT(SUB1, 3).                        00001447
              MOVE ZEROS TO OT-OUT-AMT(SUB1, 4).                        00001448
              MOVE ZEROS TO QA-OUT-AMT(SUB1, 1).                        00001445
              MOVE ZEROS TO QA-OUT-AMT(SUB1, 2).                        00001446
              MOVE ZEROS TO QA-OUT-AMT(SUB1, 3).                        00001447
              MOVE ZEROS TO QA-OUT-AMT(SUB1, 4).                        00001448
              MOVE ZEROS TO ET-OUT-ISS-CNC(SUB1, 1).                    00001445
              MOVE ZEROS TO ET-OUT-ISS-CNC(SUB1, 2).                    00001446
              MOVE ZEROS TO ET-OUT-ISS-CNC(SUB1, 3).                    00001447
              MOVE ZEROS TO ET-OUT-ISS-CNC(SUB1, 4).                    00001448
              MOVE ZEROS TO OT-OUT-ISS-CNC(SUB1, 1).                    00001445
              MOVE ZEROS TO OT-OUT-ISS-CNC(SUB1, 2).                    00001446
              MOVE ZEROS TO OT-OUT-ISS-CNC(SUB1, 3).                    00001447
              MOVE ZEROS TO OT-OUT-ISS-CNC(SUB1, 4).                    00001448
              MOVE ZEROS TO QA-OUT-ISS-CNC(SUB1, 1).                    00001445
              MOVE ZEROS TO QA-OUT-ISS-CNC(SUB1, 2).                    00001446
              MOVE ZEROS TO QA-OUT-ISS-CNC(SUB1, 3).                    00001447
              MOVE ZEROS TO QA-OUT-ISS-CNC(SUB1, 4).                    00001448
              MOVE ZEROS TO ALL-OUT-AMT(SUB1, 1).                       00001445
              MOVE ZEROS TO ALL-OUT-AMT(SUB1, 2).                       00001445
              MOVE ZEROS TO ALL-OUT-AMT(SUB1, 3).                       00001445
              MOVE ZEROS TO ALL-OUT-AMT(SUB1, 4).                       00001445
              MOVE ZEROS TO ALL-CLM(SUB1, 1).                           00001445
              MOVE ZEROS TO ALL-CLM(SUB1, 2).                           00001445
              MOVE ZEROS TO ALL-CLM(SUB1, 3).                           00001445
              MOVE ZEROS TO ALL-CLM(SUB1, 4).                           00001445
              MOVE ZEROS TO ALL-ISS-AMT(SUB1, 1).                       00001445
              MOVE ZEROS TO ALL-ISS-AMT(SUB1, 2).                       00001445
              MOVE ZEROS TO ALL-ISS-AMT(SUB1, 3).                       00001445
              MOVE ZEROS TO ALL-ISS-AMT(SUB1, 4).                       00001445
       B034-EXIT.                                                       00001420
           EXIT.                                                        00001430
                                                                        00001662
       B035-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE '                   EXTENDED TERM                  '   00001683
                 TO TYPE-OUT.                                           00001683
            COMPUTE ET-OUT-AMT(SUB1, 1) =  ET-OUT-AMT(SUB1, 1) +        00001683
                                 (ET-AMT(SUB1, 1) - ET-AMT(SUB1, 2)).   00001683
            COMPUTE ET-OUT-AMT(SUB1, 2) =  ET-OUT-AMT(SUB1, 2) +        00001683
                                 (ET-AMT(SUB1, 5) - ET-AMT(SUB1, 6)).   00001683
            COMPUTE ET-OUT-AMT(SUB1, 3) =  ET-OUT-AMT(SUB1, 3) +        00001683
                                 (ET-AMT(SUB1, 3) - ET-AMT(SUB1, 4)).   00001683
            COMPUTE ET-OUT-AMT(SUB1, 4) =  ET-OUT-AMT(SUB1, 4) +        00001683
                                 (ET-AMT(SUB1, 7) - ET-AMT(SUB1, 8)).   00001683
            COMPUTE IND-LIFE-TOT = IND-LIFE-TOT + ET-OUT-AMT(SUB1, 1).  00001669
            COMPUTE IND-LIFE-ALL = IND-LIFE-ALL + ET-OUT-AMT(SUB1, 1).  00001669
            COMPUTE ALL-OUT-AMT(SUB1, 1) = ALL-OUT-AMT(SUB1, 1) +       00001669
                                           ET-OUT-AMT(SUB1, 1).         00001669
            COMPUTE ALL-CLM(SUB1, 1) = ALL-CLM(SUB1, 1) +               00001669
                                           ET-CLM(SUB1, 1).             00001669
            COMPUTE IND-AH-TOT = IND-AH-TOT + ET-OUT-AMT(SUB1, 2).      00001669
            COMPUTE IND-AH-ALL = IND-AH-ALL + ET-OUT-AMT(SUB1, 2).      00001669
            COMPUTE ALL-OUT-AMT(SUB1, 2) = ALL-OUT-AMT(SUB1, 2) +       00001669
                                           ET-OUT-AMT(SUB1, 2).         00001669
            COMPUTE ALL-CLM(SUB1, 3) = ALL-CLM(SUB1, 3) +               00001669
                                           ET-CLM(SUB1, 3).             00001669
            COMPUTE GRP-LIFE-TOT = GRP-LIFE-TOT + ET-OUT-AMT(SUB1, 3).  00001669
            COMPUTE GRP-LIFE-ALL = GRP-LIFE-ALL + ET-OUT-AMT(SUB1, 3).  00001669
            COMPUTE ALL-OUT-AMT(SUB1, 3) = ALL-OUT-AMT(SUB1, 3) +       00001669
                                           ET-OUT-AMT(SUB1, 3).         00001669
            COMPUTE ALL-CLM(SUB1, 2) = ALL-CLM(SUB1, 2) +               00001669
                                           ET-CLM(SUB1, 2).             00001669
            COMPUTE GRP-AH-TOT = GRP-AH-TOT + ET-OUT-AMT(SUB1, 4).      00001669
            COMPUTE GRP-AH-ALL = GRP-AH-ALL + ET-OUT-AMT(SUB1, 4).      00001669
            COMPUTE ALL-OUT-AMT(SUB1, 4) = ALL-OUT-AMT(SUB1, 4) +       00001669
                                           ET-OUT-AMT(SUB1, 4).         00001669
            COMPUTE ALL-CLM(SUB1, 4) = ALL-CLM(SUB1, 4) +               00001669
                                           ET-CLM(SUB1, 4).             00001669
            IF ET-OUT-AMT (SUB1, 1) NOT = 0                             00001669
                 MOVE ET-OUT-AMT(SUB1, 1) TO IND-LIFE-PRINT             00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-OUT-AMT (SUB1, 2) NOT = 0                             00001669
                 MOVE ET-OUT-AMT(SUB1, 2) TO IND-AH-PRINT               00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-OUT-AMT (SUB1, 3) NOT = 0                             00001669
                 MOVE ET-OUT-AMT(SUB1, 3) TO GRP-LIFE-PRINT             00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-OUT-AMT (SUB1, 4) NOT = 0                             00001669
                 MOVE ET-OUT-AMT(SUB1, 4) TO GRP-AH-PRINT               00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 1) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 1) TO IND-LIFE-CLAIM                 00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 3) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 3) TO IND-AH-CLAIM                   00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 2) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 2) TO GRP-LIFE-CLAIM                 00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 4) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 4) TO GRP-AH-CLAIM                   00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            COMPUTE IND-LIFE-CLM-OUT  =                                 00001669
                                 IND-LIFE-CLM-OUT + ET-CLM(SUB1, 1).    00001669
            COMPUTE IND-LIFE-CLM-ALL  =                                 00001669
                                 IND-LIFE-CLM-ALL + ET-CLM(SUB1, 1).    00001669
            COMPUTE GRP-LIFE-CLM-OUT  =                                 00001669
                                 GRP-LIFE-CLM-OUT + ET-CLM(SUB1, 2).    00001669
            COMPUTE GRP-LIFE-CLM-ALL  =                                 00001669
                                 GRP-LIFE-CLM-ALL + ET-CLM(SUB1, 2).    00001669
            COMPUTE IND-AH-CLM-OUT  =                                   00001669
                                 IND-AH-CLM-OUT + ET-CLM(SUB1, 3).      00001669
            COMPUTE IND-AH-CLM-ALL  =                                   00001669
                                 IND-AH-CLM-ALL + ET-CLM(SUB1, 3).      00001669
            COMPUTE GRP-AH-CLM-OUT  =                                   00001669
                                 GRP-AH-CLM-OUT + ET-CLM(SUB1, 4).      00001669
            COMPUTE GRP-AH-CLM-ALL  =                                   00001669
                                 GRP-AH-CLM-ALL + ET-CLM(SUB1, 4).      00001669
            IF (STATE-PRINT NOT = '               '  AND                00001669
                      DONE = 'N')                                       00001669
                PERFORM B041-FIRST-LINES.                               00001399
            IF LINE-CT > 55                                             00001669
                PERFORM B023-PRINT-HEADINGS.                            00001669
            IF PREV-TYPE NOT = TYPE-OUT AND                             00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                PERFORM B023-PRINT-HEADINGS.                            00001669
            IF  STATE-PRINT NOT = '               '   AND               00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                WRITE REP-OUT-REC FROM PRINT-LINE9 AFTER                00001399
                                                 ADVANCING 2 LINES      00001399
                COMPUTE LINE-CT = LINE-CT + 1                           00001399
                MOVE ZEROS TO IND-LIFE-PRINT                            00001399
                MOVE ZEROS TO IND-AH-PRINT                              00001399
                MOVE ZEROS TO GRP-LIFE-PRINT                            00001399
                MOVE ZEROS TO GRP-AH-PRINT                              00001399
                MOVE ZEROS TO IND-LIFE-CLAIM                            00001399
                MOVE ZEROS TO IND-AH-CLAIM                              00001399
                MOVE ZEROS TO GRP-LIFE-CLAIM                            00001399
                MOVE ZEROS TO GRP-AH-CLAIM                              00001399
                MOVE '               ' TO STATE-PRINT                   00001399
                MOVE ZEROS TO IND-LIFE-ISS-PRINT                        00001399
                MOVE ZEROS TO IND-AH-ISS-PRINT                          00001399
                MOVE ZEROS TO GRP-LIFE-ISS-PRINT                        00001399
                MOVE ZEROS TO GRP-AH-ISS-PRINT.                         00001399
       B035-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001692
       B036-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE 'ALL BUSINESS (EXCLUDES EXT. TERM & QUICK APP.)'       00001683
                                                          TO TYPE-OUT.  00001683
            COMPUTE OT-OUT-AMT(SUB1, 1) =  OT-OUT-AMT(SUB1, 1) +        00001683
                             (ET-AMT(SUB1,  9) - ET-AMT(SUB1, 10)).     00001683
            COMPUTE OT-OUT-AMT(SUB1, 2) =  OT-OUT-AMT(SUB1, 2) +        00001683
                             (ET-AMT(SUB1, 13) - ET-AMT(SUB1, 14)).     00001683
            COMPUTE OT-OUT-AMT(SUB1, 3) =  OT-OUT-AMT(SUB1, 3) +        00001683
                             (ET-AMT(SUB1, 11) - ET-AMT(SUB1, 12)).     00001683
            COMPUTE OT-OUT-AMT(SUB1, 4) =  OT-OUT-AMT(SUB1, 4) +        00001683
                             (ET-AMT(SUB1, 15) - ET-AMT(SUB1, 16)).     00001683
            COMPUTE IND-LIFE-TOT = IND-LIFE-TOT + OT-OUT-AMT(SUB1, 1).  00001669
            COMPUTE IND-LIFE-ALL = IND-LIFE-ALL + OT-OUT-AMT(SUB1, 1).  00001669
            COMPUTE ALL-OUT-AMT(SUB1, 1) = ALL-OUT-AMT(SUB1, 1) +       00001669
                                           OT-OUT-AMT(SUB1, 1).         00001669
            COMPUTE ALL-CLM(SUB1, 1) = ALL-CLM(SUB1, 1) +               00001669
                                           ET-CLM(SUB1, 5).             00001669
            COMPUTE IND-AH-TOT = IND-AH-TOT + OT-OUT-AMT(SUB1, 2).      00001669
            COMPUTE IND-AH-ALL = IND-AH-ALL + OT-OUT-AMT(SUB1, 2).      00001669
            COMPUTE ALL-OUT-AMT(SUB1, 2) = ALL-OUT-AMT(SUB1, 2) +       00001669
                                           OT-OUT-AMT(SUB1, 2).         00001669
            COMPUTE ALL-CLM(SUB1, 3) = ALL-CLM(SUB1, 3) +               00001669
                                           ET-CLM(SUB1, 7).             00001669
            COMPUTE GRP-LIFE-TOT = GRP-LIFE-TOT + OT-OUT-AMT(SUB1, 3).  00001669
            COMPUTE GRP-LIFE-ALL = GRP-LIFE-ALL + OT-OUT-AMT(SUB1, 3).  00001669
            COMPUTE ALL-OUT-AMT(SUB1, 3) = ALL-OUT-AMT(SUB1, 3) +       00001669
                                           OT-OUT-AMT(SUB1, 3).         00001669
            COMPUTE ALL-CLM(SUB1, 2) = ALL-CLM(SUB1, 2) +               00001669
                                           ET-CLM(SUB1, 6).             00001669
            COMPUTE GRP-AH-TOT = GRP-AH-TOT + OT-OUT-AMT(SUB1, 4).      00001669
            COMPUTE GRP-AH-ALL = GRP-AH-ALL + OT-OUT-AMT(SUB1, 4).      00001669
            COMPUTE ALL-OUT-AMT(SUB1, 4) = ALL-OUT-AMT(SUB1, 4) +       00001669
                                           OT-OUT-AMT(SUB1, 4).         00001669
            COMPUTE ALL-CLM(SUB1, 4) = ALL-CLM(SUB1, 4) +               00001669
                                           ET-CLM(SUB1, 8).             00001669
            IF OT-OUT-AMT (SUB1, 1) NOT = 0                             00001669
                 MOVE OT-OUT-AMT(SUB1, 1) TO IND-LIFE-PRINT             00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF OT-OUT-AMT (SUB1, 2) NOT = 0                             00001669
                 MOVE OT-OUT-AMT(SUB1, 2) TO IND-AH-PRINT               00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF OT-OUT-AMT (SUB1, 3) NOT = 0                             00001669
                 MOVE OT-OUT-AMT(SUB1, 3) TO GRP-LIFE-PRINT.            00001669
            IF OT-OUT-AMT (SUB1, 4) NOT = 0                             00001669
                 MOVE OT-OUT-AMT(SUB1, 4) TO GRP-AH-PRINT               00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 5) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 5) TO IND-LIFE-CLAIM                 00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 7) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 7) TO IND-AH-CLAIM                   00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 6) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 6) TO GRP-LIFE-CLAIM                 00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 8) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 8) TO GRP-AH-CLAIM                   00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            COMPUTE IND-LIFE-CLM-OUT  =                                 00001669
                                 IND-LIFE-CLM-OUT + ET-CLM(SUB1, 5).    00001669
            COMPUTE IND-LIFE-CLM-ALL  =                                 00001669
                                 IND-LIFE-CLM-ALL + ET-CLM(SUB1, 5).    00001669
            COMPUTE GRP-LIFE-CLM-OUT  =                                 00001669
                                 GRP-LIFE-CLM-OUT + ET-CLM(SUB1, 6).    00001669
            COMPUTE GRP-LIFE-CLM-ALL  =                                 00001669
                                 GRP-LIFE-CLM-ALL + ET-CLM(SUB1, 6).    00001669
            COMPUTE IND-AH-CLM-OUT  =                                   00001669
                                 IND-AH-CLM-OUT + ET-CLM(SUB1, 7).      00001669
            COMPUTE IND-AH-CLM-ALL  =                                   00001669
                                 IND-AH-CLM-ALL + ET-CLM(SUB1, 7).      00001669
            COMPUTE GRP-AH-CLM-OUT  =                                   00001669
                                 GRP-AH-CLM-OUT + ET-CLM(SUB1, 8).      00001669
            COMPUTE GRP-AH-CLM-ALL  =                                   00001669
                                 GRP-AH-CLM-ALL + ET-CLM(SUB1, 8).      00001669
            IF (STATE-PRINT NOT = '               '  AND                00001669
                      DONE = 'N')                                       00001669
                PERFORM B041-FIRST-LINES.                               00001399
            IF LINE-CT > 55                                             00001669
                PERFORM B023-PRINT-HEADINGS.                            00001669
            IF PREV-TYPE NOT = TYPE-OUT  AND                            00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                PERFORM B023-PRINT-HEADINGS.                            00001669
            IF  STATE-PRINT NOT = '               '   AND               00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                WRITE REP-OUT-REC FROM PRINT-LINE9 AFTER                00001399
                                               ADVANCING 2 LINES        00001399
                COMPUTE LINE-CT = LINE-CT + 1                           00001399
                MOVE ZEROS TO IND-LIFE-PRINT                            00001399
                MOVE ZEROS TO IND-AH-PRINT                              00001399
                MOVE ZEROS TO GRP-LIFE-PRINT                            00001399
                MOVE ZEROS TO GRP-AH-PRINT                              00001399
                MOVE ZEROS TO IND-LIFE-CLAIM                            00001399
                MOVE ZEROS TO IND-AH-CLAIM                              00001399
                MOVE ZEROS TO GRP-LIFE-CLAIM                            00001399
                MOVE ZEROS TO GRP-AH-CLAIM                              00001399
                MOVE '               ' TO STATE-PRINT.                  00001399
       B036-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001692
       B038-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE '                QUICK APPLICATION                 '   00001683
                                                          TO TYPE-OUT.  00001683
            COMPUTE QA-OUT-AMT(SUB1, 1) =  QA-OUT-AMT(SUB1, 1) +        00001683
                             (ET-AMT(SUB1, 17) - ET-AMT(SUB1, 18)).     00001683
            COMPUTE IND-LIFE-TOT = IND-LIFE-TOT + QA-OUT-AMT(SUB1, 1).  00001669
            COMPUTE IND-LIFE-ALL = IND-LIFE-ALL + QA-OUT-AMT(SUB1, 1).  00001669
            COMPUTE ALL-OUT-AMT(SUB1, 1) = ALL-OUT-AMT(SUB1, 1) +       00001669
                                           QA-OUT-AMT(SUB1, 1).         00001669
            COMPUTE ALL-CLM(SUB1, 1) = ALL-CLM(SUB1, 1) +               00001669
                                           ET-CLM(SUB1, 9).             00001669
            IF QA-OUT-AMT (SUB1, 1) NOT = 0                             00001669
                 MOVE QA-OUT-AMT(SUB1, 1) TO IND-LIFE-PRINT             00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ET-CLM (SUB1, 9) NOT = 0                                 00001669
                 MOVE ET-CLM(SUB1, 9) TO IND-LIFE-CLAIM                 00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            COMPUTE IND-LIFE-CLM-OUT  =                                 00001669
                                 IND-LIFE-CLM-OUT + ET-CLM(SUB1, 9).    00001669
            COMPUTE IND-LIFE-CLM-ALL  =                                 00001669
                                 IND-LIFE-CLM-ALL + ET-CLM(SUB1, 9).    00001669
            IF (STATE-PRINT NOT = '               '  AND                00001669
                      DONE = 'N')                                       00001669
                 PERFORM B041-FIRST-LINES.                              00001399
            IF LINE-CT > 55                                             00001669
                 PERFORM B023-PRINT-HEADINGS.                           00001669
            IF PREV-TYPE NOT = TYPE-OUT AND                             00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 PERFORM B023-PRINT-HEADINGS.                           00001669
            IF  STATE-PRINT NOT = '               '   AND               00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 WRITE REP-OUT-REC FROM PRINT-LINE9 AFTER               00001399
                                                 ADVANCING 2 LINES      00001399
                 COMPUTE LINE-CT = LINE-CT + 1                          00001399
                 MOVE ZEROS TO IND-LIFE-PRINT                           00001399
                 MOVE ZEROS TO IND-AH-PRINT                             00001399
                 MOVE ZEROS TO GRP-LIFE-PRINT                           00001399
                 MOVE ZEROS TO GRP-AH-PRINT                             00001399
                 MOVE ZEROS TO IND-LIFE-CLAIM                           00001399
                 MOVE ZEROS TO IND-AH-CLAIM                             00001399
                 MOVE ZEROS TO GRP-LIFE-CLAIM                           00001399
                 MOVE ZEROS TO GRP-AH-CLAIM                             00001399
                 MOVE '               ' TO STATE-PRINT.                 00001399
       B038-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001694
       B039-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE '                ALL BUSINESS                      '   00001683
                                                          TO TYPE-OUT.  00001683
            IF ALL-OUT-AMT (SUB1, 1) NOT = 0                            00001669
                 MOVE ALL-OUT-AMT(SUB1, 1) TO IND-LIFE-PRINT            00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ALL-OUT-AMT (SUB1, 2) NOT = 0                            00001669
                 MOVE ALL-OUT-AMT(SUB1, 2) TO IND-AH-PRINT              00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ALL-OUT-AMT (SUB1, 3) NOT = 0                            00001669
                 MOVE ALL-OUT-AMT(SUB1, 3) TO GRP-LIFE-PRINT            00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ALL-OUT-AMT (SUB1, 4) NOT = 0                            00001669
                 MOVE ALL-OUT-AMT(SUB1, 4) TO GRP-AH-PRINT              00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ALL-CLM (SUB1, 1) NOT = 0                                00001669
                 MOVE ALL-CLM(SUB1, 1) TO IND-LIFE-CLAIM                00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ALL-CLM (SUB1, 3) NOT = 0                                00001669
                 MOVE ALL-CLM(SUB1, 3) TO IND-AH-CLAIM                  00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ALL-CLM (SUB1, 2) NOT = 0                                00001669
                 MOVE ALL-CLM(SUB1, 2) TO GRP-LIFE-CLAIM                00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF ALL-CLM (SUB1, 4) NOT = 0                                00001669
                 MOVE ALL-CLM(SUB1, 4) TO GRP-AH-CLAIM                  00001669
                 MOVE ET-NAME(SUB1) TO STATE-PRINT.                     00001669
            IF (STATE-PRINT NOT = '               '  AND                00001669
                      DONE = 'N')                                       00001669
                 PERFORM B041-FIRST-LINES.                              00001399
            IF LINE-CT > 55                                             00001669
                 PERFORM B023-PRINT-HEADINGS.                           00001669
            IF PREV-TYPE NOT = TYPE-OUT                                 00001669
                 PERFORM B023-PRINT-HEADINGS.                           00001669
            IF  STATE-PRINT NOT = '               '   AND               00001669
                      IND-LIFE-PRINT NOT = 0                            00001669
                 WRITE REP-OUT-REC FROM PRINT-LINE9 AFTER               00001399
                                                ADVANCING 2 LINES       00001399
                 COMPUTE LINE-CT = LINE-CT + 1                          00001399
                 MOVE ZEROS TO IND-LIFE-PRINT                           00001399
                 MOVE ZEROS TO IND-AH-PRINT                             00001399
                 MOVE ZEROS TO GRP-LIFE-PRINT                           00001399
                 MOVE ZEROS TO GRP-AH-PRINT                             00001399
                 MOVE ZEROS TO IND-LIFE-CLAIM                           00001399
                 MOVE ZEROS TO IND-AH-CLAIM                             00001399
                 MOVE ZEROS TO GRP-LIFE-CLAIM                           00001399
                 MOVE ZEROS TO GRP-AH-CLAIM                             00001399
                 MOVE '               ' TO STATE-PRINT.                 00001399
       B039-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001692
       B041-FIRST-LINES.                                                00001627
           MOVE 'Y' TO DONE.                                            00001399
           WRITE REP-OUT-REC FROM PRINT-LINE4 AFTER ADVANCING 1 LINE.   00001399
           MOVE TYPE-OUT TO PREV-TYPE.                                  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE14 AFTER ADVANCING 1 LINE.  00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE8 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE5 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE6 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
           WRITE REP-OUT-REC FROM PRINT-LINE7 AFTER ADVANCING 1 LINE.   00001399
           COMPUTE LINE-CT = LINE-CT + 1.                               00001399
       B041-EXIT.                                                       00001660
           EXIT.                                                        00001661
                                                                        00001661
       B042-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE '                   EXTENDED TERM                  '   00001683
                 TO TYPE-OUT.                                           00001683
            COMPUTE ET-OUT-ISS-CNC(SUB1, 1) = ET-OUT-ISS-CNC(SUB1, 1) + 00001683
                                  ET-ISS-CNC(SUB1, 1).                  00001683
            COMPUTE ET-OUT-ISS-CNC(SUB1, 2) = ET-OUT-ISS-CNC(SUB1, 2) + 00001683
                                  ET-ISS-CNC(SUB1, 2).                  00001683
            COMPUTE ET-OUT-ISS-CNC(SUB1, 3) = ET-OUT-ISS-CNC(SUB1, 3) + 00001683
                                  ET-ISS-CNC(SUB1, 3).                  00001683
            COMPUTE ET-OUT-ISS-CNC(SUB1, 4) = ET-OUT-ISS-CNC(SUB1, 4) + 00001683
                                  ET-ISS-CNC(SUB1, 4).                  00001683
            COMPUTE IND-LIFE-TOT = IND-LIFE-TOT +                       00001669
                                            ET-OUT-ISS-CNC(SUB1, 1).    00001669
            COMPUTE ALL-ISS-AMT(SUB1, 1) = ALL-ISS-AMT(SUB1, 1)         00001669
                                         +  ET-OUT-ISS-CNC(SUB1, 1).    00001669
            COMPUTE IND-AH-TOT = IND-AH-TOT +                           00001669
                                            ET-OUT-ISS-CNC(SUB1, 2).    00001669
            COMPUTE ALL-ISS-AMT(SUB1, 2) = ALL-ISS-AMT(SUB1, 2)         00001669
                                         +  ET-OUT-ISS-CNC(SUB1, 2).    00001669
            COMPUTE GRP-LIFE-TOT = GRP-LIFE-TOT +                       00001669
                                            ET-OUT-ISS-CNC(SUB1, 3).    00001669
            COMPUTE ALL-ISS-AMT(SUB1, 3) = ALL-ISS-AMT(SUB1, 3)         00001669
                                         +  ET-OUT-ISS-CNC(SUB1, 3).    00001669
            COMPUTE GRP-AH-TOT = GRP-AH-TOT +                           00001669
                                            ET-OUT-ISS-CNC(SUB1, 4).    00001669
            COMPUTE ALL-ISS-AMT(SUB1, 4) = ALL-ISS-AMT(SUB1, 4)         00001669
                                         +  ET-OUT-ISS-CNC(SUB1, 4).    00001669
            IF ET-OUT-ISS-CNC (SUB1, 1) NOT = 0                         00001669
                 MOVE ET-OUT-ISS-CNC(SUB1, 1) TO IND-LIFE-ISS-PRINT     00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF ET-OUT-ISS-CNC (SUB1, 2) NOT = 0                         00001669
                 MOVE ET-OUT-ISS-CNC(SUB1, 2) TO IND-AH-ISS-PRINT       00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF ET-OUT-ISS-CNC (SUB1, 3) NOT = 0                         00001669
                 MOVE ET-OUT-ISS-CNC(SUB1, 3) TO GRP-LIFE-ISS-PRINT     00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF ET-OUT-ISS-CNC (SUB1, 4) NOT = 0                         00001669
                 MOVE ET-OUT-ISS-CNC(SUB1, 4) TO GRP-AH-ISS-PRINT       00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            COMPUTE NET-IND-PRINT = ET-OUT-ISS-CNC(SUB1, 1) -           00001669
                                   ET-OUT-ISS-CNC(SUB1, 2).             00001669
            COMPUTE NET-GRP-PRINT = ET-OUT-ISS-CNC(SUB1, 3) -           00001669
                                   ET-OUT-ISS-CNC(SUB1, 4).             00001669
            MOVE ET-CNT(SUB1, 1) TO PRINT-ISS-CNT-IND.                  00001669
            MOVE ET-CNC-CNT(SUB1, 1) TO PRINT-CNC-CNT-IND.              00001669
            MOVE ET-CNT(SUB1, 2) TO PRINT-ISS-CNT-GRP.                  00001669
            MOVE ET-CNC-CNT(SUB1, 2) TO PRINT-CNC-CNT-GRP.              00001669
            COMPUTE TOT-ISS-IND-CNT = ET-CNT(SUB1, 1) + TOT-ISS-IND-CNT.00001669
            COMPUTE TOT-ISS-GRP-CNT = ET-CNT(SUB1, 2) + TOT-ISS-GRP-CNT.00001669
            COMPUTE TOT-CNC-IND-CNT                                     00001669
                              = ET-CNC-CNT(SUB1, 1) + TOT-CNC-IND-CNT.  00001669
            COMPUTE TOT-CNC-GRP-CNT                                     00001669
                              = ET-CNC-CNT(SUB1, 2) + TOT-CNC-GRP-CNT.  00001669
            COMPUTE NET-IND-CNT =                                       00001669
                                 ET-CNT(SUB1, 1) - ET-CNC-CNT(SUB1, 1). 00001669
            COMPUTE NET-GRP-CNT =                                       00001669
                                 ET-CNT(SUB1, 2) - ET-CNC-CNT(SUB1, 2). 00001669
            COMPUTE NET-IND-ISS-CNT                                     00000989
                                  = TOT-ISS-IND-CNT - TOT-CNC-IND-CNT.  00000989
            COMPUTE NET-GRP-ISS-CNT                                     00000989
                                  = TOT-ISS-GRP-CNT - TOT-CNC-GRP-CNT.  00000989
            MOVE TOT-ISS-IND-CNT TO ISS-CNT-IND.                        00001669
            MOVE TOT-ISS-GRP-CNT TO ISS-CNT-GRP.                        00001669
            MOVE TOT-CNC-IND-CNT TO CNC-CNT-IND.                        00001669
            MOVE TOT-CNC-GRP-CNT TO CNC-CNT-GRP.                        00001669
            IF (STATE-ISS-PRINT NOT = '               '  AND            00001669
                      DONE = 'N')                                       00001669
                 PERFORM B041-FIRST-LINES.                              00001399
            IF LINE-CT > 55                                             00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF PREV-TYPE NOT = TYPE-OUT AND                             00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF  STATE-ISS-PRINT NOT = '               '   AND           00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 WRITE REP-OUT-REC FROM PRINT-LINE15 AFTER              00001399
                                                 ADVANCING 2 LINES      00001399
                 COMPUTE LINE-CT = LINE-CT + 1                          00001399
                 MOVE ZEROS TO IND-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO IND-AH-ISS-PRINT                         00001399
                 MOVE ZEROS TO GRP-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO GRP-AH-ISS-PRINT                         00001399
                 MOVE '               ' TO STATE-ISS-PRINT.             00001399
       B042-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001692
       B043-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE 'ALL BUSINESS (EXCLUDES EXT. TERM & QUICK APP.)'       00001683
                                                          TO TYPE-OUT.  00001683
            COMPUTE OT-OUT-ISS-CNC(SUB1, 1) = OT-OUT-ISS-CNC(SUB1, 1) + 00001683
                                 ET-ISS-CNC(SUB1, 9).                   00001683
            COMPUTE OT-OUT-ISS-CNC(SUB1, 2) =                           00001683
                    OT-OUT-ISS-CNC(SUB1, 2) + ET-ISS-CNC(SUB1, 10).     00001683
            COMPUTE OT-OUT-ISS-CNC(SUB1, 3) =                           00001683
                    OT-OUT-ISS-CNC(SUB1, 3) + ET-ISS-CNC(SUB1, 11).     00001683
            COMPUTE OT-OUT-ISS-CNC(SUB1, 4) =                           00001683
                    OT-OUT-ISS-CNC(SUB1, 4) + ET-ISS-CNC(SUB1, 12).     00001683
            COMPUTE IND-LIFE-TOT = IND-LIFE-TOT +                       00001669
                                         OT-OUT-ISS-CNC(SUB1, 1).       00001669
            COMPUTE ALL-ISS-AMT(SUB1, 1) = ALL-ISS-AMT(SUB1, 1)         00001669
                                        +   OT-OUT-ISS-CNC(SUB1, 1).    00001669
            COMPUTE IND-AH-TOT = IND-AH-TOT +                           00001669
                                         OT-OUT-ISS-CNC(SUB1, 2).       00001669
            COMPUTE ALL-ISS-AMT(SUB1, 2) = ALL-ISS-AMT(SUB1, 2)         00001669
                                         +  OT-OUT-ISS-CNC(SUB1, 2).    00001669
            COMPUTE GRP-LIFE-TOT = GRP-LIFE-TOT +                       00001669
                                         OT-OUT-ISS-CNC(SUB1, 3).       00001669
            COMPUTE ALL-ISS-AMT(SUB1, 3) = ALL-ISS-AMT(SUB1, 3)         00001669
                                         +  OT-OUT-ISS-CNC(SUB1, 3).    00001669
            COMPUTE GRP-AH-TOT = GRP-AH-TOT +                           00001669
                                         OT-OUT-ISS-CNC(SUB1, 4).       00001669
            COMPUTE ALL-ISS-AMT(SUB1, 4) = ALL-ISS-AMT(SUB1, 4)         00001669
                                         +  OT-OUT-ISS-CNC(SUB1, 4).    00001669
            IF OT-OUT-ISS-CNC (SUB1, 1) NOT = 0                         00001669
                 MOVE OT-OUT-ISS-CNC(SUB1, 1) TO IND-LIFE-ISS-PRINT     00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF OT-OUT-ISS-CNC (SUB1, 2) NOT = 0                         00001669
                 MOVE OT-OUT-ISS-CNC(SUB1, 2) TO IND-AH-ISS-PRINT       00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF OT-OUT-ISS-CNC (SUB1, 3) NOT = 0                         00001669
                 MOVE OT-OUT-ISS-CNC(SUB1, 3) TO GRP-LIFE-ISS-PRINT     00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF OT-OUT-ISS-CNC (SUB1, 4) NOT = 0                         00001669
                 MOVE OT-OUT-ISS-CNC(SUB1, 4) TO GRP-AH-ISS-PRINT       00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            COMPUTE NET-IND-PRINT = OT-OUT-ISS-CNC(SUB1, 1) -           00001669
                                    OT-OUT-ISS-CNC(SUB1, 2).            00001669
            COMPUTE NET-GRP-PRINT = OT-OUT-ISS-CNC(SUB1, 3) -           00001669
                                    OT-OUT-ISS-CNC(SUB1, 4).            00001669
            MOVE ET-CNT(SUB1, 5) TO PRINT-ISS-CNT-IND.                  00001669
            MOVE ET-CNC-CNT(SUB1, 5) TO PRINT-CNC-CNT-IND.              00001669
            MOVE ET-CNT(SUB1, 6) TO PRINT-ISS-CNT-GRP.                  00001669
            MOVE ET-CNC-CNT(SUB1, 6) TO PRINT-CNC-CNT-GRP.              00001669
            COMPUTE TOT-ISS-IND-CNT = ET-CNT(SUB1, 5) + TOT-ISS-IND-CNT.00001669
            COMPUTE TOT-ISS-GRP-CNT = ET-CNT(SUB1, 6) + TOT-ISS-GRP-CNT.00001669
            COMPUTE TOT-CNC-IND-CNT                                     00001669
                              = ET-CNC-CNT(SUB1, 5) + TOT-CNC-IND-CNT.  00001669
            COMPUTE TOT-CNC-GRP-CNT                                     00001669
                              = ET-CNC-CNT(SUB1, 6) + TOT-CNC-GRP-CNT.  00001669
            COMPUTE NET-IND-CNT =                                       00001669
                                ET-CNT(SUB1, 5) - ET-CNC-CNT(SUB1, 5).  00001669
            COMPUTE NET-GRP-CNT =                                       00001669
                                ET-CNT(SUB1, 6) - ET-CNC-CNT(SUB1, 6).  00001669
            COMPUTE NET-IND-ISS-CNT                                     00000989
                                  = TOT-ISS-IND-CNT - TOT-CNC-IND-CNT.  00000989
            COMPUTE NET-GRP-ISS-CNT                                     00000989
                                  = TOT-ISS-GRP-CNT - TOT-CNC-GRP-CNT.  00000989
            MOVE TOT-ISS-IND-CNT TO ISS-CNT-IND.                        00001669
            MOVE TOT-ISS-GRP-CNT TO ISS-CNT-GRP.                        00001669
            MOVE TOT-CNC-IND-CNT TO CNC-CNT-IND.                        00001669
            MOVE TOT-CNC-GRP-CNT TO CNC-CNT-GRP.                        00001669
            IF (STATE-ISS-PRINT NOT = '               '  AND            00001669
                      DONE = 'N')                                       00001669
                 PERFORM B041-FIRST-LINES.                              00001399
            IF LINE-CT > 55                                             00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF PREV-TYPE NOT = TYPE-OUT AND                             00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF  STATE-ISS-PRINT NOT = '               '   AND           00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 WRITE REP-OUT-REC FROM PRINT-LINE15 AFTER              00001399
                                                 ADVANCING 2 LINES      00001399
                 COMPUTE LINE-CT = LINE-CT + 1                          00001399
                 MOVE ZEROS TO IND-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO IND-AH-ISS-PRINT                         00001399
                 MOVE ZEROS TO GRP-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO GRP-AH-ISS-PRINT                         00001399
                 MOVE '               ' TO STATE-ISS-PRINT.             00001399
       B043-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001694
       B044-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE '                QUICK APPLICATION                 '   00001683
                                                          TO TYPE-OUT.  00001683
            COMPUTE QA-OUT-ISS-CNC(SUB1, 1) = QA-OUT-ISS-CNC(SUB1, 1) + 00001683
                                 ET-ISS-CNC(SUB1, 17).                  00001683
            COMPUTE QA-OUT-ISS-CNC(SUB1, 2) =                           00001683
                    QA-OUT-ISS-CNC(SUB1, 2) + ET-ISS-CNC(SUB1, 18).     00001683
            COMPUTE IND-LIFE-TOT = IND-LIFE-TOT +                       00001669
                                         QA-OUT-ISS-CNC(SUB1, 1).       00001669
            COMPUTE ALL-ISS-AMT(SUB1, 1) = ALL-ISS-AMT(SUB1, 1)         00001669
                                         +  QA-OUT-ISS-CNC(SUB1, 1).    00001669
            COMPUTE IND-AH-TOT = IND-AH-TOT +                           00001669
                                         QA-OUT-ISS-CNC(SUB1, 2).       00001669
            COMPUTE ALL-ISS-AMT(SUB1, 2) = ALL-ISS-AMT(SUB1, 2)         00001669
                                         +  QA-OUT-ISS-CNC(SUB1, 2).    00001669
            IF QA-OUT-ISS-CNC (SUB1, 1) NOT = 0                         00001669
                 MOVE QA-OUT-ISS-CNC(SUB1, 1) TO IND-LIFE-ISS-PRINT     00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF QA-OUT-ISS-CNC (SUB1, 2) NOT = 0                         00001669
                 MOVE QA-OUT-ISS-CNC(SUB1, 2) TO IND-AH-ISS-PRINT       00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            MOVE ET-CNT(SUB1, 3) TO PRINT-ISS-CNT-IND.                  00001669
            MOVE ET-CNC-CNT(SUB1, 3) TO PRINT-CNC-CNT-IND.              00001669
            MOVE ET-CNT(SUB1, 4) TO PRINT-ISS-CNT-GRP.                  00001669
            MOVE ET-CNC-CNT(SUB1, 4) TO PRINT-CNC-CNT-GRP.              00001669
            COMPUTE TOT-ISS-IND-CNT = ET-CNT(SUB1, 3) + TOT-ISS-IND-CNT.00001669
            COMPUTE TOT-ISS-GRP-CNT = ET-CNT(SUB1, 4) + TOT-ISS-GRP-CNT.00001669
            COMPUTE TOT-CNC-IND-CNT                                     00001669
                              = ET-CNC-CNT(SUB1, 3) + TOT-CNC-IND-CNT.  00001669
            COMPUTE TOT-CNC-GRP-CNT                                     00001669
                              = ET-CNC-CNT(SUB1, 4) + TOT-CNC-GRP-CNT.  00001669
            COMPUTE NET-IND-PRINT = QA-OUT-ISS-CNC(SUB1, 1) -           00001669
                                   QA-OUT-ISS-CNC(SUB1, 2).             00001669
            COMPUTE NET-IND-CNT =                                       00001669
                                ET-CNT(SUB1, 3) - ET-CNC-CNT(SUB1, 3).  00001669
            COMPUTE NET-GRP-CNT =                                       00001669
                                ET-CNT(SUB1, 4) - ET-CNC-CNT(SUB1, 4).  00001669
            COMPUTE NET-IND-ISS-CNT                                     00000989
                                  = TOT-ISS-IND-CNT - TOT-CNC-IND-CNT.  00000989
            COMPUTE NET-GRP-ISS-CNT                                     00000989
                                  = TOT-ISS-GRP-CNT - TOT-CNC-GRP-CNT.  00000989
            MOVE TOT-ISS-IND-CNT TO ISS-CNT-IND.                        00001669
            MOVE TOT-ISS-GRP-CNT TO ISS-CNT-GRP.                        00001669
            MOVE TOT-CNC-IND-CNT TO CNC-CNT-IND.                        00001669
            MOVE TOT-CNC-GRP-CNT TO CNC-CNT-GRP.                        00001669
            IF (STATE-ISS-PRINT NOT = '               '  AND            00001669
                      DONE = 'N')                                       00001669
                 PERFORM B041-FIRST-LINES.                              00001399
            IF LINE-CT > 55                                             00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF PREV-TYPE NOT = TYPE-OUT AND                             00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF  STATE-ISS-PRINT NOT = '               '   AND           00001669
                      IND-LIFE-TOT NOT = 0                              00001669
                 WRITE REP-OUT-REC FROM PRINT-LINE15 AFTER              00001399
                                                 ADVANCING 2 LINES      00001399
                 COMPUTE LINE-CT = LINE-CT + 1                          00001399
                 MOVE ZEROS TO IND-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO IND-AH-ISS-PRINT                         00001399
                 MOVE ZEROS TO GRP-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO GRP-AH-ISS-PRINT                         00001399
                 MOVE '               ' TO STATE-ISS-PRINT.             00001399
       B044-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001692
       B045-PROCESSING.                                                 00001663
            COMPUTE SUB1 = SUB1 + 1.                                    00001669
            MOVE '                ALL BUSINESS                      '   00001683
                                                          TO TYPE-OUT.  00001683
            IF ALL-ISS-AMT (SUB1, 1) NOT = 0                            00001669
                 MOVE ALL-ISS-AMT(SUB1, 1) TO IND-LIFE-ISS-PRINT        00001669
                 COMPUTE TOT-LIFE-IND = TOT-LIFE-IND +                  00001669
                      ALL-ISS-AMT(SUB1, 1)                              00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF ALL-ISS-AMT (SUB1, 2) NOT = 0                            00001669
                 MOVE ALL-ISS-AMT(SUB1, 2) TO IND-AH-ISS-PRINT          00001669
                 COMPUTE TOT-AH-IND     = TOT-AH-IND     +              00001669
                      ALL-ISS-AMT(SUB1, 2)                              00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF ALL-ISS-AMT (SUB1, 3) NOT = 0                            00001669
                 MOVE ALL-ISS-AMT(SUB1, 3) TO GRP-LIFE-ISS-PRINT        00001669
                 COMPUTE TOT-LIFE-GRP = TOT-LIFE-GRP +                  00001669
                      ALL-ISS-AMT(SUB1, 3)                              00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            IF ALL-ISS-AMT (SUB1, 4) NOT = 0                            00001669
                 MOVE ALL-ISS-AMT(SUB1, 4) TO GRP-AH-ISS-PRINT          00001669
                 COMPUTE TOT-AH-GRP = TOT-AH-GRP +                      00001669
                      ALL-ISS-AMT(SUB1, 4)                              00001669
                 MOVE ET-NAME(SUB1) TO STATE-ISS-PRINT.                 00001669
            COMPUTE ALL-CNT-ISS-IND =                                   00001669
                                 ET-CNT(SUB1, 1) + ET-CNT(SUB1, 3) +    00001669
                                 ET-CNT(SUB1, 5).                       00001669
            COMPUTE ALL-CNT-ISS-GRP =                                   00001669
                                 ET-CNT(SUB1, 2) + ET-CNT(SUB1, 4) +    00001669
                                 ET-CNT(SUB1, 6).                       00001669
            COMPUTE ALL-CNT-CNC-IND =                                   00001669
                          ET-CNC-CNT(SUB1, 1) + ET-CNC-CNT(SUB1, 3) +   00001669
                          ET-CNC-CNT(SUB1, 5).                          00001669
            COMPUTE ALL-CNT-CNC-GRP =                                   00001669
                          ET-CNC-CNT(SUB1, 2) + ET-CNC-CNT(SUB1, 4) +   00001669
                          ET-CNC-CNT(SUB1, 6).                          00001669
            COMPUTE NET-IND-PRINT = ALL-ISS-AMT(SUB1, 1) -              00001669
                                    ALL-ISS-AMT(SUB1, 2).               00001669
            COMPUTE NET-GRP-PRINT = ALL-ISS-AMT(SUB1, 3) -              00001669
                                    ALL-ISS-AMT(SUB1, 4).               00001669
            COMPUTE NET-IND-CNT =                                       00001669
                          ALL-CNT-ISS-IND - ALL-CNT-CNC-IND.            00001669
            COMPUTE NET-GRP-CNT =                                       00001669
                          ALL-CNT-ISS-GRP - ALL-CNT-CNC-GRP.            00001669
            COMPUTE TOT-ISS-IND-CNT = ALL-CNT-ISS-IND + TOT-ISS-IND-CNT.00001669
            COMPUTE TOT-ISS-GRP-CNT = ALL-CNT-ISS-GRP + TOT-ISS-GRP-CNT.00001669
            COMPUTE TOT-CNC-IND-CNT                                     00001669
                              = ALL-CNT-CNC-IND + TOT-CNC-IND-CNT.      00001669
            COMPUTE TOT-CNC-GRP-CNT                                     00001669
                              = ALL-CNT-CNC-GRP + TOT-CNC-GRP-CNT.      00001669
            MOVE ALL-CNT-ISS-IND TO PRINT-ISS-CNT-IND.                  00001669
            MOVE ALL-CNT-CNC-IND TO PRINT-CNC-CNT-IND.                  00001669
            MOVE ALL-CNT-ISS-GRP TO PRINT-ISS-CNT-GRP.                  00001669
            MOVE ALL-CNT-CNC-GRP TO PRINT-CNC-CNT-GRP.                  00001669
            MOVE TOT-ISS-IND-CNT TO ISS-CNT-IND-ALL.                    00001669
            MOVE TOT-ISS-GRP-CNT TO ISS-CNT-GRP-ALL.                    00001669
            MOVE TOT-CNC-IND-CNT TO CNC-CNT-IND-ALL.                    00001669
            MOVE TOT-CNC-GRP-CNT TO CNC-CNT-GRP-ALL.                    00001669
            IF (STATE-ISS-PRINT NOT = '               '  AND            00001669
                      DONE = 'N')                                       00001669
                 PERFORM B041-FIRST-LINES.                              00001399
            IF LINE-CT > 55                                             00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF PREV-TYPE NOT = TYPE-OUT                                 00001669
                 PERFORM B024-PRINT-HEADINGS.                           00001669
            IF  STATE-ISS-PRINT NOT = '               '   AND           00001669
                      IND-LIFE-ISS-PRINT NOT = 0                        00001669
                 WRITE REP-OUT-REC FROM PRINT-LINE15 AFTER              00001399
                                                 ADVANCING 2 LINES      00001399
                 COMPUTE LINE-CT = LINE-CT + 1                          00001399
                 MOVE ZEROS TO IND-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO IND-AH-ISS-PRINT                         00001399
                 MOVE ZEROS TO GRP-LIFE-ISS-PRINT                       00001399
                 MOVE ZEROS TO GRP-AH-ISS-PRINT                         00001399
                 MOVE '               ' TO STATE-ISS-PRINT.             00001399
       B045-EXIT.                                                       00001691
           EXIT.                                                        00001692
                                                                        00001692
       B048-CHECK-LIFE.                                                 00001694
           IF ((CR-LFTYP = '29'  OR                                     00001695
              CR-LFTYP = '31'  OR                                       00001700
              CR-LFTYP = '32'  OR                                       00001700
              CR-LFTYP = '45'  OR                                       00001710
              CR-LFTYP = '76') AND                                      00001715
              CR-LF-TERM > 120 AND                                      00001711
              CR-LF-CURRENT-STATUS NOT = 'V' AND                        00001712
              CR-LF-CURRENT-STATUS NOT = 'D' AND                        00001713
Y2KMOD        WS-ENTRY-YR = WS-YY)                                      00001714
                MOVE 01 TO SUB2                                         00001720
                MOVE 01 TO SUB3                                         00001720
                COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2)         00001721
                                                         + CR-LFPRM     00001721
                COMPUTE ET-CNT(SUB1, 1) = ET-CNT(SUB1, 1) + 1           00001721
                MOVE WK-STATE TO ET-NAME(SUB1)                          00001721
                COMPUTE ET-ISS-CNC(SUB1, SUB3) =                        00001721
                                ET-ISS-CNC(SUB1, SUB3) + CR-LFAMT.      00001721
           IF ((CR-LFTYP = '30'  OR                                     00001724
              CR-LFTYP = '33'  OR                                       00001729
              CR-LFTYP = '38'  OR                                       00001734
              CR-LFTYP = '46'  OR                                       00001739
              CR-LFTYP = '77') AND                                      00001744
              CR-LF-TERM > 120 AND                                      00001745
              CR-LF-CURRENT-STATUS NOT = 'V' AND                        00001746
              CR-LF-CURRENT-STATUS NOT = 'D' AND                        00001747
Y2KMOD        WS-ENTRY-YR = WS-YY)                                      00001748
                MOVE 03 TO SUB2                                         00001749
                MOVE 03 TO SUB3                                         00001749
                COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2)         00001750
                                                         + CR-LFPRM     00001750
                COMPUTE ET-CNT(SUB1, 2) = ET-CNT(SUB1, 2) + 1           00001721
                MOVE WK-STATE TO ET-NAME(SUB1)                          00001721
                COMPUTE ET-ISS-CNC(SUB1, SUB3) =                        00001721
                                ET-ISS-CNC(SUB1, SUB3) + CR-LFAMT.      00001721
           PERFORM B049-CHECK-LIFE-QA.                                  00001752
       B048-EXIT.                                                       00001753
           EXIT.                                                        00001754
                                                                        00001755
       B049-CHECK-LIFE-QA.                                              00001756
           IF ((CR-LFTYP = 'QD'  OR                                     00001757
               CR-LFTYP = 'QL') AND                                     00001762
               CR-LF-TERM > 120 AND                                     00001763
               CR-LF-CURRENT-STATUS NOT = 'V' AND                       00001764
               CR-LF-CURRENT-STATUS NOT = 'D' AND                       00001765
Y2KMOD         WS-ENTRY-YR = WS-YY)                                     00001770
            MOVE 17 TO SUB2                                             00001792
            MOVE 17 TO SUB3                                             00001792
            COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2) + CR-LFPRM  00001793
            COMPUTE ET-CNT(SUB1, 3) = ET-CNT(SUB1, 3) + 1               00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            COMPUTE ET-ISS-CNC(SUB1, SUB3) =                            00001721
                                ET-ISS-CNC(SUB1, SUB3) + CR-LFAMT       00001721
            PERFORM B050-CHECK-AH.                                      00001796
       B049-EXIT.                                                       00001797
           EXIT.                                                        00001798
                                                                        00001799
       B050-CHECK-AH.                                                   00001800
           IF ((CR-AHTYP = '38'  OR                                     00001801
               CR-AHTYP = '39'  OR                                      00001806
               CR-AHTYP = '40'  OR                                      00001811
               CR-AHTYP = '41'  OR                                      00001816
               CR-AHTYP = '42'  OR                                      00001821
               CR-AHTYP = '63'  OR                                      00001826
               CR-AHTYP = '64'  OR                                      00001831
               CR-AHTYP = '65'  OR                                      00001836
               CR-AHTYP = '66'  OR                                      00001841
               CR-AHTYP = '67'  OR                                      00001846
               CR-AHTYP = '2C'  OR                                      00001851
               CR-AHTYP = '2D'  OR                                      00001856
               CR-AHTYP = '2E'  OR                                      00001861
               CR-AHTYP = '2F'  OR                                      00001866
               CR-AHTYP = '2G'  OR                                      00001871
               CR-AHTYP = '2H'  OR                                      00001876
               CR-AHTYP = '2I'  OR                                      00001881
               CR-AHTYP = '2J'  OR                                      00001886
               CR-AHTYP = '2K'  OR                                      00001891
               CR-AHTYP = '2L'  OR                                      00001896
               CR-AHTYP = '2Q'  OR                                      00001901
               CR-AHTYP = '2R'  OR                                      00001906
               CR-AHTYP = '2S'  OR                                      00001911
               CR-AHTYP = '2T'  OR                                      00001916
               CR-AHTYP = '2U'  OR                                      00001921
               CR-AHTYP = '5U'  OR                                      00001926
               CR-AHTYP = '5V'  OR                                      00001931
               CR-AHTYP = '5W'  OR                                      00001936
               CR-AHTYP = '5X'  OR                                      00001941
               CR-AHTYP = '5Y'  OR                                      00001946
               CR-AHTYP = '5Z'  OR                                      00001951
               CR-AHTYP = '6A'  OR                                      00001956
               CR-AHTYP = '6B'  OR                                      00001961
               CR-AHTYP = '6C'  OR                                      00001966
               CR-AHTYP = '6D'  OR                                      00001971
               CR-AHTYP = '6E'  OR                                      00001976
               CR-AHTYP = '6F'  OR                                      00001981
               CR-AHTYP = '6G'  OR                                      00001986
               CR-AHTYP = '6H'  OR                                      00001991
               CR-AHTYP = '6I') AND                                     00001996
               CR-AH-TERM > 120 AND                                     00001997
               CR-AH-CURRENT-STATUS NOT = 'V' AND                       00001998
               CR-AH-CURRENT-STATUS NOT = 'D' AND                       00001999
Y2KMOD         WS-ENTRY-YR = WS-YY)                                     00002000
            MOVE 05 TO SUB2                                             00002001
            COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2) + CR-AHPRM  00002002
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            PERFORM B060-CHECK-OT-LIFE.                                 00002005
       B050-EXIT.                                                       00002010
           EXIT.                                                        00002100
                                                                        00002101
       B060-CHECK-OT-LIFE.                                              00002102
           IF ((CR-LFTYP  = '01'  OR                                    00002106
               CR-LFTYP  = '02'  OR                                     00002107
               CR-LFTYP  = '05'  OR                                     00002108
               CR-LFTYP  = '19'  OR                                     00002109
               CR-LFTYP  = '20'  OR                                     00002110
               CR-LFTYP  = '22'  OR                                     00002111
               CR-LFTYP  = '24'  OR                                     00002112
               CR-LFTYP  = '26'  OR                                     00002113
               CR-LFTYP  = '35'  OR                                     00002114
               CR-LFTYP  = '36'  OR                                     00002115
               CR-LFTYP  = '39'  OR                                     00002116
               CR-LFTYP  = '41'  OR                                     00002117
               CR-LFTYP  = '43'  OR                                     00002118
               CR-LFTYP  = '47'  OR                                     00002119
               CR-LFTYP  = '49'  OR                                     00002120
               CR-LFTYP  = '51'  OR                                     00002121
               CR-LFTYP  = '57'  OR                                     00002122
               CR-LFTYP  = '59'  OR                                     00002123
               CR-LFTYP  = '62'  OR                                     00002124
               CR-LFTYP  = '64'  OR                                     00002125
               CR-LFTYP  = '66'  OR                                     00002126
               CR-LFTYP  = '68'  OR                                     00002127
               CR-LFTYP  = '70'  OR                                     00002128
               CR-LFTYP  = '72'  OR                                     00002129
               CR-LFTYP  = '74'  OR                                     00002130
               CR-LFTYP  = '78'  OR                                     00002131
               CR-LFTYP  = '80'  OR                                     00002132
               CR-LFTYP  = '82'  OR                                     00002134
               CR-LFTYP  = '84'  OR                                     00002135
               CR-LFTYP  = '86'  OR                                     00002136
               CR-LFTYP  = '88'  OR                                     00002137
               CR-LFTYP  = '1A'  OR                                     00002138
               CR-LFTYP  = '1C'  OR                                     00002139
               CR-LFTYP  = '1E'  OR                                     00002140
               CR-LFTYP  = '1G'  OR                                     00002141
               CR-LFTYP  = '1I'  OR                                     00002142
               CR-LFTYP  = '1K') AND                                    00002143
               CR-LF-TERM > 120 AND                                     00002147
               CR-LF-CURRENT-STATUS NOT = 'V' AND                       00002148
               CR-LF-CURRENT-STATUS NOT = 'D' AND                       00002149
Y2KMOD         WS-ENTRY-YR = WS-YY)                                     00002150
            MOVE 09 TO SUB2                                             00003500
            MOVE 09 TO SUB3                                             00003500
            COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2) + CR-LFPRM  00003700
            COMPUTE ET-CNT(SUB1, 5) = ET-CNT(SUB1, 5) + 1               00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            COMPUTE ET-ISS-CNC(SUB1, SUB3) =                            00001721
                                ET-ISS-CNC(SUB1, SUB3) + CR-LFAMT       00001721
            PERFORM B070-CHECK-OT-AH.                                   00004220
       B060-EXIT.                                                       00004300
           EXIT.                                                        00004400
                                                                        00004500
       B070-CHECK-OT-AH.                                                00004600
           IF ((CR-AHTYP = '01' OR                                      00004720
                CR-AHTYP = '02' OR                                      00004721
                CR-AHTYP = '03' OR                                      00004722
                CR-AHTYP = '04' OR                                      00004723
                CR-AHTYP = '05' OR                                      00004724
                CR-AHTYP = '11' OR                                      00004725
                CR-AHTYP = '12' OR                                      00004726
                CR-AHTYP = '13' OR                                      00004727
                CR-AHTYP = '14' OR                                      00004728
                CR-AHTYP = '15' OR                                      00004729
                CR-AHTYP = '24' OR                                      00004730
                CR-AHTYP = '25' OR                                      00004731
                CR-AHTYP = '26' OR                                      00004732
                CR-AHTYP = '27' OR                                      00004733
                CR-AHTYP = '43' OR                                      00004734
                CR-AHTYP = '48' OR                                      00004735
                CR-AHTYP = '49' OR                                      00004736
                CR-AHTYP = '50' OR                                      00004737
                CR-AHTYP = '51' OR                                      00004738
                CR-AHTYP = '52' OR                                      00004739
                CR-AHTYP = '68' OR                                      00004740
                CR-AHTYP = '69' OR                                      00004741
                CR-AHTYP = '70' OR                                      00004742
                CR-AHTYP = '71' OR                                      00004743
                CR-AHTYP = '72' OR                                      00004744
                CR-AHTYP = '73' OR                                      00004745
                CR-AHTYP = '74' OR                                      00004746
                CR-AHTYP = '75' OR                                      00004747
                CR-AHTYP = '76' OR                                      00004748
                CR-AHTYP = '77' OR                                      00004749
                CR-AHTYP = '78' OR                                      00004750
                CR-AHTYP = '79' OR                                      00004751
                CR-AHTYP = '80' OR                                      00004752
                CR-AHTYP = '81' OR                                      00004753
                CR-AHTYP = '82' OR                                      00004754
                CR-AHTYP = '83' OR                                      00004755
                CR-AHTYP = '84' OR                                      00004756
                CR-AHTYP = '85' OR                                      00004757
                CR-AHTYP = '86' OR                                      00004758
                CR-AHTYP = '87' OR                                      00004759
                CR-AHTYP = '2M' OR                                      00004760
                CR-AHTYP = '2N' OR                                      00004761
                CR-AHTYP = '2O' OR                                      00004762
                CR-AHTYP = '2P' OR                                      00004763
                CR-AHTYP = '3K' OR                                      00004764
                CR-AHTYP = '3L' OR                                      00004765
                CR-AHTYP = '3S' OR                                      00004766
                CR-AHTYP = '3T' OR                                      00004767
                CR-AHTYP = '3U' OR                                      00004768
                CR-AHTYP = '3V' OR                                      00004769
                CR-AHTYP = '3W' OR                                      00004770
                CR-AHTYP = '3X' OR                                      00004771
                CR-AHTYP = '3Y' OR                                      00004772
                CR-AHTYP = '3Z' OR                                      00004773
                CR-AHTYP = '4A' OR                                      00004774
                CR-AHTYP = '4B' OR                                      00004775
                CR-AHTYP = '4C' OR                                      00004776
                CR-AHTYP = '4D' OR                                      00004777
                CR-AHTYP = '4G' OR                                      00004778
                CR-AHTYP = '4H' OR                                      00004779
                CR-AHTYP = '4I' OR                                      00004780
                CR-AHTYP = '4J' OR                                      00004781
                CR-AHTYP = '4K' OR                                      00004782
                CR-AHTYP = '5A' OR                                      00004783
                CR-AHTYP = '5B' OR                                      00004784
                CR-AHTYP = '5C' OR                                      00004785
                CR-AHTYP = '5D' OR                                      00004786
                CR-AHTYP = '5E' OR                                      00004787
                CR-AHTYP = '5F' OR                                      00004788
                CR-AHTYP = '5G' OR                                      00004789
                CR-AHTYP = '5M' OR                                      00004790
                CR-AHTYP = '5N' OR                                      00004791
                CR-AHTYP = '5O' OR                                      00004792
                CR-AHTYP = '5P' OR                                      00004793
                CR-AHTYP = '5Q' OR                                      00004794
                CR-AHTYP = '5R' OR                                      00004795
                CR-AHTYP = '5S' OR                                      00004796
                CR-AHTYP = '5T') AND                                    00004797
               CR-AH-TERM > 120 AND                                     00004799
               CR-AH-CURRENT-STATUS NOT = 'V' AND                       00004800
               CR-AH-CURRENT-STATUS NOT = 'D' AND                       00004801
Y2KMOD         WS-ENTRY-YR = WS-YY)                                     00004802
            MOVE 13 TO SUB2                                             00005037
            COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2) + CR-AHPRM  00005038
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
            PERFORM B080-CHECK-OT-LIFE-GP.                              00005040
       B070-EXIT.                                                       00005050
           EXIT.                                                        00005100
                                                                        00005110
       B080-CHECK-OT-LIFE-GP.                                           00005200
           IF ((CR-LFTYP = '03'  OR                                     00005300
               CR-LFTYP  = '04'  OR                                     00005400
               CR-LFTYP  = '06'  OR                                     00005500
               CR-LFTYP  = '09'  OR                                     00005600
               CR-LFTYP  = '18'  OR                                     00005700
               CR-LFTYP  = '21'  OR                                     00005800
               CR-LFTYP  = '23'  OR                                     00005900
               CR-LFTYP  = '25'  OR                                     00006000
               CR-LFTYP  = '27'  OR                                     00006100
               CR-LFTYP  = '28'  OR                                     00006200
               CR-LFTYP  = '34'  OR                                     00006300
               CR-LFTYP  = '37'  OR                                     00006310
               CR-LFTYP  = '40'  OR                                     00006400
               CR-LFTYP  = '42'  OR                                     00006500
               CR-LFTYP  = '44'  OR                                     00006600
               CR-LFTYP  = '48'  OR                                     00006700
               CR-LFTYP  = '50'  OR                                     00006800
               CR-LFTYP  = '52'  OR                                     00006900
               CR-LFTYP  = '58'  OR                                     00007000
               CR-LFTYP  = '60'  OR                                     00007100
               CR-LFTYP  = '61'  OR                                     00007200
               CR-LFTYP  = '63'  OR                                     00007300
               CR-LFTYP  = '65'  OR                                     00007400
               CR-LFTYP  = '67'  OR                                     00007410
               CR-LFTYP  = '69'  OR                                     00007420
               CR-LFTYP  = '71'  OR                                     00007500
               CR-LFTYP  = '73'  OR                                     00007600
               CR-LFTYP  = '75'  OR                                     00007700
               CR-LFTYP  = '79'  OR                                     00007800
               CR-LFTYP  = '81'  OR                                     00007900
               CR-LFTYP  = '83'  OR                                     00008000
               CR-LFTYP  = '85'  OR                                     00008100
               CR-LFTYP  = '87'  OR                                     00008200
               CR-LFTYP  = '89'  OR                                     00008300
               CR-LFTYP  = '1B'  OR                                     00008400
               CR-LFTYP  = '1D'  OR                                     00008500
               CR-LFTYP  = '1F'  OR                                     00008600
               CR-LFTYP  = '1H'  OR                                     00008700
               CR-LFTYP  = '1J'  OR                                     00008800
               CR-LFTYP  = '1L') AND                                    00008900
               CR-LF-TERM > 120 AND                                     00009000
               CR-LF-CURRENT-STATUS NOT = 'V' AND                       00009100
               CR-LF-CURRENT-STATUS NOT = 'D' AND                       00009200
Y2KMOD         WS-ENTRY-YR = WS-YY)                                     00009300
            MOVE 11 TO SUB2                                             00009400
            MOVE 11 TO SUB3                                             00009400
            COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2) + CR-LFPRM  00009500
            COMPUTE ET-CNT(SUB1, 6) = ET-CNT(SUB1, 6) + 1               00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            COMPUTE ET-ISS-CNC(SUB1, SUB3) =                            00001721
                                ET-ISS-CNC(SUB1, SUB3) + CR-LFAMT       00001721
            PERFORM B090-CHECK-OT-AH-GP.                                00009700
       B080-EXIT.                                                       00009800
           EXIT.                                                        00009900
                                                                        00009910
       B090-CHECK-OT-AH-GP.                                             00010000
           IF ((CR-AHTYP = '09' OR                                      00010100
                CR-AHTYP = '20' OR                                      00011100
                CR-AHTYP = '21' OR                                      00011200
                CR-AHTYP = '22' OR                                      00011300
                CR-AHTYP = '23' OR                                      00011400
                CR-AHTYP = '28' OR                                      00011500
                CR-AHTYP = '29' OR                                      00011600
                CR-AHTYP = '30' OR                                      00011700
                CR-AHTYP = '31' OR                                      00011900
                CR-AHTYP = '32' OR                                      00012000
                CR-AHTYP = '33' OR                                      00012010
                CR-AHTYP = '34' OR                                      00012020
                CR-AHTYP = '35' OR                                      00012030
                CR-AHTYP = '36' OR                                      00012040
                CR-AHTYP = '37' OR                                      00012050
                CR-AHTYP = '44' OR                                      00012060
                CR-AHTYP = '45' OR                                      00012070
                CR-AHTYP = '46' OR                                      00012080
                CR-AHTYP = '47' OR                                      00012090
                CR-AHTYP = '53' OR                                      00012091
                CR-AHTYP = '54' OR                                      00012092
                CR-AHTYP = '55' OR                                      00012093
                CR-AHTYP = '56' OR                                      00012094
                CR-AHTYP = '57' OR                                      00012095
                CR-AHTYP = '58' OR                                      00012096
                CR-AHTYP = '59' OR                                      00012097
                CR-AHTYP = '60' OR                                      00012098
                CR-AHTYP = '61' OR                                      00012099
                CR-AHTYP = '62' OR                                      00012100
                CR-AHTYP = '88' OR                                      00012110
                CR-AHTYP = '89' OR                                      00012120
                CR-AHTYP = '1A' OR                                      00012200
                CR-AHTYP = '1B' OR                                      00012210
                CR-AHTYP = '1C' OR                                      00012220
                CR-AHTYP = '1D' OR                                      00012230
                CR-AHTYP = '1E' OR                                      00012240
                CR-AHTYP = '1F' OR                                      00012250
                CR-AHTYP = '1G' OR                                      00012260
                CR-AHTYP = '1H' OR                                      00012270
                CR-AHTYP = '1I' OR                                      00012280
                CR-AHTYP = '1J' OR                                      00012290
                CR-AHTYP = '1K' OR                                      00012291
                CR-AHTYP = '1L' OR                                      00012300
                CR-AHTYP = '1M' OR                                      00012400
                CR-AHTYP = '1N' OR                                      00012500
                CR-AHTYP = '1O' OR                                      00012600
                CR-AHTYP = '1P' OR                                      00012700
                CR-AHTYP = '1Q' OR                                      00012800
                CR-AHTYP = '1R' OR                                      00012900
                CR-AHTYP = '1S' OR                                      00013000
                CR-AHTYP = '1T' OR                                      00013100
                CR-AHTYP = '1U' OR                                      00013200
                CR-AHTYP = '1V' OR                                      00013300
                CR-AHTYP = '1W' OR                                      00013400
                CR-AHTYP = '1X' OR                                      00013500
                CR-AHTYP = '1Y' OR                                      00013510
                CR-AHTYP = '1Z' OR                                      00013520
                CR-AHTYP = '2A' OR                                      00013530
                CR-AHTYP = '2B' OR                                      00013540
                CR-AHTYP = '2V' OR                                      00014100
                CR-AHTYP = '2W' OR                                      00014200
                CR-AHTYP = '2X' OR                                      00014300
                CR-AHTYP = '2Y' OR                                      00014400
                CR-AHTYP = '2Z' OR                                      00014410
                CR-AHTYP = '3A' OR                                      00014500
                CR-AHTYP = '3B' OR                                      00014600
                CR-AHTYP = '3C' OR                                      00014700
                CR-AHTYP = '3D' OR                                      00014800
                CR-AHTYP = '3E' OR                                      00014900
                CR-AHTYP = '3F' OR                                      00015000
                CR-AHTYP = '3G' OR                                      00015100
                CR-AHTYP = '3H' OR                                      00015200
                CR-AHTYP = '3I' OR                                      00015300
                CR-AHTYP = '3J' OR                                      00015400
                CR-AHTYP = '3M' OR                                      00015410
                CR-AHTYP = '3N' OR                                      00015420
                CR-AHTYP = '3O' OR                                      00015430
                CR-AHTYP = '3P' OR                                      00015440
                CR-AHTYP = '3Q' OR                                      00015450
                CR-AHTYP = '3R' OR                                      00015460
                CR-AHTYP = '4E' OR                                      00015500
                CR-AHTYP = '4F' OR                                      00015600
                CR-AHTYP = '4L' OR                                      00015700
                CR-AHTYP = '4M' OR                                      00015800
                CR-AHTYP = '4N' OR                                      00015900
                CR-AHTYP = '4O' OR                                      00016000
                CR-AHTYP = '4P' OR                                      00016100
                CR-AHTYP = '4Q' OR                                      00016200
                CR-AHTYP = '4R' OR                                      00016300
                CR-AHTYP = '4S' OR                                      00016310
                CR-AHTYP = '4T' OR                                      00016320
                CR-AHTYP = '4U' OR                                      00016330
                CR-AHTYP = '4V' OR                                      00016340
                CR-AHTYP = '4W' OR                                      00016350
                CR-AHTYP = '4X' OR                                      00016360
                CR-AHTYP = '4Y' OR                                      00016370
                CR-AHTYP = '4Z' OR                                      00016380
                CR-AHTYP = '5H' OR                                      00016390
                CR-AHTYP = '5I' OR                                      00016391
                CR-AHTYP = '5J' OR                                      00016400
                CR-AHTYP = '5K' OR                                      00016500
                CR-AHTYP = '5L') AND                                    00016600
               CR-AH-TERM > 120 AND                                     00017900
               CR-AH-CURRENT-STATUS NOT = 'V' AND                       00018000
               CR-AH-CURRENT-STATUS NOT = 'D' AND                       00018100
Y2KMOD         WS-ENTRY-YR = WS-YY)                                     00018200
            MOVE 15 TO SUB2                                             00018300
            COMPUTE ET-AMT(SUB1, SUB2) = ET-AMT(SUB1, SUB2) + CR-AHPRM  00018400
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
            PERFORM B100-CHECK-LIFE-ET-REFUND.                          00018600
       B090-EXIT.                                                       00018700
           EXIT.                                                        00018800
                                                                        00018810
       B100-CHECK-LIFE-ET-REFUND.                                       00018900
           IF ((CR-LFTYP = '29'  OR                                     00019010
               CR-LFTYP = '31'  OR                                      00019500
               CR-LFTYP = '32'  OR                                      00020000
               CR-LFTYP = '45'  OR                                      00020500
               CR-LFTYP = '76') AND                                     00021000
               CR-LF-TERM > 120 AND                                     00021100
               CR-LF-CURRENT-STATUS = '8' AND                           00021110
Y2KMOD         WS-LF-CEX-YR = WS-YY)                                    00021120
            MOVE 02 TO SUB2                                             00021500
            MOVE 02 TO SUB3                                             00021500
            COMPUTE ET-AMT(SUB1, SUB2) =                                00021600
                                      ET-AMT(SUB1, SUB2) + CR-LFRFND    00021610
            COMPUTE ET-CNC-CNT(SUB1, 1) = ET-CNC-CNT(SUB1, 1) + 1       00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            MOVE WK-STATE TO ET-NAME-IN(SUB1)                           00001721
Y2KMOD      COMPUTE WK-TERM = ((WS-LF-CNC-CNYR - WS-CR-CNYR) * 12) +    00010100
Y2KMOD                        (WS-LF-CNC-MO - WS-CR-MO)                 00010100
            MOVE 'Y' TO CALC-TOTAL                                      00010100
Y2KMOD      IF WS-LF-CNC-DA LESS THAN WS-CR-DA                          00010100
                 SUBTRACT 1 FROM WK-TERM.                               00010100
            IF ((CR-LFTYP = '30'  OR                                    00021800
               CR-LFTYP = '33'  OR                                      00022300
               CR-LFTYP = '38'  OR                                      00022800
               CR-LFTYP = '46'  OR                                      00023300
               CR-LFTYP = '77') AND                                     00023800
               CR-LF-TERM > 120 AND                                     00023900
               CR-LF-CURRENT-STATUS = '8' AND                           00023910
Y2KMOD         WS-LF-CEX-YR = WS-YY)                                    00023920
            MOVE 04 TO SUB2                                             00024300
            MOVE 04 TO SUB3                                             00024300
            COMPUTE ET-AMT(SUB1, SUB2) =                                00024400
                                        ET-AMT(SUB1, SUB2) + CR-LFRFND  00024410
            COMPUTE ET-CNC-CNT(SUB1, 2) = ET-CNC-CNT(SUB1, 2) + 1       00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            MOVE WK-STATE TO ET-NAME-IN(SUB1)                           00001721
Y2KMOD      COMPUTE WK-TERM = ((WS-LF-CNC-CNYR - WS-CR-CNYR) * 12) +    00010100
Y2KMOD                        (WS-LF-CNC-MO - WS-CR-MO)                 00010100
            MOVE 'Y' TO CALC-TOTAL                                      00010100
Y2KMOD      IF WS-LF-CNC-DA LESS THAN WS-CR-DA                          00010100
                 SUBTRACT 1 FROM WK-TERM.                               00010100
            PERFORM B105-CHECK-LIFE-QA-REFUND.                          00024600
       B100-EXIT.                                                       00024700
           EXIT.                                                        00024800
                                                                        00024900
       B105-CHECK-LIFE-QA-REFUND.                                       00025000
            IF ((CR-LFTYP = 'QD'  OR                                    00025100
               CR-LFTYP = 'QL') AND                                     00025600
               CR-LF-TERM > 120 AND                                     00025700
               CR-LF-CURRENT-STATUS = '8' AND                           00025800
Y2KMOD         WS-LF-CEX-YR = WS-YY)                                    00025900
            MOVE 18 TO SUB2                                             00026100
            MOVE 18 TO SUB3                                             00026100
            COMPUTE ET-AMT(SUB1, SUB2) =                                00026200
                                       ET-AMT(SUB1, SUB2) + CR-LFRFND   00026300
            COMPUTE ET-CNC-CNT(SUB1, 3) = ET-CNC-CNT(SUB1, 3) + 1       00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            MOVE WK-STATE TO ET-NAME-IN(SUB1)                           00001721
Y2KMOD      COMPUTE WK-TERM = ((WS-LF-CNC-CNYR - WS-CR-CNYR) * 12) +    00010100
Y2KMOD                         (WS-LF-CNC-MO - WS-CR-MO)                00010100
            MOVE 'Y' TO CALC-TOTAL                                      00010100
Y2KMOD      IF WS-LF-CNC-DA LESS THAN WS-CR-DA                          00010100
                 SUBTRACT 1 FROM WK-TERM.                               00010100
            PERFORM B110-CHECK-AH-REFUND.                               00026400
       B105-EXIT.                                                       00026500
           EXIT.                                                        00026600
                                                                        00026700
       B110-CHECK-AH-REFUND.                                            00026800
           IF ((CR-AHTYP = '38'  OR                                     00026900
               CR-AHTYP = '39'  OR                                      00027400
               CR-AHTYP = '40'  OR                                      00027900
               CR-AHTYP = '41'  OR                                      00028400
               CR-AHTYP = '42'  OR                                      00028900
               CR-AHTYP = '63'  OR                                      00029400
               CR-AHTYP = '64'  OR                                      00029900
               CR-AHTYP = '65'  OR                                      00030400
               CR-AHTYP = '66'  OR                                      00030900
               CR-AHTYP = '67'  OR                                      00031400
               CR-AHTYP = '2C'  OR                                      00031900
               CR-AHTYP = '2D'  OR                                      00032400
               CR-AHTYP = '2E'  OR                                      00032900
               CR-AHTYP = '2F'  OR                                      00033400
               CR-AHTYP = '2G'  OR                                      00033900
               CR-AHTYP = '2H'  OR                                      00034400
               CR-AHTYP = '2I'  OR                                      00034900
               CR-AHTYP = '2J'  OR                                      00035400
               CR-AHTYP = '2K'  OR                                      00035900
               CR-AHTYP = '2L'  OR                                      00036400
               CR-AHTYP = '2Q'  OR                                      00036900
               CR-AHTYP = '2R'  OR                                      00037400
               CR-AHTYP = '2S'  OR                                      00037900
               CR-AHTYP = '2T'  OR                                      00038400
               CR-AHTYP = '2U'  OR                                      00038900
               CR-AHTYP = '5U'  OR                                      00039400
               CR-AHTYP = '5V'  OR                                      00039900
               CR-AHTYP = '5W'  OR                                      00040400
               CR-AHTYP = '5X'  OR                                      00040900
               CR-AHTYP = '5Y'  OR                                      00041400
               CR-AHTYP = '5Z'  OR                                      00041900
               CR-AHTYP = '6A'  OR                                      00042400
               CR-AHTYP = '6B'  OR                                      00042900
               CR-AHTYP = '6C'  OR                                      00043400
               CR-AHTYP = '6D'  OR                                      00043900
               CR-AHTYP = '6E'  OR                                      00044400
               CR-AHTYP = '6F'  OR                                      00044900
               CR-AHTYP = '6G'  OR                                      00045400
               CR-AHTYP = '6H'  OR                                      00045900
               CR-AHTYP = '6I') AND                                     00046400
               CR-AH-TERM > 120 AND                                     00046500
               CR-AH-CURRENT-STATUS = '8' AND                           00046700
Y2KMOD         WS-AH-CEX-YR = WS-YY)                                    00046810
            MOVE 06 TO SUB2                                             00046900
            COMPUTE ET-AMT(SUB1, SUB2) =                                00047000
                                       ET-AMT(SUB1, SUB2) + CR-AHRFND   00047010
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
            PERFORM B120-CHECK-OT-LIFE-RFND.                            00047200
       B110-EXIT.                                                       00047300
           EXIT.                                                        00047400
                                                                        00047410
       B120-CHECK-OT-LIFE-RFND.                                         00047500
           IF ((CR-LFTYP  = '01'  OR                                    00047600
               CR-LFTYP  = '02'  OR                                     00047700
               CR-LFTYP  = '05'  OR                                     00047800
               CR-LFTYP  = '19'  OR                                     00047900
               CR-LFTYP  = '20'  OR                                     00048000
               CR-LFTYP  = '22'  OR                                     00048100
               CR-LFTYP  = '24'  OR                                     00048200
               CR-LFTYP  = '26'  OR                                     00048300
               CR-LFTYP  = '35'  OR                                     00048400
               CR-LFTYP  = '36'  OR                                     00048500
               CR-LFTYP  = '39'  OR                                     00048600
               CR-LFTYP  = '41'  OR                                     00048700
               CR-LFTYP  = '43'  OR                                     00048800
               CR-LFTYP  = '47'  OR                                     00048900
               CR-LFTYP  = '49'  OR                                     00049000
               CR-LFTYP  = '51'  OR                                     00049100
               CR-LFTYP  = '57'  OR                                     00049200
               CR-LFTYP  = '59'  OR                                     00049300
               CR-LFTYP  = '62'  OR                                     00049400
               CR-LFTYP  = '64'  OR                                     00049500
               CR-LFTYP  = '66'  OR                                     00049600
               CR-LFTYP  = '68'  OR                                     00049700
               CR-LFTYP  = '70'  OR                                     00049800
               CR-LFTYP  = '72'  OR                                     00049900
               CR-LFTYP  = '74'  OR                                     00050000
               CR-LFTYP  = '78'  OR                                     00050100
               CR-LFTYP  = '80'  OR                                     00050200
               CR-LFTYP  = '82'  OR                                     00050300
               CR-LFTYP  = '84'  OR                                     00050400
               CR-LFTYP  = '86'  OR                                     00050500
               CR-LFTYP  = '88'  OR                                     00050600
               CR-LFTYP  = '1A'  OR                                     00050700
               CR-LFTYP  = '1C'  OR                                     00050800
               CR-LFTYP  = '1E'  OR                                     00050900
               CR-LFTYP  = '1G'  OR                                     00051000
               CR-LFTYP  = '1I'  OR                                     00051100
               CR-LFTYP  = '1K') AND                                    00051200
               CR-LF-TERM > 120 AND                                     00051300
               CR-LF-CURRENT-STATUS = '8' AND                           00051400
Y2KMOD         WS-LF-CEX-YR = WS-YY)                                    00051500
            MOVE 10 TO SUB2                                             00051700
            MOVE 10 TO SUB3                                             00051700
            COMPUTE ET-AMT(SUB1, SUB2) =                                00051800
                                       ET-AMT(SUB1, SUB2) + CR-LFRFND   00051810
            COMPUTE ET-CNC-CNT(SUB1, 5) = ET-CNC-CNT(SUB1, 5) + 1       00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            MOVE WK-STATE TO ET-NAME-IN(SUB1)                           00001721
Y2KMOD      COMPUTE WK-TERM = ((WS-LF-CNC-CNYR - WS-CR-CNYR) * 12) +    00010100
Y2KMOD                         (WS-LF-CNC-MO - WS-CR-MO)                00010100
            MOVE 'Y' TO CALC-TOTAL                                      00010100
Y2KMOD      IF WS-LF-CNC-DA LESS THAN WS-CR-DA                          00010100
                 SUBTRACT 1 FROM WK-TERM.                               00010100
            PERFORM B130-CHECK-OT-AH-REFUND.                            00052000
       B120-EXIT.                                                       00052100
           EXIT.                                                        00052200
                                                                        00052300
       B130-CHECK-OT-AH-REFUND.                                         00052400
           IF ((CR-AHTYP = '01' OR                                      00052500
                CR-AHTYP = '02' OR                                      00052600
                CR-AHTYP = '03' OR                                      00052700
                CR-AHTYP = '04' OR                                      00052800
                CR-AHTYP = '05' OR                                      00052900
                CR-AHTYP = '11' OR                                      00053000
                CR-AHTYP = '12' OR                                      00053100
                CR-AHTYP = '13' OR                                      00053200
                CR-AHTYP = '14' OR                                      00053300
                CR-AHTYP = '15' OR                                      00053400
                CR-AHTYP = '24' OR                                      00053500
                CR-AHTYP = '25' OR                                      00053600
                CR-AHTYP = '26' OR                                      00053700
                CR-AHTYP = '27' OR                                      00053800
                CR-AHTYP = '43' OR                                      00053900
                CR-AHTYP = '48' OR                                      00054000
                CR-AHTYP = '49' OR                                      00054100
                CR-AHTYP = '50' OR                                      00054200
                CR-AHTYP = '51' OR                                      00054300
                CR-AHTYP = '52' OR                                      00054400
                CR-AHTYP = '68' OR                                      00054500
                CR-AHTYP = '69' OR                                      00054600
                CR-AHTYP = '70' OR                                      00054700
                CR-AHTYP = '71' OR                                      00054800
                CR-AHTYP = '72' OR                                      00054900
                CR-AHTYP = '73' OR                                      00055000
                CR-AHTYP = '74' OR                                      00055100
                CR-AHTYP = '75' OR                                      00055200
                CR-AHTYP = '76' OR                                      00055300
                CR-AHTYP = '77' OR                                      00055400
                CR-AHTYP = '78' OR                                      00055500
                CR-AHTYP = '79' OR                                      00055600
                CR-AHTYP = '80' OR                                      00055700
                CR-AHTYP = '81' OR                                      00055800
                CR-AHTYP = '82' OR                                      00055900
                CR-AHTYP = '83' OR                                      00056000
                CR-AHTYP = '84' OR                                      00056100
                CR-AHTYP = '85' OR                                      00056200
                CR-AHTYP = '86' OR                                      00056300
                CR-AHTYP = '87' OR                                      00056400
                CR-AHTYP = '2M' OR                                      00056500
                CR-AHTYP = '2N' OR                                      00056600
                CR-AHTYP = '2O' OR                                      00056700
                CR-AHTYP = '2P' OR                                      00056800
                CR-AHTYP = '3K' OR                                      00056900
                CR-AHTYP = '3L' OR                                      00057000
                CR-AHTYP = '3S' OR                                      00057100
                CR-AHTYP = '3T' OR                                      00057200
                CR-AHTYP = '3U' OR                                      00057300
                CR-AHTYP = '3V' OR                                      00057400
                CR-AHTYP = '3W' OR                                      00057500
                CR-AHTYP = '3X' OR                                      00057600
                CR-AHTYP = '3Y' OR                                      00057700
                CR-AHTYP = '3Z' OR                                      00057800
                CR-AHTYP = '4A' OR                                      00057900
                CR-AHTYP = '4B' OR                                      00058000
                CR-AHTYP = '4C' OR                                      00058100
                CR-AHTYP = '4D' OR                                      00058200
                CR-AHTYP = '4G' OR                                      00058300
                CR-AHTYP = '4H' OR                                      00058400
                CR-AHTYP = '4I' OR                                      00058500
                CR-AHTYP = '4J' OR                                      00058600
                CR-AHTYP = '4K' OR                                      00058700
                CR-AHTYP = '5A' OR                                      00058800
                CR-AHTYP = '5B' OR                                      00058900
                CR-AHTYP = '5C' OR                                      00059000
                CR-AHTYP = '5D' OR                                      00059100
                CR-AHTYP = '5E' OR                                      00059200
                CR-AHTYP = '5F' OR                                      00059300
                CR-AHTYP = '5G' OR                                      00059400
                CR-AHTYP = '5M' OR                                      00059500
                CR-AHTYP = '5N' OR                                      00059600
                CR-AHTYP = '5O' OR                                      00059700
                CR-AHTYP = '5P' OR                                      00059800
                CR-AHTYP = '5Q' OR                                      00059900
                CR-AHTYP = '5R' OR                                      00060000
                CR-AHTYP = '5S' OR                                      00060100
                CR-AHTYP = '5T') AND                                    00060200
               CR-AH-TERM > 120 AND                                     00060300
               CR-AH-CURRENT-STATUS = '8' AND                           00060310
Y2KMOD         WS-AH-CEX-YR = WS-YY)                                    00060320
            MOVE 14 TO SUB2                                             00060700
            COMPUTE ET-AMT(SUB1, SUB2) =                                00060800
                                       ET-AMT(SUB1, SUB2) + CR-AHRFND   00060810
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B140-CHECK-OT-LIFE-GP-REFUND.                        00061000
       B130-EXIT.                                                       00061100
           EXIT.                                                        00061200
                                                                        00061300
       B140-CHECK-OT-LIFE-GP-REFUND.                                    00061400
           IF ((CR-LFTYP = '03'  OR                                     00061500
               CR-LFTYP  = '04'  OR                                     00061600
               CR-LFTYP  = '06'  OR                                     00061700
               CR-LFTYP  = '09'  OR                                     00061800
               CR-LFTYP  = '18'  OR                                     00061900
               CR-LFTYP  = '21'  OR                                     00062000
               CR-LFTYP  = '23'  OR                                     00062100
               CR-LFTYP  = '25'  OR                                     00062200
               CR-LFTYP  = '27'  OR                                     00062300
               CR-LFTYP  = '28'  OR                                     00062400
               CR-LFTYP  = '34'  OR                                     00062500
               CR-LFTYP  = '37'  OR                                     00062600
               CR-LFTYP  = '40'  OR                                     00062700
               CR-LFTYP  = '42'  OR                                     00062800
               CR-LFTYP  = '44'  OR                                     00062900
               CR-LFTYP  = '48'  OR                                     00063000
               CR-LFTYP  = '50'  OR                                     00063100
               CR-LFTYP  = '52'  OR                                     00063200
               CR-LFTYP  = '58'  OR                                     00063300
               CR-LFTYP  = '60'  OR                                     00063400
               CR-LFTYP  = '61'  OR                                     00063500
               CR-LFTYP  = '63'  OR                                     00063600
               CR-LFTYP  = '65'  OR                                     00063700
               CR-LFTYP  = '67'  OR                                     00063800
               CR-LFTYP  = '69'  OR                                     00063900
               CR-LFTYP  = '71'  OR                                     00064000
               CR-LFTYP  = '73'  OR                                     00064100
               CR-LFTYP  = '75'  OR                                     00064200
               CR-LFTYP  = '79'  OR                                     00064300
               CR-LFTYP  = '81'  OR                                     00064400
               CR-LFTYP  = '83'  OR                                     00064500
               CR-LFTYP  = '85'  OR                                     00064600
               CR-LFTYP  = '87'  OR                                     00064700
               CR-LFTYP  = '89'  OR                                     00064800
               CR-LFTYP  = '1B'  OR                                     00064900
               CR-LFTYP  = '1D'  OR                                     00065000
               CR-LFTYP  = '1F'  OR                                     00065100
               CR-LFTYP  = '1H'  OR                                     00065200
               CR-LFTYP  = '1J'  OR                                     00065300
               CR-LFTYP  = '1L') AND                                    00065400
               CR-LF-TERM > 120 AND                                     00065500
               CR-LF-CURRENT-STATUS = '8' AND                           00065510
Y2KMOD         WS-LF-CEX-YR = WS-YY)                                    00065520
            MOVE 12 TO SUB2                                             00065900
            MOVE 12 TO SUB3                                             00065900
            COMPUTE ET-AMT(SUB1, SUB2) =                                00066000
                                       ET-AMT(SUB1, SUB2) + CR-LFRFND   00066010
            COMPUTE ET-CNC-CNT(SUB1, 6) = ET-CNC-CNT(SUB1, 6) + 1       00001721
            MOVE WK-STATE TO ET-NAME(SUB1)                              00001721
            MOVE WK-STATE TO ET-NAME-IN(SUB1)                           00001721
Y2KMOD      COMPUTE WK-TERM = ((WS-LF-CNC-CNYR - WS-CR-CNYR) * 12) +    00010100
Y2KMOD                         (WS-LF-CNC-MO - WS-CR-MO)                00010100
            MOVE 'Y' TO CALC-TOTAL                                      00010100
Y2KMOD      IF WS-LF-CNC-DA LESS THAN WS-CR-DA                          00010100
                 SUBTRACT 1 FROM WK-TERM.                               00010100
            PERFORM B150-CHECK-OT-AH-GP-REFUND.                         00066200
       B140-EXIT.                                                       00066300
           EXIT.                                                        00066400
                                                                        00066500
       B150-CHECK-OT-AH-GP-REFUND.                                      00066600
           IF ((CR-AHTYP = '09' OR                                      00066700
                CR-AHTYP = '20' OR                                      00066800
                CR-AHTYP = '21' OR                                      00066900
                CR-AHTYP = '22' OR                                      00067000
                CR-AHTYP = '23' OR                                      00067100
                CR-AHTYP = '28' OR                                      00067200
                CR-AHTYP = '29' OR                                      00067300
                CR-AHTYP = '30' OR                                      00067400
                CR-AHTYP = '31' OR                                      00067500
                CR-AHTYP = '32' OR                                      00067600
                CR-AHTYP = '33' OR                                      00067700
                CR-AHTYP = '34' OR                                      00067800
                CR-AHTYP = '35' OR                                      00067900
                CR-AHTYP = '36' OR                                      00068000
                CR-AHTYP = '37' OR                                      00068100
                CR-AHTYP = '44' OR                                      00068200
                CR-AHTYP = '45' OR                                      00068300
                CR-AHTYP = '46' OR                                      00068400
                CR-AHTYP = '47' OR                                      00068500
                CR-AHTYP = '53' OR                                      00068600
                CR-AHTYP = '54' OR                                      00068700
                CR-AHTYP = '55' OR                                      00068800
                CR-AHTYP = '56' OR                                      00068900
                CR-AHTYP = '57' OR                                      00069000
                CR-AHTYP = '58' OR                                      00069100
                CR-AHTYP = '59' OR                                      00069200
                CR-AHTYP = '60' OR                                      00069300
                CR-AHTYP = '61' OR                                      00069400
                CR-AHTYP = '62' OR                                      00069500
                CR-AHTYP = '88' OR                                      00069600
                CR-AHTYP = '89' OR                                      00069700
                CR-AHTYP = '1A' OR                                      00069800
                CR-AHTYP = '1B' OR                                      00069900
                CR-AHTYP = '1C' OR                                      00070000
                CR-AHTYP = '1D' OR                                      00070100
                CR-AHTYP = '1E' OR                                      00070200
                CR-AHTYP = '1F' OR                                      00070300
                CR-AHTYP = '1G' OR                                      00070400
                CR-AHTYP = '1H' OR                                      00070500
                CR-AHTYP = '1I' OR                                      00070600
                CR-AHTYP = '1J' OR                                      00070700
                CR-AHTYP = '1K' OR                                      00070800
                CR-AHTYP = '1L' OR                                      00070900
                CR-AHTYP = '1M' OR                                      00071000
                CR-AHTYP = '1N' OR                                      00071100
                CR-AHTYP = '1O' OR                                      00071200
                CR-AHTYP = '1P' OR                                      00071300
                CR-AHTYP = '1Q' OR                                      00071400
                CR-AHTYP = '1R' OR                                      00071500
                CR-AHTYP = '1S' OR                                      00071600
                CR-AHTYP = '1T' OR                                      00071700
                CR-AHTYP = '1U' OR                                      00071800
                CR-AHTYP = '1V' OR                                      00071900
                CR-AHTYP = '1W' OR                                      00072000
                CR-AHTYP = '1X' OR                                      00072100
                CR-AHTYP = '1Y' OR                                      00072200
                CR-AHTYP = '1Z' OR                                      00072300
                CR-AHTYP = '2A' OR                                      00072400
                CR-AHTYP = '2B' OR                                      00072500
                CR-AHTYP = '2V' OR                                      00072600
                CR-AHTYP = '2W' OR                                      00072700
                CR-AHTYP = '2X' OR                                      00072800
                CR-AHTYP = '2Y' OR                                      00072900
                CR-AHTYP = '2Z' OR                                      00073000
                CR-AHTYP = '3A' OR                                      00073100
                CR-AHTYP = '3B' OR                                      00073200
                CR-AHTYP = '3C' OR                                      00073300
                CR-AHTYP = '3D' OR                                      00073400
                CR-AHTYP = '3E' OR                                      00073500
                CR-AHTYP = '3F' OR                                      00073600
                CR-AHTYP = '3G' OR                                      00073700
                CR-AHTYP = '3H' OR                                      00073800
                CR-AHTYP = '3I' OR                                      00073900
                CR-AHTYP = '3J' OR                                      00074000
                CR-AHTYP = '3M' OR                                      00074100
                CR-AHTYP = '3N' OR                                      00074200
                CR-AHTYP = '3O' OR                                      00074300
                CR-AHTYP = '3P' OR                                      00074400
                CR-AHTYP = '3Q' OR                                      00074500
                CR-AHTYP = '3R' OR                                      00074600
                CR-AHTYP = '4E' OR                                      00074700
                CR-AHTYP = '4F' OR                                      00074800
                CR-AHTYP = '4L' OR                                      00074900
                CR-AHTYP = '4M' OR                                      00075000
                CR-AHTYP = '4N' OR                                      00075100
                CR-AHTYP = '4O' OR                                      00075200
                CR-AHTYP = '4P' OR                                      00075300
                CR-AHTYP = '4Q' OR                                      00075400
                CR-AHTYP = '4R' OR                                      00075500
                CR-AHTYP = '4S' OR                                      00075600
                CR-AHTYP = '4T' OR                                      00075700
                CR-AHTYP = '4U' OR                                      00075800
                CR-AHTYP = '4V' OR                                      00075900
                CR-AHTYP = '4W' OR                                      00076000
                CR-AHTYP = '4X' OR                                      00076100
                CR-AHTYP = '4Y' OR                                      00076200
                CR-AHTYP = '4Z' OR                                      00076300
                CR-AHTYP = '5H' OR                                      00076400
                CR-AHTYP = '5I' OR                                      00076500
                CR-AHTYP = '5J' OR                                      00076600
                CR-AHTYP = '5K' OR                                      00076700
                CR-AHTYP = '5L') AND                                    00076800
               CR-AH-TERM > 120 AND                                     00076900
               CR-AH-CURRENT-STATUS = '8' AND                           00076910
Y2KMOD         WS-AH-CEX-YR = WS-YY)                                    00076920
            MOVE 16 TO SUB2                                             00077300
            COMPUTE ET-AMT(SUB1, SUB2) =                                00077400
                                       ET-AMT(SUB1, SUB2) + CR-AHRFND   00077410
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B240-CHECK-LIFE.                                     00066200
       B150-EXIT.                                                       00077700
           EXIT.                                                        00077800
                                                                        00077900
       B240-CHECK-LIFE.                                                 00001694
           IF ((CR-LFTYP = '29'  OR                                     00001695
               CR-LFTYP = '31'  OR                                      00001700
               CR-LFTYP = '32'  OR                                      00001705
               CR-LFTYP = '45'  OR                                      00001710
               CR-LFTYP = '76') AND                                     00001715
               CR-LF-TERM > 120 AND                                     00001716
               CR-DTH-RPT-YR = WS-YY)                                   00001719
            MOVE 01 TO SUB2                                             00001720
            COMPUTE ET-CLM(SUB1, SUB2) = ET-CLM(SUB1, SUB2) + CR-DTHAMT 00001721
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           IF ((CR-LFTYP = '30' OR                                      00001724
               CR-LFTYP = '33'  OR                                      00001729
               CR-LFTYP = '38'  OR                                      00001734
               CR-LFTYP = '46'  OR                                      00001739
               CR-LFTYP = '77')  AND                                    00001744
               CR-LF-TERM > 120 AND                                     00001745
               CR-DTH-RPT-YR = WS-YY)                                   00001748
            MOVE 02 TO SUB2                                             00001749
            COMPUTE ET-CLM(SUB1, SUB2) = ET-CLM(SUB1, SUB2) + CR-DTHAMT 00001750
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B245-CHECK-LIFE-QA.                                  00001752
       B240-EXIT.                                                       00001753
           EXIT.                                                        00001754
                                                                        00001755
       B245-CHECK-LIFE-QA.                                              00001756
           IF ((CR-LFTYP = 'QD'  OR                                     00001757
               CR-LFTYP = 'QL')  AND                                    00001762
               CR-LF-TERM > 120 AND                                     00001763
               CR-DTH-RPT-YR = WS-YY)                                   00001770
            MOVE 09 TO SUB2                                             00001792
            COMPUTE ET-CLM(SUB1, SUB2) = ET-CLM(SUB1, SUB2) + CR-DTHAMT 00001793
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B250-CHECK-AH.                                       00001796
       B245-EXIT.                                                       00001797
           EXIT.                                                        00001798
                                                                        00001799
       B250-CHECK-AH.                                                   00001800
           IF ((CR-AHTYP = '38'  OR                                     00001801
               CR-AHTYP = '39'  OR                                      00001806
               CR-AHTYP = '40'  OR                                      00001811
               CR-AHTYP = '41'  OR                                      00001816
               CR-AHTYP = '42'  OR                                      00001821
               CR-AHTYP = '63'  OR                                      00001826
               CR-AHTYP = '64'  OR                                      00001831
               CR-AHTYP = '65'  OR                                      00001836
               CR-AHTYP = '66'  OR                                      00001841
               CR-AHTYP = '67'  OR                                      00001846
               CR-AHTYP = '2C'  OR                                      00001851
               CR-AHTYP = '2D'  OR                                      00001856
               CR-AHTYP = '2E'  OR                                      00001861
               CR-AHTYP = '2F'  OR                                      00001866
               CR-AHTYP = '2G'  OR                                      00001871
               CR-AHTYP = '2H'  OR                                      00001876
               CR-AHTYP = '2I'  OR                                      00001881
               CR-AHTYP = '2J'  OR                                      00001886
               CR-AHTYP = '2K'  OR                                      00001891
               CR-AHTYP = '2L'  OR                                      00001896
               CR-AHTYP = '2Q'  OR                                      00001901
               CR-AHTYP = '2R'  OR                                      00001906
               CR-AHTYP = '2S'  OR                                      00001911
               CR-AHTYP = '2T'  OR                                      00001916
               CR-AHTYP = '2U'  OR                                      00001921
               CR-AHTYP = '5U'  OR                                      00001926
               CR-AHTYP = '5V'  OR                                      00001931
               CR-AHTYP = '5W'  OR                                      00001936
               CR-AHTYP = '5X'  OR                                      00001941
               CR-AHTYP = '5Y'  OR                                      00001946
               CR-AHTYP = '5Z'  OR                                      00001951
               CR-AHTYP = '6A'  OR                                      00001956
               CR-AHTYP = '6B'  OR                                      00001961
               CR-AHTYP = '6C'  OR                                      00001966
               CR-AHTYP = '6D'  OR                                      00001971
               CR-AHTYP = '6E'  OR                                      00001976
               CR-AHTYP = '6F'  OR                                      00001981
               CR-AHTYP = '6G'  OR                                      00001986
               CR-AHTYP = '6H'  OR                                      00001991
               CR-AHTYP = '6I') AND                                     00001996
               CR-AH-TERM > 120 AND                                     00001997
Y2KMOD         WS-DIS-PAY-YR = WS-YY)                                   00002000
            MOVE 03 TO SUB2                                             00002001
            COMPUTE ET-CLM(SUB1, SUB2) =                                00002002
                                    ET-CLM(SUB1, SUB2) + CR-DISAMT-YTD  00002002
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B260-CHECK-OT-LIFE.                                  00002005
       B250-EXIT.                                                       00002010
           EXIT.                                                        00002100
                                                                        00002101
       B260-CHECK-OT-LIFE.                                              00002102
           IF ((CR-LFTYP  = '01'  OR                                    00002106
               CR-LFTYP  = '02'  OR                                     00002107
               CR-LFTYP  = '05'  OR                                     00002108
               CR-LFTYP  = '19'  OR                                     00002109
               CR-LFTYP  = '20'  OR                                     00002110
               CR-LFTYP  = '22'  OR                                     00002111
               CR-LFTYP  = '24'  OR                                     00002112
               CR-LFTYP  = '26'  OR                                     00002113
               CR-LFTYP  = '35'  OR                                     00002114
               CR-LFTYP  = '36'  OR                                     00002115
               CR-LFTYP  = '39'  OR                                     00002116
               CR-LFTYP  = '41'  OR                                     00002117
               CR-LFTYP  = '43'  OR                                     00002118
               CR-LFTYP  = '47'  OR                                     00002119
               CR-LFTYP  = '49'  OR                                     00002120
               CR-LFTYP  = '51'  OR                                     00002121
               CR-LFTYP  = '57'  OR                                     00002122
               CR-LFTYP  = '59'  OR                                     00002123
               CR-LFTYP  = '62'  OR                                     00002124
               CR-LFTYP  = '64'  OR                                     00002125
               CR-LFTYP  = '66'  OR                                     00002126
               CR-LFTYP  = '68'  OR                                     00002127
               CR-LFTYP  = '70'  OR                                     00002128
               CR-LFTYP  = '72'  OR                                     00002129
               CR-LFTYP  = '74'  OR                                     00002130
               CR-LFTYP  = '78'  OR                                     00002131
               CR-LFTYP  = '80'  OR                                     00002132
               CR-LFTYP  = '82'  OR                                     00002134
               CR-LFTYP  = '84'  OR                                     00002135
               CR-LFTYP  = '86'  OR                                     00002136
               CR-LFTYP  = '88'  OR                                     00002137
               CR-LFTYP  = '1A'  OR                                     00002138
               CR-LFTYP  = '1C'  OR                                     00002139
               CR-LFTYP  = '1E'  OR                                     00002140
               CR-LFTYP  = '1G'  OR                                     00002141
               CR-LFTYP  = '1I'  OR                                     00002142
               CR-LFTYP  = '1K') AND                                    00002143
               CR-LF-TERM > 120 AND                                     00002147
               CR-DTH-RPT-YR = WS-YY)                                   00002150
            MOVE 05 TO SUB2                                             00003500
            COMPUTE ET-CLM(SUB1, SUB2) = ET-CLM(SUB1, SUB2) + CR-DTHAMT 00003700
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B270-CHECK-OT-AH.                                    00002005
       B260-EXIT.                                                       00004300
           EXIT.                                                        00004400
                                                                        00004500
       B270-CHECK-OT-AH.                                                00004600
           IF ((CR-AHTYP = '01' OR                                      00004720
                CR-AHTYP = '02' OR                                      00004721
                CR-AHTYP = '03' OR                                      00004722
                CR-AHTYP = '04' OR                                      00004723
                CR-AHTYP = '05' OR                                      00004724
                CR-AHTYP = '11' OR                                      00004725
                CR-AHTYP = '12' OR                                      00004726
                CR-AHTYP = '13' OR                                      00004727
                CR-AHTYP = '14' OR                                      00004728
                CR-AHTYP = '15' OR                                      00004729
                CR-AHTYP = '24' OR                                      00004730
                CR-AHTYP = '25' OR                                      00004731
                CR-AHTYP = '26' OR                                      00004732
                CR-AHTYP = '27' OR                                      00004733
                CR-AHTYP = '43' OR                                      00004734
                CR-AHTYP = '48' OR                                      00004735
                CR-AHTYP = '49' OR                                      00004736
                CR-AHTYP = '50' OR                                      00004737
                CR-AHTYP = '51' OR                                      00004738
                CR-AHTYP = '52' OR                                      00004739
                CR-AHTYP = '68' OR                                      00004740
                CR-AHTYP = '69' OR                                      00004741
                CR-AHTYP = '70' OR                                      00004742
                CR-AHTYP = '71' OR                                      00004743
                CR-AHTYP = '72' OR                                      00004744
                CR-AHTYP = '73' OR                                      00004745
                CR-AHTYP = '74' OR                                      00004746
                CR-AHTYP = '75' OR                                      00004747
                CR-AHTYP = '76' OR                                      00004748
                CR-AHTYP = '77' OR                                      00004749
                CR-AHTYP = '78' OR                                      00004750
                CR-AHTYP = '79' OR                                      00004751
                CR-AHTYP = '80' OR                                      00004752
                CR-AHTYP = '81' OR                                      00004753
                CR-AHTYP = '82' OR                                      00004754
                CR-AHTYP = '83' OR                                      00004755
                CR-AHTYP = '84' OR                                      00004756
                CR-AHTYP = '85' OR                                      00004757
                CR-AHTYP = '86' OR                                      00004758
                CR-AHTYP = '87' OR                                      00004759
                CR-AHTYP = '2M' OR                                      00004760
                CR-AHTYP = '2N' OR                                      00004761
                CR-AHTYP = '2O' OR                                      00004762
                CR-AHTYP = '2P' OR                                      00004763
                CR-AHTYP = '3K' OR                                      00004764
                CR-AHTYP = '3L' OR                                      00004765
                CR-AHTYP = '3S' OR                                      00004766
                CR-AHTYP = '3T' OR                                      00004767
                CR-AHTYP = '3U' OR                                      00004768
                CR-AHTYP = '3V' OR                                      00004769
                CR-AHTYP = '3W' OR                                      00004770
                CR-AHTYP = '3X' OR                                      00004771
                CR-AHTYP = '3Y' OR                                      00004772
                CR-AHTYP = '3Z' OR                                      00004773
                CR-AHTYP = '4A' OR                                      00004774
                CR-AHTYP = '4B' OR                                      00004775
                CR-AHTYP = '4C' OR                                      00004776
                CR-AHTYP = '4D' OR                                      00004777
                CR-AHTYP = '4G' OR                                      00004778
                CR-AHTYP = '4H' OR                                      00004779
                CR-AHTYP = '4I' OR                                      00004780
                CR-AHTYP = '4J' OR                                      00004781
                CR-AHTYP = '4K' OR                                      00004782
                CR-AHTYP = '5A' OR                                      00004783
                CR-AHTYP = '5B' OR                                      00004784
                CR-AHTYP = '5C' OR                                      00004785
                CR-AHTYP = '5D' OR                                      00004786
                CR-AHTYP = '5E' OR                                      00004787
                CR-AHTYP = '5F' OR                                      00004788
                CR-AHTYP = '5G' OR                                      00004789
                CR-AHTYP = '5M' OR                                      00004790
                CR-AHTYP = '5N' OR                                      00004791
                CR-AHTYP = '5O' OR                                      00004792
                CR-AHTYP = '5P' OR                                      00004793
                CR-AHTYP = '5Q' OR                                      00004794
                CR-AHTYP = '5R' OR                                      00004795
                CR-AHTYP = '5S' OR                                      00004796
                CR-AHTYP = '5T')AND                                     00004797
               CR-AH-TERM > 120 AND                                     00004799
Y2KMOD         WS-DIS-PAY-YR = WS-YY)                                   00002000
            MOVE 07 TO SUB2                                             00005037
            COMPUTE ET-CLM(SUB1, SUB2) =                                00002002
                                    ET-CLM(SUB1, SUB2) + CR-DISAMT-YTD  00002002
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B280-CHECK-OT-LIFE-GP.                               00005040
       B270-EXIT.                                                       00005050
           EXIT.                                                        00005100
                                                                        00005110
       B280-CHECK-OT-LIFE-GP.                                           00005200
           IF ((CR-LFTYP = '03'  OR                                     00005300
               CR-LFTYP  = '04'  OR                                     00005400
               CR-LFTYP  = '06'  OR                                     00005500
               CR-LFTYP  = '09'  OR                                     00005600
               CR-LFTYP  = '18'  OR                                     00005700
               CR-LFTYP  = '21'  OR                                     00005800
               CR-LFTYP  = '23'  OR                                     00005900
               CR-LFTYP  = '25'  OR                                     00006000
               CR-LFTYP  = '27'  OR                                     00006100
               CR-LFTYP  = '28'  OR                                     00006200
               CR-LFTYP  = '34'  OR                                     00006300
               CR-LFTYP  = '37'  OR                                     00006310
               CR-LFTYP  = '40'  OR                                     00006400
               CR-LFTYP  = '42'  OR                                     00006500
               CR-LFTYP  = '44'  OR                                     00006600
               CR-LFTYP  = '48'  OR                                     00006700
               CR-LFTYP  = '50'  OR                                     00006800
               CR-LFTYP  = '52'  OR                                     00006900
               CR-LFTYP  = '58'  OR                                     00007000
               CR-LFTYP  = '60'  OR                                     00007100
               CR-LFTYP  = '61'  OR                                     00007200
               CR-LFTYP  = '63'  OR                                     00007300
               CR-LFTYP  = '65'  OR                                     00007400
               CR-LFTYP  = '67'  OR                                     00007410
               CR-LFTYP  = '69'  OR                                     00007420
               CR-LFTYP  = '71'  OR                                     00007500
               CR-LFTYP  = '73'  OR                                     00007600
               CR-LFTYP  = '75'  OR                                     00007700
               CR-LFTYP  = '79'  OR                                     00007800
               CR-LFTYP  = '81'  OR                                     00007900
               CR-LFTYP  = '83'  OR                                     00008000
               CR-LFTYP  = '85'  OR                                     00008100
               CR-LFTYP  = '87'  OR                                     00008200
               CR-LFTYP  = '89'  OR                                     00008300
               CR-LFTYP  = '1B'  OR                                     00008400
               CR-LFTYP  = '1D'  OR                                     00008500
               CR-LFTYP  = '1F'  OR                                     00008600
               CR-LFTYP  = '1H'  OR                                     00008700
               CR-LFTYP  = '1J'  OR                                     00008800
               CR-LFTYP  = '1L') AND                                    00008900
               CR-LF-TERM > 120 AND                                     00009000
               CR-DTH-RPT-YR = WS-YY)                                   00001748
            MOVE 06 TO SUB2                                             00001749
            COMPUTE ET-CLM(SUB1, SUB2) = ET-CLM(SUB1, SUB2) + CR-DTHAMT 00001750
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B290-CHECK-OT-AH-GP.                                 00009700
       B280-EXIT.                                                       00009800
           EXIT.                                                        00009900
                                                                        00009910
       B290-CHECK-OT-AH-GP.                                             00010000
           IF ((CR-AHTYP = '09' OR                                      00010100
                CR-AHTYP = '20' OR                                      00011100
                CR-AHTYP = '21' OR                                      00011200
                CR-AHTYP = '22' OR                                      00011300
                CR-AHTYP = '23' OR                                      00011400
                CR-AHTYP = '28' OR                                      00011500
                CR-AHTYP = '29' OR                                      00011600
                CR-AHTYP = '30' OR                                      00011700
                CR-AHTYP = '31' OR                                      00011900
                CR-AHTYP = '32' OR                                      00012000
                CR-AHTYP = '33' OR                                      00012010
                CR-AHTYP = '34' OR                                      00012020
                CR-AHTYP = '35' OR                                      00012030
                CR-AHTYP = '36' OR                                      00012040
                CR-AHTYP = '37' OR                                      00012050
                CR-AHTYP = '44' OR                                      00012060
                CR-AHTYP = '45' OR                                      00012070
                CR-AHTYP = '46' OR                                      00012080
                CR-AHTYP = '47' OR                                      00012090
                CR-AHTYP = '53' OR                                      00012091
                CR-AHTYP = '54' OR                                      00012092
                CR-AHTYP = '55' OR                                      00012093
                CR-AHTYP = '56' OR                                      00012094
                CR-AHTYP = '57' OR                                      00012095
                CR-AHTYP = '58' OR                                      00012096
                CR-AHTYP = '59' OR                                      00012097
                CR-AHTYP = '60' OR                                      00012098
                CR-AHTYP = '61' OR                                      00012099
                CR-AHTYP = '62' OR                                      00012100
                CR-AHTYP = '88' OR                                      00012110
                CR-AHTYP = '89' OR                                      00012120
                CR-AHTYP = '1A' OR                                      00012200
                CR-AHTYP = '1B' OR                                      00012210
                CR-AHTYP = '1C' OR                                      00012220
                CR-AHTYP = '1D' OR                                      00012230
                CR-AHTYP = '1E' OR                                      00012240
                CR-AHTYP = '1F' OR                                      00012250
                CR-AHTYP = '1G' OR                                      00012260
                CR-AHTYP = '1H' OR                                      00012270
                CR-AHTYP = '1I' OR                                      00012280
                CR-AHTYP = '1J' OR                                      00012290
                CR-AHTYP = '1K' OR                                      00012291
                CR-AHTYP = '1L' OR                                      00012300
                CR-AHTYP = '1M' OR                                      00012400
                CR-AHTYP = '1N' OR                                      00012500
                CR-AHTYP = '1O' OR                                      00012600
                CR-AHTYP = '1P' OR                                      00012700
                CR-AHTYP = '1Q' OR                                      00012800
                CR-AHTYP = '1R' OR                                      00012900
                CR-AHTYP = '1S' OR                                      00013000
                CR-AHTYP = '1T' OR                                      00013100
                CR-AHTYP = '1U' OR                                      00013200
                CR-AHTYP = '1V' OR                                      00013300
                CR-AHTYP = '1W' OR                                      00013400
                CR-AHTYP = '1X' OR                                      00013500
                CR-AHTYP = '1Y' OR                                      00013510
                CR-AHTYP = '1Z' OR                                      00013520
                CR-AHTYP = '2A' OR                                      00013530
                CR-AHTYP = '2B' OR                                      00013540
                CR-AHTYP = '2V' OR                                      00014100
                CR-AHTYP = '2W' OR                                      00014200
                CR-AHTYP = '2X' OR                                      00014300
                CR-AHTYP = '2Y' OR                                      00014400
                CR-AHTYP = '2Z' OR                                      00014410
                CR-AHTYP = '3A' OR                                      00014500
                CR-AHTYP = '3B' OR                                      00014600
                CR-AHTYP = '3C' OR                                      00014700
                CR-AHTYP = '3D' OR                                      00014800
                CR-AHTYP = '3E' OR                                      00014900
                CR-AHTYP = '3F' OR                                      00015000
                CR-AHTYP = '3G' OR                                      00015100
                CR-AHTYP = '3H' OR                                      00015200
                CR-AHTYP = '3I' OR                                      00015300
                CR-AHTYP = '3J' OR                                      00015400
                CR-AHTYP = '3M' OR                                      00015410
                CR-AHTYP = '3N' OR                                      00015420
                CR-AHTYP = '3O' OR                                      00015430
                CR-AHTYP = '3P' OR                                      00015440
                CR-AHTYP = '3Q' OR                                      00015450
                CR-AHTYP = '3R' OR                                      00015460
                CR-AHTYP = '4E' OR                                      00015500
                CR-AHTYP = '4F' OR                                      00015600
                CR-AHTYP = '4L' OR                                      00015700
                CR-AHTYP = '4M' OR                                      00015800
                CR-AHTYP = '4N' OR                                      00015900
                CR-AHTYP = '4O' OR                                      00016000
                CR-AHTYP = '4P' OR                                      00016100
                CR-AHTYP = '4Q' OR                                      00016200
                CR-AHTYP = '4R' OR                                      00016300
                CR-AHTYP = '4S' OR                                      00016310
                CR-AHTYP = '4T' OR                                      00016320
                CR-AHTYP = '4U' OR                                      00016330
                CR-AHTYP = '4V' OR                                      00016340
                CR-AHTYP = '4W' OR                                      00016350
                CR-AHTYP = '4X' OR                                      00016360
                CR-AHTYP = '4Y' OR                                      00016370
                CR-AHTYP = '4Z' OR                                      00016380
                CR-AHTYP = '5H' OR                                      00016390
                CR-AHTYP = '5I' OR                                      00016391
                CR-AHTYP = '5J' OR                                      00016400
                CR-AHTYP = '5K' OR                                      00016500
                CR-AHTYP = '5L') AND                                    00016600
               CR-AH-TERM > 120 AND                                     00017900
Y2KMOD         WS-DIS-PAY-YR = WS-YY)                                   00002000
            MOVE 08 TO SUB2                                             00002001
            COMPUTE ET-CLM(SUB1, SUB2) =                                00002002
                                    ET-CLM(SUB1, SUB2) + CR-DISAMT-YTD  00002002
            MOVE WK-STATE TO ET-NAME(SUB1).                             00001721
           PERFORM B300-CHECK-CNC-AMT.                                  00009700
       B290-EXIT.                                                       00018700
           EXIT.                                                        00018800
                                                                        00001360
       B300-CHECK-CNC-AMT.                                              00010000
           IF ((CR-LF-TERM > 120 AND                                    00021100
               CR-LFTYP = '07' OR                                       00010100
               CR-LFTYP = '20' OR                                       00010100
               CR-LFTYP = '21' OR                                       00010100
               CR-LFTYP = '26' OR                                       00010100
               CR-LFTYP = '27') AND                                     00010100
               CALC-TOTAL = 'Y' AND                                     00010100
               WK-TERM > 0 AND                                          00010100
               CR-LF-CURRENT-STATUS = '8' AND                           00021110
Y2KMOD         WS-LF-CNC-YR NOT = 0 AND                                 00021110
Y2KMOD         WS-LF-CEX-YR = WS-YY)                                    00021120
             PERFORM B320-COMPUTE-TEXAS.                                00010100
           IF ((CR-LF-TERM > 120 AND                                    00021100
               CR-LFTYP = 'QL'    OR                                    00010100
               CR-LFTYP = '06'    OR                                    00010100
               CR-LFTYP = '1J'    OR                                    00010100
               CR-LFTYP = '1L'    OR                                    00010100
               CR-LFTYP = 'QD'    OR                                    00010100
               CR-LFTYP = '05'    OR                                    00010100
               CR-LFTYP = '1I'    OR                                    00010100
               CR-LFTYP = '1K'    OR                                    00010100
               CR-LFTYP = '33'    OR                                    00010100
               CR-LFTYP = '35'    OR                                    00010100
               CR-LFTYP = '37'    OR                                    00010100
               CR-LFTYP = '39'    OR                                    00010100
               CR-LFTYP = '46'    OR                                    00010100
               CR-LFTYP = '52'    OR                                    00010100
               CR-LFTYP = '32'    OR                                    00010100
               CR-LFTYP = '34'    OR                                    00010100
               CR-LFTYP = '36'    OR                                    00010100
               CR-LFTYP = '40'    OR                                    00010100
               CR-LFTYP = '45'    OR                                    00010100
               CR-LFTYP = '51'    OR                                    00010100
               CR-LFTYP = '64'    OR                                    00010100
               CR-LFTYP = '66'    OR                                    00010100
               CR-LFTYP = '71'    OR                                    00010100
               CR-LFTYP = '77'    OR                                    00010100
               CR-LFTYP = '81'    OR                                    00010100
               CR-LFTYP = '87'    OR                                    00010100
               CR-LFTYP = '89'    OR                                    00010100
               CR-LFTYP = '65'    OR                                    00010100
               CR-LFTYP = '67'    OR                                    00010100
               CR-LFTYP = '76'    OR                                    00010100
               CR-LFTYP = '80'    OR                                    00010100
               CR-LFTYP = '86'    OR                                    00010100
               CR-LFTYP = '88')   AND                                   00010100
               CALC-TOTAL = 'Y'   AND                                   00010100
               WK-TERM > 0 AND                                          00010100
               CR-LF-CURRENT-STATUS = '8' AND                           00021110
Y2KMOD         WS-LF-CNC-YR NOT = 0 AND                                 00021110
Y2KMOD         WS-LF-CEX-YR = WS-YY AND                                 00021120
Y2KMOD         WS-ENTRY-YR <  WS-YY)                                    00021120
             PERFORM B310-COMPUTE-NET-PAY                               00010100
           ELSE                                                         00010100
           IF (CALC-TOTAL = 'Y'  AND                                    00010100
              (CR-LFTYP NOT = '07' AND                                  00010100
               CR-LFTYP NOT = '20' AND                                  00010100
               CR-LFTYP NOT = '21' AND                                  00010100
               CR-LFTYP NOT = '26' AND                                  00010100
               CR-LFTYP NOT = '27'))                                    00010100
            COMPUTE DOLLAR-AMT = CR-LFAMT - ((WK-TERM / CR-LF-TERM) *   00010100
                                    CR-LFAMT)                           00010100
            COMPUTE ET-ISS-CNC(SUB1, SUB3) =                            00001721
                                ET-ISS-CNC(SUB1, SUB3) + DOLLAR-AMT.    00001721
       B300-EXIT.                                                       00018700
           EXIT.                                                        00018800
                                                                        00018800
       B310-COMPUTE-NET-PAY.                                            00010000
           MOVE 'N'                            TO  CP-EARNING-METHOD.      CL*24
           IF CR-LFTYP NOT = 'QL'                                          CL*24
              MOVE 'R'                            TO  CP-BENEFIT-TYPE      CL*24
           ELSE                                                            CL*24
              MOVE 'L'                            TO  CP-BENEFIT-TYPE.     CL*24
           IF (CR-LFTYP = '05' OR                                          CL*24
               CR-LFTYP = '06' OR                                          CL*24
               CR-LFTYP = '32' OR                                          CL*24
               CR-LFTYP = '33' OR                                          CL*24
               CR-LFTYP = '51' OR                                          CL*24
               CR-LFTYP = '52')                                            CL*24
           MOVE 'A'                            TO  CP-SPECIAL-CALC-CD.     CL*24
           IF (CR-LFTYP = 'QL' OR                                          CL*24
               CR-LFTYP = 'QD')                                            CL*24
           MOVE ' '                            TO  CP-SPECIAL-CALC-CD.     CL*24
           IF (CR-LFTYP = '1J' OR                                          CL*24
               CR-LFTYP = '1I' OR                                          CL*24
               CR-LFTYP = '37' OR                                          CL*24
               CR-LFTYP = '46' OR                                          CL*24
               CR-LFTYP = '36' OR                                          CL*24
               CR-LFTYP = '45' OR                                          CL*24
               CR-LFTYP = '64' OR                                          CL*24
               CR-LFTYP = '87' OR                                          CL*24
               CR-LFTYP = '86' OR                                          CL*24
               CR-LFTYP = '65')                                            CL*24
             MOVE 'I'                       TO  CP-SPECIAL-CALC-CD.        CL*24
           IF (CR-LFTYP = '1L' OR                                          CL*24
               CR-LFTYP = '1K' OR                                          CL*24
               CR-LFTYP = '39' OR                                          CL*24
               CR-LFTYP = '40' OR                                          CL*24
               CR-LFTYP = '66' OR                                          CL*24
               CR-LFTYP = '77' OR                                          CL*24
               CR-LFTYP = '89' OR                                          CL*24
               CR-LFTYP = '67' OR                                          CL*24
               CR-LFTYP = '76' OR                                          CL*24
               CR-LFTYP = '88')                                            CL*24
             MOVE 'V'                          TO  CP-SPECIAL-CALC-CD.     CL*24
           IF (CR-LFTYP = '34' OR                                          CL*24
               CR-LFTYP = '35' OR                                          CL*24
               CR-LFTYP = '71' OR                                          CL*24
               CR-LFTYP = '80' OR                                          CL*24
               CR-LFTYP = '81')                                            CL*24
             MOVE 'T'                          TO  CP-SPECIAL-CALC-CD.     CL*24
           MOVE CR-DT                  TO  DC-GREG-DATE-1-YMD.             CL*24
           MOVE '3'                    TO  DC-OPTION-CODE.                 CL*24
           PERFORM 8500-DATE-CONVERT-ROUTINE THRU 8599-DATE-CONVERT-X.     CL*24
           MOVE DC-BIN-DATE-1          TO  CP-CERT-EFF-DT.                 CL*24
                                                                           CL*24
           IF CR-LOAN-1ST-PMT-DT IS NOT EQUAL TO ZEROS                     CL*24
               MOVE CR-LOAN-1ST-PMT-DT TO  DC-GREG-DATE-1-YMD              CL*24
               MOVE '3'                TO  DC-OPTION-CODE                  CL*24
               PERFORM 8500-DATE-CONVERT-ROUTINE THRU                      CL*24
                   8599-DATE-CONVERT-X                                     CL*24
               MOVE DC-BIN-DATE-1      TO  CP-FIRST-PAY-DATE               CL*24
           ELSE                                                            CL*24
               MOVE CP-CERT-EFF-DT     TO  DC-BIN-DATE-1                   CL*24
               MOVE +1                 TO  DC-ELAPSED-MONTHS               CL*24
               MOVE +0                 TO  DC-ELAPSED-DAYS                 CL*24
               MOVE '6'                TO  DC-OPTION-CODE                  CL*24
               PERFORM 8500-DATE-CONVERT-ROUTINE THRU                      CL*24
                   8599-DATE-CONVERT-X                                     CL*24
               MOVE DC-BIN-DATE-2      TO  CP-FIRST-PAY-DATE.              CL*24
                                                                           CL*24
           MOVE CR-LFAMT               TO  CP-ORIGINAL-BENEFIT.            CL*24
           MOVE CR-APR                 TO  CP-LOAN-APR.                    CL*24
           MOVE CR-LF-TERM             TO  CP-ORIGINAL-TERM.               CL*24
           MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM.                   CL*24
           COMPUTE CP-REMAINING-TERM = CR-LF-TERM - WK-TERM.               CL*24
           MOVE CR-STATE                       TO  CP-STATE-STD-ABBRV.     CL*26
                                                                           CL*24
      **   CALL 'ZLRAMTX' USING CALCULATION-PASS-AREA.                     CL*24
Y2KMOD     CALL 'ELRAMTX' USING CALCULATION-PASS-AREA.                     CL*24
                                                                        CSO150
           COMPUTE DOLLAR-AMT ROUNDED =                                    CL*17
                           CP-REMAMT-FACTOR * (CR-LFAMT / +1000).          CL*24
           COMPUTE ET-ISS-CNC(SUB1, SUB3) =                             00001721
                           ET-ISS-CNC(SUB1, SUB3) + DOLLAR-AMT.         00001721
       B310-EXIT.                                                       00018700
           EXIT.                                                        00018800
                                                                        00018800
       B320-COMPUTE-TEXAS.                                              00010000
           DIVIDE CR-LFAMT BY CR-LF-TERM                                   CL*24
                  GIVING TEX-FACT-1.                                       CL*24
           DIVIDE WK-TERM BY CR-PMT-FREQ                                   CL*24
                  GIVING TEX-FACT-2 REMAINDER TEX-FACT-3.                  CL*24
           COMPUTE DOLLAR-AMT =                                            CL*24
                           (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ)).      CL*24
           COMPUTE ET-ISS-CNC(SUB1, SUB3) =                             00001721
                                ET-ISS-CNC(SUB1, SUB3) + DOLLAR-AMT.    00001721
       B320-EXIT.                                                       00018700
           EXIT.                                                        00018800
       B330-CHECK.                                                      00001442
              COMPUTE SUB1 = SUB1 + 1.                                  00001443
       B330-EXIT.                                                       00001478
           EXIT.                                                        00001479
                                                                        CSO150
       EJECT                                                            CSO150
       8500-DATE-CONVERT-ROUTINE.                                       CSO150
                                                                        CSO150
           CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   CSO150
                                                                        CSO150
       8599-DATE-CONVERT-X.                                             CSO150
           EXIT.                                                        CSO150
                                                                        CSO150
                                                                        CSO150
                                                                        00005400
