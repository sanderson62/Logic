00001  IDENTIFICATION DIVISION.                                         04/20/98
00002                                                                   EL1323
00003  PROGRAM-ID.                 EL1323.                                 LV009
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/13/96 09:46:23.                    CL**7
00007 *                            VMOD=2.008.                             CL**8
00008 *                                                                 EL1323
00008 *                                                                 EL1323
00009 *AUTHOR.    LOGIC, INC.                                              CL**7
00010 *           DALLAS, TEXAS.                                           CL**7
00011                                                                   EL1323
00012 *DATE-COMPILED.                                                      CL**7
00013                                                                   EL1323
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023                                                                   EL1323
00024 *REMARKS.                                                            CL**3
00025                                                                   EL1323
00026 *        THIS PROGRAM PROVIDES THE FULL DISPLAY OF A CERTIFICATE     CL**3
00027                                                                   EL1323
00028 *    SCREENS     - EL132C - CERTIFICATE DISPLAY                      CL**3
00029                                                                   EL1323
00030 *    ENTERED BY  - EL132 - CERTIFICATE LOOKUP                        CL**3
00031                                                                   EL1323
00032 *    EXIT TO     - CALLING PROGRAM                                   CL**3
00033                                                                   EL1323
00034 *    INPUT FILE  - ELCERT - CERTIFICATE INFORCE FILE                 CL**3
00035 *                  ELCNTL - CONTROL FILE                             CL**3
00036 *                  MPPLCY - CONVENIENCE POLICY MASTER FILE           CL**6
00037 *                  MPPLAN - CONVENIENCE PRODUCER PLAN MASTER FILE    CL**6
00038                                                                   EL1323
00039 *    OUTPUT FILE - NONE                                              CL**3
00040                                                                   EL1323
00041 *    COMMAREA    - PASSED.  IF A CERTIFICATE IS SELECTED, THE        CL**3
00042 *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE      CL**3
00043 *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR           CL**3
00044 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM    CL**3
00045 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE     CL**3
00046 *                  RECORD KEY INFORMATION NEEDED BY EL1322 TO        CL**3
00047 *                  LOCATE THE CERTIFICATE.                           CL**3
00048                                                                   EL1323
00049 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL1322 OR EL150   CL**3
00050 *                  FIRST ENTRY, USE THE KEY TO THE CERTIFICATE       CL**3
00051 *                  MASTER PASSED IN THE COMMAREA TO DISPLAY THE      CL**3
00052 *                  CERTIFICATE AND RETURN WITH THE TRANSACTION OF    CL**3
00053 *                  THE CALLING PROGRAM.                              CL**3
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID TO SCREEN HEADER
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
101501******************************************************************

00054                                                                   EL1323
00055      EJECT                                                        EL1323
00056  ENVIRONMENT DIVISION.                                            EL1323
00057                                                                   EL1323
00058  DATA DIVISION.                                                   EL1323
00059                                                                   EL1323
00060  WORKING-STORAGE SECTION.                                         EL1323
00061                                                                   EL1323
00062  77  FILLER  PIC X(32)  VALUE '********************************'. EL1323
00063  77  FILLER  PIC X(32)  VALUE '*   EL1323 WORKING STORAGE     *'. EL1323
00064  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.008 **********'.    CL**8
00065                                                                   EL1323
00066  01  WS-DATE-AREA.                                                EL1323
00067      12  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL1323
00068      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.        EL1323
00069                                                                   EL1323
00070                                                                   EL1323
00071  01  ERROR-MESSAGES.                                              EL1323
00072      12  ER-0133                 PIC X(4)  VALUE '0133'.          EL1323
00073      12  ER-0205                 PIC X(4)  VALUE '0205'.          EL1323
00074      12  ER-2848                 PIC X(4)  VALUE '2848'.             CL**8
00075      12  ER-9288                 PIC X(4)  VALUE '9288'.             CL**6
00076                                                                   EL1323
00077  01  FILLER    COMP-3.                                            EL1323
00078                                                                   EL1323
00079      12  WS-TIME-WORK            PIC S9(7)   VALUE ZERO.          EL1323
00080      12  WS-TIME      REDEFINES                                   EL1323
00081          WS-TIME-WORK            PIC S9(3)V9(4).                  EL1323
00082                                                                   EL1323
00083      12  WS-ELAPSED-MONTHS       PIC S9(3)   VALUE ZERO.          EL1323
00084                                                                   EL1323
00085  01  FILLER.                                                      EL1323
00086                                                                   EL1323
00087      12  WS-CONTROL-FILE-KEY.                                     EL1323
00088          16  WS-CFK-COMPANY-ID   PIC X(3)    VALUE SPACES.        EL1323
00089          16  WS-CFK-RECORD-TYPE  PIC X       VALUE ZERO.          EL1323
00090 *            88  LF-BENEFIT-MASTER           VALUE '4'.           EL1323
00091 *            88  AH-BENEFIT-MASTER           VALUE '5'.           EL1323
00092          16  WS-CFK-ACCESS-TYPE.                                     CL**8
00093              20 WS-CFK-STATE-ACCESS.                                 CL**8
00094                 24  WS-CFK-STATE    PIC XX.                          CL**8
00095                 24  FILLER          PIC XX.                          CL**8
00096              20  WS-CFK-BENEFIT-NO REDEFINES WS-CFK-STATE-ACCESS.    CL**8
00097                 24  FILLER          PIC XX.                          CL**8
00098                 24  WS-CFK-BENEFIT  PIC XX.                          CL**8
00099          16  WS-CFK-SEQUENCE-NO  PIC S9(4)   VALUE +0    COMP.    EL1323
00100                                                                   EL1323
00101      12  WS-CLAIM-KEY.                                            EL1323
00102          16  WS-CL-COMPANY-CD    PIC X.                           EL1323
00103          16  WS-CL-CARRIER       PIC X.                           EL1323
00104          16  WS-CL-CLAIM-NO      PIC X(7).                        EL1323
00105          16  WS-CL-CERT-NO       PIC X(11).                       EL1323
00106                                                                      CL**3
00107      12  WS-ELTRLR-KEY.                                              CL**3
00108          16  FILLER              PIC X(20).                          CL**3
00109          16  WS-ELTRLR-SEQ-NO    PIC S9(04) COMP.                    CL**3
00110                                                                   EL1323
00111      12  WS-CERTIFICATE-KEY.                                      EL1323
00112          16  WS-CK-COMPANY-CD    PIC X.                           EL1323
00113          16  WS-CK-CARRIER       PIC X.                           EL1323
00114          16  WS-CK-GROUPING      PIC X(6).                        EL1323
00115          16  WS-CK-STATE         PIC XX.                          EL1323
00116          16  WS-CK-ACCOUNT       PIC X(10).                       EL1323
00117          16  WS-CK-CERT-EFF-DT   PIC XX.                          EL1323
00118          16  WS-CK-CERT-NO.                                       EL1323
00119              20  WS-CK-CERT-PRIME PIC X(10).                      EL1323
00120              20  WS-CK-CERT-SFX  PIC X.                           EL1323
00121                                                                   EL1323
00122      12  EMPLCY-KEY.                                                 CL**6
00123          16  EMPLCY-COMPANY-CD   PIC X(01).                          CL**6
00124          16  EMPLCY-CARRIER      PIC X(01).                          CL**6
00125          16  EMPLCY-GROUPING     PIC X(06).                          CL**6
00126          16  EMPLCY-STATE        PIC X(02).                          CL**6
00127          16  EMPLCY-PRODUCER     PIC X(10).                          CL**6
00128          16  EMPLCY-EFF-DT       PIC X(02).                          CL**6
00129          16  EMPLCY-REFERENCE-NO PIC X(20).                          CL**6
00130                                                                      CL**6
00131      12  EMPLAN-KEY.                                                 CL**6
00132          16  EMPLAN-COMPANY-CD   PIC X(01).                          CL**6
00133          16  EMPLAN-CARRIER      PIC X(01).                          CL**6
00134          16  EMPLAN-GROUPING     PIC X(06).                          CL**6
00135          16  EMPLAN-STATE        PIC X(02).                          CL**6
00136          16  EMPLAN-PRODUCER     PIC X(10).                          CL**6
00137          16  EMPLAN-PLAN-CODE    PIC X(02).                          CL**6
00138          16  EMPLAN-REV-NO       PIC 9(03).                          CL**6
00139                                                                      CL**6
00140      12  WS-BENEFIT-NO           PIC XX      VALUE ZERO.             CL**3
00141      12  WS-KIND                 PIC X(3)    VALUE SPACES.        EL1323
00142      12  WS-NAME-WORK            PIC X(3)    VALUE SPACES.        EL1323
00143      12  WS-MAPSET-NAME          PIC X(8)    VALUE 'EL132S  '.    EL1323
00144      12  WS-MAP-NAME             PIC X(8)    VALUE 'EL132C  '.    EL1323
00145                                                                   EL1323
00146      12  FILLER       REDEFINES                                   EL1323
00147          WS-MAP-NAME.                                             EL1323
00148          16  FILLER              PIC XX.                          EL1323
00149          16  WS-MAP-NUMBER       PIC X(4).                        EL1323
00150          16  FILLER              PIC XX.                             CL**7
00151                                                                   EL1323
00152      12  WS-SAVE-CURRENT-DATE    PIC XX.                          EL1323
00153      12  WS-PROGRAM-ID           PIC X(8)  VALUE 'EL1323  '.      EL1323
00154                                                                   EL1323
00155      12  WS-CONTROL-FILE-DSID    PIC X(8) VALUE 'ELCNTL  '.       EL1323
00156      12  WS-CERTIFICATE-MASTER-DSID  PIC X(8) VALUE 'ELCERT  '.   EL1323
00157      12  WS-CLAIM-MASTER-DSID    PIC X(8) VALUE 'ELMSTR'.         EL1323
00158      12  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR  '.   EL1323
00159      12  WS-EMPLCY-DSID          PIC X(8)    VALUE 'MPPLCY'.         CL**6
00160      12  WS-EMPLAN-DSID          PIC X(8)    VALUE 'MPPLAN'.         CL**6
00161                                                                   EL1323
00162      12  WS-TRANS-ID             PIC X(4)    VALUE 'XXXX'.        EL1323
00163                                                                      CL**6
00164      12  WS-AGE                  PIC 9(04)  VALUE ZEROS.             CL**6
00165      12  WS-AGE-R REDEFINES WS-AGE.                                  CL**6
00166          16  WS-AGE-1-2          PIC 9(02).                          CL**6
00167          16  WS-AGE-3-4          PIC 9(02).                          CL**6
00168                                                                      CL**8
00169      12  WS-RESPONSE             PIC S9(8) COMP.                     CL**8
00170          88  WS-RESP-NORMAL                VALUE +00.                CL**8
00171          88  WS-RESP-NOTFND                VALUE +13.                CL**8
00172                                                                   EL1323
00173      12  WS-INDEX                PIC S9(4)   VALUE +0 SYNC COMP.  EL1323
00174      12  WS-WORK-SEQU.                                               CL**3
00175          16  FILLER              PIC X(01) VALUE SPACES.             CL**3
00176          16  WS-CLM-POSITION     PIC 9(02).                          CL**3
00177          16  FILLER              PIC X(04) VALUE ' OF '.             CL**3
00178          16  WS-CLM-TOTAL        PIC 9(02).                          CL**3
00179          16  FILLER              PIC X(01) VALUE SPACES.             CL**3
00180                                                                   EL1323
00181      EJECT                                                        EL1323
00182      COPY ELCINTF.                                                   CL**5
00183                                                                   EL1323
00184      EJECT                                                        EL1323
00185      COPY ELCDATE.                                                   CL**5
00186                                                                   EL1323
00187      EJECT                                                        EL1323
00188      COPY EL132S.                                                    CL**5
00189                                                                   EL1323
00190      EJECT                                                        EL1323
00191      COPY ELCEMIB.                                                   CL**5
00192                                                                   EL1323
00193      EJECT                                                        EL1323
00194      COPY ELCCALC.                                                   CL**5
00195                                                                   EL1323
00196      EJECT                                                        EL1323
00197      COPY ELCLOGOF.                                                  CL**5
00198                                                                   EL1323
00199      EJECT                                                        EL1323
00200  LINKAGE SECTION.                                                 EL1323
00201                                                                   EL1323
00202  01  DFHCOMMAREA                  PIC X(1024).                    EL1323
00203                                                                   EL1323
00204      EJECT                                                        EL1323
00205      COPY ELCCERT.                                                   CL**5
00206      EJECT                                                        EL1323
00207      COPY ELCMSTR.                                                   CL**5
00208      EJECT                                                        EL1323
00209      COPY ELCCNTL.                                                   CL**5
00210      EJECT                                                           CL**3
00211      COPY ELCTRLR.                                                   CL**5
00212      EJECT                                                           CL**6
00213      COPY MPCPLCY.                                                   CL**6
00214      EJECT                                                           CL**6
00215      COPY MPCPLAN.                                                   CL**6
00216      EJECT                                                        EL1323
00217  PROCEDURE DIVISION.                                              EL1323
00218                                                                   EL1323
00219      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL1323
00220      MOVE '5'                   TO DC-OPTION-CODE.                EL1323
00221      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                     CL**7
00222      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL1323
00223      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL1323
00224                                                                   EL1323
00225                                                                   EL1323
00226      IF EIBCALEN NOT GREATER THAN ZERO                            EL1323
00227          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL1323
00228          GO TO 8300-SEND-TEXT.                                       CL**7
00229                                                                   EL1323
00230                                                                   EL1323
00231      EXEC CICS HANDLE CONDITION                                   EL1323
00232          ERROR (9990-ERROR) END-EXEC.                             EL1323
00233                                                                   EL1323
00234      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL1323
00235                                                                   EL1323
00236      IF PI-RETURN-TO-PROGRAM NOT EQUAL WS-PROGRAM-ID              EL1323
00237          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6         EL1323
00238          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5         EL1323
00239          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4         EL1323
00240          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3         EL1323
00241          MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2         EL1323
00242          MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1         EL1323
00243          MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM       EL1323
00244          MOVE WS-PROGRAM-ID        TO  PI-CALLING-PROGRAM         EL1323
00245        ELSE                                                       EL1323
00246          MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM         EL1323
00247          MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM       EL1323
00248          MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1         EL1323
00249          MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2         EL1323
00250          MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3         EL1323
00251          MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4         EL1323
00252          MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5         EL1323
00253          MOVE SPACES               TO  PI-SAVED-PROGRAM-6.        EL1323
00254                                                                   EL1323
00255      MOVE PI-COMPANY-CD          TO  WS-CL-COMPANY-CD             EL1323
00256      MOVE PI-CARRIER             TO  WS-CL-CARRIER                EL1323
00257      MOVE PI-CLAIM-NO            TO  WS-CL-CLAIM-NO               EL1323
00258      MOVE PI-CERT-NO             TO  WS-CL-CERT-NO                EL1323
00259                                                                   EL1323
00260      EXEC CICS HANDLE CONDITION                                   EL1323
00261          NOTFND (8100-CLAIM-NOT-FOUND)                            EL1323
00262      END-EXEC.                                                    EL1323
00263                                                                   EL1323
00264      MOVE LOW-VALUES             TO  EL132CI.                     EL1323
00265      MOVE SAVE-DATE              TO  CDATEO                       EL1323
00266      MOVE EIBTIME                TO  WS-TIME-WORK                 EL1323
00267      MOVE WS-TIME                TO  CTIMEO                       EL1323
00268      MOVE PI-COMPANY-ID          TO  CCOMPO.                      EL1323
101501     MOVE PI-PROCESSOR-ID        TO  CUSERIDO.
00269                                                                   EL1323
00270      EXEC CICS READ                                               EL1323
00271          DATASET (WS-CLAIM-MASTER-DSID)                           EL1323
00272          RIDFLD  (WS-CLAIM-KEY)                                   EL1323
00273          SET     (ADDRESS OF CLAIM-MASTER)                           CL**7
00274      END-EXEC.                                                    EL1323
00275                                                                   EL1323
00276      MOVE LOW-VALUES             TO  EL132CI.                     EL1323
00277                                                                   EL1323
00278      MOVE SAVE-DATE              TO  CDATEO                       EL1323
00279      MOVE EIBTIME                TO  WS-TIME-WORK                 EL1323
00280      MOVE WS-TIME                TO  CTIMEO                       EL1323
00281      MOVE PI-COMPANY-ID          TO  CCOMPO.                      EL1323
101501     MOVE PI-PROCESSOR-ID        TO  CUSERIDO.
00282                                                                   EL1323
00283      MOVE CL-CLAIM-NO            TO  CCLAIMO                      EL1323
00284      MOVE CL-CLAIM-TYPE          TO  CTYPEO                       EL1323
00285      MOVE CL-CERT-NO             TO  CCERTNOO                     EL1323
00286      MOVE CL-CERT-SFX            TO  CCERTSXO                     EL1323
00287      MOVE CL-CARRIER             TO  CCARIERO                     EL1323
00288      MOVE CL-CLAIM-STATUS        TO  CSTATO                       EL1323
00289      MOVE CL-PROCESSOR-ID        TO  CPROCO                       EL1323
00290      MOVE CL-INSURED-LAST-NAME   TO  CLNAMEO                      EL1323
00291                                      WS-NAME-WORK                 EL1323
00292      MOVE CL-INSURED-1ST-NAME    TO  CFNAMEO                      EL1323
00293      MOVE CL-INSURED-MID-INIT    TO  CMNAMEO                      EL1323
00294                                                                   EL1323
00295      MOVE CL-ASSOC-CERT-SEQU     TO  WS-CLM-POSITION.                CL**3
00296      MOVE CL-ASSOC-CERT-TOTAL    TO  WS-CLM-TOTAL.                   CL**3
00297      MOVE WS-WORK-SEQU           TO  SEQUO.                          CL**3
00298                                                                      CL**3
00299      IF CL-INSURED-BIRTH-DT NOT = LOW-VALUES                      EL1323
00300          MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00301          MOVE CL-INSURED-BIRTH-DT    TO  DC-BIN-DATE-1            EL1323
00302          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00303          MOVE DC-GREG-DATE-1-EDIT    TO  CBDATEO.                 EL1323
00304                                                                   EL1323
00305      IF CL-SSN-STATE NOT = CL-CERT-STATE                          EL1323
00306        OR CL-SSN-ACCOUNT NOT = CL-CERT-ACCOUNT-PRIME                 CL**6
00307          MOVE CL-SOC-SEC-NO      TO  CSSNO.                       EL1323
00308                                                                   EL1323
00309      MOVE CL-INSURED-OCC-CD      TO  COCCO                        EL1323
00310      MOVE CL-BENEFICIARY         TO  CBENEO                       EL1323
00311                                                                   EL1323
00312                                                                   EL1323
00313      MOVE CL-CAUSE-CD            TO  CCAUSCDO                     EL1323
00314                                                                   EL1323
00315      IF CL-EST-END-OF-DISAB-DT NOT = LOW-VALUES                   EL1323
00316          MOVE CL-EST-END-OF-DISAB-DT TO  DC-BIN-DATE-1            EL1323
00317          MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00318          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00319          MOVE DC-GREG-DATE-1-EDIT    TO  CESTENDO.                EL1323
00320                                                                   EL1323
00321      IF CL-PAID-THRU-DT NOT = LOW-VALUES                          EL1323
00322         MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1                CL**3
00323         MOVE SPACES                 TO  DC-OPTION-CODE               CL**3
00324         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                  CL**7
00325         MOVE DC-GREG-DATE-1-EDIT    TO  CPDTHRUO                     CL**3
00326         IF PI-USES-PAID-TO                                           CL**3
00327            MOVE CL-PAID-THRU-DT        TO  DC-BIN-DATE-1             CL**3
00328            MOVE '6'                    TO  DC-OPTION-CODE            CL**3
00329            MOVE +1                     TO DC-ELAPSED-DAYS            CL**3
00330            MOVE +0                     TO DC-ELAPSED-MONTHS          CL**3
00331            PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT               CL**7
00332            MOVE DC-GREG-DATE-1-EDIT    TO  CPDTHRUO.                 CL**3
00333                                                                   EL1323
00334      MOVE CL-TOTAL-PAID-AMT      TO  CTOTPDO                      EL1323
00335      MOVE CL-NO-OF-DAYS-PAID     TO  CNODAYSO                     EL1323
00336      MOVE CL-NO-OF-PMTS-MADE     TO  CNOPMTSO                     EL1323
00337      MOVE CL-PRIME-CERT-PRIME    TO  PCERTNOO.                       CL**3
00338      MOVE CL-PRIME-CERT-SFX      TO  PSUFXO.                         CL**3
00339                                                                   EL1323
00340      IF CL-INCURRED-DT NOT = LOW-VALUES                           EL1323
00341          MOVE CL-INCURRED-DT         TO  DC-BIN-DATE-1            EL1323
00342          MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00343          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00344          MOVE DC-GREG-DATE-1-EDIT    TO  CINCREDO.                EL1323
00345                                                                   EL1323
00346      IF CL-REPORTED-DT NOT = LOW-VALUES                           EL1323
00347          MOVE CL-REPORTED-DT         TO  DC-BIN-DATE-1            EL1323
00348          MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00349          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00350          MOVE DC-GREG-DATE-1-EDIT    TO  CREPORTO.                EL1323
00351                                                                   EL1323
00352      IF CL-FILE-ESTABLISH-DT NOT = LOW-VALUES                     EL1323
00353          MOVE CL-FILE-ESTABLISH-DT   TO  DC-BIN-DATE-1            EL1323
00354          MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00355          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00356          MOVE DC-GREG-DATE-1-EDIT    TO  CESTABO.                 EL1323
00357                                                                   EL1323
00358 *    IF CL-LAST-PMT-DT NOT = LOW-VALUES                           EL1323
00359 *        MOVE CL-LAST-PMT-DT         TO  DC-BIN-DATE-1            EL1323
00360 *        MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00361          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00362 *        MOVE DC-GREG-DATE-1-EDIT    TO  ELSTPMTO.                EL1323
00363                                                                   EL1323
00364 *    MOVE CL-LAST-PMT-AMT        TO  CPMTAMTO                     EL1323
00365                                                                   EL1323
00366      IF CL-LAST-MAINT-DT NOT = LOW-VALUES                         EL1323
00367          MOVE CL-LAST-MAINT-DT       TO  DC-BIN-DATE-1            EL1323
00368          MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00369          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00370          MOVE DC-GREG-DATE-1-EDIT    TO  CMNTDTEO.                EL1323
00371                                                                   EL1323
00372      IF CL-LAST-MAINT-TYPE = SPACES                               EL1323
00373         MOVE 'SET UP'           TO  CMNTYPEO                      EL1323
00374      ELSE                                                         EL1323
00375      IF CL-LAST-MAINT-TYPE = '1'                                  EL1323
00376         MOVE 'PMT   '           TO  CMNTYPEO                      EL1323
00377      ELSE                                                         EL1323
00378      IF CL-LAST-MAINT-TYPE = '2'                                  EL1323
00379         MOVE 'LETTER'           TO  CMNTYPEO                      EL1323
00380      ELSE                                                         EL1323
00381      IF CL-LAST-MAINT-TYPE = '3'                                  EL1323
00382         MOVE 'CHANGE'           TO  CMNTYPEO                      EL1323
00383      ELSE                                                         EL1323
00384      IF CL-LAST-MAINT-TYPE = '4'                                  EL1323
00385         MOVE 'RESTOR'           TO  CMNTYPEO                      EL1323
00386      ELSE                                                         EL1323
00387      IF CL-LAST-MAINT-TYPE = '5'                                  EL1323
00388         MOVE 'INC DT'           TO  CMNTYPEO                      EL1323
00389      ELSE                                                         EL1323
00390      IF CL-LAST-MAINT-TYPE = '6'                                  EL1323
00391         MOVE 'CONV  '           TO  CMNTYPEO                      EL1323
00392      ELSE                                                         EL1323
00393         MOVE 'CONV  '           TO  CMNTYPEO.                     EL1323
00394                                                                   EL1323
00395      MOVE CL-PRIORITY-CD         TO  CPRICDO                      EL1323
00396      MOVE CL-SUPV-ATTN-CD        TO  CSUPRO                       EL1323
00397      MOVE CL-FILE-LOCATION       TO  FILETOO.                        CL**3
00398      MOVE CL-INSURED-SEX-CD      TO  CSEXO.                       EL1323
00399                                                                      CL**3
00400      MOVE CL-CONTROL-PRIMARY     TO  WS-ELTRLR-KEY.                  CL**3
00401      MOVE +90                    TO  WS-ELTRLR-SEQ-NO.               CL**3
00402                                                                      CL**3
00403      EXEC CICS HANDLE CONDITION                                      CL**3
00404          NOTFND (0010-READ-CERT-MASTER)                              CL**3
00405      END-EXEC.                                                       CL**3
00406                                                                      CL**3
00407      EXEC CICS READ                                                  CL**3
00408          DATASET (WS-ACTIVITY-TRAILERS-DSID)                         CL**3
00409          RIDFLD  (WS-ELTRLR-KEY)                                     CL**3
00410          SET     (ADDRESS OF ACTIVITY-TRAILERS)                      CL**7
00411      END-EXEC.                                                       CL**3
00412                                                                      CL**3
00413      IF AT-TRAILER-TYPE EQUAL '6'                                    CL**3
00414         MOVE AT-INFO-LINE-1      TO  CCAUSEO.                        CL**3
00415                                                                      CL**3
00416  0010-READ-CERT-MASTER.                                              CL**3
00417                                                                   EL1323
00418      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                        CL**6
00419          GO TO 0100-READ-POLICY-MASTER.                              CL**6
00420                                                                      CL**6
00421      MOVE SPACES                 TO  WS-CERTIFICATE-KEY           EL1323
00422      MOVE CL-COMPANY-CD          TO  WS-CK-COMPANY-CD             EL1323
00423      MOVE CL-CERT-CARRIER        TO  WS-CK-CARRIER                EL1323
00424      MOVE CL-CERT-GROUPING       TO  WS-CK-GROUPING               EL1323
00425      MOVE CL-CERT-STATE          TO  WS-CK-STATE                  EL1323
00426      MOVE CL-CERT-ACCOUNT        TO  WS-CK-ACCOUNT                EL1323
00427      MOVE CL-CERT-NO             TO  WS-CK-CERT-NO                EL1323
00428      MOVE CL-CERT-EFF-DT         TO  WS-CK-CERT-EFF-DT            EL1323
00429                                                                   EL1323
00430      EXEC CICS HANDLE CONDITION                                   EL1323
00431          NOTFND (8100-CERT-NOT-FOUND)                             EL1323
00432      END-EXEC.                                                    EL1323
00433                                                                   EL1323
00434      EXEC CICS READ                                               EL1323
00435          DATASET (WS-CERTIFICATE-MASTER-DSID)                     EL1323
00436          RIDFLD  (WS-CERTIFICATE-KEY)                             EL1323
00437          SET     (ADDRESS OF CERTIFICATE-MASTER)                     CL**7
00438      END-EXEC.                                                    EL1323
00439                                                                   EL1323
00440      IF CM-CERT-EFF-DT NOT = LOW-VALUES                           EL1323
00441          MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1            EL1323
00442          MOVE SPACES                 TO  DC-OPTION-CODE           EL1323
00443          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00444          MOVE DC-GREG-DATE-1-EDIT    TO  CEFFDTO.                 EL1323
00445                                                                   EL1323
00446      MOVE CM-ACCOUNT             TO  CACCNTO                      EL1323
00447      MOVE CM-STATE               TO  CSTATEO                      EL1323
00448      MOVE CM-CARRIER             TO  CCARRO                       EL1323
00449      MOVE CM-GROUPING            TO  CGRPO                        EL1323
00450      MOVE CM-INSURED-LAST-NAME   TO  CLNMEO                       EL1323
00451      MOVE CM-INSURED-FIRST-NAME  TO  CFNMEO                       EL1323
00452      MOVE CM-INSURED-INITIAL2    TO  CINITO                       EL1323
00453      MOVE CM-INSURED-ISSUE-AGE   TO  CISAGEO                      EL1323
00454      MOVE CM-INSURED-JOINT-AGE   TO  CJAGEO.                      EL1323
00455                                                                   EL1323
00456      MOVE CM-JT-LAST-NAME        TO  CJLNMEO                      EL1323
00457      MOVE CM-JT-FIRST-NAME       TO  CJFNMEO                      EL1323
00458      MOVE CM-JT-INITIAL          TO  CJINITO                      EL1323
00459                                                                   EL1323
00460      MOVE SAVE-BIN-DATE          TO WS-SAVE-CURRENT-DATE          EL1323
00461                                                                   EL1323
00462 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***                CL**8
00463      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL**8
00464      MOVE SPACES                 TO  WS-CFK-ACCESS-TYPE.             CL**8
00465      MOVE WS-CK-STATE            TO  WS-CFK-STATE.                   CL**8
00466      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.             CL**8
00467      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.             CL**8
00468                                                                      CL**8
00469      EXEC CICS READ                                                  CL**8
00470          DATASET (WS-CONTROL-FILE-DSID)                              CL**8
00471          RIDFLD  (WS-CONTROL-FILE-KEY)                               CL**8
00472          SET     (ADDRESS OF CONTROL-FILE)                           CL**8
00473          RESP    (WS-RESPONSE)                                       CL**8
00474      END-EXEC.                                                       CL**8
00475                                                                      CL**8
00476      IF WS-RESP-NOTFND                                               CL**8
00477         MOVE ER-2848            TO  EMI-ERROR                        CL**8
00478         GO TO 1000-SEND-AND-RETURN                                   CL**8
00479      ELSE                                                            CL**8
00480         MOVE CF-ST-FREE-LOOK-PERIOD                                  CL**8
00481                                  TO CP-FREE-LOOK.                    CL**8
00482                                                                      CL**8
00483 *    IF CL-CLAIM-TYPE EQUAL PI-AH-OVERRIDE-L1                        CL**3
00484 *       GO TO 0020-CHECK-AH.                                         CL**6
00485                                                                   EL1323
00486      MOVE PI-LIFE-OVERRIDE-L6    TO  LCVDSCRO.                       CL**3
00487      MOVE '4'                    TO  WS-CFK-RECORD-TYPE           EL1323
00488      MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO                EL1323
00489                                      LCVCDO                          CL**3
00490      PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT                      CL**7
00491      MOVE WS-KIND                TO  LCVKINDO                        CL**3
00492      MOVE CM-LF-ORIG-TERM        TO  LCVOTRMO                        CL**3
00493                                      CP-ORIGINAL-TERM.            EL1323
00494      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT                EL1323
00495      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE                CL**2
00496      MOVE WS-SAVE-CURRENT-DATE   TO CP-VALUATION-DT               EL1323
00497      MOVE '4'                    TO CP-REM-TERM-METHOD            EL1323
00498      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID                 EL1323
00499      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL**4
00500      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT                    EL1323
00501      MOVE CP-REMAINING-TERM-3    TO LCVRTRMO                         CL**3
00502                                                                   EL1323
00503      MOVE CM-LF-BENEFIT-AMT      TO  LCVBENEO                        CL**3
00504      MOVE CM-POLICY-FORM-NO      TO  LCVFORMO                        CL**3
00505                                                                   EL1323
00506      IF CM-LF-CURRENT-STATUS = '8'                                EL1323
00507         IF CM-LF-CANCEL-DT NOT = LOW-VALUES                       EL1323
00508            MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-1                  EL1323
00509            MOVE ' ' TO DC-OPTION-CODE                             EL1323
00510            PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT               CL**7
00511            IF NOT DATE-CONVERSION-ERROR                           EL1323
00512               MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO.                  CL**3
00513                                                                   EL1323
00514      IF CM-LF-CURRENT-STATUS = '7'                                EL1323
00515         IF CM-LF-DEATH-DT NOT = LOW-VALUES                        EL1323
00516             MOVE CM-LF-DEATH-DT TO DC-BIN-DATE-1                  EL1323
00517             MOVE ' ' TO DC-OPTION-CODE                            EL1323
00518             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT              CL**7
00519             IF NOT DATE-CONVERSION-ERROR                          EL1323
00520                 MOVE DC-GREG-DATE-1-EDIT TO LCVCNDTO.                CL**3
00521                                                                   EL1323
00522      IF CM-LF-CURRENT-STATUS EQUAL '1' OR '4'                     EL1323
00523         IF CP-REMAINING-TERM-3 EQUAL ZERO                         EL1323
00524            MOVE 'EXPIRED'        TO LCVSTATO                         CL**3
00525         ELSE                                                      EL1323
00526            MOVE 'ACTIVE'         TO LCVSTATO.                        CL**3
00527                                                                   EL1323
00528      IF CM-LF-CURRENT-STATUS = '3'                                EL1323
00529         MOVE 'RESTORE'           TO LCVSTATO.                        CL**3
00530      IF CM-LF-CURRENT-STATUS = '5'                                EL1323
00531         MOVE 'REISSUE'           TO LCVSTATO.                        CL**3
122002     IF CM-LF-CURRENT-STATUS = 'M'                                EL1323
122002        MOVE 'MONTHLY'           TO LCVSTATO.                        CL**3
00532      IF CM-LF-CURRENT-STATUS = '6'                                EL1323
00533         MOVE 'LMP DIS'           TO LCVSTATO.                        CL**3
00534      IF CM-LF-CURRENT-STATUS = '7'                                EL1323
00535         MOVE 'DEATH  '           TO LCVSTATO.                        CL**3
00536      IF CM-LF-CURRENT-STATUS = '8'                                EL1323
00537         MOVE 'CANCEL '           TO LCVSTATO.                        CL**3
00538      IF CM-LF-CURRENT-STATUS = '9'                                EL1323
00539         MOVE 'RE-ONLY'           TO LCVSTATO.                        CL**3
00540                                                                   EL1323
00541      IF CM-LF-CURRENT-STATUS = 'V'                                   CL**5
00542         MOVE 'VOID   '           TO LCVSTATO.                        CL**5
00543      IF CM-LF-CURRENT-STATUS = 'D'                                   CL**5
00544         MOVE 'DECLINE'           TO LCVSTATO.                        CL**5
00545                                                                      CL**5
00546      IF CM-LF-CURRENT-STATUS EQUAL '7'                            EL1323
00547         MOVE CM-LF-DEATH-EXIT-DT       TO  DC-BIN-DATE-1          EL1323
00548         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                  CL**7
00549         MOVE DC-GREG-DATE-1-EDIT TO  LCVEXITO.                       CL**3
00550                                                                   EL1323
00551      IF CM-LF-CURRENT-STATUS EQUAL '8'                            EL1323
00552         MOVE CM-LF-CANCEL-EXIT-DT       TO  DC-BIN-DATE-1         EL1323
00553         PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                  CL**7
00554         MOVE DC-GREG-DATE-1-EDIT TO  LCVEXITO.                       CL**3
00555      EJECT                                                        EL1323
00556  0020-CHECK-AH.                                                      CL**6
00557                                                                   EL1323
00558 *    IF CL-CLAIM-TYPE NOT EQUAL PI-AH-OVERRIDE-L1                    CL**3
00559 *       GO TO 0030-FINISH-CERT.                                      CL**6
00560                                                                   EL1323
00561      MOVE '5'                    TO  WS-CFK-RECORD-TYPE           EL1323
00562      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO                EL1323
00563                                      ACVCDO                          CL**3
00564      PERFORM 8700-LOCATE-BENEFIT THRU 8700-EXIT                      CL**7
00565      MOVE WS-KIND                TO  ACVKINDO                        CL**3
00566      MOVE PI-AH-OVERRIDE-L6      TO  ACVDSCRO.                       CL**3
00567      MOVE CM-AH-ORIG-TERM        TO  ACVOTRMO                        CL**3
00568                                      CP-ORIGINAL-TERM.               CL**3
00569      MOVE CM-CERT-EFF-DT         TO CP-CERT-EFF-DT                EL1323
00570      MOVE CM-LOAN-1ST-PMT-DT     TO CP-FIRST-PAY-DATE                CL**2
00571      MOVE WS-SAVE-CURRENT-DATE   TO CP-VALUATION-DT               EL1323
00572      MOVE '4'                    TO CP-REM-TERM-METHOD            EL1323
00573      MOVE PI-COMPANY-ID          TO CP-COMPANY-ID                 EL1323
00574      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.          CL**4
00575      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT                    EL1323
00576      MOVE CP-REMAINING-TERM-3    TO ACVRTRMO                         CL**3
00577                                                                   EL1323
00578      MOVE CM-AH-BENEFIT-AMT      TO ACVBENEO                         CL**3
00579                                                                   EL1323
00580      IF CM-AH-CURRENT-STATUS = '8'                                EL1323
00581         IF CM-AH-CANCEL-DT NOT = LOW-VALUES                       EL1323
00582             MOVE CM-AH-CANCEL-DT TO DC-BIN-DATE-1                 EL1323
00583             MOVE ' ' TO DC-OPTION-CODE                            EL1323
00584             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT              CL**7
00585             IF NOT DATE-CONVERSION-ERROR                          EL1323
00586                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO.                CL**3
00587                                                                   EL1323
00588      IF CM-AH-CURRENT-STATUS = '8'                                EL1323
00589         IF CM-AH-CANCEL-EXIT-DT NOT = LOW-VALUES                  EL1323
00590             MOVE CM-AH-CANCEL-EXIT-DT TO DC-BIN-DATE-1            EL1323
00591             MOVE ' ' TO DC-OPTION-CODE                            EL1323
00592             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT              CL**7
00593             IF NOT DATE-CONVERSION-ERROR                          EL1323
00594                 MOVE DC-GREG-DATE-1-EDIT TO ACVEXITO.                CL**3
00595                                                                   EL1323
00596      IF CM-AH-CURRENT-STATUS = '6' OR '7'                         EL1323
00597         IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES                   EL1323
00598             MOVE CM-AH-SETTLEMENT-DT TO DC-BIN-DATE-1             EL1323
00599             MOVE ' ' TO DC-OPTION-CODE                            EL1323
00600             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT              CL**7
00601             IF NOT DATE-CONVERSION-ERROR                          EL1323
00602                 MOVE DC-GREG-DATE-1-EDIT TO ACVCNDTO.                CL**3
00603                                                                   EL1323
00604      IF CM-AH-CURRENT-STATUS = '6'                                EL1323
00605         IF CM-AH-SETTLEMENT-EXIT-DT NOT = LOW-VALUES              EL1323
00606             MOVE CM-AH-SETTLEMENT-EXIT-DT TO DC-BIN-DATE-1        EL1323
00607             MOVE ' ' TO DC-OPTION-CODE                            EL1323
00608             PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT              CL**7
00609             IF NOT DATE-CONVERSION-ERROR                          EL1323
00610                 MOVE DC-GREG-DATE-1-EDIT TO ACVEXITO.                CL**3
00611                                                                   EL1323
00612      IF CM-AH-CURRENT-STATUS = '1' OR = '4'                       EL1323
00613         MOVE 'ACTIVE'            TO ACVSTATO.                        CL**3
00614      IF CM-AH-CURRENT-STATUS = '3'                                EL1323
00615         MOVE 'RESTORE'           TO ACVSTATO.                        CL**3
00616      IF CM-AH-CURRENT-STATUS = '5'                                EL1323
00617         MOVE 'REISSUE'           TO ACVSTATO.                        CL**3
122002     IF CM-AH-CURRENT-STATUS = 'M'                                EL1323
122002        MOVE 'MONTHLY'           TO ACVSTATO.                        CL**3
00618      IF CM-AH-CURRENT-STATUS = '6'                                EL1323
00619         MOVE 'LMP DIS'           TO ACVSTATO.                        CL**3
00620      IF CM-AH-CURRENT-STATUS = '7'                                EL1323
00621         MOVE 'DEATH  '           TO ACVSTATO.                        CL**3
00622      IF CM-AH-CURRENT-STATUS = '8'                                EL1323
00623         MOVE 'CANCEL '           TO ACVSTATO.                        CL**3
00624      IF CM-AH-CURRENT-STATUS = '9'                                EL1323
00625         MOVE 'RE-ONLY'           TO ACVSTATO.                        CL**3
00626      IF CM-AH-CURRENT-STATUS = 'V'                                   CL**5
00627         MOVE 'VOID   '           TO ACVSTATO.                        CL**5
00628      IF CM-AH-CURRENT-STATUS = 'D'                                   CL**5
00629         MOVE 'DECLINE'           TO ACVSTATO.                        CL**5
00630                                                                   EL1323
00631      MOVE CM-POLICY-FORM-NO      TO  ACVFORMO.                       CL**6
00632                                                                   EL1323
00633  0030-FINISH-CERT.                                                   CL**6
00634                                                                   EL1323
00635      MOVE CM-LOAN-NUMBER         TO  LOANNOO.                        CL**6
00636      MOVE CM-LOAN-BALANCE        TO  LOANBALO.                       CL**6
00637      MOVE CM-LOAN-APR            TO  CAPRO                        EL1323
00638      MOVE CM-PAY-FREQUENCY       TO  CPFREQO                      EL1323
00639      MOVE CM-IND-GRP-TYPE        TO  CINDGRPO                     EL1323
00640      IF CM-SING-PRM                                               EL1323
00641         MOVE 'SP'                TO  CPREMTPO                     EL1323
00642      ELSE                                                         EL1323
00643      IF CM-O-B-COVERAGE                                           EL1323
00644         MOVE 'OB'                TO  CPREMTPO                     EL1323
00645      ELSE                                                         EL1323
00646      IF CM-OPEN-END                                               EL1323
00647         MOVE 'OE'                TO  CPREMTPO                     EL1323
00648      ELSE                                                         EL1323
00649         MOVE CM-PREMIUM-TYPE     TO  CPREMTPO.                    EL1323
00650                                                                   EL1323
00651      MOVE CM-REIN-TABLE          TO  CREINCDO.                    EL1323
00652                                                                   EL1323
00653      GO TO 1000-SEND-AND-RETURN.                                     CL**6
00654                                                                      CL**6
00655      EJECT                                                           CL**6
00656  0100-READ-POLICY-MASTER.                                            CL**6
00657                                                                      CL**6
00658      MOVE CL-COMPANY-CD          TO  EMPLCY-COMPANY-CD.              CL**6
00659      MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER.                 CL**6
00660      MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.                CL**6
00661      MOVE CL-CERT-STATE          TO  EMPLCY-STATE.                   CL**6
00662      MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.                CL**6
00663      MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO.            CL**6
00664      MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DT.                  CL**6
00665                                                                      CL**6
00666      EXEC CICS HANDLE CONDITION                                      CL**6
00667          NOTFND   (8100-EMPLCY-NOT-FOUND)                            CL**6
00668      END-EXEC.                                                       CL**6
00669                                                                      CL**6
00670      EXEC CICS READ                                                  CL**6
00671          DATASET   (WS-EMPLCY-DSID)                                  CL**6
00672          RIDFLD    (EMPLCY-KEY)                                      CL**6
00673          SET       (ADDRESS OF POLICY-MASTER)                        CL**7
00674      END-EXEC.                                                       CL**6
00675                                                                      CL**6
00676      IF (PM-POLICY-EFF-DT IS NOT EQUAL TO SPACES AND LOW-VALUES)     CL**6
00677          MOVE PM-POLICY-EFF-DT           TO  DC-BIN-DATE-1           CL**6
00678          MOVE ' '                        TO  DC-OPTION-CODE          CL**6
00679          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00680          IF NO-CONVERSION-ERROR                                      CL**6
00681              MOVE DC-GREG-DATE-1-EDIT    TO  CEFFDTO                 CL**6
00682          ELSE                                                        CL**6
00683              MOVE SPACES                 TO  CEFFDTO.                CL**6
00684                                                                      CL**6
00685      MOVE PM-PRODUCER                TO  CACCNTO.                    CL**6
00686      MOVE PM-STATE                   TO  CSTATEO.                    CL**6
00687      MOVE PM-CARRIER                 TO  CCARRO.                     CL**6
00688      MOVE PM-GROUPING                TO  CGRPO.                      CL**6
00689      MOVE PM-INSURED-LAST-NAME       TO  CLNMEO.                     CL**6
00690      MOVE PM-INSURED-FIRST-NAME      TO  CFNMEO.                     CL**6
00691      MOVE PM-INSURED-MIDDLE-INIT     TO  CINITO.                     CL**6
00692      MOVE PM-INSURED-ISSUE-AGE       TO  WS-AGE.                     CL**6
00693      MOVE WS-AGE-3-4                 TO  CISAGEO.                    CL**6
00694                                                                      CL**6
00695      MOVE PM-JOINT-LAST-NAME         TO  CJLNMEO.                    CL**6
00696      MOVE PM-JOINT-FIRST-NAME        TO  CJFNMEO.                    CL**6
00697      MOVE PM-JOINT-MIDDLE-INIT       TO  CJINITO.                    CL**6
00698      MOVE PM-JOINT-ISSUE-AGE         TO  WS-AGE.                     CL**6
00699      MOVE WS-AGE-3-4                 TO  CJAGEO.                     CL**6
00700                                                                      CL**6
00701  0100-READ-EMPLAN.                                                   CL**6
00702                                                                      CL**6
00703      MOVE PM-COMPANY-CD              TO  EMPLAN-COMPANY-CD.          CL**6
00704      MOVE PM-CARRIER                 TO  EMPLAN-CARRIER.             CL**6
00705      MOVE PM-GROUPING                TO  EMPLAN-GROUPING.            CL**6
00706      MOVE PM-STATE                   TO  EMPLAN-STATE.               CL**6
00707      MOVE PM-PRODUCER                TO  EMPLAN-PRODUCER.            CL**6
00708      MOVE PM-INS-PLAN-CD             TO  EMPLAN-PLAN-CODE.           CL**6
00709      MOVE PM-INS-PLAN-REVISION       TO  EMPLAN-REV-NO.              CL**6
00710                                                                      CL**6
00711      EXEC CICS HANDLE CONDITION                                      CL**6
00712          NOTFND   (0100-READ-CONT)                                   CL**6
00713      END-EXEC.                                                       CL**6
00714                                                                      CL**6
00715      EXEC CICS READ                                                  CL**6
00716          DATASET   (WS-EMPLAN-DSID)                                  CL**6
00717          RIDFLD    (EMPLAN-KEY)                                      CL**6
00718          SET       (ADDRESS OF PRODUCER-PLANS)                       CL**7
00719      END-EXEC.                                                       CL**6
00720                                                                      CL**6
00721      IF PP-BENEFIT-IS-LEVEL                                          CL**6
00722          MOVE 'L'                    TO  CP-BENEFIT-TYPE             CL**6
00723      ELSE                                                            CL**6
00724          MOVE 'R'                    TO  CP-BENEFIT-TYPE.            CL**6
00725                                                                      CL**6
00726  0100-READ-CONT.                                                     CL**6
00727                                                                      CL**6
00728 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***                CL**8
00729      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.              CL**8
00730      MOVE SPACES                 TO  WS-CFK-ACCESS-TYPE.             CL**8
00731      MOVE PM-STATE               TO  WS-CFK-STATE.                   CL**8
00732      MOVE '3'                    TO  WS-CFK-RECORD-TYPE.             CL**8
00733      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.             CL**8
00734                                                                      CL**8
00735      EXEC CICS READ                                                  CL**8
00736          DATASET (WS-CONTROL-FILE-DSID)                              CL**8
00737          RIDFLD  (WS-CONTROL-FILE-KEY)                               CL**8
00738          SET     (ADDRESS OF CONTROL-FILE)                           CL**8
00739          RESP    (WS-RESPONSE)                                       CL**8
00740      END-EXEC.                                                       CL**8
00741                                                                      CL**8
00742      IF WS-RESP-NOTFND                                               CL**8
00743         MOVE ER-2848            TO  EMI-ERROR                        CL**8
00744         GO TO 1000-SEND-AND-RETURN                                   CL**8
00745      ELSE                                                            CL**8
00746         MOVE CF-ST-FREE-LOOK-PERIOD                                  CL**8
00747                                  TO CP-FREE-LOOK.                    CL**8
00748                                                                      CL**8
00749      IF PM-INS-PLAN-TYPE IS EQUAL TO 'A'                             CL**6
00750          GO TO 0110-SHOW-AH.                                         CL**6
00751                                                                      CL**6
00752      MOVE PI-LIFE-OVERRIDE-L6        TO  LCVDSCRO.                   CL**6
00753      MOVE PP-PLAN-ABBREV             TO  LCVKINDO.                   CL**6
00754      MOVE PM-INS-PLAN-CD             TO  LCVCDO.                     CL**6
00755                                                                      CL**6
00756      MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD           CL**6
00757                                          CP-RATING-METHOD.           CL**6
00758      MOVE PM-LOAN-TERM               TO  LCVOTRMO                    CL**6
00759                                          CP-ORIGINAL-TERM            CL**6
00760                                          CP-LOAN-TERM.               CL**6
00761      MOVE SAVE-BIN-DATE              TO  CP-VALUATION-DT.            CL**6
00762      MOVE PM-POLICY-EFF-DT           TO  CP-CERT-EFF-DT              CL**6
00763                                          CP-FIRST-PAY-DATE.          CL**6
00764      MOVE PM-STATE                   TO  CP-STATE.                   CL**6
00765      MOVE 'A'                        TO  CP-SPECIAL-CALC-CD.         CL**6
00766      MOVE '2'                        TO  CP-PROCESS-TYPE             CL**6
00767                                          CP-REM-TERM-METHOD.         CL**6
00768      MOVE '1'                        TO  CP-REM-TRM-CALC-OPTION.     CL**6
00769      MOVE PI-COMPANY-ID              TO  CP-COMPANY-ID.              CL**6
00770      MOVE PM-COMPANY-CD              TO  CP-COMPANY-CD.              CL**6
00771                                                                      CL**6
00772      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                      CL**6
00773                                                                      CL**6
00774      IF (PI-COMPANY-ID IS EQUAL TO 'CIG' OR 'CUK')                   CL**6
00775          COMPUTE CP-REMAINING-TERM-3 = CP-REMAINING-TERM-3 + 1       CL**6
00776          MOVE CP-REMAINING-TERM-3    TO  LCVRTRMO                    CL**6
00777      ELSE                                                            CL**6
00778          MOVE CP-REMAINING-TERM-3    TO  LCVRTRMO.                   CL**6
00779                                                                      CL**6
00780      MOVE PM-INS-TOTAL-BENEFIT       TO  LCVBENEO.                   CL**6
00781      MOVE PM-INS-POLICY-FORM         TO  LCVFORMO.                   CL**6
00782                                                                      CL**6
00783      IF PM-CANCEL-STATUS                                             CL**6
00784          IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES                  CL**6
00785              MOVE PM-CANCEL-DT       TO  DC-BIN-DATE-1               CL**6
00786              MOVE ' '                TO  DC-OPTION-CODE              CL**6
00787              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT             CL**7
00788              IF NOT DATE-CONVERSION-ERROR                            CL**6
00789                  MOVE DC-GREG-DATE-1-EDIT    TO  LCVCNDTO.           CL**6
00790                                                                      CL**6
00791      IF (PM-EXIT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)           CL**6
00792          MOVE ' '                    TO  DC-OPTION-CODE              CL**6
00793          MOVE PM-EXIT-DT             TO  DC-BIN-DATE-1               CL**6
00794          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00795          IF NOT DATE-CONVERSION-ERROR                                CL**6
00796              MOVE DC-GREG-DATE-1-EDIT        TO  LCVEXITO.           CL**6
00797                                                                      CL**6
00798      IF PM-CURRENT-STATUS IS EQUAL TO '0'                            CL**6
00799          MOVE 'LAPSED'               TO  LCVSTATO.                   CL**6
00800      IF PM-CURRENT-STATUS IS EQUAL TO '1'                            CL**6
00801          MOVE 'ACTIVE'               TO  LCVSTATO.                   CL**6
00802      IF PM-CURRENT-STATUS IS EQUAL TO '2'                            CL**6
00803          MOVE 'PEND'                 TO  LCVSTATO.                   CL**6
00804      IF PM-CURRENT-STATUS IS EQUAL TO '3'                            CL**6
00805          MOVE 'DECLIN'               TO  LCVSTATO.                   CL**6
00806      IF (PM-CURRENT-STATUS IS EQUAL TO '4' OR '9')                   CL**6
00807          MOVE 'PNDCNC'               TO  LCVSTATO.                   CL**6
00808      IF PM-CURRENT-STATUS IS EQUAL TO '5'                            CL**6
00809          MOVE 'PNDISS'               TO  LCVSTATO.                   CL**6
00810      IF PM-CURRENT-STATUS IS EQUAL TO '6'                            CL**6
00811          MOVE 'CLAIM'                TO  LCVSTATO.                   CL**6
00812      IF PM-CURRENT-STATUS IS EQUAL TO '7'                            CL**6
00813          MOVE 'CANCEL'               TO  LCVSTATO.                   CL**6
00814      IF PM-CURRENT-STATUS IS EQUAL TO '8'                            CL**6
00815          MOVE 'PNDUNW'               TO  LCVSTATO.                   CL**6
00816      IF PM-CURRENT-STATUS IS EQUAL TO 'C'                            CL**6
00817          MOVE 'TRNSFR'               TO  LCVSTATO.                   CL**6
00818      IF PM-CURRENT-STATUS IS EQUAL TO 'F'                            CL**6
00819          MOVE 'SETTLE'               TO  LCVSTATO.                   CL**6
00820      IF PM-CURRENT-STATUS IS EQUAL TO 'T'                            CL**6
00821          MOVE 'TRMNAT'               TO  LCVSTATO.                   CL**6
00822                                                                      CL**6
00823      GO TO 0120-FINISH-POLICY.                                       CL**6
00824                                                                      CL**6
00825  0110-SHOW-AH.                                                       CL**6
00826                                                                      CL**6
00827      MOVE PI-AH-OVERRIDE-L6          TO  ACVDSCRO.                   CL**6
00828      MOVE PP-PLAN-ABBREV             TO  ACVKINDO.                   CL**6
00829      MOVE PM-INS-PLAN-CD             TO  ACVCDO.                     CL**6
00830                                                                      CL**6
00831      MOVE PP-REFUND-CALC             TO  CP-EARNING-METHOD           CL**6
00832                                          CP-RATING-METHOD.           CL**6
00833      MOVE PM-LOAN-TERM               TO  ACVOTRMO                    CL**6
00834                                          CP-ORIGINAL-TERM            CL**6
00835                                          CP-LOAN-TERM.               CL**6
00836      MOVE SAVE-BIN-DATE              TO  CP-VALUATION-DT.            CL**6
00837      MOVE PM-POLICY-EFF-DT           TO  CP-CERT-EFF-DT              CL**6
00838      MOVE PM-LOAN-DT                 TO  CP-FIRST-PAY-DATE.          CL**6
00839      MOVE PM-STATE                   TO  CP-STATE.                   CL**6
00840      MOVE 'A'                        TO  CP-SPECIAL-CALC-CD.         CL**6
00841      MOVE '2'                        TO  CP-PROCESS-TYPE             CL**6
00842      MOVE '3'                        TO  CP-REM-TERM-METHOD.         CL**6
00843      MOVE '1'                        TO  CP-REM-TRM-CALC-OPTION.     CL**6
00844      MOVE PI-COMPANY-ID              TO  CP-COMPANY-ID.              CL**6
00845      MOVE PM-COMPANY-CD              TO  CP-COMPANY-CD.              CL**6
00846                                                                      CL**6
00847      PERFORM 9800-LINK-REM-TERM THRU 9800-EXIT.                      CL**6
00848                                                                      CL**6
00849      MOVE CP-REMAINING-TERM-1        TO  ACVRTRMO.                   CL**6
00850                                                                      CL**6
00851      MOVE PM-INS-MONTH-BENEFIT       TO  ACVBENEO.                   CL**6
00852      MOVE PM-INS-POLICY-FORM         TO  ACVFORMO.                   CL**6
00853                                                                      CL**6
00854      IF PM-CANCEL-STATUS                                             CL**6
00855          IF PM-CANCEL-DT IS NOT EQUAL TO LOW-VALUES                  CL**6
00856              MOVE PM-CANCEL-DT       TO  DC-BIN-DATE-1               CL**6
00857              MOVE ' '                TO  DC-OPTION-CODE              CL**6
00858              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT             CL**7
00859              IF NOT DATE-CONVERSION-ERROR                            CL**6
00860                  MOVE DC-GREG-DATE-1-EDIT    TO  ACVCNDTO.           CL**6
00861                                                                      CL**6
00862      IF (PM-EXIT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES)           CL**6
00863          MOVE ' '                    TO  DC-OPTION-CODE              CL**6
00864          MOVE PM-EXIT-DT             TO  DC-BIN-DATE-1               CL**6
00865          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT                 CL**7
00866          IF NOT DATE-CONVERSION-ERROR                                CL**6
00867              MOVE DC-GREG-DATE-1-EDIT        TO  ACVEXITO.           CL**6
00868                                                                      CL**6
00869      IF PM-CURRENT-STATUS IS EQUAL TO '0'                            CL**6
00870          MOVE 'LAPSED'               TO  ACVSTATO.                   CL**6
00871      IF PM-CURRENT-STATUS IS EQUAL TO '1'                            CL**6
00872          MOVE 'ACTIVE'               TO  ACVSTATO.                   CL**6
00873      IF PM-CURRENT-STATUS IS EQUAL TO '2'                            CL**6
00874          MOVE 'PEND'                 TO  ACVSTATO.                   CL**6
00875      IF PM-CURRENT-STATUS IS EQUAL TO '3'                            CL**6
00876          MOVE 'DECLIN'               TO  ACVSTATO.                   CL**6
00877      IF (PM-CURRENT-STATUS IS EQUAL TO '4' OR '9')                   CL**6
00878          MOVE 'PNDCNC'               TO  ACVSTATO.                   CL**6
00879      IF PM-CURRENT-STATUS IS EQUAL TO '5'                            CL**6
00880          MOVE 'PNDISS'               TO  ACVSTATO.                   CL**6
00881      IF PM-CURRENT-STATUS IS EQUAL TO '6'                            CL**6
00882          MOVE 'CLAIM'                TO  ACVSTATO.                   CL**6
00883      IF PM-CURRENT-STATUS IS EQUAL TO '7'                            CL**6
00884          MOVE 'CANCEL'               TO  ACVSTATO.                   CL**6
00885      IF PM-CURRENT-STATUS IS EQUAL TO '8'                            CL**6
00886          MOVE 'PNDUNW'               TO  ACVSTATO.                   CL**6
00887      IF PM-CURRENT-STATUS IS EQUAL TO 'C'                            CL**6
00888          MOVE 'TRNSFR'               TO  ACVSTATO.                   CL**6
00889      IF PM-CURRENT-STATUS IS EQUAL TO 'F'                            CL**6
00890          MOVE 'SETTLE'               TO  ACVSTATO.                   CL**6
00891      IF PM-CURRENT-STATUS IS EQUAL TO 'T'                            CL**6
00892          MOVE 'TRMNAT'               TO  ACVSTATO.                   CL**6
00893                                                                      CL**6
00894  0120-FINISH-POLICY.                                                 CL**6
00895                                                                      CL**6
00896      MOVE PM-LOAN-NUMBER             TO  LOANNOO.                    CL**6
00897      MOVE PM-LOAN-BALC               TO  LOANBALO.                   CL**6
00898      MOVE PM-LOAN-APR                TO  CAPRO.                      CL**6
00899      MOVE PM-BILLING-MODE            TO  CPREMTPO.                   CL**6
00900                                                                      CL**6
00901      EJECT                                                           CL**6
00902  1000-SEND-AND-RETURN.                                            EL1323
00903                                                                   EL1323
00904      IF EMI-ERROR NOT = ZERO                                      EL1323
00905          EXEC CICS LINK                                           EL1323
00906              PROGRAM  ('EL001')                                   EL1323
00907              COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)             EL1323
00908              LENGTH   (EMI-COMM-LENGTH)                           EL1323
00909          END-EXEC.                                                EL1323
00910                                                                   EL1323
00911      MOVE EMI-MESSAGE-AREA (1) TO  CEMSG1O.                       EL1323
00912      MOVE -1                     TO CPFKEYL.                      EL1323
00913                                                                      CL**3
00914      IF PI-USES-PAID-TO                                              CL**3
00915         MOVE 'PAID TO  :' TO CPTHHDGO.                               CL**3
00916                                                                   EL1323
00917      EXEC CICS SEND                                               EL1323
00918          MAPSET (WS-MAPSET-NAME)                                  EL1323
00919          MAP    (WS-MAP-NAME)                                     EL1323
00920          FROM   (EL132CI)                                         EL1323
00921          CURSOR                                                   EL1323
00922          ERASE                                                    EL1323
00923      END-EXEC.                                                    EL1323
00924                                                                   EL1323
00925      EXEC CICS RETURN                                             EL1323
00926          TRANSID  (EIBTRNID)                                      EL1323
00927          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL1323
00928          LENGTH   (PI-COMM-LENGTH)                                EL1323
00929      END-EXEC.                                                    EL1323
00930                                                                   EL1323
00931      EJECT                                                        EL1323
00932  8100-CLAIM-NOT-FOUND.                                            EL1323
00933                                                                   EL1323
00934      MOVE ER-0133                   TO  EMI-ERROR                 EL1323
00935      GO TO 1000-SEND-AND-RETURN.                                  EL1323
00936                                                                   EL1323
00937  8100-CERT-NOT-FOUND.                                             EL1323
00938                                                                   EL1323
00939      MOVE ER-0205                   TO  EMI-ERROR                 EL1323
00940      GO TO 1000-SEND-AND-RETURN.                                  EL1323
00941                                                                   EL1323
00942                                                                   EL1323
00943  8100-EMPLCY-NOT-FOUND.                                              CL**6
00944                                                                      CL**6
00945      MOVE ER-9288                   TO  EMI-ERROR.                   CL**6
00946      GO TO 1000-SEND-AND-RETURN.                                     CL**6
00947                                                                      CL**6
00948      EJECT                                                           CL**6
00949  8300-SEND-TEXT.                                                     CL**7
00950                                                                   EL1323
00951      EXEC CICS SEND TEXT                                          EL1323
00952          FROM   (LOGOFF-TEXT)                                     EL1323
00953          LENGTH (LOGOFF-LENGTH)                                   EL1323
00954          ERASE                                                    EL1323
00955          FREEKB                                                   EL1323
00956      END-EXEC.                                                    EL1323
00957                                                                   EL1323
00958      EXEC CICS RETURN                                             EL1323
00959          END-EXEC.                                                EL1323
00960                                                                   EL1323
00961  8300-EXIT.                                                       EL1323
00962                                                                   EL1323
00963      EXIT.                                                        EL1323
00964                                                                   EL1323
00965      EJECT                                                        EL1323
00966  8500-DATE-CONVERSION.                                               CL**7
00967                                                                   EL1323
00968      EXEC CICS LINK                                               EL1323
00969          PROGRAM  ('ELDATCV')                                     EL1323
00970          COMMAREA (DATE-CONVERSION-DATA)                          EL1323
00971          LENGTH   (DC-COMM-LENGTH)                                EL1323
00972      END-EXEC.                                                    EL1323
00973                                                                   EL1323
00974  8500-EXIT.                                                       EL1323
00975      EXIT.                                                        EL1323
00976                                                                   EL1323
00977  8700-LOCATE-BENEFIT.                                                CL**7
00978                                                                   EL1323
00979      EXEC CICS HANDLE CONDITION                                   EL1323
00980          NOTFND (8700-EXIT)                                       EL1323
00981      END-EXEC.                                                    EL1323
00982                                                                   EL1323
00983      MOVE SPACES                 TO  WS-KIND.                     EL1323
00984                                                                   EL1323
00985      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID            EL1323
00986      MOVE SPACES                 TO  WS-CFK-ACCESS-TYPE              CL**8
00987      MOVE WS-BENEFIT-NO          TO  WS-CFK-BENEFIT                  CL**8
00988                                                                   EL1323
00989      EXEC CICS READ                                               EL1323
00990          DATASET (WS-CONTROL-FILE-DSID)                           EL1323
00991          RIDFLD  (WS-CONTROL-FILE-KEY)                            EL1323
00992          SET     (ADDRESS OF CONTROL-FILE)                           CL**7
00993          GTEQ                                                     EL1323
00994      END-EXEC.                                                    EL1323
00995                                                                   EL1323
00996      IF (WS-CFK-COMPANY-ID NOT EQUAL CF-COMPANY-ID)               EL1323
00997        OR                                                         EL1323
00998         (WS-CFK-RECORD-TYPE NOT EQUAL CF-RECORD-TYPE)             EL1323
00999         GO TO 8700-EXIT.                                          EL1323
01000                                                                   EL1323
01001      MOVE +1                     TO  WS-INDEX.                    EL1323
01002                                                                   EL1323
01003  8700-LOOKUP-BENEFIT.                                             EL1323
01004                                                                   EL1323
01005      IF WS-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)                   CL**3
01006         MOVE CF-BENEFIT-ALPHA (WS-INDEX)  TO  WS-KIND             EL1323
01007         GO TO 8700-EXIT.                                          EL1323
01008                                                                   EL1323
01009      IF CF-BENEFIT-CODE (WS-INDEX) NOT LESS CF-HI-BEN-IN-REC         CL**3
01010          GO TO 8700-EXIT.                                         EL1323
01011                                                                   EL1323
01012      IF WS-INDEX LESS THAN +8                                     EL1323
01013          ADD +1  TO  WS-INDEX                                     EL1323
01014          GO TO 8700-LOOKUP-BENEFIT.                               EL1323
01015                                                                   EL1323
01016  8700-EXIT.                                                       EL1323
01017                                                                   EL1323
01018      EXIT.                                                        EL1323
01019      EJECT                                                        EL1323
01020  9800-LINK-REM-TERM.                                              EL1323
01021                                                                   EL1323
01022      EXEC CICS LINK                                               EL1323
01023          PROGRAM('ELRTRM')                                        EL1323
01024          COMMAREA(CALCULATION-PASS-AREA)                          EL1323
01025          LENGTH(CP-COMM-LENGTH)                                   EL1323
01026          END-EXEC.                                                EL1323
01027                                                                   EL1323
01028  9800-EXIT.                                                       EL1323
01029      EXIT.                                                        EL1323
01030                                                                      CL**9
01031  9990-ERROR.                                                         CL**7
01032                                                                   EL1323
01033      MOVE DFHEIBLK               TO EMI-LINE1                     EL1323
01034      EXEC CICS LINK                                               EL1323
01035          PROGRAM   ('EL004')                                      EL1323
01036          COMMAREA  (EMI-LINE1)                                    EL1323
01037          LENGTH    (72)                                           EL1323
01038          END-EXEC.                                                EL1323
01039                                                                   EL1323
01040      GO TO 1000-SEND-AND-RETURN.                                  EL1323
01041                                                                   EL1323
