00001  IDENTIFICATION DIVISION.                                         03/19/98
00003  PROGRAM-ID.    LDDLYACTV.                                           LV004
00010 *AUTHOR.        Pablo.                                            EL545
00011 *               Colleyvile TEXAS.                                 EL545
00032  ENVIRONMENT DIVISION.                                            EL545
00033  INPUT-OUTPUT SECTION.                                            EL545
00034  FILE-CONTROL.                                                    EL545
00036      SELECT DLYACTV-IN assign to DLYACTVIN.
      *         ASSIGN TO "e:/cid/seqfiles/eracct_tst.txt".

00037      SELECT DLYACTV assign to DLYACTV
      *         ASSIGN TO "e:/cid/seqfiles/eracct.dat"
00038           ACCESS IS DYNAMIC                                       EL545
00039           oRGANIZATION   IS INDEXED                               EL545
00040           FILE STATUS    IS DLYACTV-file-status                   EL545
00041           RECORD KEY     IS DA-KEY.                               EL545

00056                                                                   EL545
00057  EJECT                                                            EL545
00058  DATA DIVISION.                                                   EL545
00059  FILE SECTION.                                                    EL545
00060                                                                   EL545
00061  FD  DLYACTV-IN                                                   EL545
00062      BLOCK CONTAINS 0 RECORDS
00063      RECORDING MODE F.                                               CL**4
00064                                                                   EL545
00065  01  DLYACTV-IN-REC                pic x(25).                     EL545
00066                                                                   EL545
00100                                                                   EL545
00101  FD  DLYACTV                                                      EL545
00102      BLOCK CONTAINS 0 RECORDS.
00104                                  copy eLCDAR.                     EL545
00112                                                                   EL545
00124                                                                   EL545
00136  WORKING-STORAGE SECTION.                                         EL545
00137  77  FILLER  PIC X(32) VALUE '********************************'.  EL545
00138  77  FILLER  PIC X(32) VALUE '   LDDLYACTV WORKING STORAGE    '.  EL545
00139  77  FILLER  PIC X(32) VALUE '*********VMOD=2.001*************'.  EL545
00140                                                                   EL545
00141  01  MISC.                                                        EL545
           12  DLYACTV-file-status      pic xx value '00'.
           12  qsam-in-cnt             pic 9(7) value zeros.
           12  vsam-ot-cnt             pic 9(7) value zeros.
           12  ws-DLYACTV-SW             pic x value ' '.
               88  end-of-DLYACTV              value 'y'.

00152      12  WS-RETURN-CODE      PIC X(4)            VALUE ZEROS.     EL545
00153      12  ABEND-OPTION        PIC X               VALUE 'Y'.       EL545
00154      12  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.    EL545
00155      12  WS-ABEND-FILE-STATUS PIC XX             VALUE SPACES.    EL545
00156      12  WS-ZERO             PIC S9              VALUE ZERO.      EL545
00157      12  PGM-SUB             PIC S999    COMP-3  VALUE +545.      EL545
00158                                                                   EL545
00234                                                                   EL545
00236  PROCEDURE DIVISION.                                              EL545

       0000-begin-program.

      *    perform 0010-init           thru 0010-exit
           perform 0100-process        thru 0100-exit

           goback

           .                                                            EL545

       0100-process.                                                    EL545

           perform 0110-process-DLYACTV thru 0110-exit

           .
       0100-exit.
           exit.

00244  0110-process-DLYACTV.                                             EL545

           display ' Begin processing DLYACTV  '
           open  input DLYACTV-in
           open output DLYACTV

           if DLYACTV-file-status = '00' or '97' OR '05'
              continue
           else
              move ' Bad Open    DLYACTV   '
                                       to ws-abend-message
              move DLYACTV-file-status  to ws-abend-file-status
              perform abend-pgm
           end-if

           perform 0116-read-DLYACTV    thru 0116-exit
           perform 0115-load-DLYACTV    thru 0115-exit until
              end-of-DLYACTV

           close DLYACTV-in
                 DLYACTV

           if DLYACTV-file-status = '00'
              display ' Qsam in count ' qsam-in-cnt
              display ' Vsam ot count ' vsam-ot-cnt
           else
              move ' Bad Close   DLYACTV   '
                                       to ws-abend-message
              move DLYACTV-file-status  to ws-abend-file-status
              perform abend-pgm
           end-if

           .
       0110-exit.
           exit.

       0115-load-DLYACTV.

           move DLYACTV-in-rec            to DAILY-ACTIVITY-RECORD

           write DAILY-ACTIVITY-RECORD

           if DLYACTV-file-status = '00'
              add 1                    to vsam-ot-cnt
           else
              move ' Bad Write   DLYACTV   '
                                       to ws-abend-message
              move DLYACTV-file-status  to ws-abend-file-status
              perform abend-pgm
           end-if

           perform 0116-read-DLYACTV    thru 0116-exit

           .
       0115-exit.
           exit.

       0116-read-DLYACTV.

           read DLYACTV-in              at end
              set end-of-DLYACTV        to true
           end-read

           if not end-of-DLYACTV
              add 1                    to qsam-in-cnt
           end-if

           .
       0116-exit.
           exit.



00685  ABEND-PGM SECTION.                                               EL545
00686                           COPY ELCABEND.                          EL545
00687                                                                   EL545
