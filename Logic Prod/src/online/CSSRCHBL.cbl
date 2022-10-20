      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       identification division.
       program-id. CSSRCHBL.
      *
      *AUTHOR.    Pablo.
      *           Colleyville, TEXAS.
      ********************************************
      *   Coversheet search business logic
      ********************************************
       environment division.
       data division.
       working-storage section.
       01  DFH-START PIC X(04).
       01  prev-form-name       pic x(10).
       01 response-code         pic s9(8) comp.
       01 display-response      pic 9(8).
       01 bl-index              pic 9(8) comp.
       01 max-form-name         pic x(10).
       01 name-in-range-flag    pic 9.
       01 max-entries           pic s9(8) comp value 500.
       01 lower-case    pic x(26) value
                  "abcdefghijklmnopqrstuvwxyz".
       01 upper-case    pic x(26) value
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
      *                                copy FORMREC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            FORMREC                             *
      *                            VMOD=2.031                          *
      *                                                                *
      *   Billing system file for verbaige in overlays                 *
      *                                                                *
      *   FILE DESCRIPTION = FORM DEFINITION FILE                      *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 900   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = FORMDEFS                  RKP=0,LEN=12   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
092612* 092312  CR2012040900002  PEMA  NEW FILE AND COPYBOOK
      *****************************************************************
        01  form-record.
            03  form-key.
                05  form-name           pic x(10).
                05  form-month          pic 99.
            03  form-desc               pic x(30).
            03  special-notes occurs 8  pic x(75).
            03  comment-1               pic x(95).
            03  comment-2               pic x(95).
            03  filler                  pic x(68).
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01 dfhcommarea.
      *                                copy CSSRCH-COMMAREA.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2012 by CSO.                                    *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
      ****************************************
      *  commarea for QUERY by Form Name
      *  (business logic input & output)
      ****************************************
           03  bl-input.
               05  bl-input-form-name  pic x(10).
           03  bl-output.
               05  bl-status           pic x.
                   88  bl-ok                 value "P".
                   88  bl-fail               value "F".
               05  bl-output-message   pic x(50).
               05  bl-output-record-count
                                       pic 9(8) comp.
               05  bl-output-record  occurs 100 times.
                   07  bl-output-form-name
                                       pic x(10).
                   07  bl-output-form-desc
                                       pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'CSSRCHBL' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
      *****************************************************
      * Using the partial or full form name provided
      * by the user, start a browse.
      *
      * In order to find only the forms that match the
      * characters provided by the user, build
      * max-last-name by replacing trailing spaces
      * with Zs.  For example, if the user entered
      * "AH", we want to find all forms between
      * "AH" and "BAZZZZ..."
      *****************************************************
       0000-begin.
           move bl-input-form-name     to form-name
           move spaces                 to prev-form-name
                 inspect form-name converting lower-case
                                       to upper-case
           move form-name              to max-form-name
           inspect max-form-name replacing all
                 spaces by 'Z'
           
      * exec cics startbr
      *       dataset ('FORMDEFS')
      *       ridfld  (form-name)
      *       gteq
      *       resp    (response-code)
      *    end-exec
           MOVE 'FORMDEFS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00000204' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303030323034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-name, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           if response-code = 13 
      *   dfhresp(notfnd)
              move "No matching records found"
                                       to bl-output-message
              move 0                   to bl-output-record-count
              set bl-fail to true
              
      * exec cics return
      *       end-exec
      *    MOVE '.(                    &   #00000215' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           if response-code not = 0 
      *   dfhresp(normal)
              move response-code       to display-response
              string "*** Failure: STARTBR resp = "
                 display-response      into bl-output-message
              end-string
              set bl-fail              to true
              
      * exec cics return
      *       end-exec
      *    MOVE '.(                    &   #00000224' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           end-if
           move 1                      to name-in-range-flag
           move 0                      to bl-index
      ********************************************************
      * Read through the records until max-last-name is
      * exceeded, end of file is reached, an error occurs,
      * or 100 records are added to the array of the
      * commarea.  For this example application, no error
      * is returned if more than 100 records meet the
      * search criteria; only the first 100 records are
      * returned.
      ********************************************************
           perform until name-in-range-flag = 0
              
      * exec cics readnext
      *          dataset   ('FORMDEFS')
      *          into      (form-record)
      *          ridfld    (form-key)
      *          keylength (length of form-key)
      *          resp      (response-code)
      *       end-exec
           MOVE LENGTH OF
            form-record
             TO DFHEIV13
           MOVE 'FORMDEFS' TO DFHEIV1
           MOVE LENGTH OF
            form-key
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&.IL K                )  N#00000239' TO DFHEIV0
           MOVE X'262E494C204B202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303030323339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 form-record, 
                 DFHEIV13, 
                 form-key, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO response-code
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
      *********************************************************
      * If the read response is normal, verify that the
      * last-name doesn't exceed the max-last-name.  If the
      * user provided a first initial, the record read must
      * be a match.  Return all names that meet the criteria
      * in the output array.  Duplicate keys are treated the
      * same as a normal response since duplicates are
      * allowed on the last name alternate index.
      **********************************************************
              evaluate response-code
                 when 0 
      *   dfhresp(normal)
                    if form-name < max-form-name
                       if form-name not = prev-form-name
                          add 1 to bl-index
                          move form-name
                                       to bl-output-form-name (bl-index)
                          move form-desc
                                       to bl-output-form-desc (bl-index)
                          move form-name
                                       to prev-form-name
                       end-if
                    else
                       move 0          to name-in-range-flag
                    end-if
                 when 20 
      *   dfhresp(endfile)
                    move 0             to name-in-range-flag
                 when other
                    move response-code to display-response
                    string "*** Failure: READ resp = "
                       display-response into bl-output-message
                    end-string
                    move 0             to bl-output-record-count
                    set bl-fail to true
                    
      * exec cics return
      *             end-exec
      *    MOVE '.(                    &   #00000279' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              end-evaluate
              if bl-index = 100
                 move 0                to name-in-range-flag
              end-if
           end-perform
      **********************************************************
      * If the array index is still zero, no matching records
      * were found.  Otherwise, set the record count in the
      * output.
      **********************************************************
           if bl-index = 0
              move "No matching records found"
                                       to bl-output-message
              move 0                   to bl-output-record-count
           else
              move spaces              to bl-output-message
              move bl-index            to bl-output-record-count
           end-if
           set bl-ok to true.
           
      * exec cics return
      *    end-exec
      *    MOVE '.(                    &   #00000300' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSSRCHBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'CSSRCHBL' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
