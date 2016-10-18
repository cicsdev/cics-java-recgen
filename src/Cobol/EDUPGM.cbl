      *----------------------------------------------------------------*
      *  Licensed Materials - Property of IBM                          *
      *  SAMPLE                                                        *
      *  (c) Copyright IBM Corp. 2016 All Rights Reserved              *
      *  US Government Users Restricted Rights - Use, duplication or   *
      *  disclosure restricted by GSA ADP Schedule Contract with       *
      *  IBM Corp                                                      *
      *----------------------------------------------------------------*
      *       AUTHOR  :  Giovanni Creato/Italy/IBM                     *
      *       DATE    :  16 / 04 / 2016                                *
      *       VERSION :  1.1                                           *
      *       HISTORY :                                                *
      *        16/06/16  Moved 01 level DFHCOMMAREA to EDUCPY          *
      *----------------------------------------------------------------*
      * Description                                                    *
      *                                                                *
      * This program defines different types of COBOL data and can be  *
      * used to understand different COBOL data types                  *
      *                                                                *
      *----------------------------------------------------------------*
       TITLE 'Sample program that treats different types of COBOL data'
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    "EDUPGM".
       Author.        "Giovanni Creato/Italy/IBM".
       DATE-WRITTEN.   20/11/2014.
      *----------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-zSeries WITH DEBUGGING MODE.
      *SOURCE-COMPUTER.  IBM-zSeries.
      *----------------------------------------------------------------*
       DATA DIVISION.
      *----------------------------------------------------------------*
       WORKING-STORAGE SECTION.
       01 Program-Description.
           03 NN.
              05 pic x(10)   value 'EDUPGM  : '.


       01 WS-VARIABLES.
           03 DISPLAYABLE-NUMERIC             PIC -z(3)9.
           03 REQUIRED-CA-LEN                 PIC S9(4)      VALUE +0.
           03  VALID-INPUT-PARAMETERS         PIC X.
              88 VALID-INPUT-PARAMETERS-FALSE value X'00'.
              88 VALID-INPUT-PARAMETERS-TRUE  value X'01' through X'FF'.
           03  Switches.
              05 Switch-condition       Pic X value space.
                 88 Switch-condition-1 value "A".
                 88 Switch-condition-2 value "B".

       LINKAGE SECTION.
           COPY EDUCPY.
      *----------------------------------------------------------------*
       PROCEDURE DIVISION.
       MAIN-PROGRAM          section.
      D    DISPLAY NN 'Starting'

           PERFORM INPUT-PARAMETER-CHECK.

           if VALID-INPUT-PARAMETERS-TRUE  then
               PERFORM BUSINESS-LOGIC
           else
               MOVE -1 TO RESULT-CODE
               MOVE 'INVALID INPUT PARAMETERS'
                    TO RESULT-TEXT
           end-if.

      D    MOVE RESULT-CODE  TO DISPLAYABLE-NUMERIC.
      D    DISPLAY NN 'Returning with RESULT-CODE : '
      D                               DISPLAYABLE-NUMERIC.

           PERFORM END-PROGRAM.

       INPUT-PARAMETER-CHECK section.

      * If NO commarea received issue an ABEND
           IF EIBCALEN IS EQUAL TO ZERO
               DISPLAY NN 'NO COMMAREA RECEIVED. ABENDING THE TASK'
               EXEC CICS ABEND ABCODE('LENG') NODUMP END-EXEC
           END-IF

      * Compute partial LENGTH
      D    MOVE ZERO                       TO REQUIRED-CA-LEN
      D    ADD LENGTH OF BINARY-DIGIT      TO REQUIRED-CA-LEN
      D    ADD LENGTH OF CHARACTER-STRING  TO REQUIRED-CA-LEN
      D    ADD LENGTH OF NUMERIC-STRING    TO REQUIRED-CA-LEN
      D    ADD LENGTH OF PACKED-DIGIT      TO REQUIRED-CA-LEN

      D    MOVE REQUIRED-CA-LEN            TO DISPLAYABLE-NUMERIC
      D    DISPLAY NN 'PARTIAL SUM IS  : ' DISPLAYABLE-NUMERIC
      D    ADD LENGTH OF SIGNED-PACKED     TO REQUIRED-CA-LEN

      D    MOVE REQUIRED-CA-LEN            TO DISPLAYABLE-NUMERIC
      D    DISPLAY NN 'PARTIAL SUM IS  : ' DISPLAYABLE-NUMERIC
      D    ADD LENGTH OF BOOL              TO REQUIRED-CA-LEN
      D    ADD LENGTH OF RESULT-CODE       TO REQUIRED-CA-LEN
      D    MOVE REQUIRED-CA-LEN            TO DISPLAYABLE-NUMERIC
      D    DISPLAY NN 'PARTIAL SUM IS  : ' DISPLAYABLE-NUMERIC
      D    ADD LENGTH OF RESULT-TEXT       TO REQUIRED-CA-LEN
      D    MOVE REQUIRED-CA-LEN            TO DISPLAYABLE-NUMERIC
      D    DISPLAY NN 'PARTIAL SUM IS  : ' DISPLAYABLE-NUMERIC

      * if COMMAREA is less then required issue an ABEND
           MOVE LENGTH OF DATA-PAYLOAD     TO REQUIRED-CA-LEN

      D    MOVE REQUIRED-CA-LEN            TO DISPLAYABLE-NUMERIC
      D    DISPLAY NN 'MINIMUM COMMAREA LENGTH IS : '
                                              DISPLAYABLE-NUMERIC
           IF EIBCALEN IS LESS THAN REQUIRED-CA-LEN
               MOVE EIBCALEN               TO DISPLAYABLE-NUMERIC
               DISPLAY NN 'COMMAREA SHORTER THAN : ' DISPLAYABLE-NUMERIC
                          '. ABEND THE TASK'
               EXEC CICS ABEND ABCODE('GCCO') NODUMP END-EXEC
           END-IF

      *    Clean result area
           MOVE ZERO   TO RESULT-CODE
           MOVE SPACES TO RESULT-TEXT

           Set VALID-INPUT-PARAMETERS-TRUE to True.

      * NOTE BINARY-DIGIT cannot be controlled

           if CHARACTER-STRING IS NOT ALPHABETIC then
      D         DISPLAY NN 'CHARACTER-STRING IS NOT ALPHABETIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if NUMERIC-STRING IS NOT NUMERIC then
      D         DISPLAY NN 'NUMERIC-STRING IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if PACKED-DIGIT IS NOT NUMERIC then
      D         DISPLAY NN 'PACKED-DIGIT IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if SIGNED-PACKED IS NOT NUMERIC then
      D         DISPLAY NN 'SIGNED-PACKED IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.

           if BOOL IS NOT NUMERIC then
      D         DISPLAY NN 'BOOL IS NOT NUMERIC'
                Set VALID-INPUT-PARAMETERS-FALSE to TRUE.


       BUSINESS-LOGIC        section.
      D         DISPLAY NN 'Performing Business Logic'.
                MOVE ZERO TO RESULT-CODE.
                MOVE 'PARAMETERS ARE ALL OK'
                     TO RESULT-TEXT.


       END-PROGRAM           section.
      D         DISPLAY NN 'Performing END-PROGRAM'.
                EXEC CICS RETURN END-EXEC.
