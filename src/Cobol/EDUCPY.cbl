      *----------------------------------------------------------------*
      *  Licensed Materials - Property of IBM                          *
      *  SAMPLE                                                        *
      *  (c) Copyright IBM Corp. 2016 All Rights Reserved              *
      *  US Government Users Restricted Rights - Use, duplication or   *
      *  disclosure restricted by GSA ADP Schedule Contract with       *
      *  IBM Corp                                                      *
      *----------------------------------------------------------------*
      *       EDUCPY.cpy                                               *
      *                                                                *
      *       AUTHOR  :  Giovanni Creato                               *
      *       DATE    :  20 / 11 / 2014                                *
      *       VERSION :  1.0                                           *
      *                                                                *
      *       MISSION : Copy book defining interface for EDUPGM        *
      *       HISTORY :                                                *
      *        16/06/16  Added 01 level DFHCOMMAREA                    *
      *----------------------------------------------------------------*

       01   DFHCOMMAREA.
           03  DATA-PAYLOAD.
              05 BINARY-DIGIT            PIC 9(4)       COMP.
              05 CHARACTER-STRING        PIC X(30).
              05 NUMERIC-STRING          PIC 9(18).
              05 PACKED-DIGIT            PIC 9(15)      COMP-3.
              05 PACKED-DIGIT-WITH-COMMA PIC 9(12)V9(3) COMP-3.
              05 SIGNED-PACKED           PIC S9(12)     COMP-3.
              05 BOOL                    PIC X.
                 88 BOOL-FALSE           value X'00'.
                 88 BOOL-TRUE            value X'01' through X'FF'.
              05 RESULT.
                 09 RESULT-CODE          PIC S9(5)      VALUE +0.
                 09 RESULT-TEXT          PIC X(25).

      *----------------------------------------------------------------*	
