       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDREDIT.
       AUTHOR. FABIO COSTA.
      ******************************************************************
      * This checks and validates the purchase addresses
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

       DATA DIVISION.
       FILE SECTION.

       WORKING-STORAGE SECTION.
       01  CONTROLS-AND-FLAGS.
           05 IDX-CONTROL PIC 9(4)       VALUE 1.
           05 WS-ZIPCODE  PIC 9(10)      VALUE 0.
           05 FOUND-FLAG  PIC X          VALUE 'N'.
              88 FOUND                   VALUE 'Y'.
              88 NOT-FOUND               VALUE 'N'.

       01  WS-STATEZIP-RANGE.
           05  WS-STATEZIP-START       PIC 9(10) VALUE 0.
           05  WS-STATEZIP-END         PIC 9(10) VALUE 0.

       LINKAGE SECTION.
       COPY SUPADDRS. *>SUPP-ADDRESS Copybook
       COPY STATEZIP.
       COPY ERRORS.

      *01 ERRORCOUNTER   PIC 9(02)   VALUE ZEROES.

       PROCEDURE DIVISION USING
           SUPP-ADDRESS,
           STATEZIP-TABLE,
           STATEZIP-MAX,
           DATA-ERRORS.

           INITIALIZE CONTROLS-AND-FLAGS.

           MOVE ZIP-CODE TO WS-ZIPCODE.

           IF ADDRESS-1 = SPACES
           THEN
              ADD +4 TO ERRORCOUNTER
              GOBACK
           END-IF.

           IF CITY = SPACES
           THEN
              ADD +4 TO ERRORCOUNTER
              GOBACK
           END-IF.

           IF ZIP-CODE = SPACES
           THEN
              ADD +4 TO ERRORCOUNTER
              GOBACK
           END-IF.

      *     DISPLAY ADDRESS-TYPE.
           EVALUATE TRUE
      *   WHEN ORDER-ADDRESS DISPLAY "Order"
      *   WHEN SCHED-ADDRESS DISPLAY "Schedule"
      *   WHEN REMIT-ADDRESS DISPLAY "Remit"
              WHEN ORDER-ADDRESS CONTINUE
              WHEN SCHED-ADDRESS CONTINUE
              WHEN REMIT-ADDRESS CONTINUE
              WHEN OTHER
      *           DISPLAY ERRORCOUNTER
                 ADD +1 TO ERRORCOUNTER
      *           DISPLAY ERRORCOUNTER
                 IF ERRORCOUNTER > 3
                    ADD +4 TO ERRORCOUNTER
                    GOBACK
                 ELSE
                    MOVE "Warning - Invalid Address Type"
                       TO ERROR-MESSAGE (ERRORCOUNTER)
                 END-IF
           END-EVALUATE.


           IF ADDR-STATE = SPACES
           THEN
              ADD +4 TO ERRORCOUNTER
              GOBACK
           ELSE
              MOVE 'N' TO FOUND-FLAG
      *        IF NOT-FOUND
      *           DISPLAY "Initialized NOT-FOUND"
      *        END-IF
              PERFORM VARYING IDX-CONTROL
                 FROM 1 BY 1 UNTIL IDX-CONTROL > STATEZIP-MAX
                       OR FOUND
      *              DISPLAY STATE-ACRO (IDX-CONTROL)
                    IF STATE-ACRO (IDX-CONTROL) = ADDR-STATE
                       THEN
                          INITIALIZE WS-STATEZIP-RANGE
                          MOVE STATEZIP-START (IDX-CONTROL)
                             TO WS-STATEZIP-START
                          MOVE STATEZIP-END (IDX-CONTROL)
                             TO WS-STATEZIP-END
      *                    DISPLAY STATEZIP-START (IDX-CONTROL)
      *                    DISPLAY STATEZIP-END (IDX-CONTROL)
      *                    DISPLAY WS-ZIPCODE WS-STATEZIP-RANGE
                       IF WS-ZIPCODE >= WS-STATEZIP-START
                          AND WS-ZIPCODE <= WS-STATEZIP-END
                       THEN
      *                   DISPLAY "OK, FOUND!"
                          MOVE 'Y' TO FOUND-FLAG
                       END-IF
                    END-IF
              END-PERFORM
              IF NOT-FOUND
      *           DISPLAY "NOT FOUND"
      *           DISPLAY ERRORCOUNTER
                 ADD +1 TO ERRORCOUNTER
      *           DISPLAY ERRORCOUNTER
                 IF ERRORCOUNTER > 3
                    ADD +4 TO ERRORCOUNTER
                    GOBACK
                 ELSE
                    MOVE "Warning - Invalid Zip Code"
                       TO ERROR-MESSAGE (ERRORCOUNTER)
                 END-IF
      *        ELSE
      *           DISPLAY ADDR-STATE
              END-IF
      *        SET STATEZIP-IDX TO 1
      *        SEARCH ALL STATEZIP-LIST
      *           AT END ADD +1 TO ERRORCOUNTER
      *        WHEN STATE-ACRO (STATEZIP-IDX) = ADDR-STATE
      *           AND ZIP-CODE >= STATEZIP-START (STATEZIP-IDX)
      *           AND ZIP-CODE <= STATEZIP-END (STATEZIP-IDX)
      *              DISPLAY ADDR-STATE
      *        END-SEARCH
           END-IF.


      *     DISPLAY "ERRORS IN ADDREDIT: " ERRORCOUNTER.
