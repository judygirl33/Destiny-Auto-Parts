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

       LINKAGE SECTION.
       COPY SUPADDRS. *>SUPP-ADDRESS Copybook
       COPY STATEZIP.
       01 LS-ERRORCOUNTER   PIC 9(02)   VALUE ZEROES.

       PROCEDURE DIVISION USING
           SUPP-ADDRESS,
           STATEZIP-TABLE,
           LS-ERRORCOUNTER.

           IF ADDRESS-1 = SPACES
           THEN
              ADD +1 TO LS-ERRORCOUNTER
           END-IF.

           IF CITY = SPACES
           THEN
              ADD +1 TO LS-ERRORCOUNTER
           END-IF.

           IF ZIP-CODE = SPACES
           THEN
              ADD +1 TO LS-ERRORCOUNTER
           END-IF.

           EVALUATE TRUE
              WHEN ORDER-ADDRESS DISPLAY "Order"
              WHEN SCHED-ADDRESS DISPLAY "Schedule"
              WHEN REMIT-ADDRESS DISPLAY "Remit"
              WHEN OTHER ADD +1 TO LS-ERRORCOUNTER
           END-EVALUATE.

           IF ADDR-STATE = SPACES
           THEN
              ADD +1 TO LS-ERRORCOUNTER
           ELSE
              SET STATEZIP-IDX TO 1
              SEARCH STATEZIP-LIST
                 AT END ADD +1 TO LS-ERRORCOUNTER
              WHEN STATE-ACRO (STATEZIP-IDX) = ADDR-STATE
                 AND ZIP-CODE >= STATEZIP-START (STATEZIP-IDX)
                 AND ZIP-CODE <= STATEZIP-END (STATEZIP-IDX)
                    DISPLAY ADDR-STATE
              END-SEARCH
           END-IF.
