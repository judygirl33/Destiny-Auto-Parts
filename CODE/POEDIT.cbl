       IDENTIFICATION DIVISION.
       PROGRAM-ID. POEDIT.
       AUTHOR. IVANNA COLAN.
      ******************************************************************
      * This subprogram validates the data of the Purchase Order
      *  PORTION OF a PARTSUPP record
      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.

       01 WS-INPUT-DATE-INT        PIC 9(9) COMP.
       01 WS-PICSTR-IN.
           05  WS-PICSTR-LTH-IN     PIC S9(4) COMP VALUE 8.
           05  WS-PICSTR-STR-IN     PIC X(8)  value 'YYYYMMDD'.
       01 WS-DATE-IN-CEE.
           05  WS-DATE-IN-LTH-CEE   PIC S9(4) COMP VALUE 8.
           05  WS-DATE-IN-STR-CEE   PIC X(8).
       01 FC.
           05  FC-SEV              PIC S9(4) COMP.
           05  FC-MSG              PIC S9(4) COMP.
           05  FC-CTW              PIC X.
           05  FC-FAC              PIC X(3).
           05  FC-ISI              PIC S9(8) COMP.
       01  WS-DELIVERY-DATE-INT     PIC 9(8).
       01  WS-ORDER-DATE-INT        PIC 9(8).

       01  FAILURE-ON-DATES        PIC X    VALUE "N".
           88 WRONG-DATE                    VALUE "Y".


       LINKAGE SECTION.
       COPY ERRORS.
       COPY PRCHSORD.



       PROCEDURE DIVISION USING PURCHASE-ORDERS, DATA-ERRORS.

      *Validating PO-NUMBER is not blank
           IF PO-NUMBER = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.
      *Validating BUYER-CODE should NOT be blank
           IF BUYER-CODE = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.
      *Validating ORDER-DATE should NOT be blank
           IF ORDER-DATE = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.

      *Validating QUANTITY must be between 0 and 999,999
           IF QUANTITY < ZERO OR QUANTITY > 999999
              THEN
                 ADD +1 TO ERRORCOUNTER
                 IF ERRORCOUNTER > 3
                    ADD 4 TO ERRORCOUNTER
                    GOBACK
                 END-IF
           END-IF.

      *If  QUANTITY is > 0, UNIT PRICE must be > 0.
           IF QUANTITY > ZERO
                THEN
                    IF UNIT-PRICE IS LESS THAN OR EQUAL TO ZERO
                       THEN
                          ADD +1 TO ERRORCOUNTER
                          IF ERRORCOUNTER > 3
                             ADD 4 TO ERRORCOUNTER
                          GOBACK
                        END-IF
      *UNIT PRICE must be between $1 and $1,000,000.00

                    IF QUANTITY >= 1 AND QUANTITY <= 1000000
                       THEN
                          CONTINUE
                       ELSE
                          ADD +1 TO ERRORCOUNTER
                          IF ERRORCOUNTER > 3
                             ADD 4 TO ERRORCOUNTER
                          GOBACK
                    END-IF
           END-IF.

      *ORDER DATE must be a valid date
           MOVE ORDER-DATE TO WS-DATE-IN-STR-CEE
      *        DISPLAY WS-DATE-IN-CEE
              CALL "CEEDAYS" USING WS-DATE-IN-CEE, WS-PICSTR-IN,
                 WS-INPUT-DATE-INT, FC
              DISPLAY FC-SEV
              IF FC-SEV NOT = ZERO THEN
                 ADD +1 TO ERRORCOUNTER
                 IF ERRORCOUNTER > 3
                    THEN ADD +4 TO ERRORCOUNTER
                         GOBACK
                    ELSE
                       MOVE "Warning - Invalid Date for this field"
                       TO ERROR-MESSAGE (ERRORCOUNTER)
                       MOVE "Y" TO FAILURE-ON-DATES
                 END-IF
              END-IF.


      *DELIVERY DATE is optional but if there is data, it must be a
      * valid date and the date must be later than ORDER DATE

           IF DELIVERY-DATE NOT = SPACES
              MOVE DELIVERY-DATE TO WS-DATE-IN-STR-CEE
      *        DISPLAY WS-DATE-IN-CEE
              CALL "CEEDAYS" USING WS-DATE-IN-CEE, WS-PICSTR-IN,
                 WS-INPUT-DATE-INT, FC
              DISPLAY FC-SEV
              IF FC-SEV NOT = ZERO
                 THEN
                    ADD +1 TO ERRORCOUNTER
                    IF ERRORCOUNTER > 3
                       THEN
                          ADD +4 TO ERRORCOUNTER
                          GOBACK
                       ELSE
                          MOVE "Warning - Invalid Date for this field"
                             TO ERROR-MESSAGE (ERRORCOUNTER)
                          MOVE "Y" TO FAILURE-ON-DATES
                    END-IF
              END-IF.

           IF NOT WRONG-DATE
              COMPUTE WS-DELIVERY-DATE-INT = FUNCTION NUMVAL
                 (DELIVERY-DATE)
              COMPUTE WS-ORDER-DATE-INT = FUNCTION NUMVAL
                 (ORDER-DATE)
              IF WS-DELIVERY-DATE-INT < WS-ORDER-DATE-INT
                 THEN
                    ADD +1 TO ERRORCOUNTER
                    IF ERRORCOUNTER > 3
                       THEN
                          ADD +4 TO ERRORCOUNTER
                          GOBACK
                       ELSE
                          MOVE
                          "Warning -Delivery Date earlier than order"
                          TO ERROR-MESSAGE (ERRORCOUNTER)
                 END-IF
           END-IF.

           DISPLAY 'NUMBER OF ERRORS IN PURCHASE ORDER: '
           ERRORCOUNTER.

           GOBACK.
