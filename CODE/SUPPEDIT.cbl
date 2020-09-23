       IDENTIFICATION DIVISION.
       PROGRAM-ID. SUPPEDIT.
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

       01  WARNINGS.
           05 SUBCONTRACTOR-WARNING PIC X(80) VALUE
              "Warning - Subcontractor needs to be high quality".

      * 01  DATA-STRUCT.
      *     05 YEAR                 PIC 9(4).
      *     05 FILLER               PIC "-".
      *     05 MONTH                PIC 99.
      *     05 FILLER               PIC "-".
      *     05 THEDAY               PIC 99.

      * 01 FC.
      *     02 Condition-Token-Value.
      *     COPY CEEIGZCT.
      *        03 Case-1-Condition-ID.
      *           04 Severity             PIC S9(4) BINARY.
      *           04 Msg-No               PIC S9(4) BINARY.
      *        03 Case-2-Condition-ID     REDEFINES Case-1-Condition-ID.
      *           04 Class-Code           PIC S9(4) BINARY.
      *           04 Cause-Code           PIC S9(4) BINARY.
      *        03 Case-Sev-Ctl            PIC X.
      *        03 Facility-ID             PIC XXX.
      *     02 I-S-Info                   PIC S9(9) BINARY.

      * 01 PICSTR.
      *     02 Vstring-length             PIC S9(4) BINARY.
      *     02 Vstring-text.
      *        03 Vstring-char   PIC X OCCURS 0 TO 256 TIMES
      *                    DEPENDING ON Vstring-length of PICSTR.
      * 01 CURRENT-DATE.
      *     02 Vstring-length       PIC S9(4) BINARY.
      *     02 Vstring-text.
      *        03 Vstring-char    PIC X OCCURS 0 TO 256 TIMES
      *                    DEPENDING ON Vstring-lenght OF CURRENT-DATE.

      * 01 CURRENT-LILIAN         PIC S9(9) BINARY.

       LINKAGE SECTION.
       COPY SUPPLIER. *>SUPP-ADDRESS Copybook
       COPY ERRORS.

      *01 ERRORCOUNTER   PIC 9(02)   VALUE ZEROES.

       PROCEDURE DIVISION USING
           SUPPLIERS,
           DATA-ERRORS.

           INITIALIZE WARNINGS.

           IF SUPPLIER-CODE = SPACES
              ADD 4 TO ERRORCOUNTER
              GOBACK
           END-IF.

           IF SUPPLIER-TYPE = SPACES
              ADD 4 TO ERRORCOUNTER
              GOBACK
           END-IF.

           IF SUPPLIER-NAME = SPACES
              ADD 4 TO ERRORCOUNTER
              GOBACK
           END-IF.

           IF SUPPLIER-PERF = ZEROES
              ADD 4 TO ERRORCOUNTER
              GOBACK
           END-IF.

           EVALUATE TRUE
              WHEN SUBCONTRACTOR
                 IF NOT HIGHEST-QUALITY
                    THEN
                       ADD +1 TO ERRORCOUNTER
                       IF ERRORCOUNTER > 3
                          ADD +4 TO ERRORCOUNTER
                          GOBACK
                       ELSE
                          MOVE SUBCONTRACTOR-WARNING
                             TO ERROR-MESSAGE (ERRORCOUNTER)
                       END-IF
                 END-IF
              WHEN DISTRIBUTOR   CONTINUE
              WHEN MANUFACTURER  CONTINUE
              WHEN IMPORTER      CONTINUE
              WHEN OTHER
                 ADD +1 TO ERRORCOUNTER
                 IF ERRORCOUNTER > 3
                    ADD +4 TO ERRORCOUNTER
                    GOBACK
                 ELSE
                    MOVE "Warning - Invalid Supplier Type"
                       TO ERROR-MESSAGE (ERRORCOUNTER)
                 END-IF
           END-EVALUATE.

           EVALUATE TRUE
              WHEN HIGHEST-QUALITY CONTINUE
              WHEN AVERAGE-QUALITY CONTINUE
              WHEN LOWEST-QUALITY  CONTINUE
              WHEN OTHER
                 ADD +1 TO ERRORCOUNTER
                 IF ERRORCOUNTER > 3
                    ADD +4 TO ERRORCOUNTER
                    GOBACK
                 ELSE
                    MOVE "Warning - Invalid Supplier Rating"
                       TO ERROR-MESSAGE (ERRORCOUNTER)
                 END-IF
           END-EVALUATE.

           EVALUATE TRUE
              WHEN GOVT-COMM       CONTINUE
              WHEN GOVT-ONLY       CONTINUE
              WHEN COMMERCIAL-ONLY CONTINUE
              WHEN OTHER
                 ADD +1 TO ERRORCOUNTER
                 IF ERRORCOUNTER > 3
                    ADD +4 TO ERRORCOUNTER
                    GOBACK
                 ELSE
                    MOVE "Warning - Invalid Supplier Status"
                       TO ERROR-MESSAGE (ERRORCOUNTER)
                 END-IF
           END-EVALUATE.

      *********************************************************
      *
      * Call CEEDAYS to return the Lilian days tally for the *
      *
      * date value in the variable CURRENT-DATE.
      *
      *
      *********************************************************
      *     IF SUPPLIERS-ACT-DATE NOT = SPACES
      *        MOVE 8 TO Vstring-length IN PICSTR
      *        MOVE 'YYYYMMDD' TO Vstring-text IN PICSTR
      *        MOVE 8 TO Vstring-length IN CURRENT-DATE
      *        MOVE SUPPLIERS-ACT-DATE
      *           TO Vstring-text IN CURRENT-DATE
      *        CALL "CEEDAYS" USING CURRENT-DATE, PICSTR,
      *           CURRENT-LILIAN, FC.
      *        IF NOT CEE000 THEN
      *           ADD +1 TO ERRORCOUNTER
      *           IF ERRORCOUNTER > 3
      *              ADD +4 TO ERRORCOUNTER
      *              GOBACK
      *           ELSE
      *              MOVE "Warning - Invalid Date for this field"
      *                 TO ERROR-MESSAGE (ERRORCOUNTER)
      *           END-IF
      *     END-IF.
