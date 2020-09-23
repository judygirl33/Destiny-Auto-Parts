       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALEX.
      ******************************************************************
      * INSERT HERE WHAT THE PROGRAM DOES
      * version copied from Fabio Remote Github on 9/15 plus changes
      * done by Ivanna on 9/16
      ******************************************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Input File PARTSUPP with the data records
           SELECT PARTSUPPIN ASSIGN TO PARTSUPP
           FILE STATUS IS IN-PARTSUPP-KEY.

      * Input File STATEZIP with StateName, Acronym, and zipcode range
           SELECT STATEZIP ASSIGN TO STATEZIP
           FILE STATUS IS IN-STATEZIP-KEY.

      * Output File for errors
           SELECT ERRORFILE ASSIGN TO ERRFILE
           FILE STATUS IS OUT-ERRORFILE-KEY.

           SELECT WARNINGFILE ASSIGN TO WARNING
           FILE STATUS IS OUT-WARNING-KEY.

      * HERE declare the other 3 output files PARTS, ADDRESS, PURCHASE>>
           SELECT PARTS-FILE ASSIGN TO PARTS
           FILE STATUS IS OUT-PARTSFILE-KEY.

           SELECT ADDR-FILES ASSIGN TO ADDR
           FILE STATUS IS OUT-ADDRFILE-KEY.

           SELECT PURC-FILES  ASSIGN TO PURCHASE
           FILE STATUS IS OUT-PURCFILE-KEY.

       DATA DIVISION.
       FILE SECTION.
       FD  PARTSUPPIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTSUPPIN-REC.
       01  PARTSUPPIN-REC     PIC X(473).

       FD  STATEZIP
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 33 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS STATEZIP-REC.
       01  STATEZIP-REC     PIC X(33).

       FD  ERRORFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 500 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ERRORFILE-REC.
       01  ERRORFILE-REC PIC X(500).

       FD  WARNINGFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 500 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS WARNING-REC.
       01  WARNING-REC PIC X(500).

       FD  PURC-FILES
           RECORDING MODE IS F
           DATA RECORD IS PURCHASE-REC.
       01  PURCHASE-REC.
           05  REC-PO-NUMBER           PIC X(06) VALUE SPACES.
           05  REC-BUYER-CODE          PIC X(03) VALUE SPACES.
           05  REC-QUANTITY            PIC S9(8) COMP VALUE ZERO.
           05  REC-UNIT-PRICE          PIC S9(7)V99 COMP-3 VALUE ZERO.
           05  REC-ORDER-DATE          PIC X(08) VALUE SPACES.
           05  REC-DELIVERY-DATE       PIC X(08) VALUE SPACES.

       FD  ADDR-FILES
           RECORDING MODE IS F
           DATA RECORD IS ADDRESS-REC.
       01  ADDRESS-REC.
           05 REC-ADDRESSES.
              08 REC-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
              08 REC-ADDRESS-1         PIC X(15) VALUE SPACES.
              08 REC-ADDRESS-2         PIC X(15) VALUE SPACES.
              08 REC-ADDRESS-3         PIC X(15) VALUE SPACES.
              08 REC-CITY              PIC X(15) VALUE SPACES.
              08 REC-ADDR-STATE        PIC X(02) VALUE SPACES.
              08 REC-ZIP-CODE          PIC X(05) VALUE SPACES.

       FD  PARTS-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 72 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PARTS-REC.
       01  PARTS-REC.
           05  REC-PART-NUMBER       PIC X(23) VALUE SPACES.
           05  REC-PART-NAME         PIC X(14) VALUE SPACES.
           05  REC-SPEC-NUMBER       PIC X(07) VALUE SPACES.
           05  REC-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
           05  REC-BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
           05  REC-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
           05  REC-WEEKS-LEAD-TIME   PIC S9(04) COMP VALUE ZEROS.
           05  REC-VEHICLE-MAKE      PIC X(03) VALUE SPACES.
           05  REC-VEHICLE-MODEL     PIC X(05) VALUE SPACES.
           05  REC-VEHICLE-YEAR      PIC X(04) VALUE '0000'.

       WORKING-STORAGE SECTION.
           COPY PARTS. *>Parts Copybook
           COPY PARTSUB. *> PART-SUPP-ADDR-PO Copybook
           COPY PRCHSORD. *>PURCHASE-ORDERS Copybook
           COPY SUPADDRS. *>SUPP-ADDRESS Copybook
           COPY SUPPLIER. *>Suppliers Copybook]
           COPY ERRORS.   *> Used for Warnings/Errors.
           COPY STATEZIP. *> Zip State CopyBook
      *>9/16 variable to determine return code
       01 WS-RETURN-CODE                   PIC X(1) VALUE SPACE.

      *9/16 counter of errors found in Subprogram PARTSEDIT
      *01  ERRORCOUNTER         PIC 9(02).

       01 FILE-STATUS-CODES.
      * Here we need to add FILES STATUS CODES of the other output files
      * for output files PARTS, ADDRESS, PURCHASE

      * File status key for input File PARTSUPP
           05 IN-PARTSUPP-KEY           PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

      * File status key for input File STATEZIP
           05 IN-STATEZIP-KEY           PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

      * File status key for Output ErrorFile
           05 OUT-ERRORFILE-KEY          PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

           05 OUT-PARTSFILE-KEY          PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

           05 OUT-PURCFILE-KEY           PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

           05 OUT-ADDRFILE-KEY           PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

           05 OUT-WARNING-KEY            PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

       01 FILES-EOF.
           05 PARTSUPPIN-EOF-WS               PIC X(01) VALUE 'N'.
              88 PARTSUP-END-OF-FILE                    VALUE 'Y'.
           05 STATEZIP-EOF-WS                 PIC X(01) VALUE 'N'.
              88 STATEZIP-EOF                           VALUE 'Y'.


      * Internal VARIABLE GROUP FOR PART-SUPP-ADDR-PO Copybook
       01  WS-PART-SUPP-ADDR-PO-OUT.
           05 PARTS-OUT.
               10  PART-NUMBER-OUT       PIC X(23) VALUE SPACES.
               10  PART-NAME-OUT         PIC X(14) VALUE SPACES.
               10  SPEC-NUMBER-OUT       PIC X(07) VALUE SPACES.
               10  GOVT-COMML-CODE-OUT   PIC X(01) VALUE SPACES.
               10  BLUEPRINT-NUMBER-OUT  PIC X(10) VALUE SPACES.
               10  UNIT-OF-MEASURE-OUT   PIC X(03) VALUE SPACES.
               10  WEEKS-LEAD-TIME-OUT   PIC 9(03) VALUE ZERO.
               10  VEHICLE-MAKE-OUT      PIC X(03) VALUE SPACES.
                    88 CHRYSLER       VALUE 'CHR'.
                    88 FORD           VALUE 'FOR'.
                    88 GM             VALUE 'GM '.
                    88 VOLKSWAGON     VALUE 'VW '.
                    88 TOYOTA         VALUE 'TOY'.
                    88 JAGUAR         VALUE 'JAG'.
                    88 PEUGEOT        VALUE 'PEU'.
                    88 BMW            VALUE 'BMW'.
               10  VEHICLE-MODEL-OUT     PIC X(10) VALUE SPACES.
               10  VEHICLE-YEAR-OUT     PIC X(04) VALUE '0000'.
               10  FILLER            PIC X(14) VALUE SPACES.
           05 SUPPLIERS-OUT.
               10  SUPPLIER-CODE     PIC X(10) VALUE SPACES.
               10  SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                    88 SUBCONTRACTOR  VALUE 'S'.
                    88 DISTRIBUTOR    VALUE 'D'.
                    88 MANUFACTURER   VALUE 'M'.
                    88 IMPORTER       VALUE 'I'.
               10  SUPPLIER-NAME     PIC X(15) VALUE SPACES.
               10  SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
               10  SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                    88 HIGHEST-QUALITY VALUE '3'.
                    88 AVERAGE-QUALITY VALUE '2'.
                    88 LOWEST-QUALITY  VALUE '1'.
               10  SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                    88 GOVT-COMM       VALUE '1'.
                    88 GOVT-ONLY       VALUE '2'.
                    88 COMMERCIAL-ONLY VALUE '3'.
               10  SUPPLIER-ACT-DATE PIC 9(08) VALUE ZERO.
           05 SUPP-ADDRESS-OUT OCCURS 3 TIMES INDEXED BY ADDR-IDX.
               10 ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                  88 ORDER-ADDRESS           VALUE '1'.
                  88 SCHED-ADDRESS           VALUE '2'.
                  88 REMIT-ADDRESS           VALUE '3'.
               10 ADDRESS-1         PIC X(15) VALUE SPACES.
               10 ADDRESS-2         PIC X(15) VALUE SPACES.
               10 ADDRESS-3         PIC X(15) VALUE SPACES.
               10 CITY              PIC X(15) VALUE SPACES.
               10 ADDR-STATE        PIC X(02) VALUE SPACES.
               10 ZIP-CODE          PIC 9(10) VALUE ZERO.
           05 PURCHASE-ORDER-OUT OCCURS 3 TIMES INDEXED BY PO-IDX.
               10  PO-NUMBER         PIC X(06) VALUE SPACES.
               10  BUYER-CODE        PIC X(03) VALUE SPACES.
               10  QUANTITY          PIC S9(7) VALUE ZERO.
               10  UNIT-PRICE        PIC S9(7)V99 VALUE ZERO.
               10  ORDER-DATE        PIC 9(08) VALUE ZERO.
               10  DELIVERY-DATE     PIC 9(08) VALUE ZERO.

      *Counter of records readed from PARTSUPPIN file:
       01 WS-IN-PARTSUPP-CTR               PIC 9(7) VALUE ZERO.

      *9/18 ADDED THIS AUXILIAR VARIABLE AS WORKAROUND WITH COMP FIELD
       01 WS-WEEKS-LEAD-AUX                PIC 9(03) COMP.

       01 WS-ADDR-COUNTER                   PIC 9 VALUE 1.


       PROCEDURE DIVISION.

       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-Main2 UNTIL PARTSUPPIN-EOF-WS = 'Y'.
           PERFORM 600-CLOSE-FILES.
           GOBACK.

       000-Housekeeping.
      * Initialization Routine
           INITIALIZE PART-SUPP-ADDR-PO, WS-PART-SUPP-ADDR-PO-OUT.
      *9/16 Initialize the Return-Code and error-counter from subprogram
           INITIALIZE WS-RETURN-CODE.
      * Priming Read
           PERFORM 300-Open-Files.
           PERFORM 400-Read-PARTSUPPIN.


       100-Main2.
      *    DISPLAY '100-Main'.
           PERFORM 200-PROCESS-DATA.
           PERFORM 500-Write-ERRORFILE.
      * 9/18 Initializing counters before reading next record
           INITIALIZE WS-RETURN-CODE.
           PERFORM 400-Read-PARTSUPPIN.


       200-PROCESS-DATA.
      * From PARTSUPPIN file
      *    MOVE PARTS IN PART-SUPP-ADDR-PO  TO PARTS-OUT.
      *    MOVE SUPPLIERS IN PART-SUPP-ADDR-PO    TO SUPPLIERS-OUT.
      *    MOVE SUPP-ADDRESS IN PART-SUPP-ADDR-PO   TO SUPP-ADDRESS-OUT.
      *    MOVE PURCHASE-ORDER     TO PURCHASE-ORDER-OUT.
      *    DISPLAY '200-PROCESS-DATA'.
      *9/16 Added the call of PARTEDIT SUBPROGRAM
           PERFORM 205-MovePartEdit.

           INITIALIZE DATA-ERRORS.

           CALL 'PARTEDIT' USING
              PART-NUMBER-OUT,
              PART-NAME-OUT,
              SPEC-NUMBER-OUT,
              GOVT-COMML-CODE-OUT,
              BLUEPRINT-NUMBER-OUT,
              UNIT-OF-MEASURE-OUT,
              WS-WEEKS-LEAD-AUX,
              VEHICLE-MAKE-OUT,
              VEHICLE-MODEL-OUT,
              VEHICLE-YEAR-OUT,
              ERRORCOUNTER.
      *     DISPLAY ERRORCOUNTER.

      * Starting checking the addresses on PARTSUPP.
           INITIALIZE STATEZIP-INDEX.
           PERFORM
              VARYING WS-ADDR-COUNTER
              FROM 1 BY 1
              UNTIL WS-ADDR-COUNTER > 3 OR WRONG-DATA
                 MOVE SUPP-ADDRESS-PO(WS-ADDR-COUNTER) TO SUPP-ADDRESS
                 DISPLAY SUPP-ADDRESS
                 CALL 'ADDREDIT'
                    USING SUPP-ADDRESS,
                          STATEZIP-TABLE,
                          STATEZIP-MAX,
                          DATA-ERRORS
      *           DISPLAY ERRORCOUNTER
      *
      * 22/09 - After processing the address, do this check to see
      *         if you had exhausted all the warnings you could or if
      *         you had something bigger, and so engage the WRONG-DATA
      *         88 field so the next checks can be avoided
      *         (performance improvement)
      *
              IF ERRORCOUNTER > 3
                 MOVE  'Y' TO DATA-ERROR-FLAG
              END-IF
           END-PERFORM.

           EVALUATE TRUE
              WHEN WRONG-DATA       PERFORM 208-ProcessError
              WHEN ERRORCOUNTER > 0 PERFORM 208-ProcessWarning
              WHEN OTHER            PERFORM 208-ProcessOkay
           END-EVALUATE.

       208-ProcessError.
           MOVE "Wrong Data!!" TO ERRORFILE-REC.
           WRITE ERRORFILE-REC.
           MOVE PART-SUPP-ADDR-PO TO ERRORFILE-REC.
           WRITE ERRORFILE-REC.

       208-ProcessWarning.
           MOVE "Some Errors on this Data Line:" TO WARNING-REC.
           WRITE WARNING-REC.
           MOVE PART-SUPP-ADDR-PO TO WARNING-REC.
           WRITE WARNING-REC.
           PERFORM
              VARYING WS-ADDR-COUNTER
                 FROM 1 BY 1
                    UNTIL WS-ADDR-COUNTER > ERRORCOUNTER
                       MOVE ERROR-MESSAGE (WS-ADDR-COUNTER)
                          TO WARNING-REC
                       WRITE WARNING-REC
           END-PERFORM.

       208-ProcessOkay.
      *     DISPLAY "Data Ok...".
      *     DISPLAY PART-SUPP-ADDR-PO.
           PERFORM 209-MoveParts.
           PERFORM 209-MoveAddresses.
      *     PERFORM 209-MovePurchases.

       209-MoveParts.
           MOVE PARTS-OUT TO PARTS-REC.
           WRITE PARTS-REC.

       209-MoveAddresses.
           PERFORM VARYING WS-ADDR-COUNTER FROM 1 BY 1
              UNTIL WS-ADDR-COUNTER > 3
                 MOVE SUPP-ADDRESS-PO (WS-ADDR-COUNTER)
                    TO REC-ADDRESSES
                 WRITE ADDRESS-REC
           END-PERFORM.


       205-MovePartEdit.
      *9/17 CHANGE added as workaround of COMP weeks-lead-time in subprogram
           MOVE PART-NUMBER-PO IN PART-SUPP-ADDR-PO TO PART-NUMBER-OUT
              IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE PART-NAME-PO IN PART-SUPP-ADDR-PO TO PART-NAME-OUT IN
              WS-PART-SUPP-ADDR-PO-OUT.
           MOVE PART-NAME-PO IN PART-SUPP-ADDR-PO TO PART-NAME-OUT IN
              WS-PART-SUPP-ADDR-PO-OUT.
           MOVE SPEC-NUMBER-PO IN PART-SUPP-ADDR-PO TO SPEC-NUMBER-OUT
              IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE GOVT-COMML-CODE-PO IN PART-SUPP-ADDR-PO TO
              GOVT-COMML-CODE-OUT IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE BLUEPRINT-NUMBER-PO IN PART-SUPP-ADDR-PO TO
              BLUEPRINT-NUMBER-OUT IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE UNIT-OF-MEASURE-PO IN PART-SUPP-ADDR-PO TO
              UNIT-OF-MEASURE-OUT IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE WEEKS-LEAD-TIME-PO IN PART-SUPP-ADDR-PO TO
              WEEKS-LEAD-TIME-OUT IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE VEHICLE-MAKE-PO IN PART-SUPP-ADDR-PO TO
              VEHICLE-MAKE-OUT IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE VEHICLE-MODEL-PO IN PART-SUPP-ADDR-PO TO
              VEHICLE-MODEL-OUT IN WS-PART-SUPP-ADDR-PO-OUT.
           MOVE VEHICLE-YEAR-PO IN PART-SUPP-ADDR-PO TO
              VEHICLE-YEAR-OUT IN WS-PART-SUPP-ADDR-PO-OUT.
      *9/18 USING AN INTEGER AUX VARILABLE AS WORKAROUND
           COMPUTE WS-WEEKS-LEAD-AUX = 0 + WEEKS-LEAD-TIME-OUT.

       300-Open-Files.
      *    DISPLAY '300-OPEN-FILES'.
           OPEN INPUT PARTSUPPIN.
      *    Input File Status Checking for PARTSUPPIN File
           IF IN-PARTSUPP-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning Input PARTSUPPIN File'
                GO TO 2000-ABEND-RTN
           END-IF.
           OPEN INPUT STATEZIP.
      *    Input File Status Checking for STATEZIP file
           IF IN-STATEZIP-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning Input STATEZIP File'
                GO TO 2000-ABEND-RTN
           ELSE
                PERFORM 3000-LoadInitialize
           END-IF.

           OPEN OUTPUT ERRORFILE.
      *    Output File Status Checking for ERRORFILE
           IF OUT-ERRORFILE-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning ERRORFILE'
                GO TO 2000-ABEND-RTN
           END-IF.

           OPEN OUTPUT WARNINGFILE.
      *    Output File Status Checking for ERRORFILE
           IF OUT-WARNING-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning WARNING'
                GO TO 2000-ABEND-RTN
           END-IF.

           OPEN OUTPUT PARTS-FILE.
      *    Output File Status Checking for ERRORFILE
           IF OUT-PARTSFILE-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning PARTS'
                GO TO 2000-ABEND-RTN
           END-IF.

           OPEN OUTPUT ADDR-FILES.
      *    Output File Status Checking for ERRORFILE
           IF OUT-ADDRFILE-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning ADDR'
                GO TO 2000-ABEND-RTN
           END-IF.

           OPEN OUTPUT PURC-FILES.
      *    Output File Status Checking for ERRORFILE
           IF OUT-PURCFILE-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning PURCHASE'
                GO TO 2000-ABEND-RTN
           END-IF.

       400-Read-PARTSUPPIN.
           READ PARTSUPPIN INTO PART-SUPP-ADDR-PO
      * Set AT END Switch
                AT END MOVE "Y" TO PARTSUPPIN-EOF-WS
                IF IN-PARTSUPP-KEY  = '00' THEN
                    DISPLAY
                        '---------------------------------------------'
                    DISPLAY 'Input file PARTSUPPIN reading problem'
                    PERFORM 2000-ABEND-RTN
                END-IF
           END-READ.
      * To count number of records readed from PARTSUPPPIN file.
           IF (NOT PARTSUP-END-OF-FILE) THEN
              ADD +1 TO WS-IN-PARTSUPP-CTR
           END-IF.


       500-Write-ERRORFILE.
      *    DISPLAY 'WRITE ERRORFILE: '.
           WRITE ERRORFILE-REC FROM WS-PART-SUPP-ADDR-PO-OUT.
           IF OUT-ERRORFILE-KEY NOT EQUAL ZERO THEN
                DISPLAY 'Output ERRORfile writing problem'
                PERFORM 2000-ABEND-RTN
           END-IF.

       600-CLOSE-FILES.
      *     DISPLAY 'CLOSING FILES'.
           CLOSE  PARTSUPPIN, STATEZIP, ERRORFILE.


       2000-ABEND-RTN.
           DISPLAY 'PROGRAM ENCOUNTERED AN ERROR'.
           EXIT.

       3000-LoadInitialize.
           INITIALIZE STATEZIP-TABLE.
           INITIALIZE STATEZIP-INDEX.
           PERFORM 3100-LoadStateTable UNTIL STATEZIP-EOF.

       3100-LoadStateTable.
           PERFORM 3150-ReadNextState UNTIL STATEZIP-EOF.
           MOVE STATEZIP-INDEX TO STATEZIP-MAX.

       3150-ReadNextState.
           READ STATEZIP INTO STATEZIP-LIST(STATEZIP-INDEX)
              AT END
                 MOVE 'Y' TO STATEZIP-EOF-WS
           END-READ.
      *     DISPLAY STATEZIP-LIST(STATEZIP-INDEX).
           ADD 1 TO STATEZIP-INDEX.




