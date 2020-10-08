       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALRPT.
       AUTHOR IVANNA COLAN
      ******************************************************************
      * 9/26 Program to generate break out Report from the input file
      *  GOODDATA, which contains only correct records of PARTSUPP file
      * ******************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Input File GOODDATA with correct data records
           SELECT GOODDATAIN ASSIGN TO GOODDATA
           FILE STATUS IS IN-GOODDATA-KEY.

      * Error File
           SELECT ERRORFILE ASSIGN TO ERRFILE
           FILE STATUS IS OUT-ERRORFILE-KEY.

      * Output Control Break Report
      *     SELECT PRINT-LINE ASSIGN TO PRTLINE.


           SELECT RPTFILE ASSIGN TO RPTFILE
           FILE STATUS IS REPORT-KEY.


       DATA DIVISION.
       FILE SECTION.
       FD  GOODDATAIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 473 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS GOODDATAIN-REC.
       01  GOODDATAIN-REC     PIC X(473).


       FD  RPTFILE
           RECORDING MODE IS F
           RECORD CONTAINS   132 CHARACTERS
           DATA RECORD IS RPT-REC.
       01  RPT-REC PIC X(132).


       FD  ERRORFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 500 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ERRORFILE-REC.
       01  ERRORFILE-REC PIC X(500).


      *    FD  PRINT-LINE RECORDING MODE F
      *        LABEL RECORDS ARE STANDARD
      *        RECORD CONTAINS 132 CHARACTERS
      *        BLOCK CONTAINS 0 RECORDS
      *        DATA RECORD ISPRINT-REC.
      *    01  PRINT-REC      PIC X(132).


       WORKING-STORAGE SECTION.
           COPY PARTS. *>Parts Copybook
           COPY PARTSUB. *> PART-SUPP-ADDR-PO Copybook
           COPY PRCHSORD. *>PURCHASE-ORDERS Copybook
           COPY SUPADDRS. *>SUPP-ADDRESS Copybook
           COPY SUPPLIER. *>Suppliers Copybook]


       01 FILE-STATUS-CODES.


      * File status key for input File GOODDATA
           05 IN-GOODDATA-KEY           PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

      * File Status key for Report
           05 REPORT-KEY           PIC X(2).
                88 CODE-WRITE               VALUE SPACES.


      * File status key for Output ErrorFile
           05 OUT-ERRORFILE-KEY          PIC X(2).
                88 CODE-WRITE               VALUE SPACES.

       01 FILES-EOF.
           05 GOODDATAIN-EOF-WS               PIC X(01) VALUE 'N'.
              88 GOODDATA-END-OF-FILE                    VALUE 'Y'.



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

      *Counter of records readed from GOODDATAIN file:
       01 WS-IN-GOODDATA-CTR               PIC 9(7) VALUE ZERO.


       01 WS-ADDR-COUNTER                   PIC 9 VALUE 1.
       01 WS-LOOP-COUNTER                   PIC 9 VALUE 1.

       COPY OUTRPT.


       01 WS-FLAGS.
           05 WS-LINE-KTR               PIC 9(4) VALUE 0.


       PROCEDURE DIVISION.

       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-Main2 UNTIL GOODDATAIN-EOF-WS = 'Y'.
           PERFORM 600-CLOSE-FILES.
           GOBACK.

       000-Housekeeping.
      * Initialization Routine
           INITIALIZE PART-SUPP-ADDR-PO,
                      WS-PART-SUPP-ADDR-PO-OUT,
                      WS-CONTROL-KEY.
      * Priming Read
           PERFORM 300-Open-Files.
           MOVE SPACES TO RPT-REC.
           PERFORM 400-Read-GOODDATAIN.
           PERFORM 1000-WRITE-HEADER.
           PERFORM 150-INIT-WS-FIELDS.
           MOVE PART-NUMBER-PO IN PART-SUPP-ADDR-PO
               TO WS-CONTROL-KEY.


       100-Main2.
      *    DISPLAY '100-Main'.
           PERFORM 205-CONTROL-BREAK.
           PERFORM 210-CALCULATE.
           PERFORM 220-WRITE-DATA.
      * 9/18 Initializing counters before reading next record
      *S    INITIALIZE
           PERFORM 400-Read-GOODDATAIN.


       150-INIT-WS-FIELDS.
           INITIALIZE WS-COUNTERS-AND-ACCUMULATORS.
           INITIALIZE WS-ADDRESSES-1, WS-ADDRESSES-2, WS-ADDRESSES-3.
           INITIALIZE WS-FOOTER-1, WS-FOOTER-2, WS-FOOTER-3.

       205-CONTROL-BREAK.
           IF WS-CONTROL-KEY NOT EQUAL
              PART-NUMBER-PO THEN
              PERFORM 1000-WRITE-FOOTER
              PERFORM 1000-WRITE-HEADER
              PERFORM 150-INIT-WS-FIELDS
              MOVE PART-NUMBER-PO TO WS-CONTROL-KEY
           END-IF.


       210-CALCULATE.
      * Pending calculate Address and Purchase Information
           PERFORM VARYING WS-LOOP-COUNTER
           FROM 1 BY 1
           UNTIL WS-LOOP-COUNTER > 3
              DISPLAY SUPP-ADDRESS-OUT (WS-LOOP-COUNTER)
              EVALUATE ADDRESS-TYPE-PO (WS-LOOP-COUNTER)
                 WHEN '1'
                    STRING ADDRESS-1-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDRESS-2-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDRESS-3-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDR-STATE-PO (WS-LOOP-COUNTER)
                                         DELIMITED BY SIZE
                           "  "
                           ZIP-CODE-PO (WS-LOOP-COUNTER)
                           "  "
                           CITY-PO (WS-LOOP-COUNTER)
                                   DELIMITED BY SIZE
                       INTO WS-ORDER-ADDRESS
                 WHEN '2'
                    STRING ADDRESS-1-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDRESS-2-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDRESS-3-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDR-STATE-PO (WS-LOOP-COUNTER)
                                         DELIMITED BY SIZE
                           "  "
                           ZIP-CODE-PO (WS-LOOP-COUNTER)
                           "  "
                           CITY-PO (WS-LOOP-COUNTER)
                                   DELIMITED BY SIZE
                       INTO WS-SCHED-ADDRESS
                 WHEN '3'
                    STRING ADDRESS-1-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDRESS-2-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDRESS-3-PO (WS-LOOP-COUNTER)
                                        DELIMITED BY SIZE
                           ", "
                           ADDR-STATE-PO (WS-LOOP-COUNTER)
                                         DELIMITED BY SIZE
                           "  "
                           ZIP-CODE-PO (WS-LOOP-COUNTER)
                           "  "
                           CITY-PO (WS-LOOP-COUNTER)
                                   DELIMITED BY SIZE
                       INTO WS-REMIT-ADDRESS
              END-EVALUATE
              ADD 1 TO WS-TOTAL-PURCH-ORDERS
              ADD QUANTITY-PO (WS-LOOP-COUNTER)
                 TO WS-TOTAL-QTY-IN-PURCH-ORDERS
              COMPUTE WS-TOTAL-PRICE-PURCH-ORDERS =
                 WS-TOTAL-PRICE-PURCH-ORDERS +
                 (
                      QUANTITY-PO (WS-LOOP-COUNTER) *
                      UNIT-PRICE-PO (WS-LOOP-COUNTER)
                 )
           END-PERFORM.

       220-WRITE-DATA.
      *     WRITE RPT-REC FROM WS-BLANK-LINE.
           WRITE RPT-REC FROM WS-ADDRESSES-1.
           WRITE RPT-REC FROM WS-ADDRESSES-2.
           WRITE RPT-REC FROM WS-ADDRESSES-3.
           WRITE RPT-REC FROM WS-BLANK-LINE.

       300-Open-Files.
      *    DISPLAY '300-OPEN-FILES'.
           OPEN INPUT GOODDATAIN.
      *    Input File Status Checking for GOODDATAIN File
           IF IN-GOODDATA-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning Input GOODDATAIN File'
                GO TO 2000-ABEND-RTN
           END-IF.


           OPEN OUTPUT ERRORFILE.
      *    Output File Status Checking for ERRORFILE
           IF OUT-ERRORFILE-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning ERRORFILE'
                GO TO 2000-ABEND-RTN
           END-IF.

           OPEN OUTPUT RPTFILE
      *Output File Status checking for RPTFILE
           IF REPORT-KEY NOT = '00' THEN
                DISPLAY
                        '---------------------------------------------'
                DISPLAY 'File Problem openning RPTFILE'
                GO TO 2000-ABEND-RTN
           END-IF.

      *     OPEN OUTPUT PRINT-LINE.




       400-Read-GOODDATAIN.
           READ GOODDATAIN INTO PART-SUPP-ADDR-PO
      * Set AT END Switch
                AT END MOVE "Y" TO GOODDATAIN-EOF-WS
                IF IN-GOODDATA-KEY  = '00' THEN
                    DISPLAY
                        '---------------------------------------------'
                    DISPLAY 'Input file GOODDATAIN reading problem'
                    PERFORM 2000-ABEND-RTN
                END-IF
           END-READ.
      *     DISPLAY PART-SUPP-ADDR-PO.
      * To count number of records readed from GOODDATAPIN file.
           IF (NOT GOODDATA-END-OF-FILE) THEN
              ADD +1 TO WS-IN-GOODDATA-CTR
           END-IF.


       600-CLOSE-FILES.
      *     DISPLAY 'CLOSING FILES'.
           PERFORM 1000-WRITE-FOOTER.
           CLOSE  GOODDATAIN, ERRORFILE, RPTFILE.


       2000-ABEND-RTN.
           DISPLAY 'PROGRAM ENCOUNTERED AN ERROR'.
           EXIT.

       1000-WRITE-HEADER.
           PERFORM 1000-WRITE-HEADER-DATA.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           WRITE RPT-REC FROM WS-HEADER.
           WRITE RPT-REC FROM WS-UNDERLINE.
           WRITE RPT-REC FROM WS-PARTS-DATA-OUT.
           WRITE RPT-REC FROM WS-BLANK-LINE.

       1000-WRITE-HEADER-DATA.
           MOVE PART-NUMBER-PO TO PART-NUMBER-OUT IN WS-PARTS-DATA-OUT.
           MOVE WEEKS-LEAD-TIME-PO TO WEEKS-LEAD-TIME-OUT IN
           WS-PARTS-DATA-OUT.
           EVALUATE VEHICLE-MAKE-PO
                WHEN 'CHR' MOVE 'CHRYSLER' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
                WHEN 'FOR' MOVE 'FORD' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
                WHEN 'GM' MOVE 'GM' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
                WHEN 'VW' MOVE 'VOLKSWAGEN' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
                WHEN 'TOY' MOVE 'TOYOTA' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
                WHEN 'JAG' MOVE 'JAGUAR' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
                WHEN 'PEU' MOVE 'PEUGEOT' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
                WHEN 'BMW' MOVE 'BMW' TO VEHICLE-MAKE-OUT IN
                WS-PARTS-DATA-OUT
           END-EVALUATE.
      * To get PartNumber, Weekslead time, vehicle make, Suppliers Name
      * and SUPPLIER RATING
           MOVE SUPPLIER-NAME-PO TO SUPPLIER-NAME-OUT.
           EVALUATE SUPPLIER-RATING-PO
                WHEN '3' MOVE 'HIGHEST QUALITY' TO SUPPLIER-RATING-OUT
                WHEN '2' MOVE 'AVERAGE QUALITY' TO SUPPLIER-RATING-OUT
                WHEN '1' MOVE 'LOWEST QUALITY' TO SUPPLIER-RATING-OUT
           END-EVALUATE.


       1000-WRITE-FOOTER.
           PERFORM 1000-WRITE-FOOTER-DATA.
           WRITE RPT-REC FROM WS-FOOTER-1.
           WRITE RPT-REC FROM WS-FOOTER-2.
           WRITE RPT-REC FROM WS-FOOTER-3.

       1000-WRITE-FOOTER-DATA.
           MOVE WS-TOTAL-PURCH-ORDERS
             TO WS-TOTAL-PURCHASE-ORDER-O.
           MOVE WS-TOTAL-QTY-IN-PURCH-ORDERS
             TO WS-TOTAL-QTY-PURCH-ORDER-O.
           MOVE WS-TOTAL-PRICE-PURCH-ORDERS
             TO WS-TOTAL-PRICE-O.
