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
           SELECT PRINT-LINE ASSIGN TO PRTLINE.


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
           RECORD CONTAINS 133 CHARACTERS
           DATA RECORD IS RPT-Rec.
       01  RPT-REC PIC X(133).


       FD  ERRORFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 500 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ERRORFILE-REC.
       01  ERRORFILE-REC PIC X(500).


       FD  PRINT-LINE RECORDING MODE F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 133 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PRINT-REC.
       01  PRINT-REC      PIC X(133).




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



       01 WS-BREAK-CONTROLS.
           05 WS-CONTROL-KEY            PIC X(23). *> Hold/Control Key

 *************************************************************
      ****** Report headings ******
      *************************************************************
       01 WS-BLANK-LINE                 PIC X(133)     VALUE SPACES.
       01 WS-HEADER.
          05 FILLER              PIC X(3)       VALUE SPACES.
          05 FILLER              PIC X(18)      VALUE 'Part Name'.
          05 FILLER              PIC X(06)      VALUE SPACE.
          05 FILLER              PIC X(15)      VALUE 'Weeks Lead Time'.
          05 FILLER              PIC X(5)       VALUE SPACES.
          05 FILLER              PIC X(12)       VALUE 'Vehicle Make'.
          05 FILLER              PIC X(5)       VALUE SPACES.
          05 FILLER              PIC X(13)       VALUE 'Supplier Name'.
          05 FILLER              PIC X(5)       VALUE SPACES.
          05 FILLER              PIC X(15)     VALUE 'Supplier Rating'.

       01 WS-UNDERLINE.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 FILLER                    PIC X(23)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(15)      VALUE ALL '='.
           05 FILLER                    PIC X(05)      VALUE SPACE.
           05 FILLER                    PIC X(12)       VALUE ALL '='.
           05 FILLER                    PIC X(5)       VALUE SPACES.
           05 FILLER                    PIC X(13)        VALUE ALL '='.
           05 FILLER                    PIC X(05)      VALUE SPACES.
           05 FILLER                    PIC X(15)       VALUE ALL '='.


       01 WS-PARTS-DATA-OUT.
          05 FILLER               PIC X(3)       VALUE SPACES.
          05 PART-NUMBER-OUT      PIC X(23)      VALUE SPACES.
          05 FILLER               PIC X(08)      VALUE SPACES.
          05 WEEKS-LEAD-TIME-OUT  PIC 9(03)     VALUE ZERO.
          05 FILLER               PIC X(10)       VALUE SPACES.
          05 VEHICLE-MAKE-OUT     PIC X(10)       VALUE SPACES.
          05 FILLER               PIC X(7)       VALUE SPACES.
          05 SUPPLIER-NAME-OUT    PIC X(15)     VALUE SPACES.
          05 FILLER               PIC X(3)       VALUE SPACES.
          05 SUPPLIER-RATING-OUT  PIC X(15)     VALUE SPACES.


       01 WS-ADDRESSES.
           05 FILLER                PIC X(3)  VALUE SPACES.
           05 FILLER                PIC X(15) VALUE 'Order Address: '.
           05 ORDER-ADDRESS         PIC X(15) VALUE SPACES.
           05 FILLER                PIC X(100) VALUE SPACE.
           05 FILLER                PIC X(15) VALUE 'Sched Address: '.
           05 SCHED-ADDRESS         PIC X(15) VALUE SPACES.
           05 FILLER                PIC X(100) VALUE SPACE.
           05 FILLER                PIC X(15) VALUE 'Remit Address: '.
           05 REMIT-ADDRESS         PIC X(15) VALUE SPACE.



       01 WS-BOTTOM.
           05 FILLER                PIC X(3)  VALUE SPACES.
           05 FILLER                PIC X(133) VALUE SPACES.
           05 FILLER                PIC X(34) VALUE
           'Total # Purchase Orders: '.
           05 WS-TOTAL-PURCHASE-ORDER-O  PIC 9(03) VALUE ZERO.
           05 FILLER                PIC X(100) VALUE SPACE.
           05 FILLER                PIC X(28) VALUE
           'Total Price Purchase Orders:'.
           05 WS-TOTAL-PRICE-O      PIC $$$,$$$,$$$,$$$.99 VALUE ZERO.

           05 FILLER                 PIC X(33) VALUE SPACE.
           05 FILLER    PIC X(34) VALUE
           'Total Quantity in Purchase Orders:'.
           05 WS-TOTAL-QTY-PURCH-ORDER-O PIC 9(04) VALUE 0.
           05 FILLER                PIC X(133) VALUE SPACES.
           05 FILLER                PIC X(90) VALUE ALL '='.

       01 WS-FLAGS.
           05 WS-LINE-KTR               PIC 9(4) VALUE 0.


       01 WS-COUNTERS-AND-ACCUMULATORS.
           05 WS-CONTROL-BREAK-TOTAL        PIC 9(7)V99 VALUE ZERO.
           05 WS-PARTNUMBER-CTR             PIC 9(04) VALUE ZERO.
           05 WS-TOTAL-PURCH-ORDERS         PIC 9(04) VALUE ZERO.
           05 WS-TOTAL-QTY-IN-PURCH-ORDERS  PIC 9(04) VALUE ZERO.
           05 WS-TOTAL-PRICE-PURCH-ORDERS   PIC 9(08)V99 VALUE ZERO.

       PROCEDURE DIVISION.

       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-Main2 UNTIL GOODDATAIN-EOF-WS = 'Y'.
           PERFORM 600-CLOSE-FILES.
           GOBACK.

       000-Housekeeping.
      * Initialization Routine
           INITIALIZE PART-SUPP-ADDR-PO, WS-PART-SUPP-ADDR-PO-OUT.
      * Priming Read
           PERFORM 300-Open-Files.
           MOVE SPACES TO PRINT-REC.
           PERFORM 400-Read-GOODDATAIN.
           MOVE PART-NUMBER-PO TO WS-CONTROL-KEY.



       100-Main2.
      *    DISPLAY '100-Main'.
           PERFORM 200-PROCESS-DATA.
           PERFORM 700-CONTROL-BREAK.
           PERFORM 500-Write-ERRORFILE.
      * 9/18 Initializing counters before reading next record
      *S    INITIALIZE
           PERFORM 400-Read-GOODDATAIN.


       150-INIT-WS-FIELDS.
           INITIALIZE WS-COUNTERS-AND-ACCUMULATORS.
           INITIALIZE WS-ADDRESSES, WS-BOTTOM.

       200-PROCESS-DATA.
           IF NOT GOODDATA-END-OF-FILE   *> No duplicating last record
              IF WS-CONTROL-KEY = PART-NUMBER-PO *> Control Break Conditional
                THEN    PERFORM 210-CALCULATE
                        PERFORM 700-CONTROL-BREAK
                        PERFORM 400-Read-GOODDATAIN
                ELSE
                        WRITE PRINT-REC FROM WS-PARTS-DATA-OUT
                        WRITE PRINT-REC FROM WS-ADDRESSES
                        WRITE PRINT-REC FROM WS-BOTTOM
                        PERFORM 700-CONTROL-BREAK
                        INITIALIZE WS-ADDRESSES, WS-BOTTOM
                        PERFORM 210-CALCULATE
                        PERFORM 400-Read-GOODDATAIN
             END-IF
           END-IF.


       210-CALCULATE.
      * To get PartNumber, Weekslead time, vehicle make, Suppliers Name
      * and SUPPLIER RATING
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
           MOVE SUPPLIER-NAME-PO TO SUPPLIER-NAME-OUT.
           EVALUATE SUPPLIER-RATING-PO
                WHEN '3' MOVE 'HIGHEST QUALITY' TO SUPPLIER-RATING-OUT
                WHEN '2' MOVE 'AVERAGE QUALITY' TO SUPPLIER-RATING-OUT
                WHEN '1' MOVE 'LOWEST QUALITY' TO SUPPLIER-RATING-OUT
           END-EVALUATE.
      * Pending calculate Address and Purchase Information





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

           OPEN OUTPUT PRINT-LINE.




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
      * To count number of records readed from GOODDATAPIN file.
           IF (NOT GOODDATA-END-OF-FILE) THEN
              ADD +1 TO WS-IN-GOODDATA-CTR
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
           CLOSE  GOODDATAIN, ERRORFILE, PRINT-LINE.


       2000-ABEND-RTN.
           DISPLAY 'PROGRAM ENCOUNTERED AN ERROR'.
           EXIT.


       700-CONTROL-BREAK.

           IF NOT GOODDATA-END-OF-FILE
                THEN
                    ADD +1 TO WS-LINE-KTR

                    IF PART-NUMBER-PO IS NOT EQUAL TO WS-CONTROL-KEY
                        THEN
      * *> SET NEW CONTROL KEY
                            MOVE PART-NUMBER-PO TO WS-CONTROL-KEY
                            ADD +1 TO WS-PARTNUMBER-CTR
                            WRITE PRINT-REC FROM WS-BLANK-LINE
                            WRITE PRINT-REC FROM WS-HEADER
                            WRITE PRINT-REC FROM WS-UNDERLINE
                            PERFORM 150-INIT-WS-FIELDS
                        ELSE
                            WRITE PRINT-REC FROM WS-BLANK-LINE
                            WRITE PRINT-REC FROM WS-HEADER
                            WRITE PRINT-REC FROM WS-UNDERLINE
                   END-IF


           END-IF.


