       IDENTIFICATION DIVISION.
       PROGRAM-ID. FINALRPT.
       AUTHOR IVANNA COLAN
      ******************************************************************
      * 9/26 Program to generate break out Report from the input file
      *  GOODDATA, which contains only correct records of GOODDATA file
      * ******************************************

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Input File GOODDATA with correct data records
           SELECT GOODDATAIN ASSIGN TO GOODDATA
           FILE STATUS IS IN-GOODDATA-KEY.

           SELECT RPTFILE ASSIGN TO RPTFILE
           FILE STATUS IS REPORT-KEY.

      * Output report
           SELECT ERRORFILE ASSIGN TO ERRFILE
           FILE STATUS IS OUT-ERRORFILE-KEY.

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
              88 PARTSUP-END-OF-FILE                    VALUE 'Y'.



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
           PERFORM 400-Read-GOODDATAIN.


       100-Main2.
      *    DISPLAY '100-Main'.
           PERFORM 200-PROCESS-DATA.
           PERFORM 500-Write-ERRORFILE.
      * 9/18 Initializing counters before reading next record
      *S    INITIALIZE
           PERFORM 400-Read-GOODDATAIN.


       200-PROCESS-DATA.
           DISPLAY 'CREATE  REPORT'.




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
           IF (NOT PARTSUP-END-OF-FILE) THEN
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
           CLOSE  GOODDATAIN, ERRORFILE.


       2000-ABEND-RTN.
           DISPLAY 'PROGRAM ENCOUNTERED AN ERROR'.
           EXIT.
