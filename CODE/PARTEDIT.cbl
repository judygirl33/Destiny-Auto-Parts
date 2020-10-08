       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTEDIT.
       AUTHOR. IVANNA COLAN.
      ******************************************************************
      * validates the data of the PARTs PORTION OF a PARTSUPP record
      *9/18 used 05 variables instead of PARTS group variable to avoid
      * issue with COMP field weeks-lead-time.
      *9/25 testing if errors > 3 after counting one more error
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VEHICLE-YEAR-INT          PIC 9(04).



       LINKAGE SECTION.
      *COPY PARTS. *>Parts Copybook
       COPY ERRORS.
       01  PART-NUMBER       PIC X(23) VALUE SPACES.
       01  PART-NAME         PIC X(14) VALUE SPACES.
       01  SPEC-NUMBER       PIC X(07) VALUE SPACES.
       01  GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
       01  BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
       01  UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
       01  WEEKS-LEAD-TIME   PIC S9(04) COMP VALUE ZEROS.
       01  VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                88 CHRYSLER       VALUE 'CHR'.
                88 FORD           VALUE 'FOR'.
                88 GM             VALUE 'GM '.
                88 VOLKSWAGON     VALUE 'VW '.
                88 TOYOTA         VALUE 'TOY'.
                88 JAGUAR         VALUE 'JAG'.
                88 PEUGEOT        VALUE 'PEU'.
                88 BMW            VALUE 'BMW'.
       01  VEHICLE-MODEL     PIC X(10) VALUE SPACES.
       01  VEHICLE-YEAR      PIC X(04) VALUE '0000'.


       PROCEDURE DIVISION USING PART-NUMBER, PART-NAME,
           SPEC-NUMBER, GOVT-COMML-CODE, BLUEPRINT-NUMBER,
           UNIT-OF-MEASURE, WEEKS-LEAD-TIME, VEHICLE-MAKE,
           VEHICLE-MODEL, VEHICLE-YEAR, DATA-ERRORS.

      *9/24 WHEN A MANDATORY FIELD IS BLANK, WE ADD 4 TO ERRORCOUNTER TO
      * CONSIDER THE RECORD WRONG AND NOT KEEP ANALYZING THE REST>>>>>>
      *Validating PART-NUMBER should NOT be blank
           IF PART-NUMBER = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.
      *Validating PART-NAME should NOT be blank
           IF PART-NAME = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.
      *Validating VEHICLE-MAKE should NOT be blank
           IF VEHICLE-MAKE = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.
      *Validating VEHICLE-MODEL should NOT be blank
           IF VEHICLE-MODEL = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.
      *Validating VEHICLE-YEAR should NOT be blank
           IF VEHICLE-YEAR = '0000' OR VEHICLE-YEAR = SPACES
                THEN  ADD +4 TO ERRORCOUNTER
                      GOBACK
           END-IF.
      *Validating VEHICLE MAKE to be one of the 88 level fields
           EVALUATE TRUE
              WHEN VEHICLE-MAKE = 'CHR' CONTINUE
              WHEN VEHICLE-MAKE = 'FOR' CONTINUE
              WHEN VEHICLE-MAKE = 'GM'  CONTINUE
              WHEN VEHICLE-MAKE = 'VW' CONTINUE
              WHEN VEHICLE-MAKE = 'TOY' CONTINUE
              WHEN VEHICLE-MAKE = 'JAG' CONTINUE
              WHEN VEHICLE-MAKE = 'PEU' CONTINUE
              WHEN VEHICLE-MAKE = 'BMW' CONTINUE
              WHEN OTHER ADD +1 TO ERRORCOUNTER
                         IF ERRORCOUNTER > 3
                           THEN ADD +4 TO ERRORCOUNTER
                               GOBACK
                           ELSE
                             MOVE 'WARNING - INVALID VEHICLE-MAKE'
                             TO ERROR-MESSAGE (ERRORCOUNTER)
                         END-IF
           END-EVALUATE.


      *Validating VEHICLE YEAR to be between 1990 and 2019

           COMPUTE WS-VEHICLE-YEAR-INT = FUNCTION NUMVAL
                (VEHICLE-YEAR).
           IF WS-VEHICLE-YEAR-INT < 1990 OR WS-VEHICLE-YEAR-INT > 2019
                    THEN ADD +1 TO ERRORCOUNTER
                         IF ERRORCOUNTER > 3
                           THEN ADD +4 TO ERRORCOUNTER
                               GOBACK
                           ELSE
                             MOVE 'WARNING - INVALID VEHICLE YEAR'
                             TO ERROR-MESSAGE (ERRORCOUNTER)
                         END-IF
           END-IF.
      *Validate WEEKS LEAD TIME to be numeric and between 1 and 4

                IF WEEKS-LEAD-TIME < 001 OR WEEKS-LEAD-TIME > 004
                   THEN ADD +1 TO ERRORCOUNTER
                        IF ERRORCOUNTER > 3
                           THEN ADD +4 TO ERRORCOUNTER
                               GOBACK
                           ELSE
                             MOVE 'WARNING - INVALID WEEKS LEAD TIME'
                             TO ERROR-MESSAGE (ERRORCOUNTER)
                        END-IF
               END-IF.
           DISPLAY 'NUMBER OF ERRORS IN PARTS: '
           ERRORCOUNTER.

           GOBACK.
