       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTEDIT.
       AUTHOR. IVANNA COLÁN.
      ******************************************************************
      * validates the data of the PARTs PORTION OF a PARTSUPP record
      *9/18 used 05 variables instead of PARTS group variable to avoid
      * issue with COMP field weeks-lead-time. It still shows incorrect
      *  ly value fo that field.
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

       01 LS-PARTEDIT-ERRORCOUNTER      PIC 9(02) VALUE ZERO.

       PROCEDURE DIVISION USING PART-NUMBER, PART-NAME,
           SPEC-NUMBER, GOVT-COMML-CODE, BLUEPRINT-NUMBER,
           UNIT-OF-MEASURE, WEEKS-LEAD-TIME, VEHICLE-MAKE,
           VEHICLE-MODEL, VEHICLE-YEAR, LS-PARTEDIT-ERRORCOUNTER.
      *Validating PART-NUMBER should NOT be blank
           IF PART-NUMBER = SPACES
                THEN  ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-IF.
      *Validating PART-NAME should NOT be blank
           IF PART-NAME = SPACES
                THEN  ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-IF.
      *Validating VEHICLE-MAKE should NOT be blank
           IF VEHICLE-MAKE = SPACES
                THEN  ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-IF.
      *Validating VEHICLE-MODEL should NOT be blank
           IF VEHICLE-MODEL = SPACES
                THEN  ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-IF.
      *Validating VEHICLE-YEAR should NOT be blank
           IF VEHICLE-YEAR = '0000' OR VEHICLE-YEAR = SPACES
                THEN  ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-IF.
      *Validating VEHICLE MAKE to be one of the 88 level fields
           EVALUATE TRUE
              WHEN VEHICLE-MAKE = 'CHR' DISPLAY 'V-MAKE CHR'
              WHEN VEHICLE-MAKE = 'FOR' DISPLAY 'V-MAKE FOR'
              WHEN VEHICLE-MAKE = 'GM'  DISPLAY 'V-MAKE GM'
              WHEN VEHICLE-MAKE = 'VW' DISPLAY 'V-MAKE VW'
              WHEN VEHICLE-MAKE = 'TOY' DISPLAY 'V-MAKE TOY'
              WHEN VEHICLE-MAKE = 'JAG' DISPLAY 'V-MAKE JAG'
              WHEN VEHICLE-MAKE = 'PEU' DISPLAY 'V-MAKE PEU'
              WHEN VEHICLE-MAKE = 'BMW' DISPLAY 'V-MAKE BMW'
              WHEN OTHER ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-EVALUATE.


      *Validating VEHICLE YEAR to be between 1990 and 2019
           COMPUTE WS-VEHICLE-YEAR-INT = FUNCTION NUMVAL
           (VEHICLE-YEAR).
           IF WS-VEHICLE-YEAR-INT < 1990 OR
              WS-VEHICLE-YEAR-INT > 2019
              THEN ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-IF.
      *Validate WEEKS LEAD TIME to be numeric and between 1 and 4
           IF WEEKS-LEAD-TIME < 001 OR WEEKS-LEAD-TIME > 004
                   THEN ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-IF.
           DISPLAY 'NUMBER OF ERRORS IN PARTS: '
           LS-PARTEDIT-ERRORCOUNTER.

           GOBACK.