       IDENTIFICATION DIVISION.
       PROGRAM-ID. PARTEDIT.
       AUTHOR. IVANNA COLÁN.
      ******************************************************************
      * validates the data of the PARTs PORTION OF a PARTSUPP record
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VEHICLE-YEAR-INT          PIC 9(04).

       LINKAGE SECTION.
       COPY PARTS. *>Parts Copybook
       01 LS-PARTEDIT-ERRORCOUNTER      PIC 9(02) VALUE ZERO.

       PROCEDURE DIVISION USING PARTS, LS-PARTEDIT-ERRORCOUNTER.
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
              WHEN VEHICLE-MAKE IN PARTS = 'CHR' DISPLAY 'V-MAKE CHR'
              WHEN VEHICLE-MAKE IN PARTS = 'FOR' DISPLAY 'V-MAKE FOR'
              WHEN VEHICLE-MAKE IN PARTS = 'GM'  DISPLAY 'V-MAKE GM'
              WHEN VEHICLE-MAKE IN PARTS = 'VW' DISPLAY 'V-MAKE VW'
              WHEN VEHICLE-MAKE IN PARTS = 'TOY' DISPLAY 'V-MAKE TOY'
              WHEN VEHICLE-MAKE IN PARTS = 'JAG' DISPLAY 'V-MAKE JAG'
              WHEN VEHICLE-MAKE IN PARTS = 'PEU' DISPLAY 'V-MAKE PEU'
              WHEN VEHICLE-MAKE IN PARTS = 'BMW' DISPLAY 'V-MAKE BMW'
              WHEN OTHER ADD +1 TO LS-PARTEDIT-ERRORCOUNTER
           END-EVALUATE.



      *Validating VEHICLE YEAR to be between 1990 and 2019
           COMPUTE WS-VEHICLE-YEAR-INT = FUNCTION NUMVAL
           (VEHICLE-YEAR IN PARTS).
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