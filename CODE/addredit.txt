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
           COPY STATEZIP
           LS-ERRORCODE   PIC X(02)   VALUE ZEROES.

       PROCEDURE DIVISION.
