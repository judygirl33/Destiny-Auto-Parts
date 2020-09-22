      *    SAMPLE LOGIC
      *
      *    1-) If you have a Required Field on blank:
      *           ADD +4 TO ERRORCOUNTER
      *           GOBACK
      *    2-) On case of other problems
      *           IF ERRORCOUNTER > 3
      *              ADD +4 TO ERRORCOUNTER
      *              GOBACK
      *           ELSE
      *              MOVE "Warning - Invalid Address Type"
      *                 TO ERROR-MESSAGE (ERRORCOUNTER)
      *           END-IF
      *
       01  DATA-ERRORS.
           05 ERROR-MESSAGES OCCURS 3 TIMES INDEXED BY ERRORS-IDX.
              08 ERROR-MESSAGE  PIC X(80)      VALUE SPACES.
           05 ERRORCOUNTER      PIC 9(02)      VALUE ZEROES.
              88 WARNING                       VALUE 1 THROUGH 3.
           05 DATA-ERROR-FLAG   PIC X          VALUE 'N'.
              88 WRONG-DATA                    VALUE 'Y'.
