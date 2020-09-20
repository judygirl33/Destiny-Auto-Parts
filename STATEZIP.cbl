       01  STATEZIP-TABLE.
           05  STATEZIP-LIST OCCURS 3000 TIMES
               ASCENDING KEY IS STATE-ACRO
               INDEXED BY STATEZIP-IDX.
              08  STATE-NAME       PIC X(15) VALUE SPACES.
              08  FILLER           PIC X(1) VALUE SPACES.
              08  STATE-ACRO       PIC X(2) VALUE SPACES.
              08  FILLER           PIC X(1) VALUE SPACES.
              08  STATEZIP-START   PIC X(5) VALUE SPACES.
              08  FILLER           PIC X(1) VALUE SPACES.
              08  STATEZIP-END     PIC X(5) VALUE SPACES.

       01  STATEZIP-INDEX       PIC 9(4) VALUE 1.
