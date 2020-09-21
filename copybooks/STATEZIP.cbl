       01  STATEZIP-TABLE.
           05  STATEZIP-LIST OCCURS 100 TIMES
               ASCENDING KEY IS STATE-ACRO
               INDEXED BY STATEZIP-IDX.
              08  STATE-NAME       PIC X(16) VALUE SPACES.
              08  STATE-ACRO       PIC X(2) VALUE SPACES.
              08  FILLER           PIC X(2) VALUE SPACES.
              08  STATEZIP-START   PIC X(5) VALUE SPACES.
              08  FILLER           PIC X(3) VALUE SPACES.
              08  STATEZIP-END     PIC X(5) VALUE SPACES.

       01  STATEZIP-INDEX       PIC 9(4) VALUE 1.
       01  STATEZIP-MAX         PIC 9(4) VALUE 1.
