       01 WS-BREAK-CONTROLS.
           05 WS-CONTROL-KEY    PIC X(23)      VALUE SPACES.

 *************************************************************
      ****** Report headings ******
      *************************************************************
       01 WS-BLANK-LINE       PIC X(132)     VALUE SPACES.
       01 WS-HEADER.
          05 FILLER           PIC X(3)       VALUE SPACES.
          05 FILLER           PIC X(18)      VALUE 'Part Name'.
          05 FILLER           PIC X(06)      VALUE SPACE.
          05 FILLER           PIC X(15)      VALUE 'Weeks Lead Time'.
          05 FILLER           PIC X(5)       VALUE SPACES.
          05 FILLER           PIC X(12)      VALUE 'Vehicle Make'.
          05 FILLER           PIC X(5)       VALUE SPACES.
          05 FILLER           PIC X(13)      VALUE 'Supplier Name'.
          05 FILLER           PIC X(4)       VALUE SPACES.
          05 FILLER           PIC X(15)      VALUE 'Supplier Rating'.

       01 WS-UNDERLINE.
           05 FILLER          PIC X(3)       VALUE SPACES.
           05 FILLER          PIC X(23)      VALUE ALL '='.
           05 FILLER          PIC X(01)      VALUE SPACE.
           05 FILLER          PIC X(15)      VALUE ALL '='.
           05 FILLER          PIC X(05)      VALUE SPACE.
           05 FILLER          PIC X(12)      VALUE ALL '='.
           05 FILLER          PIC X(5)       VALUE SPACES.
           05 FILLER          PIC X(13)      VALUE ALL '='.
           05 FILLER          PIC X(04)      VALUE SPACES.
           05 FILLER          PIC X(15)      VALUE ALL '='.


       01 WS-PARTS-DATA-OUT.
          05 FILLER               PIC X(3)       VALUE SPACES.
          05 PART-NUMBER-OUT      PIC X(23)      VALUE SPACES.
          05 FILLER               PIC X(08)      VALUE SPACES.
          05 WEEKS-LEAD-TIME-OUT  PIC 9(03)     VALUE ZERO.
          05 FILLER               PIC X(10)       VALUE SPACES.
          05 VEHICLE-MAKE-OUT     PIC X(10)       VALUE SPACES.
          05 FILLER               PIC X(7)       VALUE SPACES.
          05 SUPPLIER-NAME-OUT    PIC X(15)     VALUE SPACES.
          05 FILLER               PIC X(2)       VALUE SPACES.
          05 SUPPLIER-RATING-OUT  PIC X(15)     VALUE SPACES.


       01 WS-ADDRESSES-1.
           05 FILLER                PIC X(3)  VALUE SPACES.
           05 FILLER                PIC X(15) VALUE 'Order Address: '.
           05 WS-ORDER-ADDRESS         PIC X(114) VALUE SPACES.
       01 WS-ADDRESSES-2.
           05 FILLER                PIC X(3)   VALUE SPACES.
           05 FILLER                PIC X(15)  VALUE 'Sched Address: '.
           05 WS-SCHED-ADDRESS         PIC X(114) VALUE SPACES.
       01 WS-ADDRESSES-3.
           05 FILLER                PIC X(3)  VALUE SPACES.
           05 FILLER                PIC X(15) VALUE 'Remit Address: '.
           05 WS-REMIT-ADDRESS         PIC X(114) VALUE SPACES.



       01 WS-FOOTER-1.
           05 FILLER   PIC X(10) VALUE SPACES.
           05 FILLER   PIC X(25)
                       VALUE 'Total # Purchase Orders: '.
           05 WS-TOTAL-PURCHASE-ORDER-O  PIC 9(03) VALUE ZERO.
       01 WS-FOOTER-2.
           05 FILLER   PIC X(6) VALUE SPACES.
           05 FILLER   PIC X(29)
                       VALUE 'Total Price Purchase Orders: '.
           05 WS-TOTAL-PRICE-O      PIC $$$,$$$,$$$,$$$.99 VALUE ZERO.
       01 WS-FOOTER-3.
           05 FILLER   PIC X(35)
                       VALUE 'Total Quantity in Purchase Orders: '.
           05 WS-TOTAL-QTY-PURCH-ORDER-O PIC 9(04) VALUE 0.

       01 WS-COUNTERS-AND-ACCUMULATORS.
           05 WS-CONTROL-BREAK-TOTAL        PIC 9(7)V99 VALUE ZERO.
           05 WS-PARTNUMBER-CTR             PIC 9(04) VALUE ZERO.
           05 WS-TOTAL-PURCH-ORDERS         PIC 9(04) VALUE ZERO.
           05 WS-TOTAL-QTY-IN-PURCH-ORDERS  PIC 9(04) VALUE ZERO.
           05 WS-TOTAL-PRICE-PURCH-ORDERS   PIC 9(08)V99 VALUE ZERO.
