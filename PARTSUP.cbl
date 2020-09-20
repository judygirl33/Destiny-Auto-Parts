       01  PART-SUPP-ADDR-PO.
           05 PARTS-PO.
               10  PART-NUMBER-PO       PIC X(23) VALUE SPACES.
               10  PART-NAME-PO         PIC X(14) VALUE SPACES.
               10  SPEC-NUMBER-PO       PIC X(07) VALUE SPACES.
               10  GOVT-COMML-CODE-PO   PIC X(01) VALUE SPACES.
               10  BLUEPRINT-NUMBER-PO  PIC X(10) VALUE SPACES.
               10  UNIT-OF-MEASURE-PO   PIC X(03) VALUE SPACES.
               10  WEEKS-LEAD-TIME-PO   PIC 9(03) VALUE ZERO.
               10  VEHICLE-MAKE-PO      PIC X(03) VALUE SPACES.
                    88 CHRYSLER-PO       VALUE 'CHR'.
                    88 FORD-PO           VALUE 'FOR'.
                    88 GM-PO             VALUE 'GM '.
                    88 VOLKSWAGON-PO     VALUE 'VW '.
                    88 TOYOTA-PO         VALUE 'TOY'.
                    88 JAGUAR-PO         VALUE 'JAG'.
                    88 PEUGEOT-PO        VALUE 'PEU'.
                    88 BMW-PO            VALUE 'BMW'.
               10  VEHICLE-MODEL-PO     PIC X(10) VALUE SPACES.
               10  VEHICLE-YEAR-PO      PIC X(04) VALUE '0000'.
               10  FILLER            PIC X(14) VALUE SPACES.
           05 SUPPLIERS-PO.
               10  SUPPLIER-CODE-PO     PIC X(10) VALUE SPACES.
               10  SUPPLIER-TYPE-PO     PIC X(01) VALUE SPACES.
                    88 SUBCONTRACTOR-PO  VALUE 'S'.
                    88 DISTRIBUTOR-PO    VALUE 'D'.
                    88 MANUFACTURER-PO   VALUE 'M'.
                    88 IMPORTER-PO       VALUE 'I'.
               10  SUPPLIER-NAME-PO     PIC X(15) VALUE SPACES.
               10  SUPPLIER-PERF-PO     PIC 9(03) VALUE ZERO.
               10  SUPPLIER-RATING-PO   PIC X(01) VALUE SPACES.
                    88 HIGHEST-QUALITY-PO VALUE '3'.
                    88 AVERAGE-QUALITY-PO VALUE '2'.
                    88 LOWEST-QUALITY-PO  VALUE '1'.
               10  SUPPLIER-STATUS-PO   PIC X(01) VALUE SPACES.
                    88 GOVT-COMM-PO       VALUE '1'.
                    88 GOVT-ONLY-PO       VALUE '2'.
                    88 COMMERCIAL-ONLY-PO VALUE '3'.
               10  SUPPLIER-ACT-DATE-PO PIC 9(08) VALUE ZERO.
           05 SUPP-ADDRESS-PO OCCURS 3 TIMES INDEXED BY ADDR-IDX.
               10 ADDRESS-TYPE-PO      PIC X(01) VALUE SPACES.
                  88 ORDER-ADDRESS-PO           VALUE '1'.
                  88 SCHED-ADDRESS-PO           VALUE '2'.
                  88 REMIT-ADDRESS-PO           VALUE '3'.
               10 ADDRESS-1-PO         PIC X(15) VALUE SPACES.
               10 ADDRESS-2-PO         PIC X(15) VALUE SPACES.
               10 ADDRESS-3-PO         PIC X(15) VALUE SPACES.
               10 CITY-PO              PIC X(15) VALUE SPACES.
               10 ADDR-STATE-PO        PIC X(02) VALUE SPACES.
               10 ZIP-CODE-PO          PIC 9(10) VALUE ZERO.
           05 PURCHASE-ORDER-PO OCCURS 3 TIMES INDEXED BY PO-IDX.
               10  PO-NUMBER-PO         PIC X(06) VALUE SPACES.
               10  BUYER-CODE-PO        PIC X(03) VALUE SPACES.
               10  QUANTITY-PO          PIC S9(7) VALUE ZERO.
               10  UNIT-PRICE-PO        PIC S9(7)V99 VALUE ZERO.
               10  ORDER-DATE-PO        PIC 9(08) VALUE ZERO.
               10  DELIVERY-DATE-PO     PIC 9(08) VALUE ZERO.
