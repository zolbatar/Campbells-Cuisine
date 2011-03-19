       IDENTIFICATION DIVISION.

       PROGRAM-ID.      CAMPBELLS-INVOICING.
       AUTHOR.          DARYL-DUDEY.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT PRINTER-FILE ASSIGN TO PRINTER.
           SELECT CURRENT-INVOICE-FILE ASSIGN TO "CURINV.DAT".
           SELECT INVOICE-FILE ASSIGN TO "CUSTINV.DAT"
	   ORGANIZATION IS RELATIVE
	   ACCESS MODE IS RANDOM
	   RELATIVE KEY IS CURRENT-INVOICE.

       DATA DIVISION.

       FILE SECTION.
       FD PRINTER-FILE REPORT IS PRINT-REPORT.

       FD CURRENT-INVOICE-FILE.
       01 CURRENT-INVOICE-FILE-DATA.
         02 CUR-INVOICE                PIC 9(4).

       FD INVOICE-FILE
	  BLOCK CONTAINS 1000 RECORDS.
       01 INVOICE-DATA.
         02 GOODS-DATA.
           03 F-DESC1                  PIC X(25).
           03 F-QTY1                   PIC 999.
           03 F-PRICE1                 PIC 99V99.
           03 F-TOTAL1                 PIC 9999V99.
           03 F-DESC2                  PIC X(25).
           03 F-QTY2                   PIC 999.
           03 F-PRICE2                 PIC 99V99.
           03 F-TOTAL2                 PIC 9999V99.
           03 F-DESC3                  PIC X(25).
           03 F-QTY3                   PIC 999.
           03 F-PRICE3                 PIC 99V99.
           03 F-TOTAL3                 PIC 9999V99.
           03 F-DESC4                  PIC X(25).
           03 F-QTY4                   PIC 999.
           03 F-PRICE4                 PIC 99V99.
           03 F-TOTAL4                 PIC 9999V99.
           03 F-DESC5                  PIC X(25).
           03 F-QTY5                   PIC 999.
           03 F-PRICE5                 PIC 99V99.
           03 F-TOTAL5                 PIC 9999V99.
           03 F-DESC6                  PIC X(25).
           03 F-QTY6                   PIC 999.
           03 F-PRICE6                 PIC 99V99.
           03 F-TOTAL6                 PIC 9999V99.
           03 F-DESC7                  PIC X(25).
           03 F-QTY7                   PIC 999.
           03 F-PRICE7                 PIC 99V99.
           03 F-TOTAL7                 PIC 9999V99.
           03 F-DESC8                  PIC X(25).
           03 F-QTY8                   PIC 999.
           03 F-PRICE8                 PIC 99V99.
           03 F-TOTAL8                 PIC 9999V99.
           03 F-DESC9                  PIC X(25).
           03 F-QTY9                   PIC 999.
           03 F-PRICE9                 PIC 99V99.
           03 F-TOTAL9                 PIC 9999V99.
           03 F-DESC10                 PIC X(25).
           03 F-QTY10                  PIC 999.
           03 F-PRICE10                PIC 99V99.
           03 F-TOTAL10                PIC 9999V99.
           03 F-DESC11                 PIC X(25).
           03 F-QTY11                  PIC 999.
           03 F-PRICE11                PIC 99V99.
           03 F-TOTAL11                PIC 9999V99.
           03 F-DESC12                 PIC X(25).
           03 F-QTY12                  PIC 999.
           03 F-PRICE12                PIC 99V99.
           03 F-TOTAL12                PIC 9999V99.
           03 F-DESC13                 PIC X(25).
           03 F-QTY13                  PIC 999.
           03 F-PRICE13                PIC 99V99.
           03 F-TOTAL13                PIC 9999V99.
           03 F-DESC14                 PIC X(25).
           03 F-QTY14                  PIC 999.
           03 F-PRICE14                PIC 99V99.
           03 F-TOTAL14                PIC 9999V99.
           03 F-DESC15                 PIC X(25).
           03 F-QTY15                  PIC 999.
           03 F-PRICE15                PIC 99V99.
           03 F-TOTAL15                PIC 9999V99.
           03 F-DESC16                 PIC X(25).
           03 F-QTY16                  PIC 999.
           03 F-PRICE16                PIC 99V99.
           03 F-TOTAL16                PIC 9999V99.
           03 F-DESC17                 PIC X(25).
           03 F-QTY17                  PIC 999.
           03 F-PRICE17                PIC 99V99.
           03 F-TOTAL17                PIC 9999V99.
           03 F-DESC18                 PIC X(25).
           03 F-QTY18                  PIC 999.
           03 F-PRICE18                PIC 99V99.
           03 F-TOTAL18                PIC 9999V99.
           03 F-DESC19                 PIC X(25).
           03 F-QTY19                  PIC 999.
           03 F-PRICE19                PIC 99V99.
           03 F-TOTAL19                PIC 9999V99.
           03 F-DESC20                 PIC X(25).
           03 F-QTY20                  PIC 999.
           03 F-PRICE20                PIC 99V99.
           03 F-TOTAL20                PIC 9999V99.
         02 FCUSTOMER-DATA.
           03 FCUSTOMER-NAME           PIC X(20).
           03 FCUSTOMER-ADDRESS1       PIC X(20).
           03 FCUSTOMER-ADDRESS2       PIC X(20).
           03 FCUSTOMER-ADDRESS3       PIC X(20).
           03 FCUSTOMER-ADDRESS4       PIC X(20).
           03 FCUSTOMER-ADDRESS5       PIC X(20).
           03 FCUSTOMER-ADDRESS6       PIC X(20).
           03 FCUSTOMER-TELEPHONE      PIC X(20).
           03 F-CUR-GOODS              PIC 99.
           03 F-DATE                   PIC X(8).

       WORKING-STORAGE SECTION.

      *SCREEN STORE RESTORE FEATURE VARIABLES.

       01 TEMP-SCREEN-DATA             PIC X(4000).
       01 SCREEN-STATE OCCURS 6 TIMES.
         02 SCREEN-DATA                PIC X(4000).
       01 FUNCTION                     PIC 99 USAGE COMP-X.
       01 ST-RE-NUMBER                 PIC 9.
       01 GET-SCREEN-DATA-TABLE.
         02 LENGTH-FIELD               PIC 9999 USAGE COMP-X.
         02 SCREEN-START               PIC 9999 USAGE COMP-X.
         02 BUFFER-START               PIC 9999 USAGE COMP-X.

      *CUSTOMER DATA VARIABLES.

       01 CUSTOMER-DATA.
         02 CUSTOMER-NAME              PIC X(20).
         02 CUSTOMER-ADDRESS1          PIC X(20).
         02 CUSTOMER-ADDRESS2          PIC X(20).
         02 CUSTOMER-ADDRESS3          PIC X(20).
         02 CUSTOMER-ADDRESS4          PIC X(20).
         02 CUSTOMER-ADDRESS5          PIC X(20).
         02 CUSTOMER-ADDRESS6          PIC X(20).
         02 CUSTOMER-TELEPHONE         PIC X(20).

      *CUSTOMER GOODS VARIABLES.

       01 CUR-GOODS                    PIC 99.
       01 ITEM-NUMBER                  PIC 99.
       01 GOODS-DATA OCCURS 20 TIMES.
         02 GOODS-DESC                 PIC X(25).
         02 GOODS-QTY                  PIC 999.
         02 GOODS-PRICE                PIC 99V99.
         02 GOODS-TOTAL                PIC 9999V99.
       01 TEMP-GD1                     PIC X(25).
       01 TEMP-GQ1                     PIC 999.
       01 TEMP-GP1                     PIC 99V99.
       01 TEMP-GT1                     PIC 9999V99.

      *CONSTANT VARIABLES.

       01 MAX-GOODS		       PIC 99 VALUE 21.
       01 CHANGE-TO-BOLD1	       PIC X VALUE X"1B".
       01 CHANGE-TO-BOLD2	       PIC X VALUE X"45".
       01 CHANGE-TO-ELONG	       PIC X VALUE X"0E".
       01 END-BOLD1		       PIC X VALUE X"1B".
       01 END-BOLD2		       PIC X VALUE X"46".
       01 END-ELONG		       PIC X VALUE X"14".

      *MAIN VARIABLES

       01 CURRENT-INVOICE              PIC 9(4).
       01 NEW-INVOICE                  PIC 9(4).
       01 COMP-TOTAL                   PIC 9(6)V99.
       01 TODAYS-DATE.
         02 DATE-YEAR                  PIC 99.
         02 DATE-MONTH                 PIC 99.
         02 DATE-DAY                   PIC 99.
       01 STRINGED-DATE                PIC X(8).
       01 PRINT-DATE                   PIC X(8).
       01 DEL-PROMPT
       PIC 99.

      *MENU VARIABLES.

       01 MENU-OPTION                  PIC X.

      *MULTI PURPOSE VARIABLES.

       01 ANY-KEY                      PIC X.
       01 D-L                          PIC 99.
       01 LOOP-VARIABLE                PIC 99.
       01 LOOP-VARIABLE2               PIC 99.
       01 L-V                          PIC 99.

       REPORT SECTION.

       RD PRINT-REPORT         PAGE LIMIT 70 LINES
                               HEADING 1
                               FIRST DETAIL 6
                               LAST DETAIL 70.

       01 TYPE PAGE HEADING.
	 02 LINE 1.
	 02 COLUMN 1 PIC X SOURCE CHANGE-TO-BOLD1.
	 02 COLUMN 2 PIC X SOURCE CHANGE-TO-BOLD2.
	 02 COLUMN 3 PIC X SOURCE CHANGE-TO-ELONG.
	 02 COLUMN 13 PIC X(21) VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
	 02 LINE 2.
	 02 COLUMN 1 PIC X SOURCE CHANGE-TO-ELONG.
	 02 COLUMN 11 PIC X(21) VALUE "ณCAMPBELL'S CUISINE ณ".
	 02 LINE 3.
	 02 COLUMN 1  PIC X SOURCE CHANGE-TO-ELONG.
	 02 COLUMN 11 PIC X(21) VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
	 02 LINE 4.
	 02 COLUMN 28 PIC X(26) VALUE "FOR THE PROFESSIONAL TOUCH".
	 02 COLUMN 76 PIC X SOURCE END-BOLD1.
	 02 COLUMN 77 PIC X SOURCE END-BOLD2.
	 02 COLUMN 78 PIC X SOURCE END-ELONG.
       01 PRINT-REPORT-GROUP TYPE DETAIL.
         02 LINE 6.
           03 COLUMN 1  PIC X(22) VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
           03 COLUMN 23 PIC X(22) VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
           03 COLUMN 50 PIC X(15) VALUE "Invoice number:".
           03 COLUMN 66 PIC Z(4) SOURCE CURRENT-INVOICE.
         02 LINE 7.
           03 COLUMN 1  PIC X(22) VALUE "ณCAMPBELL'S CUISINE  ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-NAME.
         02 LINE 8.
           03 COLUMN 1  PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 50 PIC X(5)  VALUE "Date:".
           03 COLUMN 56 PIC X(8) SOURCE PRINT-DATE.
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS1.
         02 LINE 9.
           03 COLUMN 1  PIC X(22) VALUE "ณ28, THORNBURY LANE  ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS2.
         02 LINE 10.
           03 COLUMN 1  PIC X(22) VALUE "ณCHURCH HILL NORTH   ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS3.
         02 LINE 11.
           03 COLUMN 1  PIC X(22) VALUE "ณREDDITCH            ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS4.
         02 LINE 12.
           03 COLUMN 1  PIC X(22) VALUE "ณB98 8SG             ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS5.
         02 LINE 13.
           03 COLUMN 1  PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 24 PIC X(15) SOURCE CUSTOMER-ADDRESS6.
         02 LINE 14.
           03 COLUMN 1  PIC X(22) VALUE "ณ(0527) 69910        ณ".
           03 COLUMN 23 PIC X(22) VALUE "ณ                    ณ".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-TELEPHONE.
         02 LINE 15.
           03 COLUMN 1  PIC X(22) VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
           03 COLUMN 23 PIC X(22) VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
         02 LINE 24 COLUMN 10 PIC X(60) VALUE
         "ฺฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤยฤฤฤฤฤยฤฤฤฤฤฤฤฟ".
         02 LINE 25 COLUMN 10 PIC X(60) VALUE
         "ณITEMณDESCRIPTION                        ณQTYณPRICEณTOTAL  ณ".
         02 LINE 26 COLUMN 10 PIC X(60) VALUE
         "รฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤลฤฤฤฤฤลฤฤฤฤฤฤฤด".
         02 LINE 27.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 1".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(1).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(1).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(1).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(1).
         02 LINE 28.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 2".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(2).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(2).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(2).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(2).
         02 LINE 29.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 3".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(3).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(3).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(3).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(3).
         02 LINE 30.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 4".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(4).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(4).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(4).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(4).
         02 LINE 31.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 5".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(5).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(5).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(5).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(5).
         02 LINE 32.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 6".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(6).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(6).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(6).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(6).
         02 LINE 33.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 7".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(7).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(7).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(7).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(7).
         02 LINE 34.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 8".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(8).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(8).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(8).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(8).
         02 LINE 35.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE " 9".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(9).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(9).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(9).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(9).
         02 LINE 36.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "10".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(10).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(10).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(10).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(10).
         02 LINE 37.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "11".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(11).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(11).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(11).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(11).
         02 LINE 38.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "12".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(12).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(12).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(12).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(12).
         02 LINE 39.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "13".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(13).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(13).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(13).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(13).
         02 LINE 40.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "14".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(14).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(14).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(14).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(14).
         02 LINE 41.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "15".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(15).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(15).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(15).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(15).
         02 LINE 42.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "16".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(16).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(16).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(16).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(16).
         02 LINE 43.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "17".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(17).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(17).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(17).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(17).
         02 LINE 44.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "18".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(18).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(18).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(18).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(18).
         02 LINE 45.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "19".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(19).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(19).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(19).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(19).
         02 LINE 46.
           03 COLUMN 10 PIC X(60) VALUE
         "ณ    ณ                                   ณ   ณ     ณ       ณ".
           03 COLUMN 11 PIC XX VALUE "20".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(20).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(20).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(20).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(20).
         02 LINE 47.
           03 COLUMN 10 PIC X(60) VALUE
         "ภฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤมฤฤฤมฤฤฤฤฤมฤฤฤฤฤฤฤด".
         02 LINE 48.
           03 COLUMN 10 PIC X(60) VALUE
         "                                    ณ AMOUNT DUE:          ณ".
	   03 COLUMN 59 PIC Z(6).ZZ SOURCE COMP-TOTAL.
         02 LINE 49.
           03 COLUMN 10 PIC X(60) VALUE
         "                                    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
	 02 LINE 61.
	 02 COLUMN 1 PIC X SOURCE CHANGE-TO-BOLD1.
	 02 COLUMN 2 PIC X SOURCE CHANGE-TO-BOLD2.
	 02 COLUMN 21 PIC X(43) VALUE
         "PLEASE MAKE CHEQUES PAYABLE TO S.M.CAMPBELL".
	 02 LINE 67.
	 02 COLUMN 1 PIC X SOURCE END-BOLD1.
	 02 COLUMN 2 PIC X SOURCE END-BOLD2.
	 02 LINE 68 COLUMN 15 PIC X(51) VALUE
         "BON APPETIT! BON APPETIT! BON APPETIT! BON APPETIT!".
         02 LINE 70 PIC X(1) VALUE SPACE.

       SCREEN SECTION.

       01 BLANK-SCREEN.
         02 BLANK SCREEN.
       01 MAIN-TITLE.
         02 LINE 1  COLUMN 29 VALUE "ษออออออออออออออออออออป".
         02 LINE 2  COLUMN 29 VALUE "บ CAMPBELL'S CUISINE บ".
         02 LINE 3  COLUMN 29 VALUE "ศออออออออออออออออออออผ".
       01 SMALL-CUSTOMER-DATA-SCREEN.
         02 LINE 5  COLUMN 1  VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
         02 LINE 6  COLUMN 1  VALUE "ณ       CUSTOMER DETAILS        ณ".
         02 LINE 7  COLUMN 1  VALUE "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด".
         02 LINE 8  COLUMN 1  VALUE "ณ     NAME:                     ณ".
         02 LINE 9  COLUMN 1  VALUE "ณ  ADDRESS:                     ณ".
         02 LINE 10 COLUMN 1  VALUE "ณ                               ณ".
         02 LINE 11 COLUMN 1  VALUE "ณ                               ณ".
         02 LINE 12 COLUMN 1  VALUE "ณ                               ณ".
         02 LINE 13 COLUMN 1  VALUE "ณ                               ณ".
         02 LINE 14 COLUMN 1  VALUE "ณ                               ณ".
         02 LINE 15 COLUMN 1  VALUE "ณTELEPHONE:                     ณ".
         02 LINE 16 COLUMN 1  VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
         02 LINE 8  COLUMN 12 PIC X(20) FROM CUSTOMER-NAME.
         02 LINE 9  COLUMN 12 PIC X(20) FROM CUSTOMER-ADDRESS1.
         02 LINE 10 COLUMN 12 PIC X(20) FROM CUSTOMER-ADDRESS2.
         02 LINE 11 COLUMN 12 PIC X(20) FROM CUSTOMER-ADDRESS3.
         02 LINE 12 COLUMN 12 PIC X(20) FROM CUSTOMER-ADDRESS4.
         02 LINE 13 COLUMN 12 PIC X(20) FROM CUSTOMER-ADDRESS5.
	 02 LINE 14 COLUMN 12 PIC X(20) FROM CUSTOMER-ADDRESS6.
         02 LINE 15 COLUMN 12 PIC X(20) FROM CUSTOMER-TELEPHONE.
       01 LARGE-CUSTOMER-DATA-SCREEN.
         02 LINE 5  COLUMN 1  VALUE
         "ษอออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 6  COLUMN 1  VALUE
         "บ       CUSTOMER DETAILS        บ" HIGHLIGHT.
         02 LINE 7  COLUMN 1  VALUE
         "ฬอออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 8  COLUMN 1  VALUE
         "บ     NAME:                     บ" HIGHLIGHT.
         02 LINE 9  COLUMN 1  VALUE
         "บ  ADDRESS:                     บ" HIGHLIGHT.
         02 LINE 10 COLUMN 1  VALUE
         "บ                               บ" HIGHLIGHT.
         02 LINE 11 COLUMN 1  VALUE
         "บ                               บ" HIGHLIGHT.
         02 LINE 12 COLUMN 1  VALUE
         "บ                               บ" HIGHLIGHT.
         02 LINE 13 COLUMN 1  VALUE
         "บ                               บ" HIGHLIGHT.
         02 LINE 14 COLUMN 1  VALUE
         "บ                               บ" HIGHLIGHT.
         02 LINE 15 COLUMN 1  VALUE
         "บTELEPHONE:                     บ" HIGHLIGHT.
         02 LINE 16 COLUMN 1  VALUE
         "ศอออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 ENTER-CUSTOMER-DATA.
         02 LINE 8  COLUMN 12 PIC X(20) TO CUSTOMER-NAME.
         02 LINE 9  COLUMN 12 PIC X(20) TO CUSTOMER-ADDRESS1.
         02 LINE 10 COLUMN 12 PIC X(20) TO CUSTOMER-ADDRESS2.
         02 LINE 11 COLUMN 12 PIC X(20) TO CUSTOMER-ADDRESS3.
         02 LINE 12 COLUMN 12 PIC X(20) TO CUSTOMER-ADDRESS4.
         02 LINE 13 COLUMN 12 PIC X(20) TO CUSTOMER-ADDRESS5.
	 02 LINE 14 COLUMN 12 PIC X(20) TO CUSTOMER-ADDRESS6.
         02 LINE 15 COLUMN 12 PIC X(20) TO CUSTOMER-TELEPHONE.
       01 SMALL-OPTIONS-SCREEN.
         02 LINE 5  COLUMN 40 VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
         02 LINE 6  COLUMN 40 VALUE "ณ       Options Menu           ณ".
         02 LINE 7  COLUMN 40 VALUE "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด".
         02 LINE 8  COLUMN 40 VALUE "ณ A-Enter customer details     ณ".
         02 LINE 9  COLUMN 40 VALUE "ณ B-Input invoice order        ณ".
         02 LINE 10 COLUMN 40 VALUE "ณ C-View invoice order         ณ".
         02 LINE 11 COLUMN 40 VALUE "ณ D-Delete invoice item        ณ".
         02 LINE 12 COLUMN 40 VALUE "ณ E-Amend invoice item         ณ".
         02 LINE 13 COLUMN 40 VALUE "ณ F-Clear details              ณ".
         02 LINE 14 COLUMN 40 VALUE "ณ G-Print Invoice(s)           ณ".
         02 LINE 15 COLUMN 40 VALUE "ณ H Set current invoice number ณ".
         02 LINE 16 COLUMN 40 VALUE "ณ I-Save current invoice       ณ".
         02 LINE 17 COLUMN 40 VALUE "ณ J-Load current invoice       ณ".
         02 LINE 18 COLUMN 40 VALUE "ณ K-Access online manual       ณ".
	 02 LINE 19 COLUMN 40 VALUE "ณ L-Display version number     ณ".
	 02 LINE 20 COLUMN 40 VALUE "ณ M-Setup invoice file         ณ".
	 02 LINE 21 COLUMN 40 VALUE "ณ N-Backup invoice file        ณ".
	 02 LINE 22 COLUMN 40 VALUE "ณ O-Quit program               ณ".
	 02 LINE 23 COLUMN 40 VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
       01 LARGE-OPTIONS-SCREEN.
         02 LINE 5  COLUMN 40 VALUE
         "ษออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 6  COLUMN 40 VALUE
         "บ       Options Menu           บ" HIGHLIGHT.
         02 LINE 7  COLUMN 40 VALUE
         "ฬออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 8  COLUMN 40 VALUE
         "บ A-Enter customer details     บ" HIGHLIGHT.
         02 LINE 9  COLUMN 40 VALUE
         "บ B-Input invoice order        บ" HIGHLIGHT.
         02 LINE 10 COLUMN 40 VALUE
         "บ C-View invoice order         บ" HIGHLIGHT.
         02 LINE 11 COLUMN 40 VALUE
         "บ D-Delete invoice item        บ" HIGHLIGHT.
         02 LINE 12 COLUMN 40 VALUE
         "บ E-Amend invoice item         บ" HIGHLIGHT.
         02 LINE 13 COLUMN 40 VALUE
         "บ F-Clear details              บ" HIGHLIGHT.
         02 LINE 14 COLUMN 40 VALUE
         "บ G-Print Invoice(s)           บ" HIGHLIGHT.
         02 LINE 15 COLUMN 40 VALUE
         "บ H-Set current invoice number บ" HIGHLIGHT.
         02 LINE 16 COLUMN 40 VALUE
         "บ I-Save current invoice       บ" HIGHLIGHT.
         02 LINE 17 COLUMN 40 VALUE
         "บ J-Load current invoice       บ" HIGHLIGHT.
         02 LINE 18 COLUMN 40 VALUE
         "บ K-Access online manual       บ" HIGHLIGHT.
         02 LINE 19 COLUMN 40 VALUE
	 "บ L-Display version number     บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 40 VALUE
	 "บ M-Setup invoice file         บ" HIGHLIGHT.
	 02 LINE 21 COLUMN 40 VALUE
	 "บ N-Backup invoice file        บ" HIGHLIGHT.
	 02 LINE 22 COLUMN 40 VALUE
	 "บ O-Quit program               บ" HIGHLIGHT.
	 02 LINE 23 COLUMN 40 VALUE
         "ศออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 SC-SCREEN.
         02 LINE 19 COLUMN 5 VALUE
         "ษออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 20 COLUMN 5 VALUE
         "บ Set Current Invoice Number บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5 VALUE
         "ฬออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 22 COLUMN 5 VALUE
	 "บ Current Invoice:           บ" HIGHLIGHT.
	 02 LINE 23 COLUMN 5 VALUE
	 "บ     Change Y/N             บ" HIGHLIGHT.
	 02 LINE 24 COLUMN 5 VALUE
	 "ศออออออออออออออออออออออออออออผ" HIGHLIGHT.
	 02 LINE 22 COLUMN 23 PIC Z(4) FROM CURRENT-INVOICE.
       01 SC-SCREEN2.
	 02 LINE 24 COLUMN 5 VALUE
         "บ     New Invoice:           บ" HIGHLIGHT.
	 02 LINE 25 COLUMN 5 VALUE
	 "ศออออออออออออออออออออออออออออผ" HIGHLIGHT.
	 02 LINE 24 COLUMN 23 PIC Z(4) TO NEW-INVOICE.
       01 CLEAR-FIELDS-SCREEN.
         02 LINE 18 COLUMN 5 VALUE
         "ษออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 19 COLUMN 5 VALUE
         "บ     Clear Fields         บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5 VALUE
         "ฬออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 21 COLUMN 5 VALUE
         "บ A-Clear Customer Details บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5 VALUE
         "บ B-Clear invoice Order    บ" HIGHLIGHT.
         02 LINE 23 COLUMN 5 VALUE
         "บ C-Clear All              บ" HIGHLIGHT.
         02 LINE 24 COLUMN 5 VALUE
         "บ  Any Other Key to Exit   บ" HIGHLIGHT.
         02 LINE 25 COLUMN 5 VALUE
         "ศออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 CONTINUE-GOODS-SCREEN.
         02 LINE 8  COLUMN 5 VALUE "ษออออออออออออออป" HIGHLIGHT.
         02 LINE 9  COLUMN 5 VALUE "บ CONTINUE Y/N บ" HIGHLIGHT.
         02 LINE 10 COLUMN 5 VALUE "ศออออออออออออออผ" HIGHLIGHT.
       01 GET-DEL-SCREEN.
         02 LINE 18 COLUMN 5 VALUE "ษออออออออออออออออป" HIGHLIGHT.
         02 LINE 19 COLUMN 5 VALUE "บ DELETE ITEM:   บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5 VALUE "ศออออออออออออออออผ" HIGHLIGHT.
         02 LINE 19 COLUMN 19 PIC ZZ TO DEL-PROMPT.
       01 CHECK-DEL-SCREEN.
         02 LINE 21 COLUMN 5 VALUE "ษออออออออออออออออออป" HIGHLIGHT.
         02 LINE 22 COLUMN 5 VALUE "บ  DELETE ITEM:    บ" HIGHLIGHT.
         02 LINE 23 COLUMN 5 VALUE "บ       Y/N        บ" HIGHLIGHT.
         02 LINE 24 COLUMN 5 VALUE "ศออออออออออออออออออผ" HIGHLIGHT.
         02 LINE 22 COLUMN 20 PIC ZZ FROM DEL-PROMPT.
       01 GET-AME-SCREEN.
         02 LINE 18 COLUMN 5 VALUE "ษออออออออออออออออป" HIGHLIGHT.
         02 LINE 19 COLUMN 5 VALUE "บ AMEND ITEM:    บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5 VALUE "ศออออออออออออออออผ" HIGHLIGHT.
         02 LINE 19 COLUMN 18 PIC ZZ TO L-V.
       01 PRINTING-SCREEN.
         02 LINE 18 COLUMN 5 VALUE "ษอออออออออออออป" HIGHLIGHT.
         02 LINE 19 COLUMN 5 VALUE "บ  PRINTING   บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5 VALUE "บ PRESS A KEY บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5 VALUE "ศอออออออออออออผ" HIGHLIGHT.
       01 VERSION-SCREEN.
         02 LINE 18 COLUMN 5 VALUE "ษออออออออออออออออออออออป" HIGHLIGHT.
	 02 LINE 19 COLUMN 5 VALUE "บ VERSION NUMBER:2.6   บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5 VALUE "ศออออออออออออออออออออออผ" HIGHLIGHT.
       01 SET-NUM-MAIN.
	 02 LINE 18 COLUMN 5 VALUE "ษอออออออออออออออออออออป" HIGHLIGHT.
	 02 LINE 19 COLUMN 5 VALUE "บ SAVING INVOICE:     บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5 VALUE "ศอออออออออออออออออออออผ" HIGHLIGHT.
       01 SAVE-SCREEN.
	 02 LINE 18 COLUMN 5 VALUE "ษอออออออออออออออออออออป" HIGHLIGHT.
	 02 LINE 19 COLUMN 5 VALUE "บ  SAVING INVOICE:    บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5 VALUE "ศอออออออออออออออออออออผ" HIGHLIGHT.
	 02 LINE 19 COLUMN 23 PIC Z(4) FROM CURRENT-INVOICE.
       01 LOAD-SCREEN.
	 02 LINE 18 COLUMN 5 VALUE "ษอออออออออออออออออออออป" HIGHLIGHT.
	 02 LINE 19 COLUMN 5 VALUE "บ LOADING INVOICE:    บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5 VALUE "ศอออออออออออออออออออออผ" HIGHLIGHT.
	 02 LINE 19 COLUMN 23 PIC Z(4) FROM CURRENT-INVOICE.
       01 END-SCREEN.
	 02 LINE 18 COLUMN 5 VALUE "ษออออออออออป" HIGHLIGHT.
	 02 LINE 19 COLUMN 5 VALUE "บ EXIT Y/N บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5 VALUE "ศออออออออออผ" HIGHLIGHT.
       01 BACKUP-BOX.
	 02 LINE 15 COLUMN 5 VALUE
	    "ษออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
	 02 LINE 16 COLUMN 5 VALUE
	    "บ To backup the invoice file type:   บ" HIGHLIGHT.
	 02 LINE 17 COLUMN 5 VALUE
	    "บ CD\                                บ" HIGHLIGHT.
	 02 LINE 18 COLUMN 5 VALUE
	    "บ BACKUP C:\INVOICE\CUSTINV.DAT A:   บ" HIGHLIGHT.
	 02 LINE 19 COLUMN 5 VALUE
	    "บ To restore the file type:          บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5 VALUE
	    "บ CD\                                บ" HIGHLIGHT.
	 02 LINE 21 COLUMN 5 VALUE
	    "บ RESTORE A: C:\INVOICE\CUSTINV.DAT  บ" HIGHLIGHT.
	 02 LINE 22 COLUMN 5 VALUE
	    "บ                                    บ" HIGHLIGHT.
	 02 LINE 23 COLUMN 5 VALUE
	    "บ You will need 3*360kb or 1*1.2mb   บ" HIGHLIGHT.
	 02 LINE 24 COLUMN 5 VALUE
	    "ศออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 SET-NUM.
	 02 LINE 19 COLUMN 22 PIC Z(4) FROM CURRENT-INVOICE.
       01 EDIT-GOODS-TITLE.
         02 BLANK SCREEN.
         02 LINE 1 COLUMN 1 VALUE
         "ษอออออหอออออออออออออออออออออออออหอออออหอออออออหอออออออออป".
         02 LINE 2 COLUMN 1 VALUE
         "บITEM บDESCRIPTION              บ QTY บ PRICE บ TOTAL   บ".
         02 LINE 3 COLUMN 1 VALUE
         "บNO.  บ                         บ     บ EACH  บ VALUE   บ".
         02 LINE 4 COLUMN 1 VALUE
         "ฬอออออฮอออออออออออออออออออออออออฮอออออฮอออออออฮอออออออออน".
         02 LINE 5 COLUMN 1 VALUE
         "บ 01  บ                         บ     บ       บ         บ".
         02 LINE 6 COLUMN 1 VALUE
         "บ 02  บ                         บ     บ       บ         บ".
         02 LINE 7 COLUMN 1 VALUE
         "บ 03  บ                         บ     บ       บ         บ".
         02 LINE 8 COLUMN 1 VALUE
         "บ 04  บ                         บ     บ       บ         บ".
         02 LINE 9 COLUMN 1 VALUE
         "บ 05  บ                         บ     บ       บ         บ".
         02 LINE 10 COLUMN 1 VALUE
         "บ 06  บ                         บ     บ       บ         บ".
         02 LINE 11 COLUMN 1 VALUE
         "บ 07  บ                         บ     บ       บ         บ".
         02 LINE 12 COLUMN 1 VALUE
         "บ 08  บ                         บ     บ       บ         บ".
         02 LINE 13 COLUMN 1 VALUE
         "บ 09  บ                         บ     บ       บ         บ".
         02 LINE 14 COLUMN 1 VALUE
         "บ 10  บ                         บ     บ       บ         บ".
         02 LINE 15 COLUMN 1 VALUE
         "บ 11  บ                         บ     บ       บ         บ".
         02 LINE 16 COLUMN 1 VALUE
         "บ 12  บ                         บ     บ       บ         บ".
         02 LINE 17 COLUMN 1 VALUE
         "บ 13  บ                         บ     บ       บ         บ".
         02 LINE 18 COLUMN 1 VALUE
         "บ 14  บ                         บ     บ       บ         บ".
         02 LINE 19 COLUMN 1 VALUE
         "บ 15  บ                         บ     บ       บ         บ".
         02 LINE 20 COLUMN 1 VALUE
         "บ 16  บ                         บ     บ       บ         บ".
         02 LINE 21 COLUMN 1 VALUE
         "บ 17  บ                         บ     บ       บ         บ".
         02 LINE 22 COLUMN 1 VALUE
         "บ 18  บ                         บ     บ       บ         บ".
         02 LINE 23 COLUMN 1 VALUE
         "บ 19  บ                         บ     บ       บ         บ".
         02 LINE 24 COLUMN 1 VALUE
         "บ 20  บ                         บ     บ       บ         บ".
         02 LINE 25 COLUMN 1 VALUE
         "ศอออออสอออออออออออออออออออออออออสอออออสอออออออสอออออออออผ".
       01 EDIT-GOODS-SCREEN.
         02 LINE D-L COLUMN 8 PIC X(25) TO GOODS-DESC(CUR-GOODS).
         02 LINE D-L COLUMN 35 PIC ZZZ TO GOODS-QTY(CUR-GOODS).
         02 LINE D-L COLUMN 41 PIC ZZ.ZZ TO GOODS-PRICE(CUR-GOODS).
       01 EDIT-FINAL-DATA.
         02 LINE D-L COLUMN 49 PIC ZZZZ.ZZ FROM GOODS-TOTAL(CUR-GOODS).
       01 DISP-GOODS-LINE.
         02 LINE D-L COLUMN 8 PIC X(25) FROM GOODS-DESC(L-V).
         02 LINE D-L COLUMN 35 PIC ZZZ FROM GOODS-QTY(L-V).
         02 LINE D-L COLUMN 41 PIC ZZ.ZZ FROM GOODS-PRICE(L-V).
         02 LINE D-L COLUMN 49 PIC ZZZZ.ZZ FROM GOODS-TOTAL(L-V).
       01 EDIT-GOODS-SCREEN2.
         02 LINE D-L COLUMN 8 PIC X(25) TO GOODS-DESC(L-V).
         02 LINE D-L COLUMN 35 PIC ZZZ TO GOODS-QTY(L-V).
         02 LINE D-L COLUMN 41 PIC ZZ.ZZ TO GOODS-PRICE(L-V).
       01 EDIT-FINAL-DATA2.
         02 LINE D-L COLUMN 49 PIC ZZZZ.ZZ FROM GOODS-TOTAL(L-V).
       01 MANUAL-SCREEN.
         02 LINE 10 COLUMN 5  VALUE
         "ษอออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 11 COLUMN 5  VALUE
         "บONLINE MANUAL - HELP TOPICSบ" HIGHLIGHT.
         02 LINE 12 COLUMN 5  VALUE
         "ฬอออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 13 COLUMN 5  VALUE
         "บ A-Options menu            บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5  VALUE
	 "บ B-Customer details        บ" HIGHLIGHT.
	 02 LINE 15 COLUMN 5  VALUE
	 "บ C-Invoice data            บ" HIGHLIGHT.
	 02 LINE 16 COLUMN 5  VALUE
	 "บ D-Printer setup           บ" HIGHLIGHT.
	 02 LINE 17 COLUMN 5  VALUE
         "ศอออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 SMALL-MANUAL-SCREEN.
         02 LINE 10 COLUMN 5 VALUE "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ".
         02 LINE 11 COLUMN 5 VALUE "ณONLINE MANUAL - HELP TOPICSณ".
         02 LINE 12 COLUMN 5 VALUE "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด".
         02 LINE 13 COLUMN 5 VALUE "ณ A-Options menu            ณ".
	 02 LINE 14 COLUMN 5 VALUE "ณ B-Customer details        ณ".
	 02 LINE 15 COLUMN 5 VALUE "ณ C-Invoice data            ณ".
	 02 LINE 16 COLUMN 5 VALUE "ณ D-Printer setup           ณ".
	 02 LINE 17 COLUMN 5 VALUE "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู".
       01 MANUAL-PAGE1.
         02 LINE 2  COLUMN 5
            VALUE "ษอออออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 3  COLUMN 5
            VALUE "บ            Options Menu               บ" HIGHLIGHT.
         02 LINE 4  COLUMN 5
            VALUE "ฬอออออออออออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 5  COLUMN 5
            VALUE "บ A-Enter customer details              บ" HIGHLIGHT.
         02 LINE 6  COLUMN 5
            VALUE "บ   This option is for entering customerบ" HIGHLIGHT.
         02 LINE 7  COLUMN 5
            VALUE "บ   details e.g Name,address etc.       บ" HIGHLIGHT.
         02 LINE 8  COLUMN 5
            VALUE "บ B-Input invoice order                 บ" HIGHLIGHT.
         02 LINE 9  COLUMN 5
            VALUE "บ   Use this option for entering invoiceบ" HIGHLIGHT.
         02 LINE 10 COLUMN 5
            VALUE "บ   data e.g Name of goods,quantity etc.บ" HIGHLIGHT.
         02 LINE 11 COLUMN 5
            VALUE "บ C-View invoice order                  บ" HIGHLIGHT.
         02 LINE 12 COLUMN 5
            VALUE "บ   Use this option to view all of the  บ" HIGHLIGHT.
         02 LINE 13 COLUMN 5
            VALUE "บ   current invoice order.              บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5
            VALUE "บ D-Delete invoice item                 บ" HIGHLIGHT.
         02 LINE 15 COLUMN 5
            VALUE "บ   Use this option for deleting an     บ" HIGHLIGHT.
         02 LINE 16 COLUMN 5
            VALUE "บ   invoice item.                       บ" HIGHLIGHT.
         02 LINE 17 COLUMN 5
            VALUE "บ E-Amend invoice item                  บ" HIGHLIGHT.
         02 LINE 18 COLUMN 5
            VALUE "บ   Use this option for amending an     บ" HIGHLIGHT.
         02 LINE 19 COLUMN 5
            VALUE "บ   invoice item.                       บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5
            VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5
            VALUE "บ    Press a key for the next page      บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5
            VALUE "ศอออออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 MANUAL-PAGE2.
         02 LINE 2  COLUMN 5
            VALUE "ษอออออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 3  COLUMN 5
            VALUE "บ            Options Menu               บ" HIGHLIGHT.
         02 LINE 4  COLUMN 5
            VALUE "ฬอออออออออออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 5  COLUMN 5
            VALUE "บ F-Clear details                       บ" HIGHLIGHT.
         02 LINE 6  COLUMN 5
            VALUE "บ   When you select this options you    บ" HIGHLIGHT.
         02 LINE 7  COLUMN 5
            VALUE "บ   get a choice of what to delete      บ" HIGHLIGHT.
         02 LINE 8  COLUMN 5
            VALUE "บ   either customer details,invoice dataบ" HIGHLIGHT.
         02 LINE 9  COLUMN 5
            VALUE "บ   or both.                            บ" HIGHLIGHT.
         02 LINE 10 COLUMN 5
            VALUE "บ G-Print invoice                       บ" HIGHLIGHT.
         02 LINE 11 COLUMN 5
            VALUE "บ   When you select this option the     บ" HIGHLIGHT.
         02 LINE 12 COLUMN 5
            VALUE "บ   invoice will be printed in full.    บ" HIGHLIGHT.
         02 LINE 13 COLUMN 5
            VALUE "บ H-Set current invoice number          บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5
            VALUE "บ   After selecting this option a menu  บ" HIGHLIGHT.
         02 LINE 15 COLUMN 5
            VALUE "บ   will appear displaying the current  บ" HIGHLIGHT.
         02 LINE 16 COLUMN 5
            VALUE "บ   invoice number and requesting a new บ" HIGHLIGHT.
         02 LINE 17 COLUMN 5
            VALUE "บ   one.                                บ" HIGHLIGHT.
         02 LINE 18 COLUMN 5
            VALUE "บ I-Save current invoice                บ" HIGHLIGHT.
         02 LINE 19 COLUMN 5
            VALUE "บ   This will save the current invoice. บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5
            VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5
            VALUE "บ    Press a key for the next page      บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5
            VALUE "ศอออออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 MANUAL-PAGE3.
         02 LINE 2  COLUMN 5
            VALUE "ษอออออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 3  COLUMN 5
            VALUE "บ            Options Menu               บ" HIGHLIGHT.
         02 LINE 4  COLUMN 5
            VALUE "ฬอออออออออออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 5  COLUMN 5
            VALUE "บ J-Load invoice                        บ" HIGHLIGHT.
         02 LINE 6  COLUMN 5
            VALUE "บ   This will load the current invoice. บ" HIGHLIGHT.
         02 LINE 7  COLUMN 5
            VALUE "บ K-Access online manual                บ" HIGHLIGHT.
         02 LINE 8  COLUMN 5
            VALUE "บ   This is what you are doing now!     บ" HIGHLIGHT.
         02 LINE 9  COLUMN 5
            VALUE "บ L-Display version number              บ" HIGHLIGHT.
         02 LINE 10 COLUMN 5
            VALUE "บ   To display the version number of theบ" HIGHLIGHT.
         02 LINE 11 COLUMN 5
            VALUE "บ   program you are using select this   บ" HIGHLIGHT.
         02 LINE 12 COLUMN 5
            VALUE "บ   option.                             บ" HIGHLIGHT.
         02 LINE 13 COLUMN 5
	    VALUE "บ M-Setup invoice file                  บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5
	    VALUE "บ   This option sets up the invoice fileบ" HIGHLIGHT.
         02 LINE 15 COLUMN 5
	    VALUE "บ   ready for saving data.              บ" HIGHLIGHT.
         02 LINE 16 COLUMN 5
	    VALUE "บ N-Backup invoice file                 บ" HIGHLIGHT.
         02 LINE 17 COLUMN 5
	    VALUE "บ   This option displays a box          บ" HIGHLIGHT.
         02 LINE 18 COLUMN 5
	    VALUE "บ   describing how to backup and restoreบ" HIGHLIGHT.
         02 LINE 19 COLUMN 5
	    VALUE "บ   the invoice file from MS-DOS.       บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5
	    VALUE "บ    Press a key for the next page      บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5
            VALUE "ศอออออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 MANUAL-PAGE4.
         02 LINE 2  COLUMN 5
            VALUE "ษอออออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 3  COLUMN 5
            VALUE "บ            Options Menu               บ" HIGHLIGHT.
         02 LINE 4  COLUMN 5
            VALUE "ฬอออออออออออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 5  COLUMN 5
	    VALUE "บ O-Exit program                        บ" HIGHLIGHT.
         02 LINE 6  COLUMN 5
	    VALUE "บ   This option will close all files    บ" HIGHLIGHT.
         02 LINE 7  COLUMN 5
	    VALUE "บ   and exit the program.               บ" HIGHLIGHT.
         02 LINE 8  COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 9  COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 10 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 11 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 12 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 13 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 15 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 16 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 17 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 18 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 19 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5
            VALUE "บ Press a key to return to options menu บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5
            VALUE "ศอออออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 MANUAL-PAGE5.
         02 LINE 2  COLUMN 5
            VALUE "ษอออออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 3  COLUMN 5
            VALUE "บ      Entering Customer details        บ" HIGHLIGHT.
         02 LINE 4  COLUMN 5
            VALUE "ฬอออออออออออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 5  COLUMN 5
            VALUE "บ   To change customer details press 1  บ" HIGHLIGHT.
         02 LINE 6  COLUMN 5
            VALUE "บ on the options menu.   The line with  บ" HIGHLIGHT.
         02 LINE 7  COLUMN 5
            VALUE "บ 'name:' on should have several        บ" HIGHLIGHT.
         02 LINE 8  COLUMN 5
            VALUE "บ underlined spaces to the right of it. บ" HIGHLIGHT.
         02 LINE 9  COLUMN 5
            VALUE "บ If you type something it will take up บ" HIGHLIGHT.
         02 LINE 10 COLUMN 5
            VALUE "บ one of these spaces.   Type the text  บ" HIGHLIGHT.
         02 LINE 11 COLUMN 5
            VALUE "บ e.g Name into this space.             บ" HIGHLIGHT.
         02 LINE 12 COLUMN 5
            VALUE "บ   If you depress the tab key the      บ" HIGHLIGHT.
         02 LINE 13 COLUMN 5
            VALUE "บ cursor should go to the next line.    บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5
            VALUE "บ If you try to go beyond the last fieldบ" HIGHLIGHT.
         02 LINE 15 COLUMN 5
            VALUE "บ an error will occur.                  บ" HIGHLIGHT.
         02 LINE 16 COLUMN 5
	    VALUE "บ   Depress shift+tab (by pressing      บ" HIGHLIGHT.
         02 LINE 17 COLUMN 5
            VALUE "บ shift;holding it down then pressing   บ" HIGHLIGHT.
         02 LINE 18 COLUMN 5
            VALUE "บ the tab key;release tab first then    บ" HIGHLIGHT.
         02 LINE 19 COLUMN 5
            VALUE "บ shift) to return to the previous line.บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5
            VALUE "บ   Press return to end entry.          บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5
            VALUE "บ Press a key to return to options menu บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5
            VALUE "ศอออออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 MANUAL-PAGE6.
         02 LINE 2  COLUMN 5
            VALUE "ษอออออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 3  COLUMN 5
	    VALUE "บ      Entering invoice data            บ" HIGHLIGHT.
         02 LINE 4  COLUMN 5
            VALUE "ฬอออออออออออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 5  COLUMN 5
	    VALUE "บ   To enter invoice data enter 2 on    บ" HIGHLIGHT.
         02 LINE 6  COLUMN 5
	    VALUE "บ the options menu.   At the left the   บ" HIGHLIGHT.
         02 LINE 7  COLUMN 5
	    VALUE "บ current item number will be displayed บ" HIGHLIGHT.
         02 LINE 8  COLUMN 5
	    VALUE "บ with underlined spaces to the right ofบ" HIGHLIGHT.
         02 LINE 9  COLUMN 5
	    VALUE "บ it.   Enter the description of the    บ" HIGHLIGHT.
         02 LINE 10 COLUMN 5
	    VALUE "บ goods.  Press tab to go to the next   บ" HIGHLIGHT.
         02 LINE 11 COLUMN 5
	    VALUE "บ field.   Repeat this procedure with   บ" HIGHLIGHT.
         02 LINE 12 COLUMN 5
	    VALUE "บ quantity and price.                   บ" HIGHLIGHT.
         02 LINE 13 COLUMN 5
	    VALUE "บ     Check what you have entered if it บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5
	    VALUE "บ isnt correct then press tab and       บ" HIGHLIGHT.
         02 LINE 15 COLUMN 5
	    VALUE "บ shift+tab to change what is needed.   บ" HIGHLIGHT.
         02 LINE 16 COLUMN 5
	    VALUE "บ   After you have finished the item    บ" HIGHLIGHT.
         02 LINE 17 COLUMN 5
	    VALUE "บ press return to return to the options บ" HIGHLIGHT.
         02 LINE 18 COLUMN 5
	    VALUE "บ menu.                                 บ" HIGHLIGHT.
         02 LINE 19 COLUMN 5
	    VALUE "บ   To change anything later use the    บ" HIGHLIGHT.
         02 LINE 20 COLUMN 5
	    VALUE "บ delete and amend options on the menu. บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5
            VALUE "บ Press a key to return to options menu บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5
            VALUE "ศอออออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.
       01 MANUAL-PAGE7.
         02 LINE 2  COLUMN 5
            VALUE "ษอออออออออออออออออออออออออออออออออออออออป" HIGHLIGHT.
         02 LINE 3  COLUMN 5
	    VALUE "บ            Printer Setup              บ" HIGHLIGHT.
         02 LINE 4  COLUMN 5
            VALUE "ฬอออออออออออออออออออออออออออออออออออออออน" HIGHLIGHT.
         02 LINE 5  COLUMN 5
	    VALUE "บ   The program will work on an         บ" HIGHLIGHT.
         02 LINE 6  COLUMN 5
	    VALUE "บ   IBM printer or compatible  e.g.     บ" HIGHLIGHT.
         02 LINE 7  COLUMN 5
	    VALUE "บ   Facit 4511 and 4512.    Here is a   บ" HIGHLIGHT.
         02 LINE 8  COLUMN 5
	    VALUE "บ   list of the dip switch settings.    บ" HIGHLIGHT.
         02 LINE 9  COLUMN 5
	    VALUE "บ  Switch 1                             บ" HIGHLIGHT.
         02 LINE 10 COLUMN 5
	    VALUE "บ  1 0 0 0 0 0 1 0                      บ" HIGHLIGHT.
         02 LINE 11 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 12 COLUMN 5
	    VALUE "บ  Switch 2                             บ" HIGHLIGHT.
         02 LINE 13 COLUMN 5
	    VALUE "บ  0 1 1 1 1 1 1 1                      บ" HIGHLIGHT.
         02 LINE 14 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 15 COLUMN 5
	    VALUE "บ  Switch 3                             บ" HIGHLIGHT.
         02 LINE 16 COLUMN 5
	    VALUE "บ  1 0 0 0                              บ" HIGHLIGHT.
         02 LINE 17 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 18 COLUMN 5
	    VALUE "บ   The program is setup for 70 line    บ" HIGHLIGHT.
         02 LINE 19 COLUMN 5
	    VALUE "บ   A4 paper e.g. Nokia Data Paper.     บ" HIGHLIGHT.
	 02 LINE 20 COLUMN 5
	    VALUE "บ                                       บ" HIGHLIGHT.
         02 LINE 21 COLUMN 5
            VALUE "บ Press a key to return to options menu บ" HIGHLIGHT.
         02 LINE 22 COLUMN 5
            VALUE "ศอออออออออออออออออออออออออออออออออออออออผ" HIGHLIGHT.

       PROCEDURE DIVISION.

       CAMP-START SECTION.

       START-PROGRAM.
           DISPLAY BLANK-SCREEN.
           PERFORM INIT.
           PERFORM MAIN-PROGRAM THRU MAIN-PROGRAM-LOOP.

       INIT.
           COMPUTE CUR-GOODS = 1.
           PERFORM DISPLAY-MAIN-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
           ADD 1 TO ST-RE-NUMBER.
           DISPLAY EDIT-GOODS-TITLE.
           PERFORM STORE-SCREEN.
           OPEN INPUT CURRENT-INVOICE-FILE.
           READ CURRENT-INVOICE-FILE INTO CURRENT-INVOICE.
           CLOSE CURRENT-INVOICE-FILE.
           ACCEPT TODAYS-DATE FROM DATE.
           STRING DATE-DAY "/" DATE-MONTH "/" DATE-YEAR
	       DELIMITED BY SIZE INTO STRINGED-DATE.
           STRING DATE-DAY "/" DATE-MONTH "/" DATE-YEAR
               DELIMITED BY SIZE INTO PRINT-DATE.
           OPEN I-O INVOICE-FILE.

       CAMP-MAIN SECTION.

       MAIN-PROGRAM.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM RESTORE-SCREEN.
       MAIN-PROGRAM-LOOP.
           DISPLAY LARGE-OPTIONS-SCREEN.
           PERFORM GET-MENU-OPTION.
           GO TO MAIN-PROGRAM-LOOP.
           STOP RUN.

       DISPLAY-MAIN-SCREEN.
           DISPLAY BLANK-SCREEN.
           DISPLAY MAIN-TITLE.
           DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
           DISPLAY SMALL-OPTIONS-SCREEN.

       GET-MENU-OPTION.
           CALL X"83" USING MENU-OPTION.
           EVALUATE MENU-OPTION
               WHEN "A" PERFORM EDIT-CUSTOMER-DATA THRU END-ECD
               WHEN "B" PERFORM EDIT-GOODS-DATA THRU END-EGD
               WHEN "C" PERFORM VIEW-INVOICE THRU END-VI
               WHEN "D" PERFORM DELETE-INVOICE THRU END-DI
               WHEN "E" PERFORM AMEND-INVOICE THRU END-AI
               WHEN "F" PERFORM CLEAR-ALL-FIELDS THRU END-CAF
               WHEN "G" PERFORM PRINT-INVOICE THRU END-PI
               WHEN "H" PERFORM SET-CUR-INV THRU END-SCI
               WHEN "I" PERFORM SAVE-INVOICE THRU END-SI
               WHEN "J" PERFORM LOAD-INVOICE THRU END-LI
               WHEN "K" PERFORM ONLINE-MANUAL THRU END-OM
	       WHEN "L" PERFORM VIEW-VERSION THRU END-VV
	       WHEN "M" PERFORM SETUP-FILE THRU END-SF
	       WHEN "N" PERFORM BACKUP-FILE THRU END-BF
	       WHEN "O" PERFORM END-PROG THRU END-EP
               WHEN "a" PERFORM EDIT-CUSTOMER-DATA THRU END-ECD
               WHEN "b" PERFORM EDIT-GOODS-DATA THRU END-EGD
               WHEN "c" PERFORM VIEW-INVOICE THRU END-VI
               WHEN "d" PERFORM DELETE-INVOICE THRU END-DI
               WHEN "e" PERFORM AMEND-INVOICE THRU END-AI
               WHEN "f" PERFORM CLEAR-ALL-FIELDS THRU END-CAF
               WHEN "g" PERFORM PRINT-INVOICE THRU END-PI
               WHEN "h" PERFORM SET-CUR-INV THRU END-SCI
               WHEN "i" PERFORM SAVE-INVOICE THRU END-SI
               WHEN "j" PERFORM LOAD-INVOICE THRU END-LI
               WHEN "k" PERFORM ONLINE-MANUAL THRU END-OM
               WHEN "l" PERFORM VIEW-VERSION THRU END-VV
	       WHEN "m" PERFORM SETUP-FILE THRU END-SF
	       WHEN "n" PERFORM BACKUP-FILE THRU END-BF
	       WHEN "o" PERFORM END-PROG THRU END-EP.
       EDIT-CUSTOMER-DATA.
           DISPLAY SMALL-OPTIONS-SCREEN.
           DISPLAY LARGE-CUSTOMER-DATA-SCREEN.
           ACCEPT ENTER-CUSTOMER-DATA.
           DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
       END-ECD.

       EDIT-GOODS-DATA.
	   COMPUTE ST-RE-NUMBER = 2.
           PERFORM RESTORE-SCREEN.
	   IF CUR-GOODS = MAX-GOODS
	       GO TO END-EGD.
       EDIT-GOODS-LOOP.
	   IF CUR-GOODS = MAX-GOODS
	       GO TO END-EGD.
           COMPUTE D-L = 4 + CUR-GOODS.
           ACCEPT EDIT-GOODS-SCREEN.
           COMPUTE GOODS-TOTAL(CUR-GOODS) =
               GOODS-QTY(CUR-GOODS) * GOODS-PRICE(CUR-GOODS).
           DISPLAY EDIT-FINAL-DATA.
	   ADD 1 TO CUR-GOODS.
	   IF CUR-GOODS = MAX-GOODS
	       GO TO END-EGD.
           PERFORM CONTINUE-EDITING-GOODS.
           IF ANY-KEY = "Y" OR ANY-KEY = "y"
               GO TO EDIT-GOODS-LOOP.
       END-EGD.
           COMPUTE ST-RE-NUMBER = 2.
           PERFORM STORE-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM RESTORE-SCREEN.

       CONTINUE-EDITING-GOODS.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY CONTINUE-GOODS-SCREEN.
           CALL X"83" USING ANY-KEY.
           PERFORM RESTORE-SCREEN.

       VIEW-INVOICE.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           COMPUTE ST-RE-NUMBER = 2.
           PERFORM RESTORE-SCREEN.
           CALL X"83" USING ANY-KEY.
           ADD 1 TO ST-RE-NUMBER.
           PERFORM RESTORE-SCREEN.
       END-VI.

       DELETE-INVOICE.
           DISPLAY SMALL-OPTIONS-SCREEN.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY GET-DEL-SCREEN.
           ACCEPT GET-DEL-SCREEN.
           DISPLAY CHECK-DEL-SCREEN.
           IF DEL-PROMPT > CUR-GOODS
               GO TO END-DI.
           CALL X"83" USING ANY-KEY.
           IF ANY-KEY = "Y" OR ANY-KEY = "y"
               GO TO CONT-DEL-INV.
           GO TO END-DI.
       CONT-DEL-INV.
           IF CUR-GOODS = 2
               PERFORM DEL-ONE-INV
               GO TO END-DI.
           COMPUTE LOOP-VARIABLE = DEL-PROMPT.
       DEL-INV-LOOP.
           ADD 1 TO LOOP-VARIABLE.
           MOVE GOODS-DESC(LOOP-VARIABLE) TO TEMP-GD1.
           MOVE GOODS-QTY(LOOP-VARIABLE) TO TEMP-GQ1.
           MOVE GOODS-PRICE(LOOP-VARIABLE) TO TEMP-GP1.
           MOVE GOODS-TOTAL(LOOP-VARIABLE) TO TEMP-GT1.
           COMPUTE LOOP-VARIABLE2 = LOOP-VARIABLE - 1.
           MOVE TEMP-GD1 TO GOODS-DESC(LOOP-VARIABLE2).
           MOVE TEMP-GQ1 TO GOODS-QTY(LOOP-VARIABLE2).
           MOVE TEMP-GP1 TO GOODS-PRICE(LOOP-VARIABLE2).
           MOVE TEMP-GT1 TO GOODS-TOTAL(LOOP-VARIABLE2).
           IF LOOP-VARIABLE NOT > CUR-GOODS
               GO TO DEL-INV-LOOP.
           SUBTRACT 1 FROM CUR-GOODS.
           MOVE SPACES TO GOODS-DESC(CUR-GOODS).
           MOVE ZERO TO GOODS-QTY(CUR-GOODS).
           MOVE ZERO TO GOODS-PRICE(CUR-GOODS).
           MOVE ZERO TO GOODS-TOTAL(CUR-GOODS).
           SUBTRACT 1 FROM CUR-GOODS.
           DISPLAY BLANK-SCREEN.
           DISPLAY EDIT-GOODS-TITLE.
	   COMPUTE D-L = 4.
	   COMPUTE L-V = 0.
       DEL-INV-LOOP2.
           ADD 1 TO D-L.
           ADD 1 TO L-V.
           DISPLAY DISP-GOODS-LINE.
           IF L-V NOT = 20
               GO TO DEL-INV-LOOP2.
           ADD 1 TO CUR-GOODS.
           COMPUTE ST-RE-NUMBER = 2.
           PERFORM STORE-SCREEN.
       END-DI.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM RESTORE-SCREEN.

       DEL-ONE-INV.
           PERFORM CLEAR-IO THRU END-CF.

       AMEND-INVOICE.
           DISPLAY SMALL-OPTIONS-SCREEN.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY GET-AME-SCREEN.
           ACCEPT GET-AME-SCREEN.
           COMPUTE ST-RE-NUMBER = 2.
           PERFORM RESTORE-SCREEN.
           IF L-V = 0
               GO TO END-AI.
           COMPUTE D-L = 4 + L-V.
           ACCEPT EDIT-GOODS-SCREEN2.
           COMPUTE GOODS-TOTAL(L-V) =
               GOODS-QTY(L-V) * GOODS-PRICE(L-V).
           DISPLAY EDIT-FINAL-DATA2.
           PERFORM STORE-SCREEN.
       END-AI.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM RESTORE-SCREEN.

       CLEAR-ALL-FIELDS.
           DISPLAY SMALL-OPTIONS-SCREEN.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY CLEAR-FIELDS-SCREEN.
           CALL X"83" USING ANY-KEY.
           EVALUATE ANY-KEY
               WHEN "A" PERFORM CLEAR-CD
               WHEN "B" PERFORM CLEAR-IO THRU END-CF
               WHEN "C" PERFORM CLEAR-CD
                        PERFORM CLEAR-IO THRU END-CF
               WHEN "a" PERFORM CLEAR-CD
               WHEN "b" PERFORM CLEAR-IO THRU END-CF
               WHEN "c" PERFORM CLEAR-CD
                        PERFORM CLEAR-IO THRU END-CF.
	   IF ANY-KEY NOT = SPACES
	       STRING DATE-DAY "/" DATE-MONTH "/" DATE-YEAR
		   DELIMITED BY SIZE INTO STRINGED-DATE.
	   IF ANY-KEY NOT = SPACES
	       STRING DATE-DAY "/" DATE-MONTH "/" DATE-YEAR
		   DELIMITED BY SIZE INTO PRINT-DATE.
	   COMPUTE ST-RE-NUMBER = 3.
           PERFORM RESTORE-SCREEN.
       END-CAF.

       CLEAR-CD.
           MOVE SPACES TO CUSTOMER-NAME.
           MOVE SPACES TO CUSTOMER-ADDRESS1.
           MOVE SPACES TO CUSTOMER-ADDRESS2.
           MOVE SPACES TO CUSTOMER-ADDRESS3.
           MOVE SPACES TO CUSTOMER-ADDRESS4.
           MOVE SPACES TO CUSTOMER-ADDRESS5.
           MOVE SPACES TO CUSTOMER-ADDRESS6.
           MOVE SPACES TO CUSTOMER-TELEPHONE.
           PERFORM DISPLAY-MAIN-SCREEN.
           DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM RESTORE-SCREEN.
           DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
           PERFORM STORE-SCREEN.

       CLEAR-IO.
           COMPUTE LOOP-VARIABLE = 0.
       CLEAR-FIELDS-LOOP.
           ADD 1 TO LOOP-VARIABLE.
           MOVE SPACES TO GOODS-DESC(LOOP-VARIABLE).
           COMPUTE GOODS-QTY(LOOP-VARIABLE) = 0.
           COMPUTE GOODS-PRICE(LOOP-VARIABLE) = 0.
           COMPUTE GOODS-TOTAL(LOOP-VARIABLE) = 0.
           IF LOOP-VARIABLE NOT = MAX-GOODS
               GO TO CLEAR-FIELDS-LOOP.
           COMPUTE CUR-GOODS = 1.
           DISPLAY BLANK-SCREEN.
           COMPUTE ST-RE-NUMBER = 2.
           DISPLAY EDIT-GOODS-TITLE.
           PERFORM STORE-SCREEN.
       END-CF.

       PRINT-INVOICE.
           DISPLAY SMALL-OPTIONS-SCREEN.
           OPEN OUTPUT PRINTER-FILE.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           COMPUTE COMP-TOTAL = 0.
           COMPUTE LOOP-VARIABLE = 0.
       PRINT-INVOICE-LOOP.
           ADD 1 TO LOOP-VARIABLE.
           ADD GOODS-TOTAL(LOOP-VARIABLE) TO COMP-TOTAL.
           IF LOOP-VARIABLE NOT = MAX-GOODS
               GO TO PRINT-INVOICE-LOOP.
           DISPLAY PRINTING-SCREEN.
           INITIATE PRINT-REPORT.
           GENERATE PRINT-REPORT-GROUP.
           CALL X"83" USING ANY-KEY.
           PERFORM RESTORE-SCREEN.
           CLOSE PRINTER-FILE.
           ADD 1 TO CURRENT-INVOICE.
           OPEN OUTPUT CURRENT-INVOICE-FILE.
           WRITE CURRENT-INVOICE-FILE-DATA FROM CURRENT-INVOICE.
           CLOSE CURRENT-INVOICE-FILE.
       END-PI.

       SET-CUR-INV.
           DISPLAY SMALL-OPTIONS-SCREEN.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
	   DISPLAY SC-SCREEN.
	   CALL X"83" USING ANY-KEY.
	   IF ANY-KEY = "N" OR ANY-KEY = "n"
	       GO TO END-SCI.
	   DISPLAY SC-SCREEN2.
	   ACCEPT SC-SCREEN2.
           COMPUTE CURRENT-INVOICE = NEW-INVOICE.
           OPEN OUTPUT CURRENT-INVOICE-FILE.
           WRITE CURRENT-INVOICE-FILE-DATA FROM CURRENT-INVOICE.
	   CLOSE CURRENT-INVOICE-FILE.
       END-SCI.
	   PERFORM RESTORE-SCREEN.

       SAVE-INVOICE.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM STORE-SCREEN.
	   DISPLAY SAVE-SCREEN.
           MOVE GOODS-DESC(1) TO F-DESC1.
           MOVE GOODS-QTY(1) TO F-QTY1.
           MOVE GOODS-PRICE(1) TO F-PRICE1.
           MOVE GOODS-TOTAL(1) TO F-TOTAL1.
           MOVE GOODS-DESC(2) TO F-DESC2.
           MOVE GOODS-QTY(2) TO F-QTY2.
           MOVE GOODS-PRICE(2) TO F-PRICE2.
           MOVE GOODS-TOTAL(2) TO F-TOTAL2.
           MOVE GOODS-DESC(3) TO F-DESC3.
           MOVE GOODS-QTY(3) TO F-QTY3.
           MOVE GOODS-PRICE(3) TO F-PRICE3.
           MOVE GOODS-TOTAL(3) TO F-TOTAL3.
           MOVE GOODS-DESC(4) TO F-DESC4.
           MOVE GOODS-QTY(4) TO F-QTY4.
           MOVE GOODS-PRICE(4) TO F-PRICE4.
           MOVE GOODS-TOTAL(4) TO F-TOTAL4.
           MOVE GOODS-DESC(5) TO F-DESC5.
           MOVE GOODS-QTY(5) TO F-QTY5.
           MOVE GOODS-PRICE(5) TO F-PRICE5.
           MOVE GOODS-TOTAL(5) TO F-TOTAL5.
           MOVE GOODS-DESC(6) TO F-DESC6.
           MOVE GOODS-QTY(6) TO F-QTY6.
           MOVE GOODS-PRICE(6) TO F-PRICE6.
           MOVE GOODS-TOTAL(6) TO F-TOTAL6.
           MOVE GOODS-DESC(7) TO F-DESC7.
           MOVE GOODS-QTY(7) TO F-QTY7.
           MOVE GOODS-PRICE(7) TO F-PRICE7.
           MOVE GOODS-TOTAL(7) TO F-TOTAL7.
           MOVE GOODS-DESC(8) TO F-DESC8.
           MOVE GOODS-QTY(8) TO F-QTY8.
           MOVE GOODS-PRICE(8) TO F-PRICE8.
           MOVE GOODS-TOTAL(8) TO F-TOTAL8.
           MOVE GOODS-DESC(9) TO F-DESC9.
           MOVE GOODS-QTY(9) TO F-QTY9.
           MOVE GOODS-PRICE(9) TO F-PRICE9.
           MOVE GOODS-TOTAL(9) TO F-TOTAL9.
           MOVE GOODS-DESC(10) TO F-DESC10.
           MOVE GOODS-QTY(10) TO F-QTY10.
           MOVE GOODS-PRICE(10) TO F-PRICE10.
           MOVE GOODS-TOTAL(10) TO F-TOTAL10.
           MOVE GOODS-DESC(11) TO F-DESC11.
           MOVE GOODS-QTY(11) TO F-QTY11.
           MOVE GOODS-PRICE(11) TO F-PRICE11.
           MOVE GOODS-TOTAL(11) TO F-TOTAL11.
           MOVE GOODS-DESC(12) TO F-DESC12.
           MOVE GOODS-QTY(12) TO F-QTY12.
           MOVE GOODS-PRICE(12) TO F-PRICE12.
           MOVE GOODS-TOTAL(12) TO F-TOTAL12.
           MOVE GOODS-DESC(13) TO F-DESC13.
           MOVE GOODS-QTY(13) TO F-QTY13.
           MOVE GOODS-PRICE(13) TO F-PRICE13.
           MOVE GOODS-TOTAL(13) TO F-TOTAL13.
	   MOVE GOODS-DESC(14) TO F-DESC14.
           MOVE GOODS-QTY(14) TO F-QTY14.
           MOVE GOODS-PRICE(14) TO F-PRICE14.
           MOVE GOODS-TOTAL(14) TO F-TOTAL14.
           MOVE GOODS-DESC(15) TO F-DESC15.
           MOVE GOODS-QTY(15) TO F-QTY15.
           MOVE GOODS-PRICE(15) TO F-PRICE15.
           MOVE GOODS-TOTAL(15) TO F-TOTAL15.
           MOVE GOODS-DESC(16) TO F-DESC16.
           MOVE GOODS-QTY(16) TO F-QTY16.
           MOVE GOODS-PRICE(16) TO F-PRICE16.
           MOVE GOODS-TOTAL(16) TO F-TOTAL16.
           MOVE GOODS-DESC(17) TO F-DESC17.
           MOVE GOODS-QTY(17) TO F-QTY17.
           MOVE GOODS-PRICE(17) TO F-PRICE17.
           MOVE GOODS-TOTAL(17) TO F-TOTAL17.
           MOVE GOODS-DESC(18) TO F-DESC18.
           MOVE GOODS-QTY(18) TO F-QTY18.
           MOVE GOODS-PRICE(18) TO F-PRICE18.
           MOVE GOODS-TOTAL(18) TO F-TOTAL18.
           MOVE GOODS-DESC(19) TO F-DESC19.
           MOVE GOODS-QTY(19) TO F-QTY19.
           MOVE GOODS-PRICE(19) TO F-PRICE19.
           MOVE GOODS-TOTAL(19) TO F-TOTAL19.
           MOVE GOODS-DESC(20) TO F-DESC20.
           MOVE GOODS-QTY(20) TO F-QTY20.
           MOVE GOODS-PRICE(20) TO F-PRICE20.
           MOVE GOODS-TOTAL(20) TO F-TOTAL20.
           MOVE CUR-GOODS TO F-CUR-GOODS.
           MOVE CUSTOMER-NAME TO FCUSTOMER-NAME.
           MOVE CUSTOMER-ADDRESS1 TO  FCUSTOMER-ADDRESS1.
           MOVE CUSTOMER-ADDRESS2 TO FCUSTOMER-ADDRESS2.
           MOVE CUSTOMER-ADDRESS3 TO FCUSTOMER-ADDRESS3.
           MOVE CUSTOMER-ADDRESS4 TO FCUSTOMER-ADDRESS4.
           MOVE CUSTOMER-ADDRESS5 TO FCUSTOMER-ADDRESS5.
           MOVE CUSTOMER-ADDRESS6 TO FCUSTOMER-ADDRESS6.
           MOVE CUSTOMER-TELEPHONE TO FCUSTOMER-TELEPHONE.
           MOVE STRINGED-DATE TO F-DATE.
	   REWRITE INVOICE-DATA.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM RESTORE-SCREEN.
       END-SI.

       LOAD-INVOICE.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM STORE-SCREEN.
	   DISPLAY LOAD-SCREEN.
           READ INVOICE-FILE.
           MOVE F-DESC1 TO GOODS-DESC(1).
           MOVE F-QTY1 TO GOODS-QTY(1).
           MOVE F-PRICE1 TO GOODS-PRICE(1).
           MOVE F-TOTAL1 TO GOODS-TOTAL(1).
           MOVE F-DESC2 TO GOODS-DESC(2).
           MOVE F-QTY2 TO GOODS-QTY(2).
           MOVE F-PRICE2 TO GOODS-PRICE(2).
           MOVE F-TOTAL2 TO GOODS-TOTAL(2).
           MOVE F-DESC3 TO GOODS-DESC(3).
           MOVE F-QTY3 TO GOODS-QTY(3).
           MOVE F-PRICE3 TO GOODS-PRICE(3).
           MOVE F-TOTAL3 TO GOODS-TOTAL(3).
           MOVE F-DESC4 TO GOODS-DESC(4).
           MOVE F-QTY4 TO GOODS-QTY(4).
           MOVE F-PRICE4 TO GOODS-PRICE(4).
           MOVE F-TOTAL4 TO GOODS-TOTAL(4). 
           MOVE F-DESC5 TO GOODS-DESC(5).
           MOVE F-QTY5 TO GOODS-QTY(5).
           MOVE F-PRICE5 TO GOODS-PRICE(5).
           MOVE F-TOTAL5 TO GOODS-TOTAL(5). 
           MOVE F-DESC6 TO GOODS-DESC(6).
           MOVE F-QTY6 TO GOODS-QTY(6).
           MOVE F-PRICE6 TO GOODS-PRICE(6).
           MOVE F-TOTAL6 TO GOODS-TOTAL(6).
           MOVE F-DESC7 TO GOODS-DESC(7).
           MOVE F-QTY7 TO GOODS-QTY(7).
           MOVE F-PRICE7 TO GOODS-PRICE(7).
           MOVE F-TOTAL7 TO GOODS-TOTAL(7).
           MOVE F-DESC8 TO GOODS-DESC(8).
           MOVE F-QTY8 TO GOODS-QTY(8).
           MOVE F-PRICE8 TO GOODS-PRICE(8).
           MOVE F-TOTAL8 TO GOODS-TOTAL(8).
           MOVE F-DESC9 TO GOODS-DESC(9).
           MOVE F-QTY9 TO GOODS-QTY(9).
           MOVE F-PRICE9 TO GOODS-PRICE(9).
           MOVE F-TOTAL9 TO GOODS-TOTAL(9).
	   MOVE F-DESC10 TO GOODS-DESC(10).
	   MOVE F-QTY10 TO GOODS-QTY(10).
	   MOVE F-PRICE10 TO GOODS-PRICE(10).
	   MOVE F-TOTAL10 TO GOODS-TOTAL(10).
	   MOVE F-DESC11 TO GOODS-DESC(11).
	   MOVE F-QTY11 TO GOODS-QTY(11).
	   MOVE F-PRICE11 TO GOODS-PRICE(11).
	   MOVE F-TOTAL11 TO GOODS-TOTAL(11).
	   MOVE F-DESC12 TO GOODS-DESC(12).
	   MOVE F-QTY12 TO GOODS-QTY(12).
	   MOVE F-PRICE12 TO GOODS-PRICE(12).
	   MOVE F-TOTAL12 TO GOODS-TOTAL(12).
	   MOVE F-DESC13 TO GOODS-DESC(13).
	   MOVE F-QTY13 TO GOODS-QTY(13).
	   MOVE F-PRICE13 TO GOODS-PRICE(13).
	   MOVE F-TOTAL13 TO GOODS-TOTAL(13).
	   MOVE F-DESC14 TO GOODS-DESC(14).
	   MOVE F-QTY14 TO GOODS-QTY(14).
	   MOVE F-PRICE14 TO GOODS-PRICE(14).
	   MOVE F-TOTAL14 TO GOODS-TOTAL(14).
	   MOVE F-DESC15 TO GOODS-DESC(15).
	   MOVE F-QTY15 TO GOODS-QTY(15).
	   MOVE F-PRICE15 TO GOODS-PRICE(15).
	   MOVE F-TOTAL15 TO GOODS-TOTAL(15).
	   MOVE F-DESC16 TO GOODS-DESC(16).
	   MOVE F-QTY16 TO GOODS-QTY(16).
	   MOVE F-PRICE16 TO GOODS-PRICE(16).
	   MOVE F-TOTAL16 TO GOODS-TOTAL(16).
	   MOVE F-DESC17 TO GOODS-DESC(17).
	   MOVE F-QTY17 TO GOODS-QTY(17).
	   MOVE F-PRICE17 TO GOODS-PRICE(17).
	   MOVE F-TOTAL17 TO GOODS-TOTAL(17).
	   MOVE F-DESC18 TO GOODS-DESC(18).
	   MOVE F-QTY18 TO GOODS-QTY(18).
	   MOVE F-PRICE18 TO GOODS-PRICE(18).
	   MOVE F-TOTAL18 TO GOODS-TOTAL(18).
	   MOVE F-DESC19 TO GOODS-DESC(19).
	   MOVE F-QTY19 TO GOODS-QTY(19).
	   MOVE F-PRICE19 TO GOODS-PRICE(19).
	   MOVE F-TOTAL19 TO GOODS-TOTAL(19).
	   MOVE F-DESC20 TO GOODS-DESC(20).
	   MOVE F-QTY20 TO GOODS-QTY(20).
	   MOVE F-PRICE20 TO GOODS-PRICE(20).
	   MOVE F-TOTAL20 TO GOODS-TOTAL(20).
	   MOVE FCUSTOMER-NAME TO CUSTOMER-NAME.
           MOVE FCUSTOMER-ADDRESS1 TO CUSTOMER-ADDRESS1.
           MOVE FCUSTOMER-ADDRESS2 TO CUSTOMER-ADDRESS2.
           MOVE FCUSTOMER-ADDRESS3 TO CUSTOMER-ADDRESS3.
           MOVE FCUSTOMER-ADDRESS4 TO CUSTOMER-ADDRESS4.
           MOVE FCUSTOMER-ADDRESS5 TO CUSTOMER-ADDRESS5.
           MOVE FCUSTOMER-ADDRESS6 TO CUSTOMER-ADDRESS6.
           MOVE FCUSTOMER-TELEPHONE TO CUSTOMER-TELEPHONE.
	   MOVE F-DATE TO PRINT-DATE.
	   MOVE F-CUR-GOODS TO CUR-GOODS.
           DISPLAY BLANK-SCREEN.
           DISPLAY EDIT-GOODS-TITLE.
           COMPUTE D-L = 4.
           COMPUTE L-V = 0.
       LOAD-INVOICE-LOOP.
           ADD 1 TO D-L.
           ADD 1 TO L-V.
           DISPLAY DISP-GOODS-LINE.
           IF L-V NOT = 20
               GO TO LOAD-INVOICE-LOOP.
           COMPUTE ST-RE-NUMBER = 2.
           PERFORM STORE-SCREEN.
           PERFORM DISPLAY-MAIN-SCREEN.
           DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM RESTORE-SCREEN.
	   DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
	   PERFORM STORE-SCREEN.
       END-LI.

       CAMP-HELP SECTION.

       ONLINE-MANUAL.
           DISPLAY SMALL-OPTIONS-SCREEN.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY MANUAL-SCREEN.
           CALL X"83" USING ANY-KEY.
           EVALUATE ANY-KEY
               WHEN "A" PERFORM DISP-PAGE1
	       WHEN "B" PERFORM DISP-PAGE5
	       WHEN "C" PERFORM DISP-PAGE6
	       WHEN "D" PERFORM DISP-PAGE7
	       WHEN "a" PERFORM DISP-PAGE1
	       WHEN "b" PERFORM DISP-PAGE5
	       WHEN "c" PERFORM DISP-PAGE6
	       WHEN "d" PERFORM DISP-PAGE7.
	   COMPUTE ST-RE-NUMBER = 3.
           PERFORM RESTORE-SCREEN.
       END-OM.

       DISP-PAGE1.
           DISPLAY SMALL-MANUAL-SCREEN.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM STORE-SCREEN.
           DISPLAY MANUAL-PAGE1.
           CALL X"83" USING ANY-KEY.
           DISPLAY MANUAL-PAGE2.
           CALL X"83" USING ANY-KEY.
           DISPLAY MANUAL-PAGE3.
	   CALL X"83" USING ANY-KEY.
	   DISPLAY MANUAL-PAGE4.
	   CALL X"83" USING ANY-KEY.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM RESTORE-SCREEN.

       DISP-PAGE5.
           DISPLAY SMALL-MANUAL-SCREEN.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM STORE-SCREEN.
	   DISPLAY MANUAL-PAGE5.
           CALL X"83" USING ANY-KEY.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM RESTORE-SCREEN.

       DISP-PAGE6.
           DISPLAY SMALL-MANUAL-SCREEN.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM STORE-SCREEN.
	   DISPLAY MANUAL-PAGE6.
           CALL X"83" USING ANY-KEY.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM RESTORE-SCREEN.

       DISP-PAGE7.
           DISPLAY SMALL-MANUAL-SCREEN.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM STORE-SCREEN.
	   DISPLAY MANUAL-PAGE7.
           CALL X"83" USING ANY-KEY.
           COMPUTE ST-RE-NUMBER = 4.
           PERFORM RESTORE-SCREEN.

       VIEW-VERSION.
           DISPLAY SMALL-OPTIONS-SCREEN.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY VERSION-SCREEN.
           CALL X"83" USING ANY-KEY.
           PERFORM RESTORE-SCREEN.
       END-VV.

       SETUP-FILE.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM STORE-SCREEN.
	   MOVE SPACES TO CUSTOMER-NAME.
	   MOVE SPACES TO CUSTOMER-ADDRESS1.
	   MOVE SPACES TO CUSTOMER-ADDRESS2.
	   MOVE SPACES TO CUSTOMER-ADDRESS3.
	   MOVE SPACES TO CUSTOMER-ADDRESS4.
	   MOVE SPACES TO CUSTOMER-ADDRESS5.
	   MOVE SPACES TO CUSTOMER-ADDRESS6.
	   MOVE SPACES TO CUSTOMER-TELEPHONE.
	   PERFORM DISPLAY-MAIN-SCREEN.
	   DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
	   COMPUTE ST-RE-NUMBER = 1.
	   PERFORM STORE-SCREEN.
	   PERFORM CLEAR-IO THRU END-CF.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM RESTORE-SCREEN.
	   DISPLAY SET-NUM-MAIN.
	   COMPUTE CURRENT-INVOICE = 0.
       SETUP-LOOP.
	   ADD 1 TO CURRENT-INVOICE.
	   DISPLAY SET-NUM.
	   WRITE INVOICE-DATA.
	   IF CURRENT-INVOICE NOT = 1000
	       GO TO SETUP-LOOP.
	   COMPUTE CURRENT-INVOICE = 1.
	   OPEN OUTPUT CURRENT-INVOICE-FILE.
	   WRITE CURRENT-INVOICE-FILE-DATA FROM CURRENT-INVOICE.
	   CLOSE CURRENT-INVOICE-FILE.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM RESTORE-SCREEN.
       END-SF.

       BACKUP-FILE.
	   DISPLAY SMALL-OPTIONS-SCREEN.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM STORE-SCREEN.
	   DISPLAY BACKUP-BOX.
	   CALL X"83" USING ANY-KEY.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM RESTORE-SCREEN.
       END-BF.

       END-PROG.
	   DISPLAY SMALL-OPTIONS-SCREEN.
	   COMPUTE ST-RE-NUMBER = 3.
	   PERFORM STORE-SCREEN.
	   DISPLAY END-SCREEN.
	   CALL X"83" USING ANY-KEY.
	   IF ANY-KEY = "N" OR ANY-KEY = "n"
	       GO TO END-EP.
	   CLOSE INVOICE-FILE.
	   STOP RUN.
       END-EP.
	   PERFORM RESTORE-SCREEN.

       CAMP-STORE-RESTORE SECTION.

       STORE-SCREEN.
           COMPUTE LENGTH-FIELD = 2000.
           COMPUTE SCREEN-START = 0001.
           COMPUTE FUNCTION = 0.
           COMPUTE BUFFER-START = 1.
           CALL X"B7" USING
           FUNCTION,GET-SCREEN-DATA-TABLE,TEMP-SCREEN-DATA.
           COMPUTE LENGTH-FIELD = 2000.
           COMPUTE SCREEN-START = 0001.
           COMPUTE FUNCTION = 2.
           COMPUTE BUFFER-START = 2001.
           CALL X"B7" USING
           FUNCTION,GET-SCREEN-DATA-TABLE,TEMP-SCREEN-DATA.
           MOVE TEMP-SCREEN-DATA TO SCREEN-DATA(ST-RE-NUMBER).

       RESTORE-SCREEN.
           DISPLAY BLANK-SCREEN.
           COMPUTE LENGTH-FIELD = 2000.
           COMPUTE SCREEN-START = 0001.
           COMPUTE FUNCTION = 1.
           COMPUTE BUFFER-START = 1.
           CALL X"B7" USING
           FUNCTION,GET-SCREEN-DATA-TABLE,SCREEN-DATA(ST-RE-NUMBER).
           COMPUTE LENGTH-FIELD = 2000.
           COMPUTE SCREEN-START = 0001.
           COMPUTE FUNCTION = 3.
           COMPUTE BUFFER-START = 2001.
           CALL X"B7" USING
           FUNCTION,GET-SCREEN-DATA-TABLE,SCREEN-DATA(ST-RE-NUMBER).

       END-PROGRAM CAMPBELLS-CUISINE.
