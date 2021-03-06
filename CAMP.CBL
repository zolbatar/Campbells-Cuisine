       IDENTIFICATION DIVISION.

       PROGRAM-ID.      CAMPBELLS-INVOICING.
       AUTHOR.          DARYL-DUDEY.

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT PRINTER-FILE ASSIGN TO PRINTER.
           SELECT CURRENT-INVOICE-FILE ASSIGN TO "CURINV.DAT".

       DATA DIVISION.

       FILE SECTION.

       FD PRINTER-FILE REPORT IS PRINT-REPORT.

       FD CURRENT-INVOICE-FILE.
       01 CURRENT-INVOICE-FILE-DATA.
         02 CUR-INVOICE                PIC 9(4).

       WORKING-STORAGE SECTION.

      *SCREEN STORE RESTORE FEATURE VARIABLES.

       01 TEMP-SCREEN-DATA             PIC X(4000).
       01 SCREEN-STATE OCCURS 4 TIMES.
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

      *CONSTANT VARIABLES.

       01 MAX-GOODS                    PIC 99 VALUE 20.

      *MAIN VARIABLES

       01 CURRENT-INVOICE              PIC 9(4).
       01 COMP-TOTAL                   PIC 9(6)V99.
       01 TODAYS-DATE.
         02 DATE-YEAR                  PIC 99.
         02 DATE-MONTH                 PIC 99.
         02 DATE-DAY                   PIC 99.
       01 STRINGED-DATE                PIC X(8).

      *MENU VARIABLES.

       01 MENU-OPTION                  PIC X.

      *MULTI PURPOSE VARIABLES.

       01 ANY-KEY                      PIC X.
       01 D-L                          PIC 99.
       01 LOOP-VARIABLE                PIC 99.

       REPORT SECTION.

       RD PRINT-REPORT         PAGE LIMIT 70 LINES
                               HEADING 1
                               FIRST DETAIL 4
                               LAST DETAIL 70.

       01 TYPE PAGE HEADING.
         02 LINE 1  COLUMN 30 PIC X(21) VALUE "旼컴컴컴컴컴컴컴컴컴�".
         02 LINE 2  COLUMN 30 PIC X(21) VALUE "쿎AMPBELL'S CUISINE �".
         02 LINE 3  COLUMN 30 PIC X(21) VALUE "읕컴컴컴컴컴컴컴컴컴�".
       01 PRINT-REPORT-GROUP TYPE DETAIL.
         02 LINE 4.
           03 COLUMN 1  PIC X(22) VALUE "旼컴컴컴컴컴컴컴컴컴커".
           03 COLUMN 23 PIC X(22) VALUE "旼컴컴컴컴컴컴컴컴컴커".
           03 COLUMN 50 PIC X(15) VALUE "Invoice number:".
           03 COLUMN 66 PIC Z(4) SOURCE CURRENT-INVOICE.
         02 LINE 5.
           03 COLUMN 1  PIC X(22) VALUE "퀈xxxxxxx'S CUISINE  �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-NAME.
         02 LINE 6.
           03 COLUMN 1  PIC X(22) VALUE "�                    �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 50 PIC X(5)  VALUE "Date:".
           03 COLUMN 56 PIC X(8) SOURCE STRINGED-DATE.
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS1.
         02 LINE 7.
           03 COLUMN 1  PIC X(22) VALUE "퀈xxxxxxxxxxxxxxxxx  �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS2.
         02 LINE 8.
           03 COLUMN 1  PIC X(22) VALUE "퀈xxxxxxxxxxxxxxxx   �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS3.
         02 LINE 9.
           03 COLUMN 1  PIC X(22) VALUE "퀈xxxxxxx            �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS4.
         02 LINE 10.
           03 COLUMN 1  PIC X(22) VALUE "퀈xxxxxx             �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-ADDRESS5.
         02 LINE 11.
           03 COLUMN 1  PIC X(22) VALUE "�                    �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 24 PIC X(15) SOURCE CUSTOMER-ADDRESS6.
         02 LINE 12.
           03 COLUMN 1  PIC X(22) VALUE "�(000) 111111        �".
           03 COLUMN 23 PIC X(22) VALUE "�                    �".
           03 COLUMN 24 PIC X(20) SOURCE CUSTOMER-TELEPHONE.
         02 LINE 13.
           03 COLUMN 1  PIC X(22) VALUE "읕컴컴컴컴컴컴컴컴컴켸".
           03 COLUMN 23 PIC X(22) VALUE "읕컴컴컴컴컴컴컴컴컴켸".
         02 LINE 20 COLUMN 10 PIC X(60) VALUE
         "旼컴컫컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컫컴컫컴컴컫컴컴컴커".
         02 LINE 21 COLUMN 10 PIC X(60) VALUE
         "쿔TEM쿏ESCRIPTION                        쿜TY쿛RICE쿟OTAL  �".
         02 LINE 22 COLUMN 10 PIC X(60) VALUE
         "쳐컴컵컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컵컴컵컴컴컵컴컴컴캑".
         02 LINE 23.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 1".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(1).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(1).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(1).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(1).
         02 LINE 24.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 2".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(2).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(2).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(2).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(2).
         02 LINE 25.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 3".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(3).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(3).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(3).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(3).
         02 LINE 26.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 4".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(4).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(4).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(4).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(4).
         02 LINE 27.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 5".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(5).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(5).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(5).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(5).
         02 LINE 28.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 6".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(6).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(6).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(6).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(6).
         02 LINE 29.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 7".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(7).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(7).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(7).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(7).
         02 LINE 30.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 8".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(8).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(8).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(8).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(8).
         02 LINE 31.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE " 9".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(9).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(9).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(9).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(9).
         02 LINE 32.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "10".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(10).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(10).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(10).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(10).
         02 LINE 33.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "11".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(11).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(11).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(11).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(11).
         02 LINE 34.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "12".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(12).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(12).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(12).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(12).
         02 LINE 35.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "13".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(13).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(13).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(13).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(13).
         02 LINE 36.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "14".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(14).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(14).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(14).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(14).
         02 LINE 37.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "15".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(15).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(15).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(15).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(15).
         02 LINE 38.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "16".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(16).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(16).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(16).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(16).
         02 LINE 39.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "17".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(17).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(17).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(17).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(17).
         02 LINE 40.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "18".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(18).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(18).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(18).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(18).
         02 LINE 41.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "19".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(19).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(19).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(19).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(19).
         02 LINE 42.
           03 COLUMN 10 PIC X(60) VALUE
         "�    �                                   �   �     �       �".
           03 COLUMN 11 PIC XX VALUE "20".
           03 COLUMN 16 PIC X(35) SOURCE GOODS-DESC(20).
           03 COLUMN 52 PIC ZZZ SOURCE GOODS-QTY(20).
           03 COLUMN 56 PIC ZZ.ZZ SOURCE GOODS-PRICE(20).
           03 COLUMN 62 PIC ZZZZ.ZZ SOURCE GOODS-TOTAL(20).
         02 LINE 43.
           03 COLUMN 10 PIC X(60) VALUE
         "읕컴컨컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴쩡컴컨컴컨컴컴컨컴컴컴캑".
         02 LINE 44.
           03 COLUMN 10 PIC X(60) VALUE
         "                                    � AMOUNT DUE:          �".
           03 COLUMN 59 PIC Z(6).ZZ SOURCE COMP-TOTAL.
         02 LINE 45.
           03 COLUMN 10 PIC X(60) VALUE
         "                                    읕컴컴컴컴컴컴컴컴컴컴켸".
         02 LINE 68 COLUMN 15 PIC X(51) VALUE
         "BON APPETIT! BON APPETIT! BON APPETIT! BON APPETIT!".
         02 LINE 70 PIC X(1) VALUE SPACE.

       SCREEN SECTION.

       01 BLANK-SCREEN.
         02 BLANK SCREEN.
       01 SMALL-CUSTOMER-DATA-SCREEN.
         02 LINE 1  COLUMN 1  VALUE "旼컴컴컴컴컴컴컴컴컴컴컴컴커".
         02 LINE 2  COLUMN 1  VALUE "�    CUSTOMER DETAILS      �".
         02 LINE 3  COLUMN 1  VALUE "쳐컴컴컴컴컴컴컴컴컴컴컴컴캑".
         02 LINE 4  COLUMN 1  VALUE "쿙AME:                     �".
         02 LINE 5  COLUMN 1  VALUE "쿌DDR:                     �".
         02 LINE 6  COLUMN 1  VALUE "�                          �".
         02 LINE 7  COLUMN 1  VALUE "�                          �".
         02 LINE 8  COLUMN 1  VALUE "�                          �".
         02 LINE 9  COLUMN 1  VALUE "�                          �".
         02 LINE 10 COLUMN 1  VALUE "�                          �".
         02 LINE 11 COLUMN 1  VALUE "� TEL:                     �".
         02 LINE 12 COLUMN 1  VALUE "읕컴컴컴컴컴컴컴컴컴컴컴컴켸".
         02 LINE 4  COLUMN 7 PIC X(20) FROM CUSTOMER-NAME.
         02 LINE 5  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS1.
         02 LINE 6  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS2.
         02 LINE 7  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS3.
         02 LINE 8  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS4.
         02 LINE 9  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS5.
         02 LINE 10 COLUMN 7 PIC X(15) FROM CUSTOMER-ADDRESS6.
         02 LINE 11 COLUMN 7 PIC X(20) FROM CUSTOMER-TELEPHONE.
       01 LARGE-CUSTOMER-DATA-SCREEN.
         02 LINE 1  COLUMN 1  VALUE "�袴袴袴袴袴袴袴袴袴袴袴袴袴�".
         02 LINE 2  COLUMN 1  VALUE "�    CUSTOMER DETAILS      �".
         02 LINE 3  COLUMN 1  VALUE "勁袴袴袴袴袴袴袴袴袴袴袴袴攷".
         02 LINE 4  COLUMN 1  VALUE "튞AME:                     �".
         02 LINE 5  COLUMN 1  VALUE "튍DDR:                     �".
         02 LINE 6  COLUMN 1  VALUE "�                          �".
         02 LINE 7  COLUMN 1  VALUE "�                          �".
         02 LINE 8  COLUMN 1  VALUE "�                          �".
         02 LINE 9  COLUMN 1  VALUE "�                          �".
         02 LINE 10 COLUMN 1  VALUE "�                          �".
         02 LINE 11 COLUMN 1  VALUE "� TEL:                     �".
         02 LINE 12 COLUMN 1  VALUE "훤袴袴袴袴袴袴袴袴袴袴袴袴暠".
         02 LINE 4  COLUMN 7 PIC X(20) FROM CUSTOMER-NAME.
         02 LINE 5  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS1.
         02 LINE 6  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS2.
         02 LINE 7  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS3.
         02 LINE 8  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS4.
         02 LINE 9  COLUMN 7 PIC X(20) FROM CUSTOMER-ADDRESS5.
         02 LINE 10 COLUMN 7 PIC X(15) FROM CUSTOMER-ADDRESS6.
         02 LINE 11 COLUMN 7 PIC X(20) FROM CUSTOMER-TELEPHONE.
       01 ENTER-CUSTOMER-DATA.
         02 LINE 4  COLUMN 7 PIC X(20) TO CUSTOMER-NAME.
         02 LINE 5  COLUMN 7 PIC X(20) TO CUSTOMER-ADDRESS1.
         02 LINE 6  COLUMN 7 PIC X(20) TO CUSTOMER-ADDRESS2.
         02 LINE 7  COLUMN 7 PIC X(20) TO CUSTOMER-ADDRESS3.
         02 LINE 8  COLUMN 7 PIC X(20) TO CUSTOMER-ADDRESS4.
         02 LINE 9  COLUMN 7 PIC X(20) TO CUSTOMER-ADDRESS5.
         02 LINE 10 COLUMN 7 PIC X(20) TO CUSTOMER-ADDRESS6.
         02 LINE 11 COLUMN 7 PIC X(20) TO CUSTOMER-TELEPHONE.
       01 SMALL-OPTIONS-SCREEN.
         02 LINE 13 COLUMN 1  VALUE "旼컴컴컴컴컴컴컴컴컴컴컴컴커".
         02 LINE 14 COLUMN 1  VALUE "�       Options Menu       �".
         02 LINE 15 COLUMN 1  VALUE "쳐컴컴컴컴컴컴컴컴컴컴컴컴캑".
         02 LINE 16 COLUMN 1  VALUE "�1-Enter customer details  �".
         02 LINE 17 COLUMN 1  VALUE "�2-Input invoice order     �".
         02 LINE 18 COLUMN 1  VALUE "�3-Clear all details       �".
         02 LINE 19 COLUMN 1  VALUE "�4-Print Invoice(s)        �".
         02 LINE 20 COLUMN 1  VALUE "�5 Set current invoice no. �".
         02 LINE 21 COLUMN 1  VALUE "�6-Quit program            �".
         02 LINE 22 COLUMN 1  VALUE "읕컴컴컴컴컴컴컴컴컴컴컴컴켸".
       01 LARGE-OPTIONS-SCREEN.
         02 LINE 13 COLUMN 1  VALUE "�袴袴袴袴袴袴袴袴袴袴袴袴袴�".
         02 LINE 14 COLUMN 1  VALUE "�       Options Menu       �".
         02 LINE 15 COLUMN 1  VALUE "勁袴袴袴袴袴袴袴袴袴袴袴袴攷".
         02 LINE 16 COLUMN 1  VALUE "�1-Enter customer details  �".
         02 LINE 17 COLUMN 1  VALUE "�2-Input invoice order     �".
         02 LINE 18 COLUMN 1  VALUE "�3-Clear all details       �".
         02 LINE 19 COLUMN 1  VALUE "�4-Print Invoice(s)        �".
         02 LINE 20 COLUMN 1  VALUE "�5-Set current invoice no. �".
         02 LINE 21 COLUMN 1  VALUE "�6-Quit program            �".
         02 LINE 22 COLUMN 1  VALUE "훤袴袴袴袴袴袴袴袴袴袴袴袴暠".
       01 SC-SCREEN.
         02 LINE 8  COLUMN 30 VALUE "�袴袴袴袴袴袴袴袴袴袴袴袴袴袴�".
         02 LINE 9  COLUMN 30 VALUE "� Set Current Invoice NUmber �".
         02 LINE 10 COLUMN 30 VALUE "勁袴袴袴袴袴袴袴袴袴袴袴袴袴攷".
         02 LINE 11 COLUMN 30 VALUE "� Current Invoice:           �".
         02 LINE 12 COLUMN 30 VALUE "훤袴袴袴袴袴袴袴袴袴袴袴袴袴暠".
         02 LINE 11 COLUMN 48 PIC Z(4) TO CURRENT-INVOICE.
       01 CLEAR-FIELDS-SCREEN.
         02 LINE 8  COLUMN 30 VALUE "�袴袴袴袴袴袴袴袴袴袴袴敲".
         02 LINE 9  COLUMN 30 VALUE "� CLEAR ALL DETAILS Y/N �".
         02 LINE 10 COLUMN 30 VALUE "훤袴袴袴袴袴袴袴袴袴袴袴�".
       01 CONTINUE-GOODS-SCREEN.
         02 LINE 8  COLUMN 30 VALUE "�袴袴袴袴袴袴袴�".
         02 LINE 9  COLUMN 30 VALUE "� CONTINUE Y/N �".
         02 LINE 10 COLUMN 30 VALUE "훤袴袴袴袴袴袴暠".
       01 PRINTING-SCREEN.
         02 LINE 8  COLUMN 30 VALUE "�袴袴袴袴袴袴敲".
         02 LINE 9  COLUMN 30 VALUE "�  PRINTING   �".
         02 LINE 10 COLUMN 30 VALUE "� PRESS A KEY �".
         02 LINE 11 COLUMN 30 VALUE "훤袴袴袴袴袴袴�".
       01 EDIT-GOODS-TITLE.
         02 BLANK SCREEN.
         02 LINE 1 COLUMN 1
         "�袴袴藁袴袴袴袴袴袴袴袴袴袴袴袴藁袴袴藁袴袴袴藁袴袴袴袴敲".
         02 LINE 2 COLUMN 1
         "튘TEM 튒ESCRIPTION              � QTY � PRICE � TOTAL   �".
         02 LINE 3 COLUMN 1
         "튞O.  �                         �     � EACH  � VALUE   �".
         02 LINE 4 COLUMN 1
         "훤袴袴姦袴袴袴袴袴袴袴袴袴袴袴袴姦袴袴姦袴袴袴姦袴袴袴袴�".
       01 DISPLAY-GOODS-SCREEN.
         02 LINE D-L COLUMN 3 PIC 99 FROM ITEM-NUMBER.
       01 EDIT-GOODS-SCREEN.
         02 LINE D-L COLUMN 8 PIC X(25) TO GOODS-DESC(CUR-GOODS).
         02 LINE D-L COLUMN 35 PIC ZZZ TO GOODS-QTY(CUR-GOODS).
         02 LINE D-L COLUMN 41 PIC ZZ.ZZ TO GOODS-PRICE(CUR-GOODS).
       01 EDIT-FINAL-DATA.
         02 LINE D-L COLUMN 49 PIC ZZZZ.ZZ FROM GOODS-TOTAL(CUR-GOODS).

       PROCEDURE DIVISION.

       CAMP-START SECTION.

       START-PROGRAM.
           DISPLAY BLANK-SCREEN.
           PERFORM INIT.
           PERFORM MAIN-PROGRAM THRU MAIN-PROGRAM-LOOP.

       INIT.
           COMPUTE CUR-GOODS = 1.
           DISPLAY BLANK-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
           ADD 1 TO ST-RE-NUMBER.
           DISPLAY EDIT-GOODS-TITLE.
           PERFORM STORE-SCREEN.
           OPEN INPUT CURRENT-INVOICE-FILE.
           READ CURRENT-INVOICE-FILE INTO CURRENT-INVOICE.
           CLOSE CURRENT-INVOICE-FILE.
           ACCEPT TODAYS-DATE FROM DATE.
           STRING
           DATE-DAY
           "/"
           DATE-MONTH
           "/"
           DATE-YEAR
           DELIMITED BY SIZE
           INTO STRINGED-DATE.

       CAMP-MAIN SECTION.

       MAIN-PROGRAM.
           DISPLAY BLANK-SCREEN.
           PERFORM DISPLAY-MAIN-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
       MAIN-PROGRAM-LOOP.
           DISPLAY LARGE-OPTIONS-SCREEN.
           PERFORM GET-MENU-OPTION.
           GO TO MAIN-PROGRAM-LOOP.
           STOP RUN.

       DISPLAY-MAIN-SCREEN.
           DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
           DISPLAY SMALL-OPTIONS-SCREEN.

       GET-MENU-OPTION.
           CALL X"83" USING MENU-OPTION.
           EVALUATE MENU-OPTION
               WHEN "1"
                   PERFORM EDIT-CUSTOMER-DATA THRU END-ECD
               WHEN "2"
                   PERFORM EDIT-GOODS-DATA THRU END-EGD
               WHEN "3"
                   PERFORM CLEAR-ALL-FIELDS
               WHEN "4"
                   PERFORM PRINT-INVOICE THRU END-PI
               WHEN "5"
                   PERFORM SET-CUR-INV THRU END-SCI
               WHEN "6"
                   STOP RUN.

       EDIT-CUSTOMER-DATA.
           DISPLAY SMALL-OPTIONS-SCREEN.
           DISPLAY LARGE-CUSTOMER-DATA-SCREEN.
           ACCEPT ENTER-CUSTOMER-DATA.
           DISPLAY SMALL-CUSTOMER-DATA-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
       END-ECD.

       EDIT-GOODS-DATA.
           IF CUR-GOODS = MAX-GOODS
               GO TO END-EGD.
           COMPUTE ST-RE-NUMBER = 2.
           PERFORM RESTORE-SCREEN.
       EDIT-GOODS-LOOP.
           COMPUTE D-L = 4 + CUR-GOODS.
           COMPUTE ITEM-NUMBER = CUR-GOODS.
           DISPLAY DISPLAY-GOODS-SCREEN.
           ACCEPT EDIT-GOODS-SCREEN.
           COMPUTE GOODS-TOTAL(CUR-GOODS) =
               GOODS-QTY(CUR-GOODS) * GOODS-PRICE(CUR-GOODS).
           DISPLAY EDIT-FINAL-DATA.
           IF CUR-GOODS = MAX-GOODS
               GO TO END-EGD.
           ADD 1 TO CUR-GOODS.
           PERFORM CONTINUE-EDITING-GOODS.
           IF ANY-KEY = "Y"
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

       CLEAR-ALL-FIELDS.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY CLEAR-FIELDS-SCREEN.
           CALL X"83" USING ANY-KEY.
           IF ANY-KEY = "Y"
               PERFORM CLEAR-FIELDS THRU END-CF.
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM RESTORE-SCREEN.

       CLEAR-FIELDS.
           MOVE SPACES TO CUSTOMER-NAME.
           MOVE SPACES TO CUSTOMER-ADDRESS1.
           MOVE SPACES TO CUSTOMER-ADDRESS2.
           MOVE SPACES TO CUSTOMER-ADDRESS3.
           MOVE SPACES TO CUSTOMER-ADDRESS4.
           MOVE SPACES TO CUSTOMER-ADDRESS5.
           MOVE SPACES TO CUSTOMER-ADDRESS6.
           MOVE SPACES TO CUSTOMER-TELEPHONE.
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
           PERFORM DISPLAY-MAIN-SCREEN.
           COMPUTE ST-RE-NUMBER = 1.
           PERFORM STORE-SCREEN.
           ADD 1 TO ST-RE-NUMBER.
           DISPLAY EDIT-GOODS-TITLE.
           PERFORM STORE-SCREEN.
       END-CF.

       PRINT-INVOICE.
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
           COMPUTE ST-RE-NUMBER = 3.
           PERFORM STORE-SCREEN.
           DISPLAY SC-SCREEN.
           ACCEPT SC-SCREEN.
           OPEN OUTPUT CURRENT-INVOICE-FILE.
           WRITE CURRENT-INVOICE-FILE-DATA FROM CURRENT-INVOICE.
           CLOSE CURRENT-INVOICE-FILE.
           PERFORM RESTORE-SCREEN.
       END-SCI.

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