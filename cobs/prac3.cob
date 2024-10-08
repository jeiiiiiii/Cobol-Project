       IDENTIFICATION DIVISION.
       PROGRAM-ID. POPULATION-REPORT.
      *AUTHOR. jei
      *INSTALLATION. PUP-MANILA.
      *DATE-WRITTEN. 02/12/2024.
      *DATE-COMPILED. 02/12/2024.
      *SECURITY. BSIT22N ONLY.
      *REMARKS. PATAPOS PLS. 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. GIGABYTE.
       OBJECT-COMPUTER. GIGABYTE.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT OUTFILE ASSIGN TO "JEI".
       DATA DIVISION.
       FILE SECTION.
       FD   OUTFILE
            LABEL RECORD IS STANDARD
            DATA RECORD IS OUTREC.
       01   OUTREC.
           02 D-EXANO PIC 9(10).
           02 D-EXANA PIC X(25).
           02 D-DB PIC X(20).
           02 D-UN PIC X(5).
           02 D-CN PIC X(4).
           02 D-R PIC X(6).
           02 D-UC PIC 9.
           02 D-CC PIC 9.
           02 D-RR PIC X(6).
       WORKING-STORAGE SECTION.
       01  EOFSW PIC 9 VALUE 0.
       01  ENO PIC X VALUE SPACES.
       01  EXANO PIC 9(10) VALUE 0.
       01  EXANA PIC X(25) VALUE SPACES.
       01  DB PIC X(20) VALUE SPACES.
       01  UC PIC 9 VALUE 0.
       01  UN PIC X(5) VALUE SPACES.
       01  CC PIC 9 VALUE 0.
       01  CN PIC X(4) VALUE SPACES.
       01  TNI PIC 9(3) VALUE 0.
       01  TS PIC 99 VALUE 0.
       01  R PIC X(6) VALUE SPACES.
       01  TNP PIC 99 VALUE 0.
       01  TNF PIC 99 VALUE 0.
       01  RR PIC X(6) VALUE SPACES.
       01  PER PIC 9(2)V99 VALUE 0.
       SCREEN SECTION.
       01   SCRE.
            02 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
            OPEN OUTPUT OUTFILE.
            PERFORM ACCEPT-RTN THRU ACCEPT-RTN-END UNTIL EOFSW = 1.
            PERFORM FINISH-RTN.
            CLOSE OUTFILE.
            STOP RUN.
       ACCEPT-RTN.
            DISPLAY SCRE.
            MOVE SPACES TO ENO.
            MOVE 0 TO UC.
            MOVE 0 TO CC.
            MOVE SPACES TO R.
            DISPLAY "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES"
            LINE 1 COLUMN 19.
            DISPLAY "IT PROFESSIONAL BOARD EXAM RESULT"
            LINE 2 COLUMN 20.
            DISPLAY "Examinee Number:" LINE 4 COLUMN 2.
            ACCEPT EXANO LINE 4 COLUMN 40.
            MOVE EXANO TO D-EXANO.
            DISPLAY "Examinee Name:" LINE 5 COLUMN 2.
            ACCEPT EXANA LINE 5 COLUMN 40.
            MOVE EXANA TO D-EXANA.
            DISPLAY "Date of birth:" LINE 6 COLUMN 2.
            ACCEPT DB LINE 6 COLUMN 40.
            MOVE DB TO D-DB.         
            PERFORM UCODE-RTN UNTIL UC > 0 AND UC < 6.
            MOVE UC TO D-UC.
            DISPLAY "UNIVESITY NAME: " LINE 8 COLUMN 2.
            DISPLAY UN LINE 8 COLUMN 40.
            PERFORM CC-RTN UNTIL CC > 0 AND CC < 4.
            MOVE CC TO D-CC
            DISPLAY "COURSE NAME" LINE 10 COLUMN 2.
            DISPLAY CN LINE 10 COLUMN 40.
            DISPLAY "TOTAL NO. OF ITEMS:" LINE 11 COLUMN 2.
            ACCEPT TNI LINE 11 COLUMN 40.
            DISPLAY "TEST RESULT (SCORE):" LINE 12 COLUMN 2.
            ACCEPT TS LINE 12 COLUMN 40.
           PERFORM R-RTN.
           MOVE R TO D-R.
           MOVE RR TO D-RR.
           DISPLAY "REMARKS" LINE 13 COLUMN 2.
           DISPLAY R LINE 13 COLUMN 40.
           COMPUTE TS DIVIDE BY TNI MULTIPLY BY 100 = PER.
            PERFORM ANOTHER-RTN
            UNTIL ENO = 'Y' OR  ENO = 'y' 
            OR  ENO = 'N' OR  ENO = 'n'.
       ACCEPT-RTN-END.
       ANOTHER-RTN.
            DISPLAY "INPUT ANOTHER RECORD (Y/N)?"
            LINE 18 COLUMN 25.
            ACCEPT ENO LINE 18 COLUMN 54.
            IF ENO = 'N' OR 'n' MOVE 1 TO EOFSW.
            IF ENO = 'Y' OR 'y' MOVE 0 TO EOFSW.
       UCODE-RTN.
           DISPLAY "Univesity Code:" line 7 COLUMN 2
           ACCEPT UC line 7 COLUMN 40
            IF UC = 1 MOVE "UP" TO UN.
            IF UC = 2 MOVE "PUP" TO UN.
            IF UC = 3 MOVE "DLSU" TO UN.
            IF UC = 4 MOVE "ADNMU" TO UN.
            IF UC = 5 MOVE "MAPUA" TO UN.
       CC-RTN.
           DISPLAY "COURSE CODE:" line 9 COLUMN 2
           ACCEPT CC line 9 COLUMN 40
            IF CC = 1 MOVE "BSIT" TO CN.
            IF CC = 2 MOVE "BSCS" TO CN.
            IF CC = 3 MOVE "BSIS" TO CN.
       R-RTN.
           IF CC = 1
               IF PER > 60 MOVE "PASSED" TO R.
           IF CC = 1
               IF PER < 60 MOVE "FAILED" TO RR.
           IF CC = 2
               IF PER > 70 MOVE "PASSED" TO R.
           IF CC = 2
               IF PER < 70 MOVE "FAILED" TO RR.
           IF CC = 3
               IF PER > 50 MOVE "PASSED" TO R.
           IF CC = 3
               IF PER < 50 MOVE "FAILED" TO RR.
       FINISH-RTN.
           DISPLAY "TOTAL NO. OF PASSED" LINE 15 COLUMN 3.
            DISPLAY TNP LINE 15 COLUMN 40.
            DISPLAY "TOTAL NO. OF FAILED:" LINE 16 COLUMN 3.
            DISPLAY TNF LINE 16 COLUMN 40.
       