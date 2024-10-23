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
           02 D-SNO PIC X(10).
           02 D-SNA PIC X(25).
           02 D-ST PIC X(10).
           02 D-Y PIC 9.
           02 D-TAF PIC 9(5)V99.
           02 D-CC PIC 9.
       WORKING-STORAGE SECTION.
       01  EOFSW PIC 9 VALUE 0.
       01  ENO PIC X VALUE SPACES.
       01  SNO PIC 9(10) VALUE 0.
       01  SNA PIC X(25) VALUE SPACES.
       01  CC PIC 9 VALUE 0.
       01  CN PIC X(25) VALUE SPACES.
       01  Y PIC 9 VALUE 0.
       01  S PIC 9 VALUE 0.
       01  ST PIC X VALUE SPACES.
       01  STN PIC X(10) VALUE SPACES.
       01  TF PIC 9(4)V99 VALUE 0.
       01  SCF PIC 9(3)V99 VALUE 0.
       01  LF PIC 9(3)V99 VALUE 0.
       01  MF PIC 9(4)V99 VALUE 0.
       01  TAF PIC 9(5)V99 VALUE 0.
       SCREEN SECTION.
       01   SCRE.
            02 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
            OPEN OUTPUT OUTFILE.
            PERFORM ACCEPT-RTN THRU ACCEPT-RTN-END UNTIL EOFSW = 1.
            CLOSE OUTFILE.
            STOP RUN.
       ACCEPT-RTN.
            DISPLAY SCRE.
            MOVE SPACES TO ENO.
            MOVE 0 TO CC.
            MOVE SPACES TO ST.
            DISPLAY "POLYTECHNIC UNIVERSITY OF THE PHILIPPINES"
            LINE 1 COLUMN 19.
            DISPLAY "STA. MESA, MANILA" LINE 2 COLUMN 31.
            DISPLAY "STUDENT'S STATEMENT OF ACCOUNT" LINE 4 COLUMN 25.
            DISPLAY "STUDENT NO.:" LINE 5 COLUMN 3.
            ACCEPT SNO LINE 5 COLUMN 40.
            MOVE SNO TO D-SNO.
            DISPLAY "STUDENT NAME:" LINE 6 COLUMN 3.
            ACCEPT SNA LINE 6 COLUMN 40.
            MOVE SNA TO D-SNA.
            PERFORM COURSE-RTN UNTIL CC > 0 AND CC < 7.
            MOVE CC TO D-CC.
            DISPLAY "COURSE NAME:" LINE 8 COLUMN 3.
            DISPLAY CN LINE 8 COLUMN 40.
            DISPLAY "YEAR:" LINE 9 COLUMN 3.
            ACCEPT Y LINE 9 COLUMN 40.
            MOVE Y TO D-Y.

            DISPLAY "SECTION:" LINE 10 COLUMN 3.
            ACCEPT SNA LINE 10 COLUMN 40.
           PERFORM STN-RTN UNTIL ST = 'R' OR ST = 'I' OR ST = 'r' OR
            'i'.
            
            DISPLAY "STUDENT TYPE NAME:" LINE 12 COLUMN 3.
            DISPLAY STN LINE 12 COLUMN 40.
            MOVE ST TO D-ST.

            DISPLAY "TUITION FEE:" LINE 13 COLUMN 3.
            ACCEPT TF LINE 13 COLUMN 40.
            DISPLAY "STUDENT COUNCIL FEE:" LINE 14 COLUMN 3.
            ACCEPT SCF LINE 14 COLUMN 40.
            DISPLAY "LABORATORY FEE:" LINE 15 COLUMN 3.
            ACCEPT LF LINE 15 COLUMN 40.
            DISPLAY "MISCELLANEOUS FEE:" LINE 16 COLUMN 3.
            ACCEPT MF LINE 16 COLUMN 40.           
            PERFORM ANOTHER-RTN
            UNTIL ENO = 'Y' OR  ENO = 'y' 
            OR  ENO = 'N' OR  ENO = 'n'.
       ACCEPT-RTN-END.
       COURSE-RTN.
           DISPLAY "COURSE CODE:" LINE 7 COLUMN 3.
           ACCEPT CC LINE 7 COLUMN 40.
            IF CC = 1 MOVE "ACCOUNTING" TO CN.
            IF CC = 2 MOVE "ARTS" TO CN.
            IF CC = 3 MOVE "BUSINESS" TO CN.
            IF CC = 4 MOVE "COMSCIE" TO CN.
            IF CC = 5 MOVE "EDUC" TO CN.
            IF CC = 6 MOVE "ENG" TO CN.
       STN-RTN.
            DISPLAY "STUDENT TYPE:" LINE 11 COLUMN 3.
            ACCEPT ST LINE 11 COLUMN 40.
            IF ST = 'R' MOVE "REGULAR" TO STN.
            IF ST = 'I' MOVE "IRREGULAR" TO STN.
            IF ST = 'i' MOVE "IRREGULAR" TO STN.
            IF ST = 'r' MOVE "REGULAR" TO STN.
       ANOTHER-RTN.
            DISPLAY "INPUT ANOTHER RECORD (Y/N)?"
            LINE 18 COLUMN 25.
            ACCEPT ENO LINE 18 COLUMN 54.
            IF ENO = 'N' OR 'n' MOVE 1 TO EOFSW.
            IF ENO = 'Y' OR 'y' MOVE 0 TO EOFSW.

