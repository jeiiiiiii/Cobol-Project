       IDENTIFICATION DIVISION.
       PROGRAM-ID. BROW.
      *AUTHOR. IT22N jei mond.
      *INSTALLATION. PUP-MANILA.
      *DATE-WRITTEN. 02/12/24.
      *DATE-COMPILED. 02/12/24.
      *SECURITY. ONLY ME.
      *REMARKS. HANDSON PRACTIVCE. 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LAPTOP-GUPS4DA8.
       OBJECT-COMPUTER. LAPTOP-GUPS4DA8.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT OUTFILE ASSIGN TO "out".
       DATA DIVISION.
       FILE SECTION.
       FD   OUTFILE
            LABEL RECORD IS OMITTED
            DATA RECORD IS OUTREC.
       01   OUTREC.
            02 D-ANO PIC X(10).
            02 D-CNA PIC X(25).
            02 D-ATY PIC X(10).
            02 D-KWH PIC Z(6).
            02 D-SCH PIC Z,ZZZ.ZZ.
            02 D-TAB PIC ZZ,ZZZ.ZZ.
       WORKING-STORAGE SECTION.
       01   EOFSW PIC 9 VALUE 0.
       01   ENO PIC X VALUE SPACES.
       01   ANO PIC X(10) VALUE SPACES.
       01   CNA PIC X(25) VALUE SPACES.
       01   HIGH-CNA PIC X(25) VALUE SPACES.
       01   PRR PIC 9(6) VALUE 0.
       01   CRR PIC 9(6) VALUE 0.
       01   KWH PIC 9(6).
       01   HIGH-KWH PIC 9(6) VALUE 0.
       01   ACC PIC A VALUE SPACES.
       01   ATY PIC X(11).
       01   PRC PIC 9(2) VALUE 0.
       01   ARC PIC 9 VALUE 0.
       01   SCH PIC 9(4)V99.
       01   TAB PIC ZZ,ZZZ.ZZ.
       01   ELB PIC 9(6)V99 VALUE 0.
       SCREEN SECTION.
       01   SCRE.
            02 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
            OPEN OUTPUT OUTFILE.
            PERFORM HEADING-RTN.
            PERFORM PROCESS-RTN THRU PROCESS-RTN-END UNTIL EOFSW = 1.
            STOP RUN.
       HEADING-RTN.
            
       PROCESS-RTN.
            DISPLAY SCRE.
            MOVE SPACES TO ACC
            DISPLAY "BROWNOUT ELECTRIC COMPANY" LINE 1 COLUMN 19.
            DISPLAY "BILLING REPORT" LINE 2 COLUMN 19.
            
            DISPLAY "ACCOUTNT NUMBER:" LINE 4 COLUMN 1.
            ACCEPT ANO LINE 4 COLUMN 35.

            DISPLAY "CUSTOMER NAME:" LINE 5 COLUMN 1.
            ACCEPT CNA LINE 5 COLUMN 35.

            DISPLAY "PREVIOUS READING:" LINE 6 COLUMN 1.
            ACCEPT PRR LINE 6 COLUMN 35.

            DISPLAY "CURRENT READING:" LINE 7 COLUMN 1.
            ACCEPT CRR LINE 7 COLUMN 35.

            PERFORM KWH-RTN.
            IF KWH > HIGH-KWH
                 MOVE KWH TO HIGH-KWH
                 MOVE CNA TO HIGH-CNA.

            PERFORM ACC-RTN UNTIL
            ACC = 'R' OR ACC = 'r' OR
            ACC = 'C' OR ACC = 'c' OR
            ACC = 'I' OR ACC = 'i'.
            DISPLAY "ACCOUNT TYPE:" LINE 10 COLUMN 1.
            DISPLAY ATY LINE 10 COLUMN 35.

           DISPLAY "ELECTRIC BILL:" LINE 11 COLUMN 1.
           DISPLAY ELB LINE 10 COLUMN 35.

            PERFORM ARC-RTN UNTIL ARC > 0 AND ARC < 4.
            DISPLAY "SYSTEM CHARGES:" LINE 12 COLUMN 1.
            DISPLAY SCH LINE 12 COLUMN 35.
            
            COMPUTE TAB = ELB + SCH.
            DISPLAY "TOTAL BILL:" LINE 13 COLUMN 1.
            DISPLAY TAB LINE 13 COLUMN 35.

            PERFORM ANOTHER-RTN UNTIL ENO = 'y' OR ENO = 'Y'
            OR ENO = 'n' OR ENO = 'N'.
       PROCESS-RTN-END.
       KWH-RTN.
            COMPUTE KWH = CRR - PRR.
            DISPLAY "KWH USED:" LINE 8 COLUMN 1.
            DISPLAY KWH LINE 8 COLUMN 35.
       ACC-RTN.
            DISPLAY "ACCOUNT CODE:" LINE 9 COLUMN 1.
            ACCEPT ACC LINE 9 COLUMN 35.
            IF ACC = 'r' OR 'R' 
                 MOVE "RESIDENTIAL" TO ATY
                 MOVE 14 TO PRC.
            IF ACC = 'c' OR 'C'
                 MOVE "COMMERCIAL" TO ATY
                 MOVE 28 TO PRC.
            IF ACC = 'i' OR 'I'
                 MOVE "INDUSTRIAL" TO ATY
                 MOVE 42 TO PRC.
            COMPUTE ELB = KWH * PRC.


       ANOTHER-RTN.
            DISPLAY "INPUT ANOTHER RECORD (Y/N)?"
            LINE 15 COLUMN 1.
            ACCEPT ENO LINE 15 COLUMN 35.
            IF ENO = 'N' OR ENO = 'n' MOVE 1 TO EOFSW.


                   ARC-RTN.
            DISPLAY "AREA CODE:" LINE 11 COLUMN 1.
            ACCEPT ARC LINE 11 COLUMN 35.
            IF ARC = 1
                 COMPUTE SCH = ELB * 0.3.
            IF ARC = 2
                 COMPUTE SCH = ELB * 0.5.
            IF ARC = 3
                 COMPUTE SCH = ELB * 0.7.
            COMPUTE ELB = KWH * PRC.