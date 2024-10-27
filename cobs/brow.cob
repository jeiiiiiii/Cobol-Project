       IDENTIFICATION DIVISION.
       PROGRAM-ID. HANDS-ON-FINALS.
      *AUTHOR. IT22N JEI MOND.
      *INSTALLATION. PUP-MANILA.
      *DATE-WRITTEN. 02/12/24.
      *DATE-COMPILED. 02/12/24.
      *SECURITY. ONLY ME.
      *REMARKS. HANDS-ON. 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.
       OBJECT-COMPUTER.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT OUTFILE ASSIGN TO "MONDEJAR".
       DATA DIVISION.
       FILE SECTION.
       FD   OUTFILE
            LABEL RECORD IS OMITTED
            DATA RECORD IS OUTREC.
       01   OUTREC.
            02 D-ANO PIC X(10).
            02 D-CNA PIC X(25).
            02 D-KWH PIC ZZ9.
            02 D-TAB PIC ZZZ,ZZ9.99.
       WORKING-STORAGE SECTION.
       01   EOFSW PIC 9 VALUE 0.
       01   ENO PIC X VALUE SPACES.
       01   ANO PIC X(10) VALUE SPACES.
       01   CNA PIC X(25) VALUE SPACES.
       01   PRR PIC 999 VALUE 0.
       01   CRR PIC 999 VALUE 0.
       01   KWH PIC 999 VALUE 0.
       01   SVKWH PIC ZZ9.
       01   ACC PIC A VALUE SPACES.
       01   ATY PIC X(12).
       01   ELB PIC 9(6)V99 VALUE 0.
       01   SVELB PIC ZZZ,ZZ9V99.
       01   SCH PIC 9(5)V99 VALUE 0.
       01   SVSCH PIC ZZ,ZZ9V99.
       01   TAB PIC 9(6)V99 VALUE 0.
       01   SVTAB PIC ZZZ,ZZ9V99.
       01   PRC PIC 99V99 VALUE 0.
       SCREEN SECTION.
       01   SCRE.
            02 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
            OPEN OUTPUT OUTFILE.
            PERFORM PROCESS-RTN THRU PROCESS-RTN-END UNTIL EOFSW = 1.
            PERFORM FINISH-RTN.
            STOP RUN.
       PROCESS-RTN.
            DISPLAY SCRE.
            MOVE 0 TO KWH.
            MOVE SPACES TO ACC.
            MOVE 0 TO ELB.
            MOVE 0 TO SCH.
            MOVE 0 TO TAB.
            MOVE SPACES TO ENO.
            DISPLAY "BROWNOUT ELECTRIC COMPANY" LINE 1 COLUMN 19.
            DISPLAY "BILLING REPORT" LINE 2 COLUMN 25.
            
            DISPLAY "ACCOUNT NUMBER:" LINE 5 COLUMN 1.
            ACCEPT ANO LINE 5 COLUMN 35.

            DISPLAY "CUSTOMER NAME:" LINE 6 COLUMN 1.
            ACCEPT CNA LINE 6 COLUMN 35.

            DISPLAY "PREVIOUS READING:" LINE 7 COLUMN 1.
            ACCEPT PRR LINE 7 COLUMN 35.

            DISPLAY "CURRENT READING:" LINE 8 COLUMN 1.
            ACCEPT CRR LINE 8 COLUMN 35.

            PERFORM KWH-RTN.

            PERFORM ACC-RTN UNTIL
            ACC = 'R' OR ACC = 'r' OR
            ACC = 'C' OR ACC = 'c' OR
            ACC = 'I' OR ACC = 'i'.
            DISPLAY "ACCOUNT TYPE:" LINE 11 COLUMN 1.
            DISPLAY ATY LINE 11 COLUMN 35.

            DISPLAY "ELECTRIC BILL:" LINE 12 COLUMN 1.
            DISPLAY SVELB LINE 12 COLUMN 35.

            DISPLAY "SYSTEM CHARGES:" LINE 13 COLUMN 1.
            DISPLAY SVSCH LINE 13 COLUMN 35.
            
            COMPUTE TAB = ELB + SCH.
            MOVE TAB TO SVTAB.
            DISPLAY "TOTAL BILL:" LINE 14 COLUMN 1.
            DISPLAY SVTAB LINE 14 COLUMN 35.
            
            MOVE ANO TO D-ANO.
            MOVE CNA TO D-CNA.
            MOVE KWH TO D-KWH.
            MOVE TAB TO D-TAB.
            WRITE OUTREC.

            PERFORM ANOTHER-RTN UNTIL ENO = 'y' OR ENO = 'Y'
            OR ENO = 'n' OR ENO = 'N'.
       PROCESS-RTN-END.
       KWH-RTN.
            COMPUTE KWH = CRR - PRR.
            DISPLAY "KWH USED:" LINE 9 COLUMN 1.
            MOVE KWH TO SVKWH.
            DISPLAY SVKWH LINE 9 COLUMN 35.
       ACC-RTN.
            DISPLAY "ACCOUNT CODE:" LINE 10 COLUMN 1.
            ACCEPT ACC LINE 10 COLUMN 35.
            IF ACC = 'r' OR 'R' 
                 MOVE "RESIDENTIAL" TO ATY
                 MOVE 15.00 TO PRC
                 COMPUTE ELB = KWH * PRC
                 COMPUTE SCH = ELB * 0.03.
            IF ACC = 'c' OR 'C'
                 MOVE "COMMERCIAL" TO ATY
                 MOVE 30.00 TO PRC
                 COMPUTE ELB = KWH * PRC
                 COMPUTE SCH = ELB * 0.06.
            IF ACC = 'i' OR 'I'
                 MOVE "INDUSTRIAL" TO ATY
                 MOVE 45.00 TO PRC
                 COMPUTE ELB = KWH * PRC
                 COMPUTE SCH = ELB * 0.09.
            MOVE ELB TO SVELB.
            MOVE SCH TO SVSCH.
       ANOTHER-RTN.
            DISPLAY "INPUT ANOTHER RECORD (Y/N)?"
            LINE 16 COLUMN 1.
            ACCEPT ENO LINE 16 COLUMN 35.
            IF ENO = 'N' OR ENO = 'n' MOVE 1 TO EOFSW.
       FINISH-RTN.
            CLOSE OUTFILE.

