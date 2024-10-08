       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRACTICE-7.
      *AUTHOR. IT22N REIN FURAGGANAN.
      *INSTALLATION. PUP-MANILA.
      *DATE-WRITTEN. 02/11/24.
      *DATE-COMPILED. 02/11/24.
      *SECURITY. ONLY ME.
      *REMARKS. PRACTICE SET 3. 
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. LAPTOP-GUPS4DA8.
       OBJECT-COMPUTER. LAPTOP-GUPS4DA8.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
            SELECT OUTFILE ASSIGN TO "12OUTPUT.TXT".
       DATA DIVISION.
       FILE SECTION.
       FD   OUTFILE
            LABEL RECORD IS OMITTED
            DATA RECORD IS OUTREC.
       01   OUTREC.
            02 FILLER PIC X(80).
       WORKING-STORAGE SECTION.
       01   EOFSW PIC 9 VALUE 0.
       01   ANO PIC X VALUE SPACES.
       01   ENO PIC 9(10) VALUE 0.
       01   ENA PIC X(25) VALUE SPACES.
       01   DOB PIC X(20) VALUE SPACES.
       01   UNC PIC 9 VALUE 0.
       01   UNA PIC X(5) VALUE SPACES.
       01   CRC PIC 9 VALUE 0.
       01   CNA PIC X(4) VALUE SPACES.
       01   TNI PIC 9(3) VALUE 0.
       01   TRS PIC 99 VALUE 0.
       01   DEC PIC 9V99 VALUE 0.
       01   RMK PIC X(6) VALUE SPACES.
       01   TNP PIC 99 VALUE 0.
       01   TNF PIC 99 VALUE 0.
       01   SUB-1.
            02 FILLER PIC X(8) VALUE "EXAMINEE".
            02 FILLER PIC X(2) VALUE SPACES.
            02 FILLER PIC X(8) VALUE "EXAMINEE".
            02 FILLER PIC X(17) VALUE SPACES.
            02 FILLER PIC X(9) VALUE "BIRTHDATE".
            02 FILLER PIC X(11) VALUE SPACES.
            02 FILLER PIC X(10) VALUE "UNIVERSITY".
            02 FILLER PIC X(1) VALUE SPACES.
            02 FILLER PIC X(6) VALUE "COURSE".
            02 FILLER PIC X(1) VALUE SPACES.
            02 FILLER PIC X(7) VALUE "REMARKS".
       01   SUB-2.
            02 FILLER PIC X(6) VALUE "NUMBER".
            02 FILLER PIC X(4) VALUE SPACES.
            02 FILLER PIC X(4) VALUE "NAME".
            02 FILLER PIC X(41) VALUE SPACES.
            02 FILLER PIC X(4) VALUE "NAME".
            02 FILLER PIC X(7) VALUE SPACES.
            02 FILLER PIC X(4) VALUE "NAME".
            02 FILLER PIC X(10) VALUE SPACES.
       01   DETAILS.
            02 P-ENO PIC 9(10).
            02 P-ENA PIC X(25).
            02 P-DOB PIC X(20).
            02 P-UNA PIC X(5).
            02 FILLER PIC X(6) VALUE SPACES.
            02 P-CNA PIC X(4).
            02 FILLER PIC X(3) VALUE SPACES.
            02 P-RMK PIC X(6).
            02 FILLER PIC X(1) VALUE SPACES.
       SCREEN SECTION.
       01   SCRE.
            02 BLANK SCREEN.
       PROCEDURE DIVISION.
       MAIN-RTN.
            OPEN OUTPUT OUTFILE.
            PERFORM HEADING-RTN.
            PERFORM ACCEPT-RTN THRU ACCEPT-RTN-END UNTIL EOFSW = 1.
            PERFORM FINISH-RTN.
            STOP RUN.
       HEADING-RTN.
            WRITE OUTREC FROM SUB-1 AFTER PAGE.
            WRITE OUTREC FROM SUB-2 AFTER 1.
       ACCEPT-RTN.
            DISPLAY SCRE.
            MOVE 0 TO UNC.
            MOVE 0 TO CRC.
            MOVE SPACES TO RMK.
            MOVE SPACES TO ANO.

            DISPLAY "PROFESSIONAL REGULATION COMMISSION"
            LINE 1 COLUMN 1.
            DISPLAY "IT PROFESSIONAL BOARD EXAM RESULT"
            LINE 2 COLUMN 1.

            DISPLAY "EXAMINEE NUMBER:" LINE 4 COLUMN 1.
            ACCEPT ENO LINE 4 COLUMN 35.

            DISPLAY "EXAMINEE NAME:" LINE 5 COLUMN 1.
            ACCEPT ENA LINE 5 COLUMN 35.

            DISPLAY "DATE OF BIRTH:" LINE 6 COLUMN 1.
            ACCEPT DOB LINE 6 COLUMN 35.

            PERFORM UNI-RTN UNTIL UNC > 0 AND UNC < 6.
            DISPLAY "UNIVERSITY NAME" LINE 8 COLUMN 1.
            DISPLAY UNA LINE 8 COLUMN 35.

            PERFORM CRC-RTN UNTIL CRC > 0 AND CRC < 4.
            DISPLAY "COURSE NAME:" LINE 10 COLUMN 1.
            DISPLAY CNA LINE 10 COLUMN 35.
            
            DISPLAY "TOTAL NO. OF ITEMS:" LINE 11 COLUMN 1.
            ACCEPT TNI LINE 11 COLUMN 35.
            
            PERFORM SCR-RTN.
                 
            PERFORM RMK-RTN.
            DISPLAY "REMARKS:" LINE 13 COLUMN 1.
            DISPLAY RMK LINE 13 COLUMN 35.
            
            MOVE ENO TO P-ENO.
            MOVE ENA TO P-ENA.
            MOVE DOB TO P-DOB.
            MOVE UNA TO P-UNA.
            MOVE CNA TO P-CNA.
            MOVE RMK TO P-RMK.
            WRITE OUTREC FROM DETAILS AFTER 1.

            PERFORM ANOTHER-RTN UNTIL ANO = 'Y' OR ANO = 'y'
            OR ANO = 'N' OR ANO = 'n'.
       ACCEPT-RTN-END.
       UNI-RTN.
            DISPLAY "UNIVERSITY CODE:" LINE 7 COLUMN 1.
            ACCEPT UNC LINE 7 COLUMN 35.
            IF UNC = 1 MOVE "UP" TO UNA.
            IF UNC = 2 MOVE "PUP" TO UNA.
            IF UNC = 3 MOVE "DLSU" TO UNA.
            IF UNC = 4 MOVE "ADMU" TO UNA.
            IF UNC = 5 MOVE "MAPUA" TO UNA.
       CRC-RTN.
            DISPLAY "COURSE CODE:" LINE 9 COLUMN 1.
            ACCEPT CRC LINE 9 COLUMN 35.
            IF CRC = 1 MOVE "BSIT" TO CNA.
            IF CRC = 2 MOVE "BSCS" TO CNA.
            IF CRC = 3 MOVE "BSIS" TO CNA.
       SCR-RTN.
            DISPLAY "TEST RESULT (SCORE):" LINE 12 COLUMN 1.
            ACCEPT TRS LINE 12 COLUMN 35.
       RMK-RTN.
            COMPUTE DEC = TRS / TNI.
            IF (CRC = 1 AND DEC > 0.60) 
            OR (CRC = 2 AND DEC > 0.70)
            OR (CRC = 3 AND DEC > 0.50)
                 MOVE "PASSED" TO RMK
                 ADD 1 TO TNP
            ELSE 
                 MOVE "FAILED" TO RMK
                 ADD 1 TO TNF.
       ANOTHER-RTN.
            DISPLAY "INPUT ANOTHER RECORD (Y/N)?"
            LINE 15 COLUMN 1.
            ACCEPT ANO LINE 15 COLUMN 35.
            IF ANO = 'N' OR ANO = 'n' MOVE 1 TO EOFSW.
       FINISH-RTN.
            DISPLAY "TOTAL NO. OF PASSED:" LINE 17 COLUMN 1.
            DISPLAY TNP LINE 17 COLUMN 35.
            DISPLAY "TOTAL NO. OF FAILED:" LINE 18 COLUMN 1.
            DISPLAY TNF LINE 18 COLUMN 35.
            CLOSE OUTFILE.
            DISPLAY "PROGRAM PROCESSED" LINE 20 COLUMN 31.

       