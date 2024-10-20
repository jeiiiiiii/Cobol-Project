IDENTIFICATION DIVISION.
        PROGRAM-ID. FACULTY.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
                SELECT OUTFILE ASSIGN TO "D:\COBOL\FACULTY.TXT".
        DATA DIVISION.
        FILE SECTION.
        FD OUTFILE.
        01 OUTREC.
                05 FILLER PIC X(80).
        WORKING-STORAGE SECTION.
        01 REC-OUT.
                05 FNO-OUT PIC 9(10).
                05 FNAME PIC X(25).
                05 FTNAME PIC X(20).
                05 DNAME PIC X(22).
                05 HEAN PIC X(15).
                05 UA-OUT PIC Z9.
                05 LPE-OUT PIC X(17).
        01 TOTALS.
                05 PCTR PIC 999 VALUE 0.
                05 TRCTR PIC 999 VALUE 0.
                05 PTCTR PIC 999 VALUE 0.
                05 PCTR-OUT PIC ZZ9.
                05 TRCTR-OUT PIC ZZ9.
                05 PTCTR-OUT PIC ZZ9.
        01 INITIALIZATION.
                05 FNO PIC 9(10) VALUE 0.
                05 FTYPE PIC X(2).
                05 DCODE PIC X(2).
                05 GCODE PIC X.
                05 GNAME PIC X(6).
                05 HEAC PIC X(2).
                05 UA PIC 9(2) VALUE 0.
                05 LPE PIC X(2).
                05 ANS PIC X.
                05 VALID PIC 9 VALUE 0.
                05 VALIDANS PIC 9 VALUE 0.
        SCREEN SECTION.
        01 CLRSCR.
                05 BLANK SCREEN.
        PROCEDURE DIVISION.
        MAIN-RTN.
                DISPLAY CLRSCR.
                OPEN OUTPUT OUTFILE.
                PERFORM PROCESS-RTN THRU PROCESS-END
                    UNTIL VALIDANS = 1.
                PERFORM FINISH-RTN THRU FINISH-END.
                STOP RUN.                         
        PROCESS-RTN.
                DISPLAY CLRSCR.
                DISPLAY (1 , 25) "COLLEGE OF COMPUTER MANAGEMENT".
                DISPLAY (2 , 27) "AND INFORMATION TECHNOLOGY".
                DISPLAY (5 , 32) "FACULTY PROFILE".
                MOVE 0 TO VALID.
                DISPLAY (7 , 1) "FACULTY NO: ".
                ACCEPT (7 , 40) FNO.
                MOVE FNO TO FNO-OUT.
                DISPLAY (7 , 40) FNO-OUT.
                DISPLAY (8 , 1) "FACULTY NAME: ".
                ACCEPT (8 , 40) FNAME.
                DISPLAY (9 , 1) "FACULTY TYPE: ".
                PERFORM F-RTN.
                MOVE 0 TO VALID.
                DISPLAY (10 , 1) "FACULTY TYPE NAME: ".
                DISPLAY (10 , 40) FTNAME.
                DISPLAY (11 , 1) "DEPARTMENT CODE: ".
                PERFORM D-RTN.
                MOVE 0 TO VALID.
                DISPLAY (12 , 1) "DEPARTMENT NAME: ".
                DISPLAY (12 , 40) DNAME.
                DISPLAY (13 , 1) "GENDER CODE: ".
                PERFORM G-RTN.
                MOVE 0 TO VALID.
                DISPLAY (14 , 1) "GENDER NAME: " .
                DISPLAY (14 , 40) GNAME.
                DISPLAY (15 , 1)
                        "HIGHEST EDUCATIONAL ATTAINMENT CODE: ".
                PERFORM H-RTN.
                MOVE 0 TO VALID.      
                DISPLAY (17 , 1) "LATEST PERFORMANCE EVALUATION: ".
                PERFORM P-RTN.
                WRITE OUTREC FROM REC-OUT.
                DISPLAY (19 , 27) "INPUT ANOTHER RECORD(Y/N)?".
                PERFORM ANS-RTN THRU ANS-END.
        PROCESS-END.
        F-RTN.
                ACCEPT (9 , 40) FTYPE.
                IF FTYPE = 'PE'
                        MOVE "PERMANENT" TO FTNAME
                        ADD 1 TO PCTR
                        MOVE 1 TO VALID
                ELSE
                IF FTYPE = 'TR'
                        MOVE "TEMPORARY REGULAR" TO FTNAME
                        ADD 1 TO TRCTR
                        MOVE 1 TO VALID
                ELSE
                IF FTYPE = 'PT'
                        MOVE "PART TIMER" TO FTNAME
                        ADD 1 TO PTCTR
                        MOVE 1 TO VALID
                ELSE
                IF VALID = 0
                        PERFORM F-RTN THRU F-END UNTIL VALID = 1.
        F-END.
        D-RTN.
                ACCEPT (11 , 40) DCODE.
                IF DCODE = 'CS'
                        MOVE "COMPUTER SCIENCE" TO DNAME
                        MOVE 1 TO VALID
                ELSE
                IF DCODE = 'IT'
                        MOVE "INFORMATION TECHNOLOGY" TO DNAME
                        MOVE 1 TO VALID
                ELSE
                IF VALID = 0
                        PERFORM D-RTN THRU D-END UNTIL VALID = 1.
        D-END.
        G-RTN.
                ACCEPT (13 , 40) GCODE.
                IF GCODE = 'M'
                        MOVE "MALE" TO GNAME
                        MOVE 1 TO VALID
                ELSE
                IF GCODE = 'F'
                        MOVE "FEMALE" TO GNAME
                        MOVE 1 TO VALID
                ELSE
                IF VALID = 0
                        PERFORM G-RTN THRU G-END UNTIL VALID = 1.
        G-END.
        H-RTN.
                ACCEPT (15 , 40) HEAC.
                IF HEAC = 'DD'
                        MOVE "DOCTORAL DEGREE" TO HEAN
                        MOVE 1 TO VALID
                        DISPLAY (16 , 1) "NO OF UNITS TO BE ASSIGNED: "
                        IF FTYPE = 'PT'
                                PERFORM BD-RTN
                        ELSE
                                PERFORM DD-RTN
                ELSE
                IF HEAC = 'MD'
                        MOVE "MASTER DEGREE" TO HEAN
                        MOVE 1 TO VALID
                        DISPLAY (16 , 1) "NO OF UNITS TO BE ASSIGNED: "
                        IF FTYPE = 'PT'
                                PERFORM BD-RTN
                        ELSE
                                PERFORM MD-RTN
                ELSE
                IF HEAC = 'BD'
                        MOVE "BACHELOR DEGREE" TO HEAN
                        MOVE 1 TO VALID
                        DISPLAY (16 , 1) "NO OF UNITS TO BE ASSIGNED: "
                        PERFORM BD-RTN
                ELSE
                IF VALID = 0
                        PERFORM H-RTN THRU H-END UNTIL VALID = 1.
        H-END.
        DD-RTN.
                ACCEPT (16 , 40) UA.       
                IF UA > 42 OR UA < 36
                        PERFORM DD-RTN THRU DD-END UNTIL UA <= 42
                                AND UA >= 36.
        DD-END.
        MD-RTN.
                ACCEPT (16 , 40) UA.
                IF UA > 33  OR UA < 18
                        PERFORM MD-RTN THRU MD-END UNTIL UA <=  33
                                AND UA >= 18.
        MD-END.
        BD-RTN.
                ACCEPT (16 , 40) UA.
                IF UA > 15 OR UA < 3
                        PERFORM BD-RTN THRU BD-END UNTIL UA <= 15
                                AND UA >= 3.
        BD-END.
        P-RTN.
                ACCEPT (17 , 40) LPE.
                IF LPE = 'SA'
                        MOVE "SATISFACTORY" TO LPE-OUT
                        MOVE 1 TO VALID
                ELSE
                IF LPE = 'VS'
                        MOVE "VERY SATISFACTORY" TO LPE-OUT
                        MOVE 1 TO VALID
                ELSE
                IF LPE = 'OT'
                        MOVE "OUTSTANDING" TO LPE-OUT
                        MOVE 1 TO VALID
                ELSE
                IF VALID = 0
                        PERFORM P-RTN THRU P-END UNTIL VALID = 1.
        P-END.

        ANS-RTN.
                ACCEPT (19, 55) ANS.
                IF ANS = 'Y' OR ANS = 'y'
                        PERFORM PROCESS-RTN THRU PROCESS-END
                ELSE IF ANS = 'N' OR ANS = 'n'
                        MOVE 1 TO VALIDANS
                ELSE
                        PERFORM ANS-RTN THRU ANS-END.
        ANS-END.
        FINISH-RTN.
                MOVE PCTR TO PCTR-OUT.
                MOVE TRCTR TO TRCTR-OUT.
                MOVE PTCTR TO PTCTR-OUT.
                DISPLAY (21, 7) "TOTAL NUMBER OF PERMANENT:", PCTR-OUT.
                DISPLAY (22, 7) "TOTAL NUMBER OF TEMPO:", TRCTR-OUT.
                DISPLAY (23, 7) "TOTAL NUMBER OF PART TIME:", PTCTR-OUT.
                CLOSE OUTFILE.                                                                
        FINISH-END.

