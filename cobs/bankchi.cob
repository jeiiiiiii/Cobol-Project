
        Identification Division.

        
        Program-ID. Problem3.
        Environment Division.
        Input-Output Section.
        File-Control.
            Select Outfile Assign to "bankchi".
        Data Division.
        File Section.
        FD Outfile.
        01 Outrec.
           05 Filler Pic X(80).

        Working-Storage Section.
        01 Rec-out.
           05 AccNo-Out Pic 9(10).
           05 Filler Pic X(2) Value Spaces.
           05 AccName-Out Pic X(20).
           05 Filler Pic X(2) Value Spaces.
           05 TransactionName-Out Pic X(12).
           05 Filler Pic X(3) Value Spaces.
           05 AccountTypeName-Out Pic X(16).
           05 Filler Pic X(1) Value Spaces.
           05 Balance-Out Pic ZZZ,ZZZ,ZZ9.99.
           05 Filler Pic X(2) Value Spaces.
        01 Heading-1.
           05 Filler Pic X(32) Value Spaces.
           05 Filler Pic X(16) Value "Chika Trust Bank".
           05 Filler Pic X(32) Value Spaces.
        01 Heading-2.
           05 Filler Pic X(34) Value Spaces.
           05 Filler Pic X(11) Value "Makati City".
           05 Filler Pic X(35) Value Spaces.
        01 Heading-3.
           05 Filler Pic X(31) Value Spaces.
           05 Filler Pic X(18) Value "Customer's Account".
           05 Filler Pic X(31) Value Spaces.
        01 Sub-1.
           05 Filler Pic X(1) Value Spaces.
           05 Filler Pic X(7) Value "Account".
           05 Filler Pic X(10) Value Spaces.
           05 Filler Pic X(7) Value "Account".
           05 Filler Pic X(9) Value Spaces.
           05 Filler Pic X(11) Value "Transaction".
           05 Filler Pic X(4) Value Spaces.
           05 Filler Pic X(12) Value "Account Type".
           05 Filler Pic X(8) Value Spaces.
           05 Filler Pic X(7) Value "Balance".
           05 Filler Pic X(4) Value Spaces.
        01 Sub-2.
           05 Filler Pic X(3) Value Spaces.
           05 Filler Pic X(7) Value "No.".
           05 Filler Pic X(13) Value Spaces.
           05 Filler Pic X(4) Value "Name".
           05 Filler Pic X(14) Value Spaces.
           05 Filler Pic X(4) Value "Name".
           05 Filler Pic X(11) Value Spaces.
           05 Filler Pic X(4) Value "Name".
           05 Filler Pic X(24) Value Spaces.

        01 Initialization.
           05 AccNo-In Pic 9(10) Value 0.
           05 AccName-In Pic X(20).
           05 GenderCode Pic x.
           05 GenderName pic  X(6).
           05 TransactionType Pic X.
           05 TransactionName-In Pic X(10).
           05 Amount Pic 9(7)v99 Value 0.
           05 AccountType Pic X.
           05 AccountTypeName-In Pic X(16).
           05 Balance-In Pic 9(9)v99 Value 0.
           05 InitialDeposit Pic 9(7)v99 Value 0.
           05 BranchCode Pic X(3).
           05 BranchName Pic x(11).
           05 Ans Pic X.
           05 Valid Pic 9 Value 0.
           05 Validans Pic 9 Value 0.
        
         Screen Section.
         01 CLRSCR.
            05 Blank Screen.

         Procedure Division.

         Main-Rtn.
            Display CLRSCR.
            Open Output Outfile.
            Perform Heading-Rtn thru Heading-end.
            Perform Process-Rtn thru Process-End
               until Validans = 1.
            Stop Run.

         Heading-Rtn.
            Write Outrec from Heading-1.
            Write Outrec from Heading-2 after advancing 1 line.
            Write Outrec from Heading-3 after advancing 1 line.
            Write Outrec from Sub-1 after advancing 3 lines.
            Write Outrec from Sub-2 after advancing 1 line.
         Heading-End.

         Process-Rtn.
            Display CLRSCR.
            Display (1 , 32) "China Trust Bank".
            Display (2 , 35) "Makati City".
            Display (3 , 31) "Customer's Account".
                                                                                                
            Display (5 , 1) "Account Number: ".
            Accept (5 , 30) AccNo-In.
            Move AccNo-In to AccNo-Out.
            Display(5 , 30) AccNo-Out.

            Display (6 , 1) "Account Name: ".
            Accept(6 , 30) AccName-In.
            Move AccName-In to AccName-Out.
            Display (6, 30) AccName-Out.

            Display (7 , 1) "Gender Code:  ".
            Perform Gender-Rtn.
            Move 0 to Valid.

            Display (8 , 1) "Gender Name:  ".
            Display (8 , 30) GenderName.

            Display (9 , 1) "Transaction Type: ". 
            Perform Transaction-Rtn.
            Move 0 to Valid.

            Display (10 , 1) "Transaction Name: ". 
            Display (10 , 30) TransactionName-Out.

            Display (11 , 1) "Amount: ".
            Accept (11 , 30) Amount.   

            Display (12  , 1) "Account Type: ".
            Perform AccType-Rtn.
            Move 0 to Valid.

            Display (13 , 1) "Account Type Name: ".
            Display ( 13 , 30 ) AccountTypeName-Out.

            Display (14 , 1) "Initial Deposit: ".
            Accept ( 14 , 30 ) InitialDeposit.

             If TransactionType = 'D'
                Compute Balance-In = Amount + InitialDeposit
                Move Balance-In to Balance-Out
             Else
             If TransactionType = 'W'
                Compute Balance-In = Amount - InitialDeposit
                Move Balance-In to Balance-Out
           End-If.

            Display (15 , 1) "Balance: ".
            Display ( 15 , 30 ) Balance-Out.

            Display ( 16 , 1) "Branch Code: "
            Perform Branch-Rtn.

            Display ( 17 , 1) "Branch Code: "
            Display ( 17 , 30) BranchName.

            Write Outrec from  Rec-out after advancing 2 lines.
            Display (19 , 27) "Input Another Record (Y/N)?".
            Perform Ans-Rtn Thru Ans-End.
         Process-End.
                                                                                                                    
         Gender-Rtn.
            Accept (7 , 30) GenderCode.
            If GenderCode = 'M' or GenderCode = 'm'
               Move "Male  " to GenderName
               Move 1 to Valid
            Else
            If GenderCode = 'F' or GenderCode = 'f'
               Move "Female" to GenderName
               Move 1 to Valid
            Else
              Perform Gender-Rtn Thru Gender-End until Valid = 1
             End-If.
         Gender-End.

         Transaction-Rtn.
            Accept (9 , 30) TransactionType.
            If TransactionType = 'D' or TransactionType = 'd'
               Move "Deposit   " to TransactionName-In
               Move TransactionName-In to TransactionName-Out
               Move 1 to Valid
            Else
            If TransactionType = 'W' or TransactionType = 'w'
               Move "Withdrawal" to TransactionName-In
               Move TransactionName-In to TransactionName-Out
               Move 1 to Valid
            Else
               Perform Transaction-Rtn Thru Transaction-End 
                     until Valid = 1
             End-If.
         Transaction-End.

         AccType-Rtn.
            Accept ( 12 , 30) AccountType.
            If AccountType = 'S' or AccountType = 's'
               Move "Savings Deposit" to AccountTypeName-In
               Move AccountTypeName-In to AccountTypeName-Out
               Move 1 to Valid
            Else
            If AccountType = 'C' or AccountType = 'c'
               Move "Checking Account" to AccountTypeName-In
               Move AccountTypeName-In to AccountTypeName-Out
               Move 1 to Valid
            Else
            If AccountType = 'D' or AccountType = 'd'
               Move "Dollar Account  " to AccountTypeName-In
               Move AccountTypeName-In to AccountTypeName-Out
               Move 1 to Valid
            Else
               Perform AccType-Rtn Thru AccType-End
                         until Valid = 1
               End-If.
         AccType-End.


       Branch-Rtn.
            Accept(16 , 30) BranchCode.
            If BranchCode = 'PAR'  or BranchCode = 'par'
               Move "Paranaque  " to BranchName
               Move 1 to Valid
            Else
            If BranchCode = 'PAS' or BranchCode = 'pas'
               Move "Pasay      " to BranchName
               Move 1 to Valid
            Else
            If BranchCode = 'MAN' or BranchCode = 'man'
               Move "Mandaluyong" to BranchName
               Move 1 to Valid
            Else
            If BranchCode = 'SME' or BranchCode = 'sme'
               Move "Sta.Mesa   " to BranchName
               Move 1 to Valid
            Else
            If BranchCode = 'SJA' or BranchCode = 'sja'
               Move "San Juan   " to BranchName
               Move 1 to Valid
            Else
               Perform Branch-Rtn thru Branch-End until Valid = 1
             End-If.
       Branch-End.

        Ans-Rtn.
                Accept (20, 40) Ans.
                If ans = 'Y' or ANS = 'y'
                Perform Process-Rtn thru Process-End
                Else If Ans = 'N' or  ANS = 'n'
                        Move 1 to Validans
                Else
                        Perform Ans-Rtn Thru Ans-End.
        Ans-End.        