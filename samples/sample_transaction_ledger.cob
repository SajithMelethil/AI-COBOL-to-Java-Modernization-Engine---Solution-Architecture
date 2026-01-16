       IDENTIFICATION DIVISION.
       PROGRAM-ID. TRANSLEDG.
       AUTHOR. BANKING-SYSTEMS.
       DATE-WRITTEN. 2024-01-15.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TRANSACTION-RECORD.
           05  TRX-DATE        PIC 9(8).
           05  TRX-TYPE        PIC X(2).
               88  VALID-TRX   VALUES 'CR', 'DR', 'TF', 'AD'.
           05  TRX-AMOUNT      PIC S9(9)V99.
           05  TRX-ACCOUNT     PIC X(20).
           05  TRX-DESCRIPTION PIC X(50).

       01  LEDGER-SUMMARY.
           05  TOTAL-CREDITS   PIC S9(9)V99 VALUE ZEROS.
           05  TOTAL-DEBITS    PIC S9(9)V99 VALUE ZEROS.
           05  NET-BALANCE     PIC S9(9)V99 VALUE ZEROS.
           05  TRX-COUNT       PIC 9(5)     VALUE ZEROS.

       01  CURRENT-BALANCE     PIC S9(9)V99 VALUE ZEROS.

       PROCEDURE DIVISION USING TRANSACTION-RECORD.
       PROCESS-TRANSACTION.
           IF NOT VALID-TRX
               DISPLAY 'ERROR: INVALID TRANSACTION TYPE: ' 
                       TRX-TYPE
               EXIT PROGRAM
           END-IF.
           
           PERFORM VALIDATE-TRANSACTION.
           
           IF TRX-COUNT = 0
               MOVE 50000.00 TO CURRENT-BALANCE
           END-IF.
           
           EVALUATE TRX-TYPE
               WHEN 'CR'
                   PERFORM PROCESS-CREDIT
               WHEN 'DR'
                   PERFORM PROCESS-DEBIT
               WHEN 'TF'
                   PERFORM PROCESS-TRANSFER
               WHEN 'AD'
                   PERFORM PROCESS-ADJUSTMENT
           END-EVALUATE.
           
           ADD 1 TO TRX-COUNT.
           PERFORM UPDATE-LEDGER.
           
           DISPLAY 'TRANSACTION PROCESSED: ' TRX-TYPE 
                   ' AMOUNT: ' TRX-AMOUNT
                   ' NEW BALANCE: ' CURRENT-BALANCE.

       VALIDATE-TRANSACTION.
           IF TRX-AMOUNT <= 0
               DISPLAY 'ERROR: INVALID AMOUNT: ' TRX-AMOUNT
               EXIT PROGRAM
           END-IF.
           
           IF TRX-ACCOUNT = SPACES
               DISPLAY 'ERROR: ACCOUNT NUMBER REQUIRED'
               EXIT PROGRAM
           END-IF.

       PROCESS-CREDIT.
           ADD TRX-AMOUNT TO CURRENT-BALANCE.
           ADD TRX-AMOUNT TO TOTAL-CREDITS.

       PROCESS-DEBIT.
           IF TRX-AMOUNT > CURRENT-BALANCE
               DISPLAY 'ERROR: INSUFFICIENT FUNDS'
               EXIT PROGRAM
           END-IF.
           
           SUBTRACT TRX-AMOUNT FROM CURRENT-BALANCE.
           ADD TRX-AMOUNT TO TOTAL-DEBITS.

       PROCESS-TRANSFER.
           DISPLAY 'TRANSFER TO ACCOUNT: ' TRX-ACCOUNT.
           SUBTRACT TRX-AMOUNT FROM CURRENT-BALANCE.
           ADD TRX-AMOUNT TO TOTAL-DEBITS.

       PROCESS-ADJUSTMENT.
           DISPLAY 'ADJUSTMENT TRANSACTION'.
           COMPUTE CURRENT-BALANCE = 
               CURRENT-BALANCE + TRX-AMOUNT.
           IF TRX-AMOUNT > 0
               ADD TRX-AMOUNT TO TOTAL-CREDITS
           ELSE
               ADD TRX-AMOUNT TO TOTAL-DEBITS
           END-IF.

       UPDATE-LEDGER.
           COMPUTE NET-BALANCE = 
               TOTAL-CREDITS - TOTAL-DEBITS.