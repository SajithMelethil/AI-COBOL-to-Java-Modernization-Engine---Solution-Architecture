       IDENTIFICATION DIVISION.
       PROGRAM-ID. ACCTVALID.
       AUTHOR. BANKING-SYSTEMS.
       DATE-WRITTEN. 2024-01-15.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  ACCOUNT-DATA.
           05  ACCOUNT-NUMBER   PIC X(20)   VALUE SPACES.
           05  ACCOUNT-TYPE     PIC X(2)    VALUE SPACES.
               88  VALID-TYPE   VALUES 'SA', 'CA', 'LA', 'CC'.
           05  ACCOUNT-STATUS   PIC X(1)    VALUE SPACE.
               88  ACTIVE-ACCOUNT VALUE 'A'.
               88  CLOSED-ACCOUNT VALUE 'C'.
               88  DORMANT-ACCOUNT VALUE 'D'.
           05  ACCOUNT-BALANCE  PIC S9(9)V99 VALUE ZEROS.
           05  MIN-BALANCE      PIC 9(5)V99  VALUE 1000.00.
           05  OVERDRAFT-LIMIT  PIC 9(5)V99  VALUE 5000.00.

       01  VALIDATION-RESULTS.
           05  IS-VALID-ACCOUNT PIC X(3)    VALUE 'NO '.
               88  VALID-ACCOUNT  VALUE 'YES'.
           05  ERROR-MESSAGE    PIC X(50)   VALUE SPACES.

       PROCEDURE DIVISION USING ACCOUNT-DATA, VALIDATION-RESULTS.
       MAIN-VALIDATION.
           MOVE 'NO ' TO IS-VALID-ACCOUNT.
           MOVE SPACES TO ERROR-MESSAGE.
           
           PERFORM VALIDATE-ACCOUNT-NUMBER.
           IF VALID-ACCOUNT
               PERFORM VALIDATE-ACCOUNT-TYPE
           END-IF.
           
           IF VALID-ACCOUNT
               PERFORM VALIDATE-ACCOUNT-STATUS
           END-IF.
           
           IF VALID-ACCOUNT
               PERFORM VALIDATE-ACCOUNT-BALANCE
           END-IF.
           
           EXIT PROGRAM.

       VALIDATE-ACCOUNT-NUMBER.
           IF ACCOUNT-NUMBER = SPACES
               MOVE 'ACCOUNT NUMBER IS REQUIRED' 
                 TO ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF.
           
           IF ACCOUNT-NUMBER (1:2) NOT NUMERIC
               MOVE 'ACCOUNT NUMBER MUST START WITH DIGITS'
                 TO ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF.
           
           IF FUNCTION LENGTH(ACCOUNT-NUMBER) < 10
               MOVE 'ACCOUNT NUMBER TOO SHORT'
                 TO ERROR-MESSAGE
               EXIT PARAGRAPH
           END-IF.
           
           MOVE 'YES' TO IS-VALID-ACCOUNT.

       VALIDATE-ACCOUNT-TYPE.
           IF NOT VALID-TYPE
               MOVE 'INVALID ACCOUNT TYPE'
                 TO ERROR-MESSAGE
               MOVE 'NO ' TO IS-VALID-ACCOUNT
           END-IF.

       VALIDATE-ACCOUNT-STATUS.
           IF CLOSED-ACCOUNT
               MOVE 'ACCOUNT IS CLOSED'
                 TO ERROR-MESSAGE
               MOVE 'NO ' TO IS-VALID-ACCOUNT
           END-IF.
           
           IF DORMANT-ACCOUNT
               MOVE 'ACCOUNT IS DORMANT'
                 TO ERROR-MESSAGE
               MOVE 'NO ' TO IS-VALID-ACCOUNT
           END-IF.

       VALIDATE-ACCOUNT-BALANCE.
           EVALUATE ACCOUNT-TYPE
               WHEN 'SA'
                   IF ACCOUNT-BALANCE < MIN-BALANCE
                       MOVE 'BELOW MINIMUM BALANCE FOR SAVINGS'
                         TO ERROR-MESSAGE
                       MOVE 'NO ' TO IS-VALID-ACCOUNT
                   END-IF
               WHEN 'CA'
                   IF ACCOUNT-BALANCE < -OVERDRAFT-LIMIT
                       MOVE 'EXCEEDS OVERDRAFT LIMIT'
                         TO ERROR-MESSAGE
                       MOVE 'NO ' TO IS-VALID-ACCOUNT
                   END-IF
               WHEN OTHER
                   CONTINUE
           END-EVALUATE.