       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC2202_1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT inputfile
           ASSIGN TO "aoc2202_in.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION. 
       FD inputfile RECORD CONTAINS 80 CHARACTERS.
       01 inputline PIC X(80).
       WORKING-STORAGE SECTION.
       01 elf PIC X.
       01 you PIC X.
       01 elfVal PIC 9.
       01 youVal PIC 9.
       01 win PIC 9.
       01 score PIC 9(10).
       01 eof PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN SECTION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof > 0
              READ inputfile AT END MOVE 1 TO eof NOT AT END
              UNSTRING inputline DELIMITED BY SPACE INTO elf, you
              CALL "CharVal" USING elf, elfVal 
              CALL "CharVal" USING you, youVal 
              CALL "Win" USING elfVal, youVal, win 
              DISPLAY elf " " you " " win
              COMPUTE score = score + youVal + win 
           END-PERFORM.
           DISPLAY score.
           CLOSE inputfile.
           STOP RUN.
       END PROGRAM AOC2202_1.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. CharVal.
       DATA DIVISION.
       LINKAGE SECTION. 
       01 char PIC X.
       01 ret PIC 9.
       PROCEDURE DIVISION USING char, ret.
       MAIN SECTION.
           EVALUATE char
              WHEN "A" MOVE 1 TO ret 
              WHEN "B" MOVE 2 TO ret
              WHEN "C" MOVE 3 TO ret 
              WHEN "X" MOVE 1 TO ret 
              WHEN "Y" MOVE 2 TO ret 
              WHEN "Z" MOVE 3 TO ret
           END-EVALUATE.
           GOBACK.
       END PROGRAM CharVal.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Win.
       DATA DIVISION.
       LOCAL-STORAGE SECTION. 
       01 elf1 PIC 9.
       LINKAGE SECTION. 
       01 elf PIC 9.
       01 you PIC 9.
       01 ret PIC 9.
       PROCEDURE DIVISION USING elf, you, ret.
       MAIN SECTION.
           IF elf = you THEN 
              MOVE 3 TO ret
              GOBACK
           END-IF.
           ADD 1 TO elf GIVING  elf1.
           IF elf1 > 3 THEN
              COMPUTE elf1 = elf1 - 3
           END-IF.
           IF elf1 = you THEN
              MOVE 6 TO ret 
              GOBACK 
           END-IF.
           MOVE 0 TO ret.
           GOBACK.
       END PROGRAM Win.


