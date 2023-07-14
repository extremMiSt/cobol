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
       01 res PIC X.
       01 resPoint PIC 9.
       01 elfVal PIc 9.
       01 you PIC 9.
       01 score PIC 9(10).
       01 eof PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN SECTION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof > 0
              READ inputfile AT END MOVE 1 TO eof NOT AT END
              UNSTRING inputline DELIMITED BY SPACE INTO elf, res
              CALL "CharVal" USING elf, elfVal 
              CALL "ResPoint" USING res, resPoint 
              CALL "MakeWin" USING elfVal, resPoint, you 
              DISPLAY elf " " res " " you
              COMPUTE score = score + you + resPoint
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
       PROGRAM-ID. ResPoint.
       DATA DIVISION.
       LINKAGE SECTION. 
       01 char PIC X.
       01 ret PIC 9.
       PROCEDURE DIVISION USING char, ret.
       MAIN SECTION.
           EVALUATE char
              WHEN "X" MOVE 0 TO ret 
              WHEN "Y" MOVE 3 TO ret 
              WHEN "Z" MOVE 6 TO ret
           END-EVALUATE.
           GOBACK.
       END PROGRAM ResPoint.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. MakeWin.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION. 
       01 elf PIC 9.
       01 res PIC 9.
       01 you PIC 9.
       PROCEDURE DIVISION USING elf, res, you.
       MAIN SECTION.
           IF res=3 THEN 
              MOVE elf TO you
              GOBACK
           END-IF.
           IF res = 0 THEN 
              SUBTRACT 1 FROM elf GIVING you
              IF you = 0 THEN
                 MOVE 3 TO you
              END-IF
              GOBACK
           END-IF.
           IF res = 6 THEN 
              ADD 1 TO elf GIVING you
              IF you = 4 THEN
                 MOVE 1 TO you
              END-IF
              GOBACK
           END-IF.
           GOBACK.
       END PROGRAM MakeWin.


