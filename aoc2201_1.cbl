       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC2201_1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT inputfile
           ASSIGN TO "aoc2201_in.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION. 
       FD inputfile RECORD CONTAINS 80 CHARACTERS.
       01 inputline PIC X(80).
       WORKING-STORAGE SECTION.
       01 max PIC 9(10).
       01 cur PIC 9(10).
       01 eof PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN SECTION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof > 0
              READ inputfile AT END MOVE 1 TO eof NOT AT END
              IF inputline NOT > SPACES
                 IF max < cur THEN
                    MOVE cur TO max
                    MOVE 0 TO cur
                 ELSE
                    MOVE 0 TO cur
                 END-IF
              ELSE 
                 COMPUTE cur = cur + FUNCTION NUMVAL(inputline)
              END-IF
           END-PERFORM.
           DISPLAY max.
           CLOSE inputfile.
           STOP RUN.
       END PROGRAM AOC2201_1.
