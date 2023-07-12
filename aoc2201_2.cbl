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
       01 max1 PIC 9(10) VALUE 0.
       01 max2 PIC 9(10) VALUE 0.
       01 max3 PIC 9(10) VALUE 0.
       01 fin PIC 9(10).
       01 cur PIC 9(10) VALUE 0.
       01 eof PIC 9 VALUE 0.
       PROCEDURE DIVISION.
       MAIN SECTION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof > 0
              READ inputfile AT END MOVE 1 TO eof NOT AT END
              IF inputline NOT > SPACES
                 IF max1 < cur THEN
                    MOVE max2 TO max3
                    MOVE max1 TO max2
                    MOVE cur TO max1
                 ELSE 
                    IF max2 < cur THEN
                    MOVE max2 TO max3 
                    MOVE cur TO max2
                    ELSE 
                       IF max3 < cur THEN
                       MOVE cur TO max3
                       END-IF
                    END-IF
                 END-IF
                 MOVE 0 TO cur
              ELSE 
                 COMPUTE cur = cur + FUNCTION NUMVAL(inputline)
              END-IF
           END-PERFORM.
           COMPUTE fin = max1+max2+max3.
           DISPLAY fin.
           CLOSE inputfile.
           STOP RUN.
       END PROGRAM AOC2201_1.
