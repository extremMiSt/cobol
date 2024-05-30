       IDENTIFICATION DIVISION.
       PROGRAM-ID. AOC2203_1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT inputfile
           ASSIGN TO "aoc2203_in.txt"
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION. 
       FD inputfile RECORD IS VARYING FROM 1 TO 80 CHARACTERS
           DEPENDING ON len.
       01 inputline PIC X(80).
       WORKING-STORAGE SECTION.
       01 eof PIC 9 VALUE 0.
       01 len PIC 999.
       PROCEDURE DIVISION.
       MAIN SECTION.
           OPEN INPUT inputfile.
           PERFORM UNTIL eof > 0
              READ inputfile AT END MOVE 1 TO eof NOT AT END
                 DISPLAY inputline " " len
                 UNSTRING inputline COUNT IN 
              END-READ
           END-PERFORM.
           CLOSE inputfile.
           STOP RUN.
       END PROGRAM AOC2203_1.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. StrLen.
       DATA DIVISION.
       LINKAGE SECTION. 
       01 str PIC X(80).
       01 len PIC 999 VALUE 0.
       PROCEDURE DIVISION USING str, len.
           move function reverse(str) to str.
           inspect str
              tallying len
              for leading space.
           subtract len
              from length of str
              giving len.
           move function reverse(str) to str.
           GOBACK.
       END PROGRAM StrLen.
