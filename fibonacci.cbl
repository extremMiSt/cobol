       IDENTIFICATION DIVISION.
       PROGRAM-ID. Fibonacci.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 n PIC 9(3).
       01 ret PIC 9(21).
       PROCEDURE DIVISION.
       MAIN SECTION.
           ACCEPT n
           CALL "FibonacciLoop" USING n, ret
           DISPLAY ret
           CALL "FibonacciRec" USING n, ret
           DISPLAY ret
           STOP RUN.
       END PROGRAM Fibonacci.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FibonacciLoop.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 c PIC 9(3) VALUE 1.
       01 fn PIC 9(21) VALUE 0.
       01 fn1 PIC 9(21) VALUE 1.
       01 fn2 PIC 9(21).
       LINKAGE SECTION.
       01 n PIC 9(3).
       01 ret PIC 9(21).
       PROCEDURE DIVISION USING n,ret.
       MAIN SECTION.
           PERFORM UNTIL c>=n
              ADD fn TO fn1 GIVING fn2
              MOVE fn1 TO fn 
              MOVE fn2 TO fn1
              ADD 1 TO c GIVING c
           END-PERFORM.
           MOVE fn1 to ret.
           GOBACK.
       END PROGRAM FibonacciLoop.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FibonacciRec IS RECURSIVE.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01 nx PIC 9(3).
       01 nxx PIC 9(3).
       01 rx PIC 9(21).
       01 rxx PIC 9(21).
       LINKAGE SECTION.
       01 n PIC 9(3).
       01 ret PIC 9(21).
       PROCEDURE DIVISION USING BY REFERENCE n,ret.
       MAIN SECTION.
           IF n < 1 
              MOVE 0 TO ret
              GOBACK 
           ELSE IF n = 1
              MOVE 1 TO ret 
              GOBACK
           ELSE IF n > 1
              SUBTRACT 1 FROM n GIVING nx
              SUBTRACT 2 FROM n GIVING nxx
              CALL 'FibonacciRec' USING nx, rx
              CALL 'FibonacciRec' USING nxx, rxx
              ADD rx TO rxx GIVING ret
              GOBACK
           END-IF.
       END PROGRAM FibonacciRec.
