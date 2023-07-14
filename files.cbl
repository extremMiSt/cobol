       IDENTIFICATION DIVISION.
       PROGRAM-ID. Files.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT linedat ASSIGN TO "line.txt" 
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD linedat 
           RECORD VARYING FROM 0 TO 80 DEPENDING ON textlen.
       01 textline PIC X(80).
       WORKING-STORAGE SECTION.
       01 textlen PIC 99.
       PROCEDURE DIVISION.
       MAIN SECTION.
           OPEN INPUT linedat.
           READ linedat
           DISPLAY textlen.
           DISPLAY textline.
           CLOSE linedat.
           STOP RUN.
       END PROGRAM Files.
