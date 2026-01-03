       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCDVOP.
      *---------------------------------------------------------------*
      * VIBEGARDEN: STANDALONE DEVOPS VERSION
      * READS FROM JCL SYSIN, CALCULATES, AND DISPLAYS TO SYSOUT
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-INPUT-DATA.
          05 WS-BASE-SCORE      PIC 9(3)V99 VALUE ZERO.
      
       01 WS-CALC-RESULTS.
          05 WS-BONUS-MULTIPLIER PIC 9V9  VALUE 2.5.
          05 WS-TOTAL-RESULT     PIC 9(7)V99 VALUE ZERO.
          05 WS-DISPLAY-FINAL    PIC Z,ZZZ,ZZ9.99.

       PROCEDURE DIVISION.
       000-MAIN.
      * 1. Accept the value from //SYSIN in your RUNJCL
           ACCEPT WS-BASE-SCORE.

      * 2. Perform the VibeGarden Logic (1.5x Multiplier)
           COMPUTE WS-TOTAL-RESULT = WS-BASE-SCORE * WS-BONUS-MULTIPLIER.

      * 3. Format and Display for the PowerShell Script to see
           MOVE WS-TOTAL-RESULT TO WS-DISPLAY-FINAL.
           DISPLAY "VibeGarden Result: " WS-DISPLAY-FINAL.

           GOBACK.