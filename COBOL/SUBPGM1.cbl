       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCDVOP.
      *---------------------------------------------------------------*
      * VIBEGARDEN: REFACTORED FOR DEVOPS CALLING
      *---------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-CALC-LOGIC.
          05 WS-BONUS-MULTIPLIER PIC 9V9 VALUE 1.5.
      ****************************************************************
       LINKAGE SECTION.
       01 LS-VIBE-INTERFACE.
          05 LS-BASE-SCORE      PIC 9(03).
          05 LS-TOTAL-RESULT    PIC 9(04).
      ****************************************************************
       PROCEDURE DIVISION USING LS-VIBE-INTERFACE.
       000-MAIN.
      * Instead of hardcoding, we use the input from the caller
           COMPUTE LS-TOTAL-RESULT = LS-BASE-SCORE * WS-BONUS-MULTIPLIER

           GOBACK.
