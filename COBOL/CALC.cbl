       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOZ.
      *---------------------------------------------------------------*
      * VIBEGARDEN: CALCULATION TEST
      *---------------------------------------------------------------*
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-VIBE-DATA.
          05 WS-BASE-SCORE      PIC 9(03) VALUE 200.
          05 WS-MODERN-BONUS    PIC 9(03) VALUE 050.
          05 WS-TOTAL-VIBE      PIC 9(04).

       01 WS-DISPLAY-MSG.
          05 FILLER             PIC X(15) VALUE 'TOTAL SCORE IS:'.
          05 WS-OUT-TOTAL       PIC Z,ZZ9.

       PROCEDURE DIVISION.
       000-MAIN.
           DISPLAY '--- VIBEGARDEN MODERNIZATION ENGINE ---'

           COMPUTE WS-TOTAL-VIBE = WS-BASE-SCORE + WS-MODERN-BONUS

           MOVE WS-TOTAL-VIBE TO WS-OUT-TOTAL
           DISPLAY WS-DISPLAY-MSG

           DISPLAY '--- PROCESS COMPLETE ---'
           GOBACK.
