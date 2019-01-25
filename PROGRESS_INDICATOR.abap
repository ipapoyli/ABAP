**  perc display
    DATA: v_lines type i,
          v_index TYPE i,
          v_perc  TYPE i,
          v_text  type c LENGTH 100.



      CLEAR: v_index, v_lines, v_text.
      v_lines = lines( t_itab ).
      
      
   loop   
** ------------------------------------------------------- **
        ADD 1 TO v_index.
        v_perc = 100 * v_index / v_lines.
        v_text = |Get data |.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = v_perc
            text       = v_text.
** ------------------------------------------------------- **
endloop
