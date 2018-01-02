* Concatanate into val_string
  DO.
    UNASSIGN <fs>.
    ASSIGN COMPONENT sy-index OF STRUCTURE gs_itab TO <fs>.
    IF <fs> IS ASSIGNED.
      CONCATENATE val_string <fs> INTO val_string RESPECTING BLANKS.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.
