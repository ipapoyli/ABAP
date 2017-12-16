  DO.
    REPLACE ' ' WITH '0' INTO p_field_ll.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.
  ENDDO.
