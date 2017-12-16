CALL FUNCTION 'MOVE_CHAR_TO_NUM'
  EXPORTING
    chr             = t_cuval-atwrt
  IMPORTING
    num             = l_width_in
  EXCEPTIONS
    convt_no_number = 1
    convt_overflow  = 2
    OTHERS          = 3.
