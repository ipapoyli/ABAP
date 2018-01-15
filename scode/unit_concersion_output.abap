  CALL FUNCTION 'CONVERSION_EXIT_CUNIT_OUTPUT'
    EXPORTING
      input    = gv_unit
      language = sy-langu
    IMPORTING
*     LONG_TEXT            = LONG_TEXT
      output   = lv_unit
*     SHORT_TEXT           = SHORT_TEXT
*   EXCEPTIONS
*     UNIT_NOT_FOUND       = 1
*     OTHERS   = 2
    .
