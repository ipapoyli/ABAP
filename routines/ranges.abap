RANGES: r_zlsch for bsid-zlsch. 

perform fill_range_paym using 'B'.

*&---------------------------------------------------------------------*
*&      Form  FILL_RANGE_PAYM
*&---------------------------------------------------------------------*
FORM FILL_RANGE_PAYM  USING p_zlsch.
*
  r_zlsch-sign   = 'I'.
  r_zlsch-option = 'EQ'.
  r_zlsch-low    = p_zlsch.
  append r_zlsch.
*
ENDFORM.

