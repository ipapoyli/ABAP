
*----------------------------------------------------------------------*
* Global Variables
*----------------------------------------------------------------------*
DATA: lt_dynp_value TYPE TABLE OF dynpread.
DATA: ls_dynp_value TYPE dynpread.



*&---------------------------------------------------------------------*
*&      Form  DYNP_VALUES_READ
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM dynp_values_read .

  CLEAR: lt_dynp_value, ls_dynp_value.
  ls_dynp_value-fieldname = 'S1_LGNUM'.
  APPEND ls_dynp_value TO lt_dynp_value.
  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = lt_dynp_value.

  CLEAR: ls_dynp_value.
  READ TABLE lt_dynp_value INTO ls_dynp_value INDEX 1.
  IF sy-subrc EQ 0.
    pa_lgnum = ls_dynp_value-fieldvalue.
  ENDIF.

ENDFORM.
