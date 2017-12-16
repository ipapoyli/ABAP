*DATA NUM TYPE P.

CALL FUNCTION 'MOVE_CHAR_TO_NUM'
  EXPORTING
    chr                   = chr
* IMPORTING
*   NUM                   = NUM
* EXCEPTIONS
*   CONVT_NO_NUMBER       = 1
*   CONVT_OVERFLOW        = 2
*   OTHERS                = 3
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.
