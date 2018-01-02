*----------------------------------------------------------------------*
* POP UP data                                                          *
*----------------------------------------------------------------------*
DATA: gv_qst(70).
DATA: answer.

*&---------------------------------------------------------------------*
*&      Form  POPUP_CONFIRM
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PAYMENT_METHOD  text
*----------------------------------------------------------------------*
FORM POPUP_CONFIRM .
  
  CLEAR gv_qst.
*  CONCATENATE ' ' text-c01 INTO gv_qst SEPARATED BY space.

  CLEAR answer.
  CALL FUNCTION 'POPUP_TO_CONFIRM'
  EXPORTING
    TITLEBAR              = TEXT-sv1
    text_question         = gv_qst
    text_button_1         = TEXT-an1
    icon_button_1         = 'ICON_CHECKED'
    text_button_2         = TEXT-an2
    icon_button_2         = 'ICON_CANCEL'
    display_cancel_button = ' '
    popup_type            = 'ICON_MESSAGE_QUESTION'
  IMPORTING
    answer                = answer.

  IF answer = 1.
*    PERFORM create_txt_file USING p_payment_method.
  ENDIF.

ENDFORM.
