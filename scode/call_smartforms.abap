 
*----------------------------------------------------------------------*
* Smartform Variables                                                  *
*----------------------------------------------------------------------*
DATA lf_formname TYPE tdsfname.
DATA lf_fm_name TYPE rs38l_fnam.

DATA ls_control_param   TYPE ssfctrlop.
DATA ls_output_opt      TYPE ssfcompop.
 
 
 
 CLEAR lf_fm_name.
  CHECK lf_formname IS NOT INITIAL.
  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = lf_formname
    IMPORTING
      fm_name            = lf_fm_name
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.

  IF sy-subrc <> 0.
*   error handling
  ENDIF.

  CALL FUNCTION lf_fm_name
    EXPORTING
      control_parameters = ls_control_param
      output_options     = ls_output_opt
      user_settings      = space
      iv_bukrs              = p_bukrs
      iv_kunnr              = p_kunnr
      iv_matnr              = p_matnr
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
