** create first object subobject from slg0


data: lo_msg_collector TYPE REF TO if_reca_message_list.
CONSTANTS : gv_balobj    TYPE balobj_d  VALUE 'ZEKO_PO_CREATE',
            gv_balsubibj TYPE balsubobj VALUE 'ZEKO_PO_CREATE1'.


* Message Collector
  if lo_msg_collector is initial.
    lo_msg_collector = cf_reca_message_list=>create( id_object    = gv_balobj
                                                     id_subobject = gv_balsubibj ).
  endif.



******************************************************************************************

    lo_msg_collector->clear( ).
    LOOP AT it_return INTO ls_return.

      lo_msg_collector->add_from_bapi( EXPORTING is_bapiret = ls_return
        if_cumulate = abap_true ).
    ENDLOOP.
    PERFORM show_messages.


******************************************************************************************

*&---------------------------------------------------------------------*
*&      Form  SHOW_MESSAGES
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM show_messages .
  DATA : ls_log_disp_prof TYPE bal_s_prof.
  DATA: it_bal_t_logh TYPE  bal_t_logh.

  DATA(lv_i) = lo_msg_collector->count( ).
  CHECK lv_i > 0.

  DATA(lv_stats) = lo_msg_collector->get_statistics( ).
  APPEND lo_msg_collector->get_handle( ) TO it_bal_t_logh.

  CALL FUNCTION 'BAL_DSP_PROFILE_POPUP_GET'
    EXPORTING
      end_col             = 100
      end_row             = 60
    IMPORTING
      e_s_display_profile = ls_log_disp_prof.

  ls_log_disp_prof-use_grid   = abap_true.
  IF lv_stats-msg_cnt_e > 0.
    ls_log_disp_prof-grid_title-gridtitle = TEXT-001.
  ELSE.
    ls_log_disp_prof-grid_title-no_gridtitle = abap_true.
  ENDIF.
  ls_log_disp_prof-disvariant = 'lg'.
*    ls_log_disp_prof-disvariant = VALUE #( handle = lo_msg_collector->get_handle( )
*                                           report = sy-repid ).

  CALL FUNCTION 'BAL_DSP_LOG_DISPLAY'
    EXPORTING
      i_s_display_profile  = ls_log_disp_prof
      i_t_log_handle       = it_bal_t_logh
    EXCEPTIONS
      profile_inconsistent = 1
      internal_error       = 2
      no_data_available    = 3
      no_authority         = 4
      OTHERS               = 5.
ENDFORM.
