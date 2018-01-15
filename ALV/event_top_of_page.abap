*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DG_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM event_top_of_page  USING    p_dg_dyndoc_id.

*  DATA : dl_text(255) TYPE c.  "Text
*  DATA : tr_quant_char(20).
*  DATA : t_lines_c(3),
*         r_line_c(4).
*  DATA : lv_unit TYPE meins.

* ------------------------------------------------------------- *
*                         add gap                               *
* ------------------------------------------------------------- *
  CALL METHOD dg_dyndoc_id->add_gap             "add gap
    EXPORTING
      width = 5.
      
* ------------------------------------------------------------- *
*                        add text                               *
* ------------------------------------------------------------- *
  CLEAR : dl_text.  
  CONCATENATE 'Mat. D.:' gv_mat_des INTO dl_text SEPARATED BY space.
  CALL METHOD dg_dyndoc_id->add_text             "add text 
    EXPORTING
      text         = dl_text
      sap_fontsize = cl_dd_area=>medium
      sap_emphasis = cl_dd_area=>medium.
      
* ------------------------------------------------------------- *
*                       Add new-line                            *
* ------------------------------------------------------------- *
  CALL METHOD dg_dyndoc_id->new_line.        "Add new-line
  
* ------------------------------------------------------------- *
*                       add icon                                *
* ------------------------------------------------------------- *
  CALL METHOD dg_dyndoc_id->add_icon(           "add icon
      sap_icon = 'ICON_WAREHOUSES'
      sap_size = cl_dd_area=>extra_large ).

  PERFORM html.

ENDFORM.
