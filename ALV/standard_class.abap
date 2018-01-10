
*----------------------------------------------------------------------*
* Class LCL_EVENT_RECEIVER DEFINITION                                  *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive,

      handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm,

      handle_hotspot_click FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id e_column_id es_row_no.

ENDCLASS.

*----------------------------------------------------------------------*
* Class LCL_EVENT_RECEIVER IMPLEMENTATION Methods                      *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
    DATA: lv_toolbar TYPE stb_button.
* Push Button
    CLEAR lv_toolbar.
    IF tbp[] IS NOT INITIAL.
      MOVE 'CRTO' TO lv_toolbar-function.
      MOVE TEXT-b01 TO lv_toolbar-text.
      MOVE TEXT-b01 TO lv_toolbar-quickinfo.
      MOVE icon_move TO lv_toolbar-icon.
      MOVE ' ' TO lv_toolbar-disabled.
      APPEND lv_toolbar TO e_object->mt_toolbar.
    ENDIF.
  ENDMETHOD.

* User Command
  METHOD handle_user_command.
    CASE e_ucomm.
      WHEN 'CRTO'.

        PERFORM check_selected_rows.
        IF no_process <> 'X'.
          LOOP AT t_rows INTO s_row.
            CLEAR tbp.
            READ TABLE tbp INTO tbp INDEX s_row-index.
            IF p_mode = '925' OR
               p_mode = '903'.
              PERFORM create_transfer_ord_e USING tbp.
            ELSE.
              PERFORM create_transfer_ord_a USING tbp.
            ENDIF.
            IF creation_ok = 'X'.
              tbp-icon = icon_green_light.
            ELSE.
              tbp-icon = icon_red_light.
            ENDIF.
            MODIFY tbp FROM tbp INDEX s_row-index.
          ENDLOOP.
          CALL METHOD g_grid->refresh_table_display.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

* Hotspot click control
  METHOD handle_hotspot_click.

    CLEAR tbp.
    READ TABLE tbp INTO tbp INDEX e_row_id.

*   Transfer Requirement Number
    IF sy-subrc = 0.
      IF e_column_id-fieldname = 'TBNUM'.
        SET PARAMETER ID 'LGN' FIELD tbp-lgnum.
        SET PARAMETER ID 'TBN' FIELD tbp-tbnum.
        SET PARAMETER ID 'TBP' FIELD tbp-tbpos.
        CALL TRANSACTION 'LB03' AND SKIP FIRST SCREEN.
        SET PARAMETER ID 'LGN' FIELD space.
        SET PARAMETER ID 'TBN' FIELD space.
        SET PARAMETER ID 'TBP' FIELD space.
      ENDIF.

*     Number of Material Document
      IF e_column_id-fieldname = 'MBLNR'.
        CHECK NOT tbp-mblnr IS INITIAL.
        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = tbp-mblnr
            i_mjahr             = tbp-mjahr.
*          i_zeile             = list-zeile.
      ENDIF.
    ENDIF.

  ENDMETHOD .

ENDCLASS.

DATA: gv_event_receiver TYPE REF TO lcl_event_receiver.
