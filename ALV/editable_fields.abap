*-----------------------------------------------------------------------*   

CLASS lcl_event_receiver DEFINITION.




  PUBLIC SECTION.

    METHODS:

      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid

        IMPORTING er_data_changed.




ENDCLASS.

*-----------------------------------------------------------------------*

CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD handle_data_changed.

    DATA: ls_good TYPE lvc_s_modi.

    DATA ls_vbak TYPE vbak.

    DATA lv_kalab_act TYPE mseg-menge.

    DATA kalab_char(17).

    DATA lv_mess TYPE string.




    LOOP AT er_data_changed->mt_mod_cells INTO ls_good.

      CASE ls_good-fieldname.

        WHEN 'KALAB_ACT'.

          CALL FUNCTION 'MOVE_CHAR_TO_NUM'

            EXPORTING

              chr = ls_good-value

            IMPORTING

              num = lv_kalab_act.




          CLEAR gt_mska.

          READ TABLE gt_mska INDEX ls_good-row_id INTO gt_mska.




          IF lv_kalab_act > gt_mska-kalab.




            CALL METHOD er_data_changed->modify_cell

              EXPORTING

                i_row_id    = ls_good-row_id

                i_fieldname = 'KALAB_ACT'

                i_value     = space.




            WRITE gt_mska-kalab TO kalab_char.

            CONCATENATE TEXT-e01 kalab_char INTO lv_mess SEPARATED BY space.

            MESSAGE lv_mess TYPE 'S'

            DISPLAY LIKE 'E'.




          ENDIF.

      ENDCASE.

    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*-----------------------------------------------------------------------*

DATA: gv_event_receiver TYPE REF TO lcl_event_receiver.

   

*-----------------------------------------------------------------------*

       CREATE OBJECT gv_event_receiver.

    SET HANDLER gv_event_receiver->handle_data_changed FOR g_grid.

   

*-----------------------------------------------------------------------*

   " SET_TABLE_FOR_FIRST_DISPLAY

    CALL METHOD g_grid->set_table_for_first_display

      EXPORTING

        is_layout            = gs_layout

        it_toolbar_excluding = lt_exclude

      CHANGING

        it_fieldcatalog      = gs_fieldcatalog

        it_outtab            = itab[]. " Data




    CALL METHOD g_grid->register_edit_event

      EXPORTING

        i_event_id = cl_gui_alv_grid=>mc_evt_enter.




    CALL METHOD g_grid->register_edit_event

      EXPORTING

        i_event_id = cl_gui_alv_grid=>mc_evt_modified.




    g_grid->activate_display_protocol( i_dialog = space ).
