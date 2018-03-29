*&---------------------------------------------------------------------*
*& Report Z_TEST22
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_test22.


* ----------------------------------------------------------------- *
* Class Definition Deferred.
* ----------------------------------------------------------------- *
CLASS lcl_general DEFINITION DEFERRED.
CLASS lcl_display DEFINITION DEFERRED.


* ----------------------------------------------------------------- *
* Types / Global Data
* ----------------------------------------------------------------- *
TABLES mara.


TYPES : BEGIN OF uplog_typ,
          msgid  LIKE sy-msgid,
          msgty  LIKE sy-msgty,
          msgno  LIKE sy-msgno,
          msgv1  LIKE sy-msgv1,
          msgv2  LIKE sy-msgv2,
          msgv3  LIKE sy-msgv3,
          msgv4  LIKE sy-msgv4,
          lineno LIKE mesg-zeile,
        END OF uplog_typ.

TYPES BEGIN OF ty_itab .
INCLUDE STRUCTURE mara.
TYPES END OF   ty_itab.

DATA :ok_code LIKE sy-ucomm.


DATA go_orders  TYPE REF TO lcl_general .
DATA go_display TYPE REF TO lcl_display.


* ----------------------------------------------------------------- *
* Selection screen
* ----------------------------------------------------------------- *
SELECTION-SCREEN BEGIN OF BLOCK s01 .
SELECT-OPTIONS: s_matnr FOR mara-matnr.
SELECTION-SCREEN END OF BLOCK s01.



* ----------------------------------------------------------------- *
* Class Definition
* ----------------------------------------------------------------- *
CLASS lcl_general DEFINITION.

* ----------------------------------------------------------------- *
  PUBLIC SECTION.
* ----------------------------------------------------------------- *

* ----------------------------------------------------------------- *
* Data
* ----------------------------------------------------------------- *
    DATA:  t_sel_params TYPE TABLE OF rsparams.
    DATA:  uplog_tab    TYPE TABLE OF uplog_typ.

    DATA t_itab TYPE STANDARD TABLE OF ty_itab .


* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS collect_data .
    METHODS display_messages.



* ----------------------------------------------------------------- *
  PRIVATE SECTION.
* ----------------------------------------------------------------- *

* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS  add_message
      IMPORTING i_msgid TYPE any
                i_msgty TYPE any
                i_msgno TYPE any
                i_msgv1 TYPE any
                i_msgv2 TYPE any
                i_msgv3 TYPE any
                i_msgv4 TYPE any.

ENDCLASS.

CLASS lcl_display DEFINITION.

* ----------------------------------------------------------------- *
  PUBLIC SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Data
* ----------------------------------------------------------------- *
    DATA: gt_fieldcatalog TYPE TABLE OF lvc_s_fcat .


* DECLARE REFERENCE VARIABLES TO THE ALV GRID AND THE CONTAINER
    DATA :g_container        TYPE scrfname VALUE 'CC_CONTAINER_GR',
          g_custom_container TYPE REF TO cl_gui_custom_container,
          g_grid             TYPE REF TO cl_gui_alv_grid.

    DATA: dg_dyndoc_id   TYPE REF TO cl_dd_document,            "Reference to document
          dg_splitter    TYPE REF TO cl_gui_splitter_container, "Reference to split container
          dg_parent_grid TYPE REF TO cl_gui_container,          "Reference to grid container
          dg_html_cntrl  TYPE REF TO cl_gui_html_viewer,        "Reference to html container
          dg_parent_html TYPE REF TO cl_gui_container.          "Reference to html container


    DATA: s_ord_layout TYPE lvc_s_layo,
          t_exclude    TYPE ui_functions.

* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS set_layout .

    METHODS set_exclude_buttons.

    METHODS get_fieldcat.

* ----------------------------------------------------------------- *
  PRIVATE SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Data
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *


ENDCLASS.



* ----------------------------------------------------------------- *
* Class Implementation
* ----------------------------------------------------------------- *
CLASS lcl_general IMPLEMENTATION.

* ----------------------------------------------------------------- *
* Colllect Data
* ----------------------------------------------------------------- *
  METHOD collect_data.

    DATA ls_mara TYPE mara.

    SELECT * FROM mara INTO TABLE me->t_itab
      WHERE matnr IN s_matnr.

    IF me->t_itab[] IS NOT INITIAL.
      CALL METHOD add_message
        EXPORTING
          i_msgid = '00'
          i_msgty = 'E'
          i_msgno = '398'
          i_msgv1 = TEXT-e02
          i_msgv2 = ''
          i_msgv3 = ''
          i_msgv4 = ''.

      CALL METHOD display_messages( ).
    ENDIF.


  ENDMETHOD.

* ----------------------------------------------------------------- *
* Display_messages
* ----------------------------------------------------------------- *
  METHOD display_messages.

    CHECK NOT me->uplog_tab IS INITIAL.
    CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
      TABLES
        i_message_tab = me->uplog_tab.

*    CLEAR me->uplog_tab.


  ENDMETHOD.

* ----------------------------------------------------------------- *
* Add message
* ----------------------------------------------------------------- *
  METHOD add_message .
    DATA : uplog_wa TYPE uplog_typ.

    uplog_wa-msgid = i_msgid.
    uplog_wa-msgty = i_msgty.
    uplog_wa-msgno = i_msgno.
    uplog_wa-msgv1 = i_msgv1.
    uplog_wa-msgv2 = i_msgv2.
    uplog_wa-msgv3 = i_msgv3.
    uplog_wa-msgv4 = i_msgv4.

    APPEND uplog_wa TO me->uplog_tab.

  ENDMETHOD.

ENDCLASS .

CLASS lcl_display IMPLEMENTATION.
* ----------------------------------------------------------------- *
* Set layout
* ----------------------------------------------------------------- *
  METHOD set_layout.
    me->s_ord_layout-zebra             = 'X'.
    me->s_ord_layout-cwidth_opt        = 'X'.
    me->s_ord_layout-ctab_fname        = 'COLOR_TAB'.
    me->s_ord_layout-sel_mode          = 'A'.
    me->s_ord_layout-box_fname         = 'SEL'.
    me->s_ord_layout-no_toolbar        = 'X'.
    ENDMETHOD.

* ----------------------------------------------------------------- *
* Set exclude buttons
* ----------------------------------------------------------------- *
    METHOD set_exclude_buttons.
      DATA ls_exclude TYPE ui_func.


      ls_exclude = cl_gui_alv_grid=>mc_fc_maximum .
      APPEND ls_exclude TO me->t_exclude.
*
      ls_exclude = cl_gui_alv_grid=>mc_fc_minimum .
      APPEND ls_exclude TO me->t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_graph.
      APPEND ls_exclude TO me->t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_help.
      APPEND ls_exclude TO me->t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_info.
      APPEND ls_exclude TO me->t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_sort_asc.
      APPEND ls_exclude TO me->t_exclude.
      ls_exclude = cl_gui_alv_grid=>mc_fc_sort_dsc.
      APPEND ls_exclude TO me->t_exclude.
      ls_exclude = cl_gui_alv_grid=>mc_fc_sort.
      APPEND ls_exclude TO me->t_exclude.
*
      ls_exclude = cl_gui_alv_grid=>mc_fc_delete_filter.
      APPEND ls_exclude TO me->t_exclude.
*
      ls_exclude = cl_gui_alv_grid=>mc_fc_filter.
      APPEND ls_exclude TO me->t_exclude.
      ls_exclude = cl_gui_alv_grid=>mc_fc_detail.
      APPEND ls_exclude TO me->t_exclude.
      ls_exclude = cl_gui_alv_grid=>mc_mb_filter.
      APPEND ls_exclude TO me->t_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_CURRENT_VARIANT.
*  APPEND LS_EXCLUDE TO t_exclude.
*  LS_EXCLUDE = CL_GUI_ALV_GRID=>MC_FC_SAVE_VARIANT.
*  APPEND LS_EXCLUDE TO t_exclude.
      ls_exclude = cl_gui_alv_grid=>mc_fc_find.
      APPEND ls_exclude TO me->t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_append_row.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_copy_row.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_cut.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_delete_row.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_insert_row.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_move_row.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_loc_undo.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_refresh.
      APPEND ls_exclude TO t_exclude.

      ls_exclude = cl_gui_alv_grid=>mc_fc_check.
      APPEND ls_exclude TO t_exclude.

    ENDMETHOD.


* ----------------------------------------------------------------- *
* Fieldcatalog
* ----------------------------------------------------------------- *
    METHOD get_fieldcat.

      DATA gv_fcat TYPE lvc_s_fcat.

      DATA  lv_col_pos TYPE lvc_colpos.
      CLEAR lv_col_pos.

      CLEAR gv_fcat.
      ADD 1 TO lv_col_pos.
      gv_fcat-fieldname = 'MATNR'.
      gv_fcat-col_pos = lv_col_pos.
      gv_fcat-ref_table = 'MARA'.
      gv_fcat-ref_field = 'MATNR'.
      INSERT gv_fcat INTO TABLE me->gt_fieldcatalog.

    ENDMETHOD.


ENDCLASS.


* ------------------------------------------------------------------- *
INITIALIZATION.
* ------------------------------------------------------------------- *
  CREATE OBJECT go_orders .
  CREATE OBJECT go_display.



* ------------------------------------------------------------------- *
START-OF-SELECTION.
* ------------------------------------------------------------------- *

  go_orders->collect_data( ).


  go_display->set_layout( ) .
  go_display->get_fieldcat( ).

END-OF-SELECTION.

  CALL SCREEN 100.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'SF01'.
*  SET TITLEBAR 'xxx'.

  IF go_display->g_custom_container IS INITIAL.
*----------------------------------------------------------------------*
    " Create CONTAINER object with reference to container name in the screen
    CREATE OBJECT go_display->g_custom_container
      EXPORTING
        container_name = go_display->g_container.
*----------------------------------------------------------------------*
    " Create GRID object with reference to parent name
    CREATE OBJECT go_display->g_grid
      EXPORTING
        i_parent = go_display->g_custom_container.
*----------------------------------------------------------------------*


    " SET_TABLE_FOR_FIRST_DISPLAY
    CALL METHOD go_display->g_grid->set_table_for_first_display
      EXPORTING
        is_layout            = go_display->s_ord_layout
        it_toolbar_excluding = go_display->t_exclude
      CHANGING
        it_fieldcatalog      = go_display->gt_fieldcatalog
        it_outtab            = go_orders->t_itab. " Data



*    CALL METHOD go_display->g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_enter.
*
*    CALL METHOD go_display->g_grid->register_edit_event
*    EXPORTING
*      i_event_id = cl_gui_alv_grid=>mc_evt_modified.
*
*
*
*    go_display->g_grid->activate_display_protocol( i_dialog = space ).

  ELSE.
    CALL METHOD go_display->g_grid->refresh_table_display.
  ENDIF.


ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.

  CASE ok_code.


  WHEN 'LOG'.
    go_orders->display_messages( ).

    WHEN '&F03' .
      LEAVE TO SCREEN 0.
    WHEN '&F15' OR '&F12' .
      LEAVE PROGRAM .

    WHEN OTHERS.

  ENDCASE.

ENDMODULE.
