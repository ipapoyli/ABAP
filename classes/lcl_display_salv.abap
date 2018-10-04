
CLASS lcl_display_salv DEFINITION.

* ----------------------------------------------------------------- *
  PUBLIC SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Data
* ----------------------------------------------------------------- *
    DATA: t_fieldcatalog TYPE TABLE OF lvc_s_fcat .

    DATA: t_salv_dcl     TYPE REF TO cl_salv_table.

    DATA: l_salv_columns TYPE REF TO cl_salv_columns_table.
    DATA: l_salv_aggrs   TYPE REF TO cl_salv_aggregations.

    DATA: l_salv_column  TYPE REF TO cl_salv_column_table.
    DATA: l_salv_funct   TYPE REF TO cl_salv_functions_list.

    DATA: l_salv_layout  TYPE REF TO cl_salv_layout.

    DATA: l_salv_events  TYPE REF TO cl_salv_events_table.

    DATA: l_salv__selections TYPE REF TO cl_salv_selections.

    DATA: l_salv_display TYPE REF TO cl_salv_display_settings.

* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS alv_display.

    METHODS get_alv CHANGING table TYPE ANY TABLE.

    METHODS set_handlers.

* ----------------------------------------------------------------- *
* Handle Methods
* ----------------------------------------------------------------- *

    METHODS:
      handle_hotspot_click FOR EVENT link_click OF cl_salv_events_table
        IMPORTING row column.

    METHODS:
      handle_on_user_command FOR EVENT added_function OF cl_salv_events
        IMPORTING e_salv_function.

*    METHODS:
*      handle_data_changed FOR EVENT data_changed OF cl_gui_alv_grid
*        IMPORTING er_data_changed sender.


* ----------------------------------------------------------------- *
  PRIVATE SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Data
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *

    METHODS display_basic_toolbar.

    METHODS set_layout IMPORTING variant TYPE disvariant-variant.

    METHODS set_display_settings.

    METHODS set_selection_mode.

    METHODS set_status.

*   Set Top of page
    METHODS set_top_of_page CHANGING t_salv TYPE REF TO cl_salv_table.
*
*   Set End of page
    METHODS set_end_of_page CHANGING t_salv TYPE REF TO cl_salv_table.


ENDCLASS.



CLASS lcl_display_salv IMPLEMENTATION.

* ----------------------------------------------------------------- *
* ALV display
* ----------------------------------------------------------------- *
  METHOD alv_display.
    t_salv_dcl->display( ).
  ENDMETHOD.

* ----------------------------------------------------------------- *
* Fieldcatalog
* ----------------------------------------------------------------- *
  METHOD get_alv.

* --------------------------------------------------------------- *
*   Creating the SALV Object
* --------------------------------------------------------------- *
    TRY.
*        cl_salv_table=>factory( IMPORTING r_salv_table = t_salv_dcl
*                                CHANGING t_table = go_data->t_itab ).

        cl_salv_table=>factory( IMPORTING r_salv_table = t_salv_dcl
                                  CHANGING t_table = table ).

      CATCH cx_salv_msg.
    ENDTRY.

    CHECK sy-subrc EQ 0.

* --------------------------------------------------------------- *
*    Making Sure a Toolbar Appears at the Top of the Report
* --------------------------------------------------------------- *
    display_basic_toolbar( ).

* --------------------------------------------------------------- *
*    Set status
* --------------------------------------------------------------- *
    set_status( ).


* --------------------------------------------------------------- *
*   In case you want to change some column attributes
* --------------------------------------------------------------- *
    l_salv_columns  = t_salv_dcl->get_columns( ).
    l_salv_columns->set_optimize( abap_true ).

    l_salv_aggrs    = t_salv_dcl->get_aggregations( ).

**   Add TOTAL for COLUMN NETWR
*    TRY.
*        CALL METHOD lo_aggrs->add_aggregation
*          EXPORTING
*            columnname  = 'NETWR'
*            aggregation = if_salv_c_aggregation=>total.
*      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
*      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
*      CATCH cx_salv_existing .                          "#EC NO_HANDLER
*    ENDTRY.
***
**   Bring the total line to top
*    lo_aggrs->set_aggregation_before_items( ).
* --------------------------------------------------------------- *
*  COLUMN  modifications
* --------------------------------------------------------------- *
    TRY.
****set key
        l_salv_column ?= l_salv_columns->get_column( 'MTART' ).
        l_salv_column->set_key( ).

        l_salv_column ?= l_salv_columns->get_column( 'MATNR' ).
        l_salv_column->set_key( ).

*   Set the HotSpot for vbeln Column
        TRY.
            CALL METHOD l_salv_column->set_cell_type
              EXPORTING
                value = if_salv_c_cell_type=>hotspot.
          CATCH cx_salv_data_error .
        ENDTRY.

*****set space for zero
*        l_salv_column ?= l_salv_columns->get_column( 'BPMNG' ).
*        l_salv_column->set_zero( ' ' ).
*
****set visible column
*        l_salv_column ?= l_salv_columns->get_column( 'LIFNR' ).
*        l_salv_column->set_visible( if_salv_c_bool_sap=>false ).

      CATCH cx_salv_not_found.
    ENDTRY.


* --------------------------------------------------------------- *
*  Selection mode
* --------------------------------------------------------------- *
    set_selection_mode( ).

* --------------------------------------------------------------- *
*  Display settings
* --------------------------------------------------------------- *
    set_display_settings( ).

* --------------------------------------------------------------- *
*   Setting up the Layout  // Variant  / also save authorazition
* --------------------------------------------------------------- *
    set_layout( p_vari ).

* --------------------------------------------------------------- *
*   set header
* --------------------------------------------------------------- *
*   Calling the top of page method
    set_top_of_page( CHANGING t_salv = t_salv_dcl ).
*


* --------------------------------------------------------------- *
*   set footer
* --------------------------------------------------------------- *
*   Calling the End of Page method
    CALL METHOD me->set_end_of_page
      CHANGING
        t_salv = t_salv_dcl.

  ENDMETHOD.


* ----------------------------------------------------------------- *
* Set handlers methods
* ----------------------------------------------------------------- *
  METHOD set_handlers.

    l_salv_events = t_salv_dcl->get_event( ).

    SET HANDLER handle_hotspot_click FOR l_salv_events.

    SET HANDLER handle_on_user_command FOR l_salv_events.


  ENDMETHOD.


***********************************************************************
***** Handle methods

* ----------------------------------------------------------------- *
* Hotspot click control
* ----------------------------------------------------------------- *
  METHOD handle_hotspot_click.

    DATA wa_itab TYPE mara.

    CLEAR wa_itab.
    READ TABLE go_data->t_itab INTO wa_itab INDEX row.
    IF sy-subrc = 0.
      CASE column.
        WHEN 'MATNR'.



      ENDCASE.


    ENDIF.
  ENDMETHOD .


  METHOD handle_on_user_command.
    CASE e_salv_function.
      WHEN '&AFC'.
        BREAK-POINT.

    ENDCASE.
  ENDMETHOD.

**************************************************************************
*** Privete Methods


* ----------------------------------------------------------------- *
* Set layouts
* ----------------------------------------------------------------- *
  METHOD set_layout.

* Local Variables
    DATA: ls_key TYPE salv_s_layout_key.

    l_salv_layout = t_salv_dcl->get_layout( ).

* Set the Layout Key
    ls_key-report = sy-cprog.
    l_salv_layout->set_key( ls_key ).

* set usage of default Layouts
    l_salv_layout->set_default( 'X' ).

* set initial Layout
    IF variant IS NOT INITIAL.
      l_salv_layout->set_initial_layout( variant ).
    ENDIF.

* Set save restriction
* Check authority to change display variants.
    AUTHORITY-CHECK OBJECT 'S_ALV_LAYO' ID 'ACTVT' FIELD '23'.

    IF sy-subrc = 0. " does he ride a white horse?
      l_salv_layout->set_save_restriction( if_salv_c_layout=>restrict_none ).

      " yes, allow user and global display variants
    ELSE.

      l_salv_layout->set_save_restriction( if_salv_c_layout=>restrict_user_dependant ).
    ENDIF.
  ENDMETHOD.   "set layout

* ----------------------------------------------------------------- *
* Toolbar
* ----------------------------------------------------------------- *
  METHOD display_basic_toolbar.
    DATA: l_text TYPE string,
          l_icon TYPE string.

    l_salv_funct = t_salv_dcl->get_functions( ).
    l_salv_funct->set_all( if_salv_c_bool_sap=>true ).


  ENDMETHOD.

* ----------------------------------------------------------------- *
* Display settings
* ----------------------------------------------------------------- *
  METHOD set_display_settings.

    l_salv_display = t_salv_dcl->get_display_settings( ).

* zebra stripes
    l_salv_display->set_striped_pattern( cl_salv_display_settings=>true ).

* Title
    l_salv_display->set_list_header( 'SALV test report' ).

  ENDMETHOD.


* ----------------------------------------------------------------- *
* Selection mode
* ----------------------------------------------------------------- *
  METHOD set_selection_mode.
    l_salv__selections = t_salv_dcl->get_selections( ).

    l_salv__selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  ENDMETHOD.

* ----------------------------------------------------------------- *
* Set status
* ----------------------------------------------------------------- *
  METHOD set_status.
    t_salv_dcl->set_screen_status(
    pfstatus      =  'STANDARD'
    report        =  sy-repid
    set_functions =  t_salv_dcl->c_functions_all ).
  ENDMETHOD.

* ----------------------------------------------------------------- *
* Set top_of_page
* ----------------------------------------------------------------- *
  METHOD set_top_of_page.
*
    DATA: lo_header  TYPE REF TO cl_salv_form_layout_grid,
          lo_h_label TYPE REF TO cl_salv_form_label,
          lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.
*
*   header object
    CREATE OBJECT lo_header.
*
*   To create a Lable or Flow we have to specify the target
*     row and column number where we need to set up the output
*     text.
*
*   information in Bold
    lo_h_label = lo_header->create_label( row = 1 column = 1 ).
    lo_h_label->set_text( 'Header in Bold' ).
*
*   information in tabular format
    lo_h_flow = lo_header->create_flow( row = 2  column = 1 ).
    lo_h_flow->create_text( text = 'This is text of flow' ).
*
    lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
    lo_h_flow->create_text( text = 'Number of Records in the output' ).
*
    lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
    lo_h_flow->create_text( text = 20 ).
*
*   set the top of list using the header for Online.
    t_salv->set_top_of_list( lo_header ).
*
*   set the top of list using the header for Print.
    t_salv->set_top_of_list_print( lo_header ).
*
  ENDMETHOD.                    "set_top_of_page


* ----------------------------------------------------------------- *
* Set end of page
* ----------------------------------------------------------------- *
  METHOD set_end_of_page.
*
    DATA: lo_footer  TYPE REF TO cl_salv_form_layout_grid,
          lo_f_label TYPE REF TO cl_salv_form_label,
          lo_f_flow  TYPE REF TO cl_salv_form_layout_flow.
*
*   footer object
    CREATE OBJECT lo_footer.
*
*   information in bold
    lo_f_label = lo_footer->create_label( row = 1 column = 1 ).
    lo_f_label->set_text( 'Footer .. here it goes' ).
*
*   tabular information
    lo_f_flow = lo_footer->create_flow( row = 2  column = 1 ).
    lo_f_flow->create_text( text = 'This is text of flow in footer' ).
*
    lo_f_flow = lo_footer->create_flow( row = 3  column = 1 ).
    lo_f_flow->create_text( text = 'Footer number' ).
*
    lo_f_flow = lo_footer->create_flow( row = 3  column = 2 ).
    lo_f_flow->create_text( text = 1 ).
*
*   Online footer
    t_salv->set_end_of_list( lo_footer ).
*
*   Footer in print
    t_salv->set_end_of_list_print( lo_footer ).
*
  ENDMETHOD.                    "set_end_of_page
ENDCLASS.
