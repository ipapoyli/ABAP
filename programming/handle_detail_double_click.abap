CLASS lcl_handler DEFINITION.
  PUBLIC SECTION.
    METHODS handle_detail_double_click
    FOR EVENT double_click OF cl_salv_events_table
    IMPORTING sender row column.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.
  METHOD handle_detail_double_click.
    CHECK row IS NOT INITIAL.
    READ TABLE gt_delvals INTO gs_delval INDEX row.
    IF gs_delval-vbeln IS NOT INITIAL.
     set parameter id 'VL' FIELD gs_delval-vbeln.
     call TRANSACTION 'VL03N' AND SKIP FIRST SCREEN.
    ENDIF.
  ENDMETHOD.                    "handle_detail_double_click
ENDCLASS.

DATA : lo_handler TYPE REF TO lcl_handler.

*************************************************************************

            WHEN 'RFMNG'.
              READ TABLE alv_tab1 INDEX rs_selfield-tabindex.
              CHECK alv_tab1-rfmng IS NOT INITIAL.
              PERFORM show_rfmng_analysis.
              
              
              
  *************************************************************************
  
  *&---------------------------------------------------------------------*
*&      Form  SHOW_RFMNG_ANALYSIS
*&---------------------------------------------------------------------*
FORM show_rfmng_analysis .

  CLEAR gt_delvals.
  SELECT likp~vkorg,
         likp~vbeln,
         lips~posnr,
         lips~matnr,
         lips~arktx,
         lips~werks,
         lips~lgmng AS rfmng ,
         lips~meins
    FROM lips INNER JOIN likp ON lips~vbeln EQ likp~vbeln
              INNER JOIN vbup ON lips~vbeln EQ vbup~vbeln AND
                                 lips~posnr EQ vbup~posnr
               INTO CORRESPONDING FIELDS OF TABLE @gt_delvals
              WHERE likp~vkorg EQ @porgan  "WTF?!?!?!?!
                AND lips~matnr IN @s_matnr
                AND lips~werks IN @s_werks
                AND ( vbup~wbsta = 'A' OR vbup~wbsta = 'B' ).



  DATA : l_pop_alv        TYPE REF TO cl_salv_table.
  DATA : l_alv_selections TYPE REF TO cl_salv_selections.
  DATA : l_alv_display    TYPE REF TO cl_salv_display_settings.
  DATA : l_alv_functions  TYPE REF TO cl_salv_functions.
  DATA : l_alv_layout     TYPE REF TO cl_salv_layout.
  DATA : l_alv_key        TYPE salv_s_layout_key.
  DATA : l_alv_columns    TYPE REF TO cl_salv_columns_table.
  DATA : l_alv_events     TYPE REF TO cl_salv_events_table.
  DATA : l_alv_aggregations TYPE REF TO cl_salv_aggregations.
  DATA : lv_start_column TYPE i,
         lv_end_column   TYPE i,
         lv_start_line   TYPE i,
         lv_end_line     TYPE i.


  TRY.
      cl_salv_table=>factory( IMPORTING r_salv_table = l_pop_alv
                               CHANGING t_table = gt_delvals ).
    CATCH cx_salv_msg.
  ENDTRY.
  CHECK sy-subrc EQ 0.



  l_alv_events = l_pop_alv->get_event( ).

  CLEAR lo_handler.
  CREATE OBJECT lo_handler.
  SET HANDLER lo_handler->handle_detail_double_click FOR l_alv_events.


*    <!-- activate the zebra look -->
  l_alv_display = l_pop_alv->get_display_settings( ).
  l_alv_display->set_striped_pattern( cl_salv_display_settings=>true ).
  l_alv_display->set_list_header( 'DELIVERY LIST' ).

  l_alv_functions = l_pop_alv->get_functions( ).
  l_alv_functions->set_all( abap_true ).


*    <!--  Set up selections.  -->
  l_alv_selections = l_pop_alv->get_selections( ).
  l_alv_selections->set_selection_mode( if_salv_c_selection_mode=>none )
  .

*    <!-- manage the grid layout -->
  l_alv_layout = l_pop_alv->get_layout( ).
  l_alv_layout->set_save_restriction( cl_salv_layout=>restrict_none ).
  l_alv_key-report = sy-repid.
  l_alv_key-handle = '1'.
*    l_alv_layout->set_key( l_alv_key ).

*    <!-- manage the fieldcatalog  -->
  l_alv_columns = l_pop_alv->get_columns( ).
  l_alv_columns->set_optimize( abap_true ).


  l_alv_aggregations = l_pop_alv->get_aggregations( ).
  TRY.
      l_alv_aggregations->add_aggregation( columnname = 'RFMNG' ).
    CATCH cx_salv_not_found cx_salv_data_error cx_salv_existing.
  ENDTRY.


  lv_start_column = 35.
  lv_end_column = 140.
  lv_start_line = 3.
  lv_end_line   = 15.

  l_pop_alv->set_screen_popup( EXPORTING start_column = lv_start_column
                                     end_column   = lv_end_column
                                     start_line   = lv_start_line
                                     end_line     = lv_end_line    ).

  l_pop_alv->display( ).


ENDFORM.
              
