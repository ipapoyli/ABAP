**----------------------------------------------------------------------
** ALV Data first screen
**----------------------------------------------------------------------
* DECLARE REFERENCE VARIABLES TO THE ALV GRID AND THE CONTAINER
DATA :
  g_container        TYPE scrfname VALUE 'CC_CONTAINER_GR',
  g_custom_container TYPE REF TO cl_gui_custom_container,
  g_grid             TYPE REF TO cl_gui_alv_grid.

DATA:
  dg_dyndoc_id   TYPE REF TO cl_dd_document,            "Reference to document
  dg_splitter    TYPE REF TO cl_gui_splitter_container, "Reference to split container
  dg_parent_grid TYPE REF TO cl_gui_container,          "Reference to grid container
  dg_html_cntrl  TYPE REF TO cl_gui_html_viewer,        "Reference to html container
  dg_parent_html TYPE REF TO cl_gui_container.          "Reference to html container

DATA: first_display.
**----------------------------------------------------------------------
**----------------------------------------------------------------------
**----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Class LCL_EVENT_RECEIVER DEFINITION                                  *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS:
      top_of_page FOR EVENT top_of_page OF cl_gui_alv_grid
        IMPORTING e_dyndoc_id.
ENDCLASS.
*----------------------------------------------------------------------*
* Class LCL_EVENT_RECEIVER IMPLEMENTATION Methods                      *
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
* Top of Page
  METHOD top_of_page.
    PERFORM event_top_of_page USING dg_dyndoc_id
                                    gv_counter.
  ENDMETHOD.
ENDCLASS.

**----------------------------------------------------------------------
**----------------------------------------------------------------------
**----------------------------------------------------------------------
IF g_custom_container IS INITIAL.

*----------------------------------------------------------------------*
    " Create CONTAINER object with reference to container name in the screen
    CREATE OBJECT g_custom_container
      EXPORTING
        container_name = g_container.
*----------------------------------------------------------------------*
* Create TOP-Document (this is for top of page)
    CREATE OBJECT dg_dyndoc_id
      EXPORTING
        style = 'ALV_GRID'.
*----------------------------------------------------------------------*
* Create Splitter for alv_container
    CREATE OBJECT dg_splitter
      EXPORTING
        parent  = g_custom_container
        rows    = 2
        columns = 1.
*----------------------------------------------------------------------*
* Split the alv_container to two containers and move the reference
* to receiving containers dg_parent_html and dg_parent_grid
    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 1
        column    = 1
      RECEIVING
        container = dg_parent_html.
**----------------------------------------------------------------------*
    CALL METHOD dg_splitter->get_container
      EXPORTING
        row       = 2
        column    = 1
      RECEIVING
        container = dg_parent_grid.
**----------------------------------------------------------------------*
** Set height for dg_parent_html
    CALL METHOD dg_splitter->set_row_height
      EXPORTING
        id     = 1
        height = 8.
**----------------------------------------------------------------------*
**Specify parent as splitter part for ALV grid creation
    CREATE OBJECT g_grid
      EXPORTING
        i_parent = dg_parent_grid.
**----------------------------------------------------------------------*


CREATE OBJECT gv_event_receiver.
      SET HANDLER gv_event_receiver->top_of_page FOR g_grid.

      " SET_TABLE_FOR_FIRST_DISPLAY
      CALL METHOD g_grid->set_table_for_first_display
        EXPORTING
          is_layout       = gs_layout
        CHANGING
          it_fieldcatalog = gs_fieldcatalog
          it_outtab       = xitab[]. " Data

      PERFORM process_top_of_page.

      first_display = 'X'.
    ENDIF.

  ELSE.

    IF first_display = 'X'.
      CALL METHOD g_grid->refresh_table_display.
      PERFORM process_top_of_page.
    ENDIF.
  ENDIF.
**----------------------------------------------------------------------
**----------------------------------------------------------------------
**----------------------------------------------------------------------
FORM process_top_of_page.
  CALL METHOD dg_dyndoc_id->initialize_document. " Initializing document

  CALL METHOD g_grid->list_processing_events                   " Processing events
    EXPORTING
      i_event_name = 'TOP_OF_PAGE'
      i_dyndoc_id  = dg_dyndoc_id.
ENDFORM.
**----------------------------------------------------------------------
**----------------------------------------------------------------------
**----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DG_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM event_top_of_page  USING    p_dg_dyndoc_id  counter.

  DATA : dl_text(255) TYPE c.  "Text

  CLEAR : dl_text.
  dl_text = 'Compare MM with WM stock'.

  CALL METHOD dg_dyndoc_id->add_icon(           "add icon
      sap_icon = 'ICON_WAREHOUSES'
      sap_size = cl_dd_area=>extra_large ).

  CALL METHOD dg_dyndoc_id->add_text            "add text
    EXPORTING
      text         = dl_text
      sap_fontsize = cl_dd_area=>large
      sap_emphasis = cl_dd_area=>strong.

  CALL METHOD dg_dyndoc_id->add_gap             "add gap
    EXPORTING
      width = 50.

  CLEAR : dl_text,gv_counter_char.
  WRITE counter TO gv_counter_char.
  CONCATENATE TEXT-t02 gv_counter_char INTO dl_text SEPARATED BY space.
  CALL METHOD dg_dyndoc_id->add_text
    EXPORTING
      text         = dl_text
      sap_fontsize = cl_dd_area=>medium
      sap_emphasis = cl_dd_area=>strong.

*  CALL METHOD dg_dyndoc_id->new_line.           "Add new-line

  CALL METHOD dg_dyndoc_id->add_gap              "add gap
    EXPORTING
      width = 50.

  CLEAR : dl_text.
  WRITE sy-datum TO dl_text.
  CALL METHOD dg_dyndoc_id->add_text              "add text
    EXPORTING
      text         = dl_text
      sap_fontsize = cl_dd_area=>medium
      sap_emphasis = cl_dd_area=>strong.

*  CALL METHOD dg_dyndoc_id->new_line. " Add new-line

  PERFORM html.

ENDFORM.

**----------------------------------------------------------------------
**----------------------------------------------------------------------
**----------------------------------------------------------------------

*&---------------------------------------------------------------------*
*&      Form  HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM html.

  DATA : dl_length        TYPE i, " Length
         dl_background_id TYPE sdydo_key VALUE space. " Background_id

  IF dg_html_cntrl IS INITIAL.                      " Creating html control
    CREATE OBJECT dg_html_cntrl
      EXPORTING
        parent = dg_parent_html.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'     " Reuse_alv_grid_commentary_set
    EXPORTING
      document = dg_dyndoc_id
      bottom   = space
    IMPORTING
      length   = dl_length.

  CALL METHOD dg_dyndoc_id->merge_document. " Get TOP->HTML_TABLE ready

  CALL METHOD dg_dyndoc_id->set_document_background  " Set wallpaper
    EXPORTING
      picture_id = dl_background_id.

  dg_dyndoc_id->html_control = dg_html_cntrl.        " Connect TOP document to HTML-Control

  CALL METHOD dg_dyndoc_id->display_document         " Display TOP document
    EXPORTING
      reuse_control      = 'X'
      parent             = dg_parent_html
    EXCEPTIONS
      html_display_error = 1.


ENDFORM.

