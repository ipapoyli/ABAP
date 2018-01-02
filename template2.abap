*&---------------------------------------------------------------------*
*& Report Z_TEST_12
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_TEST_12.

************************************************************************
*               D A T A   D E C L A R A T I O N S                      *
************************************************************************
*----------------------------------------------------------------------*
* DDIC-Objects                                                         *
*----------------------------------------------------------------------*
*TABLES:

*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
*TYPE-POOLS: slis, sscr, icon.

*----------------------------------------------------------------------*
* Types                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF ITAB_TYPES ,
*--------------------------------------

*--------------------------------------
         CELLCOLOR TYPE LVC_T_SCOL.
TYPES: END OF ITAB_TYPES .


*----------------------------------------------------------------------*
* Global Variables                                                     *
*----------------------------------------------------------------------*
DATA: ITAB  TYPE TABLE OF ITAB_TYPES WITH HEADER LINE.


**---------------------------------------------------------------------*
** ALV Data screen 100                                                 *
**---------------------------------------------------------------------*
DATA:
  GS_FIELDCATALOG TYPE LVC_S_FCAT OCCURS 0,
  GV_FCAT         LIKE LINE OF GS_FIELDCATALOG,
  GS_LAYOUT       TYPE LVC_S_LAYO.

DATA: WA_CELLCOLOR TYPE LVC_S_SCOL.

* Reference variables to the ALV GRID and the container
DATA :
  G_CONTAINER        TYPE SCRFNAME VALUE 'CC_CONTAINER_GR',
  G_CUSTOM_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
  G_GRID             TYPE REF TO CL_GUI_ALV_GRID.

DATA:
  DG_DYNDOC_ID   TYPE REF TO CL_DD_DOCUMENT,            "Reference to document
  DG_SPLITTER    TYPE REF TO CL_GUI_SPLITTER_CONTAINER, "Reference to split container
  DG_PARENT_GRID TYPE REF TO CL_GUI_CONTAINER,          "Reference to grid container
  DG_HTML_CNTRL  TYPE REF TO CL_GUI_HTML_VIEWER,        "Reference to html container
  DG_PARENT_HTML TYPE REF TO CL_GUI_CONTAINER.          "Reference to html container

DATA: FIRST_DISPLAY.

DATA :OK_CODE LIKE SY-UCOMM,
      SAVE_OK LIKE SY-UCOMM.

DATA  GV_LINES   TYPE N.
DATA  T_ROWS     TYPE LVC_T_ROW.
DATA  S_ROW      TYPE LVC_S_ROW.
DATA  GV_MESS    TYPE C LENGTH 50.
DATA  GV_COUNTER TYPE I.
data  GV_COUNTER_CHAR(30).

*----------------------------------------------------------------------*
* Ranges                                                               *
*----------------------------------------------------------------------*
*RANGES R_CHARG FOR MCHB-CHARG.
*          REFRESH R_CHARG.
*          R_CHARG-LOW = ITAB_WM-CHARG.
*          R_CHARG-OPTION = 'EQ'.
*          R_CHARG-SIGN = 'I'.
*          APPEND R_CHARG TO R_CHARG.

***********************************************************************
*                S E L E C T I O N   S C R E E N
***********************************************************************


*----------------------------------------------------------------------*
* Class LCL_EVENT_RECEIVER DEFINITION                                  *
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER DEFINITION.
  PUBLIC SECTION.
    METHODS:
      HANDLE_TOOLBAR FOR EVENT TOOLBAR OF CL_GUI_ALV_GRID
        IMPORTING E_OBJECT E_INTERACTIVE,

      HANDLE_USER_COMMAND FOR EVENT USER_COMMAND OF CL_GUI_ALV_GRID
        IMPORTING E_UCOMM,
    
      HANDLE_HOTSPOT_CLICK FOR EVENT HOTSPOT_CLICK OF CL_GUI_ALV_GRID
        IMPORTING E_ROW_ID E_COLUMN_ID ES_ROW_NO,

      TOP_OF_PAGE FOR EVENT TOP_OF_PAGE OF CL_GUI_ALV_GRID
        IMPORTING E_DYNDOC_ID.
ENDCLASS.

*----------------------------------------------------------------------*
* Class LCL_EVENT_RECEIVER IMPLEMENTATION Methods                      *
*----------------------------------------------------------------------*
CLASS LCL_EVENT_RECEIVER IMPLEMENTATION.

*----------------------------------------------------------------------*
  METHOD handle_toolbar.

    DATA: lv_toolbar TYPE stb_button.
* Push Button Create txt
    CLEAR lv_toolbar.
    MOVE 'CTXT' TO lv_toolbar-FUNCTION.
    MOVE TEXT-c01 TO lv_toolbar-TEXT.
    MOVE TEXT-c02 TO lv_toolbar-quickinfo.
    MOVE icon_create_text TO lv_toolbar-ICON.
    MOVE ' ' TO lv_toolbar-disabled.
    APPEND lv_toolbar TO e_object->mt_toolbar.

  ENDMETHOD.
*----------------------------------------------------------------------*
* User Command
  METHOD handle_user_command.
    CASE e_ucomm.
    WHEN 'CTXT'.

      PERFORM select_rows.
      IF gv_lines NE 0.
      ELSE.
        MESSAGE TEXT-e02 TYPE 'S' DISPLAY LIKE 'e' .
      ENDIF.
    ENDCASE.
  ENDMETHOD.

*----------------------------------------------------------------------*
* Hotspot click control
  METHOD HANDLE_HOTSPOT_CLICK.
    CLEAR ITAB.
    READ TABLE ITAB INTO ITAB INDEX E_ROW_ID.
    IF SY-SUBRC = 0.
      CASE E_COLUMN_ID-FIELDNAME.
        WHEN 'CHRAG'.
*      IF E_COLUMN_ID-FIELDNAME = 'CHARG'.
*        SET PARAMETER ID 'MAT' FIELD XITAB-MATNR.
*        SET PARAMETER ID 'CHA' FIELD XITAB-CHARG.
*        SET PARAMETER ID 'WRK' FIELD XITAB-WERKS.
*        CALL TRANSACTION 'MSC3N' AND SKIP FIRST SCREEN.
*        SET PARAMETER ID 'MAT' FIELD SPACE.
*        SET PARAMETER ID 'CHA' FIELD SPACE.
*        SET PARAMETER ID 'WRK' FIELD SPACE.
*      ENDIF.
        WHEN 'CHARG'.
*          SUBMIT RLLT2400
*          WITH T2_LGNUM     = ITAB_WM-LGNUM
*          WITH T2_MATNR-LOW = ITAB_WM-MATNR
*          WITH T2_OFFTA     = ''
*          WITH T2_QUITA     = ''
*          WITH T2_ALLTA     = 'X'
*          WITH CHARG    = R_CHARG-LOW
*          AND RETURN .
        WHEN OTHERS.
      ENDCASE.
    ENDIF.
  ENDMETHOD .

* Top of Page
  METHOD TOP_OF_PAGE.
    PERFORM EVENT_TOP_OF_PAGE USING DG_DYNDOC_ID GV_COUNTER.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
DATA: GV_EVENT_RECEIVER    TYPE REF TO LCL_EVENT_RECEIVER.
*----------------------------------------------------------------------*

************************************************************************
*                          E V E N T S                                 *
************************************************************************
*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
  CLEAR GS_LAYOUT.
  PERFORM LAYOUT CHANGING GS_LAYOUT.

*----------------------------------------------------------------------
AT SELECTION-SCREEN OUTPUT.
*----------------------------------------------------------------------
  LOOP AT SCREEN.
*    IF screen-name = 'S_WERKS-LOW'.
*      screen-intensified = '1'.
*      MODIFY SCREEN.
*    ENDIF.
*    IF SCREEN-NAME = 'S_MEINS-HIGH'.
*      SCREEN-INVISIBLE = '1'.
*      SCREEN-ACTIVE = '0'.
*      MODIFY SCREEN.
*    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------
AT SELECTION-SCREEN.
*----------------------------------------------------------------------
  PERFORM CHECK_DATA.

*----------------------------------------------------------------------
START-OF-SELECTION.
*----------------------------------------------------------------------
*screen 100
  CALL SCREEN 100 .

*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT    PBO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  SET PF-STATUS 'STANDART'.
*  SET TITLEBAR 'TITLE'.

  REFRESH: ITAB.
  PERFORM GET_DATA .

  IF G_CUSTOM_CONTAINER IS INITIAL.

*----------------------------------------------------------------------*
    " Create CONTAINER object with reference to container name in the screen
    CREATE OBJECT G_CUSTOM_CONTAINER
      EXPORTING
        CONTAINER_NAME = G_CONTAINER.
*----------------------------------------------------------------------*
* Create TOP-Document (this is for top of page)
    CREATE OBJECT DG_DYNDOC_ID
      EXPORTING
        STYLE = 'ALV_GRID'.
*----------------------------------------------------------------------*
* Create Splitter for alv_container
    CREATE OBJECT DG_SPLITTER
      EXPORTING
        PARENT  = G_CUSTOM_CONTAINER
        ROWS    = 2
        COLUMNS = 1.
*----------------------------------------------------------------------*
* Split the alv_container to two containers and move the reference
* to receiving containers dg_parent_html and dg_parent_grid
    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 1
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_HTML.
**----------------------------------------------------------------------*
    CALL METHOD DG_SPLITTER->GET_CONTAINER
      EXPORTING
        ROW       = 2
        COLUMN    = 1
      RECEIVING
        CONTAINER = DG_PARENT_GRID.
**----------------------------------------------------------------------*
** Set height for dg_parent_html
    CALL METHOD DG_SPLITTER->SET_ROW_HEIGHT
      EXPORTING
        ID     = 1
        HEIGHT = 8.
**----------------------------------------------------------------------*
**Specify parent as splitter part for ALV grid creation
    CREATE OBJECT G_GRID
      EXPORTING
        I_PARENT = DG_PARENT_GRID.
**----------------------------------------------------------------------*

    IF FIRST_DISPLAY <> 'X'.
      CREATE OBJECT GV_EVENT_RECEIVER.
      SET HANDLER GV_EVENT_RECEIVER->HANDLE_HOTSPOT_CLICK FOR G_GRID.
      SET HANDLER GV_EVENT_RECEIVER->TOP_OF_PAGE FOR G_GRID.

      PERFORM U_PREPARE_FIELDCATALOG.

      " SET_TABLE_FOR_FIRST_DISPLAY
      CALL METHOD G_GRID->SET_TABLE_FOR_FIRST_DISPLAY
        EXPORTING
          IS_LAYOUT       = GS_LAYOUT
        CHANGING
          IT_FIELDCATALOG = GS_FIELDCATALOG
          IT_OUTTAB       = ITAB[]. " Data

      PERFORM PROCESS_TOP_OF_PAGE.

      FIRST_DISPLAY = 'X'.
    ENDIF.

  ELSE.

    IF FIRST_DISPLAY = 'X'.
      CALL METHOD G_GRID->REFRESH_TABLE_DISPLAY.
      PERFORM PROCESS_TOP_OF_PAGE.
    ENDIF.
  ENDIF.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CLEAR: OK_CODE.

  CASE OK_CODE .

    WHEN '&BACK' .
      LEAVE TO SCREEN 0.
    WHEN '&EXIT' OR '&CANSEL' .
      LEAVE PROGRAM .

    WHEN OTHERS.
  ENDCASE.

ENDMODULE.



*&---------------------------------------------------------------------*
*&      Form  U_PREPARE_FIELDCATALOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM U_PREPARE_FIELDCATALOG .

*  DATA LV_COL_POS TYPE LVC_COLPOS.
*  CLEAR LV_COL_POS.
*
*  CLEAR GV_FCAT.
*  ADD 1 TO LV_COL_POS.
*  GV_FCAT-FIELDNAME = 'WERKS'.
*  GV_FCAT-COL_POS = LV_COL_POS.
*  GV_FCAT-REF_TABLE = 'MCHB'.
*  GV_FCAT-REF_FIELD = 'WERKS'.
*  INSERT GV_FCAT INTO TABLE GS_FIELDCATALOG.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  GET_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM GET_DATA .

** Columns colors
*    IF ITAB-DIFMMWM < 0.
*      CLEAR WA_CELLCOLOR.
*      WA_CELLCOLOR-FNAME = 'DIFMMWM'.
*      WA_CELLCOLOR-COLOR-COL = 6.
*      WA_CELLCOLOR-COLOR-INT = 1.
*      APPEND WA_CELLCOLOR TO ITAB-CELLCOLOR.
*    ELSEIF ITAB-DIFMMWM > 0.
*      CLEAR WA_CELLCOLOR.
*      WA_CELLCOLOR-FNAME = 'DIFMMWM'.
*      WA_CELLCOLOR-COLOR-COL = 5.
*      WA_CELLCOLOR-COLOR-INT = 1.
*      APPEND WA_CELLCOLOR TO ITAB-CELLCOLOR.
*    ENDIF.
*
*    MODIFY ITAB.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  SELECT_ROWS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM SELECT_ROWS .

  CLEAR: GV_LINES,T_ROWS.
  CLEAR: T_ROWS , S_ROW .
  CALL METHOD G_GRID->GET_SELECTED_ROWS
    IMPORTING
      ET_INDEX_ROWS = T_ROWS.
  DESCRIBE TABLE T_ROWS LINES GV_LINES .

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CHECK_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM CHECK_DATA .

*  CLEAR GV_MESS.
*  CONCATENATE TEXT-E03 S_WERKS INTO GV_MESS SEPARATED BY SPACE.
*  MESSAGE GV_MESS TYPE 'E'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  EVENT_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_DG_DYNDOC_ID  text
*----------------------------------------------------------------------*
FORM EVENT_TOP_OF_PAGE  USING    P_DG_DYNDOC_ID  COUNTER.

  DATA : DL_TEXT(255) TYPE C.  "Text

  CLEAR : DL_TEXT.
  DL_TEXT = 'Compare MM with WM stock'.

  CALL METHOD DG_DYNDOC_ID->ADD_ICON(           "add icon
      SAP_ICON = 'ICON_WAREHOUSES'
      SAP_SIZE = CL_DD_AREA=>EXTRA_LARGE ).

  CALL METHOD DG_DYNDOC_ID->ADD_TEXT            "add text
    EXPORTING
      TEXT         = DL_TEXT
      SAP_FONTSIZE = CL_DD_AREA=>LARGE
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

  CALL METHOD DG_DYNDOC_ID->ADD_GAP             "add gap
    EXPORTING
      WIDTH = 50.

  CLEAR : DL_TEXT,GV_COUNTER_CHAR.
  WRITE COUNTER TO GV_COUNTER_CHAR.
  CONCATENATE TEXT-T02 GV_COUNTER_CHAR INTO DL_TEXT SEPARATED BY SPACE.
  CALL METHOD DG_DYNDOC_ID->ADD_TEXT
    EXPORTING
      TEXT         = DL_TEXT
      SAP_FONTSIZE = CL_DD_AREA=>MEDIUM
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*  CALL METHOD dg_dyndoc_id->new_line.           "Add new-line

  CALL METHOD DG_DYNDOC_ID->ADD_GAP              "add gap
    EXPORTING
      WIDTH = 50.

  CLEAR : DL_TEXT.
  WRITE SY-DATUM TO DL_TEXT.
  CALL METHOD DG_DYNDOC_ID->ADD_TEXT              "add text
    EXPORTING
      TEXT         = DL_TEXT
      SAP_FONTSIZE = CL_DD_AREA=>MEDIUM
      SAP_EMPHASIS = CL_DD_AREA=>STRONG.

*  CALL METHOD dg_dyndoc_id->new_line. " Add new-line

  PERFORM HTML.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  PROCESS_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM PROCESS_TOP_OF_PAGE.
  CALL METHOD DG_DYNDOC_ID->INITIALIZE_DOCUMENT. " Initializing document

  CALL METHOD G_GRID->LIST_PROCESSING_EVENTS                   " Processing events
    EXPORTING
      I_EVENT_NAME = 'TOP_OF_PAGE'
      I_DYNDOC_ID  = DG_DYNDOC_ID.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  HTML
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM HTML.

  DATA : DL_LENGTH        TYPE I, " Length
         DL_BACKGROUND_ID TYPE SDYDO_KEY VALUE SPACE. " Background_id

  IF DG_HTML_CNTRL IS INITIAL.                      " Creating html control
    CREATE OBJECT DG_HTML_CNTRL
      EXPORTING
        PARENT = DG_PARENT_HTML.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_GRID_COMMENTARY_SET'     " Reuse_alv_grid_commentary_set
    EXPORTING
      DOCUMENT = DG_DYNDOC_ID
      BOTTOM   = SPACE
    IMPORTING
      LENGTH   = DL_LENGTH.

  CALL METHOD DG_DYNDOC_ID->MERGE_DOCUMENT. " Get TOP->HTML_TABLE ready

  CALL METHOD DG_DYNDOC_ID->SET_DOCUMENT_BACKGROUND  " Set wallpaper
    EXPORTING
      PICTURE_ID = DL_BACKGROUND_ID.

  DG_DYNDOC_ID->HTML_CONTROL = DG_HTML_CNTRL.        " Connect TOP document to HTML-Control

  CALL METHOD DG_DYNDOC_ID->DISPLAY_DOCUMENT         " Display TOP document
    EXPORTING
      REUSE_CONTROL      = 'X'
      PARENT             = DG_PARENT_HTML
    EXCEPTIONS
      HTML_DISPLAY_ERROR = 1.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  LAYOUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      <--P_GS_LAYOUT  text
*----------------------------------------------------------------------*
FORM LAYOUT  CHANGING P_GS_LAYOUT TYPE LVC_S_LAYO.

  P_GS_LAYOUT-ZEBRA = 'X'.
  P_GS_LAYOUT-BOX_FNAME = 'SEL'.
  P_GS_LAYOUT-SEL_MODE = 'A' .
  P_GS_LAYOUT-CTAB_FNAME = 'CELLCOLOR'.
  P_GS_LAYOUT-COL_OPT = 'X'.  " optimize columns

ENDFORM.
