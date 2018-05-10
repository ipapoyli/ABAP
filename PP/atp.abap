*&---------------------------------------------------------------------*
*& Report ZTK_TG_PP_056_ATP
************************************************************************
* Author : Papoulias Ioannis, Teka Systems S.A.
* Date   : 08/05/2018
*
* Purpose:  ATP
*
* Changes:
* [ Date  ] - [ Name      ] - [ Action                                 ]
* DD-Mon-YY   ?               ?
************************************************************************
REPORT ztk_tg_pp_056_atp.


* ----------------------------------------------------------------- *
* Class Definition Deferred.
* ----------------------------------------------------------------- *
CLASS lcl_display_salv   DEFINITION DEFERRED.
CLASS lcl_data           DEFINITION DEFERRED.
CLASS lcl_display_popup  DEFINITION DEFERRED.


* ----------------------------------------------------------------- *
* Tables
* ----------------------------------------------------------------- *
TABLES : marc,
         mara,
         ztk_tg_pp_056.

TYPE-POOLS: slis , col.

* ----------------------------------------------------------------- *
* Types
* ----------------------------------------------------------------- *
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

TYPES: BEGIN OF itab_coll_ty ,
*         status TYPE char1,
         icon      LIKE icon-id,
*----------------------------------------------*
         werks     LIKE marc-werks,
         matnr     LIKE mara-matnr,
         maktx     LIKE makt-maktx,
         bismt     LIKE mara-bismt,
         zeinr     LIKE mara-zeinr,
         mtart     LIKE mara-mtart,
         dispo     LIKE marc-dispo,
         meins     LIKE mara-meins,
         stock     LIKE mdps-mng01, " WE
         stock_e   LIKE mdps-mng01, " KB
         stock_f   LIKE mdps-mng01, " SH
         open_prs  LIKE mdps-mng01, " BA
         open_pos  LIKE mdps-mng01, " BE
         prod_ord  LIKE mdps-mng01, " FE
         plan_ord  LIKE mdps-mng01, " PA
         open_so   LIKE mdps-mng01, " VC
         deliver   LIKE mdps-mng01, " VG
         dep_req   LIKE mdps-mng01, " AR OR SB
         stock_bal LIKE mdps-mng01,
         mrp       LIKE mdps-mng01,
         aufnr     LIKE mdps-aufnr,
         dat01     LIKE mdps-dat01,
         kdauf     LIKE mdps-kdauf,
         kdpos     LIKE mdps-kdpos,
         kunnr     LIKE mdps-kunnr,
         name1     LIKE adrc-name1.
*----------------------------------------------*
*         t_color   TYPE lvc_s_scol.
TYPES END OF itab_coll_ty.

TYPES: BEGIN OF itab_ty,
         t_color TYPE lvc_t_scol.
    INCLUDE TYPE itab_coll_ty.
TYPES: END OF   itab_ty.


TYPES: t_itab_ty TYPE TABLE OF itab_ty.


TYPES: BEGIN OF det_ty,
         matnr  LIKE mara-matnr,
         meins  LIKE mara-meins,
         delkz  LIKE mdps-delkz,
         ddtext LIKE ddfixvalue-ddtext,
         dat00  LIKE mdps-dat00,
         dat01  LIKE mdps-dat01,
         mng01  LIKE mdps-mng01,
         sobkz  LIKE mdps-sobkz,
         delnr  LIKE mdps-delnr,
         delps  LIKE mdps-delps,
         del12  LIKE mdps-del12,
         aufvr  LIKE mdps-aufvr,
         lifnr  LIKE mdps-lifnr,
         name_l LIKE adrc-name1,
         kunnr  LIKE mdps-kunnr,
         name_k LIKE adrc-name1.
TYPES  END OF det_ty.

TYPES: t_det_ty TYPE TABLE OF det_ty.

TYPES: BEGIN OF ty_range,
         sign(1)   TYPE c,
         option(2) TYPE c,
         low       TYPE  equinr,
         high      TYPE  equinr,
       END   OF ty_range.

TYPES: ty_ranges TYPE TABLE OF ty_range.


* ----------------------------------------------------------------- *
* Global Data
* ----------------------------------------------------------------- *
DATA go_display    TYPE REF TO lcl_display_salv.
DATA go_data       TYPE REF TO lcl_data .
DATA go_popup      TYPE REF TO lcl_display_popup.


DATA: rs_delkz TYPE  ty_range,
      rt_delkz TYPE  ty_ranges.

* ----------------------------------------------------------------- *
** Data for the Popup Screen 100
* ----------------------------------------------------------------- *
DATA fcode  TYPE sy-ucomm.
DATA v_unit TYPE mara-meins.
DATA f_process TYPE c LENGTH 1.

* ----------------------------------------------------------------- *
* Class Definition
* ----------------------------------------------------------------- *

* ----------------------------------------------------------------- *
* DISPLAY
* ----------------------------------------------------------------- *
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

    DATA: l_salv_selections TYPE REF TO cl_salv_selections.

    DATA: l_salv_display TYPE REF TO cl_salv_display_settings.

    DATA f_success TYPE c LENGTH 1.

    DATA unit_ty TYPE c LENGTH 6.

    DATA :ok_code LIKE sy-ucomm.

* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS alv_display.

    METHODS get_alv   IMPORTING value TYPE char6
                      CHANGING  table TYPE ANY TABLE.

*    METHODS get_popup CHANGING table TYPE ANY TABLE.

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

    METHODS set_layout IMPORTING variant TYPE disvariant-variant .

    METHODS set_display_settings .

    METHODS set_selection_mode.

    METHODS set_status.

    METHODS fields_modification .

    METHODS check_selected_row EXPORTING v_success TYPE c
                                         v_index   TYPE i.

    METHODS set_hotspot IMPORTING columnname TYPE lvc_fname.

*   Set Top of page
    METHODS set_top_of_page CHANGING t_salv TYPE REF TO cl_salv_table.
*
*   Set End of page
    METHODS set_end_of_page CHANGING t_salv TYPE REF TO cl_salv_table.

ENDCLASS.

* ----------------------------------------------------------------- *
* DATA
* ----------------------------------------------------------------- *
CLASS lcl_data DEFINITION.

* ----------------------------------------------------------------- *
  PUBLIC SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Data
* ----------------------------------------------------------------- *
    DATA:  t_sel_params TYPE TABLE OF rsparams.
    DATA:  uplog_tab    TYPE TABLE OF uplog_typ.

    DATA:  t_itab       TYPE STANDARD TABLE OF itab_ty .
    DATA:  w_itab       TYPE itab_ty.

    DATA t_details  TYPE STANDARD TABLE OF det_ty.
    DATA w_details  TYPE det_ty.

    DATA:  t_itab_conv  TYPE STANDARD TABLE OF itab_ty.
* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS collect_data .
    METHODS display_messages.

    METHODS get_details IMPORTING v_matnr TYPE mara-matnr
                                  v_werks TYPE marc-werks
                                  v_delkz TYPE mdps-delkz .

    METHODS convert_quantities IMPORTING value TYPE char6
                               CHANGING  table TYPE t_itab_ty.

    METHODS convert_det_quant  IMPORTING value TYPE char6
                               CHANGING  table TYPE t_det_ty.

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

    METHODS check_update_z056 IMPORTING i_delkz TYPE delkz
                                        i_werks TYPE werks_d.

    METHODS convert_mat_unit IMPORTING i_matnr  TYPE mara-matnr
                                       i_in_me  TYPE mara-meins
                                       i_out_me TYPE mara-meins
                                       i_menge  TYPE ekpo-menge
                             EXPORTING e_menge  TYPE ekpo-menge.

    METHODS select_unit IMPORTING i_value TYPE char6
                                  i_matnr TYPE mara-matnr
                                  i_werks TYPE werks_d
                        EXPORTING i_unit  TYPE mara-meins.
ENDCLASS.


CLASS lcl_display_popup DEFINITION.
* ----------------------------------------------------------------- *
  PUBLIC SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Data
* ----------------------------------------------------------------- *
    DATA: lt_salv_dcl        TYPE REF TO cl_salv_table.
    DATA: l_salv_funct_pop   TYPE REF TO cl_salv_functions_list.
    DATA: l_salv_columns_pop TYPE REF TO cl_salv_columns_table.
    DATA: l_salv_column_pop  TYPE REF TO cl_salv_column_table.
    DATA: l_salv_aggrs_pop   TYPE REF TO cl_salv_aggregations.
    DATA: l_salv_display_pop TYPE REF TO cl_salv_display_settings.

* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS get_popup CHANGING table TYPE ANY TABLE.


* ----------------------------------------------------------------- *
  PRIVATE SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
    METHODS set_screen_popup.

    METHODS set_display_settings.

    METHODS field_modification.

ENDCLASS.

* ----------------------------------------------------------------- *
* Selection screen
* ----------------------------------------------------------------- *
SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME  .
PARAMETERS     p_werks LIKE marc-werks OBLIGATORY .

SELECTION-SCREEN SKIP.
SELECT-OPTIONS s_matnr FOR  mara-matnr .

SELECTION-SCREEN SKIP.

SELECT-OPTIONS s_dispo FOR marc-dispo.
SELECT-OPTIONS s_mtart FOR mara-mtart .

SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME .
PARAMETERS:     p_vari TYPE disvariant-variant.
SELECTION-SCREEN END OF BLOCK b02.

* ----------------------------------------------------------------- *
INITIALIZATION.
* ----------------------------------------------------------------- *
  CREATE OBJECT go_display.
  CREATE OBJECT go_data.
  CREATE OBJECT go_popup.

* ----------------------------------------------------------------- *
START-OF-SELECTION .
* ----------------------------------------------------------------- *
  go_data->collect_data( ).

*  go_data->display_messages( ).

  go_display->get_alv(  EXPORTING value = ' '
                        CHANGING table = go_data->t_itab ).
  go_display->set_handlers( ).
  go_display->alv_display( ).

* ----------------------------------------------------------------- *
END-OF-SELECTION.
* ----------------------------------------------------------------- *


* ----------------------------------------------------------------- *
* Class implementaion
* ----------------------------------------------------------------- *
CLASS lcl_data IMPLEMENTATION.

* ----------------------------------------------------------------- *
* Colllect Data
* ----------------------------------------------------------------- *
  METHOD collect_data.
    DATA t_mdpsx              TYPE STANDARD TABLE OF mdps .
    DATA w_mdpsx              TYPE mdps.
*    DATA t_mdezx              TYPE STANDARD TABLE OF mdez .
*    DATA t_mdsux              TYPE STANDARD TABLE OF mdsu .

    DATA t_itab_coll TYPE STANDARD TABLE OF itab_coll_ty.
    DATA w_itab_coll TYPE itab_coll_ty.

    DATA lt_color     TYPE lvc_t_scol.
    DATA ls_color     TYPE lvc_s_scol.

    FIELD-SYMBOLS <wa_itab>   TYPE itab_ty.
    FIELD-SYMBOLS <wa_itab_coll>   TYPE itab_coll_ty.


    REFRESH me->t_itab.

    SELECT marc~matnr, marc~werks, mara~mtart, marc~dispo
      FROM marc
      INNER JOIN mara ON marc~matnr EQ mara~matnr
      INTO TABLE @DATA(lt_material)
       WHERE marc~werks EQ @p_werks
         AND marc~matnr IN @s_matnr
         AND mara~mtart IN @s_mtart
         AND marc~dispo IN @s_dispo.


    IF lt_material[] IS INITIAL.
      CALL METHOD add_message
        EXPORTING
          i_msgid = 'ZTK_PP'
          i_msgty = 'E'
          i_msgno = '078'
          i_msgv1 = TEXT-e01
          i_msgv2 = ''
          i_msgv3 = ''
          i_msgv4 = ''.

      CALL METHOD display_messages( ).

      LEAVE LIST-PROCESSING.
    ENDIF.


    LOOP AT lt_material INTO DATA(wa_material).

      CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
        EXPORTING
*         PLSCN                    = PLSCN
          matnr                    = wa_material-matnr
          werks                    = wa_material-werks
*         BERID                    = BERID
*         ERGBZ                    = ERGBZ
*         AFIBZ                    = AFIBZ
*         INPER                    = INPER
*         DISPLAY_LIST_MDPSX       = DISPLAY_LIST_MDPSX
*         DISPLAY_LIST_MDEZX       = DISPLAY_LIST_MDEZX
*         DISPLAY_LIST_MDSUX       = DISPLAY_LIST_MDSUX
*         NOBUF                    = NOBUF
*         PLAUF                    = PLAUF
*         I_VRFWE                  = I_VRFWE
*         IS_SFILT                 = IS_SFILT
*         IS_AFILT                 = IS_AFILT
* IMPORTING
*         E_MT61D                  = E_MT61D
*         E_MDKP                   = E_MDKP
*         E_CM61M                  = E_CM61M
*         E_MDSTA                  = E_MDSTA
*         E_ERGBZ                  = E_ERGBZ
        TABLES
          mdpsx                    = t_mdpsx
*         mdezx                    = t_mdezx
*         mdsux                    = t_mdsux
        EXCEPTIONS
          material_plant_not_found = 1
          plant_not_found          = 2
          OTHERS                   = 3.

      IF sy-subrc <> 0.
        CALL METHOD add_message
          EXPORTING
            i_msgid = sy-msgid
            i_msgty = sy-msgty
            i_msgno = sy-msgno
            i_msgv1 = sy-msgv1
            i_msgv2 = sy-msgv2
            i_msgv3 = sy-msgv3
            i_msgv4 = sy-msgv4.

        CALL METHOD display_messages( ).
        EXIT.
      ENDIF.

      SORT t_mdpsx BY delkz kdauf kunnr.

      LOOP AT t_mdpsx INTO DATA(wa_mdpsx).
        CLEAR w_itab_coll.

        SELECT SINGLE * FROM ztk_tg_pp_056 INTO @DATA(wa_ztk_tg_pp_056)
          WHERE werks EQ @wa_material-werks
            AND delkz EQ @wa_mdpsx-delkz.

        IF sy-subrc NE 0.

          SELECT SINGLE * FROM ztk_tg_pp_056 INTO wa_ztk_tg_pp_056
              WHERE delkz EQ wa_mdpsx-delkz.

          IF sy-subrc NE 0.

            CALL METHOD check_update_z056
              EXPORTING
                i_delkz = wa_mdpsx-delkz
                i_werks = wa_material-werks.

          ENDIF.
        ENDIF.

        IF wa_ztk_tg_pp_056 IS NOT INITIAL.

          CASE wa_ztk_tg_pp_056-repkey.
            WHEN 'STOCK'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-stock.
            WHEN 'STOCK_E'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-stock_e.
            WHEN 'OPEN_PRS'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-open_prs.
            WHEN 'OPEN_POS'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-open_pos.
            WHEN 'PROD_ORD'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-prod_ord.
            WHEN 'PLAN_ORD'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-plan_ord.
            WHEN 'OPEN_SO'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-open_so.
            WHEN 'PLAN_ORD'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-plan_ord.
            WHEN 'DELIVER'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-deliver.
            WHEN 'DEP_REQ'.
              ADD wa_mdpsx-mng01 TO w_itab_coll-dep_req.
          ENDCASE.

          IF w_itab_coll IS NOT INITIAL.
            w_itab_coll-matnr = wa_material-matnr.
            w_itab_coll-werks = wa_material-werks.
            w_itab_coll-dispo = wa_material-dispo.
            w_itab_coll-mtart = wa_material-mtart.
            w_itab_coll-kdauf = wa_mdpsx-kdauf.
            w_itab_coll-kdpos = wa_mdpsx-kdpos.

            IF w_itab_coll-kdauf IS NOT INITIAL AND
               w_itab_coll-kdpos IS NOT INITIAL.

              w_itab_coll-kunnr = wa_mdpsx-kunnr.
            ENDIF.

            COLLECT w_itab_coll INTO t_itab_coll.
          ENDIF.

        ENDIF.

      ENDLOOP.


* ----------------------------------------------------------------- *
** get most recent date by material
* ----------------------------------------------------------------- *
      SORT t_mdpsx BY dat01 ASCENDING.
      LOOP AT t_itab_coll ASSIGNING <wa_itab_coll>
                          where matnr eq wa_material-matnr
                            and werks eq wa_material-werks.

        READ TABLE t_mdpsx INTO w_mdpsx WITH KEY delkz = 'FE'
                                                 kdauf = <wa_itab_coll>-kdauf
                                                 kdpos = <wa_itab_coll>-kdpos.

        IF sy-subrc EQ 0.
          <wa_itab_coll>-aufnr = w_mdpsx-del12.
          <wa_itab_coll>-dat01 = w_mdpsx-dat01.
        ELSE.
          READ TABLE t_mdpsx INTO w_mdpsx WITH KEY delkz = 'BE'
                                                 kdauf = <wa_itab_coll>-kdauf
                                                 kdpos = <wa_itab_coll>-kdpos.
          IF sy-subrc EQ 0.
            <wa_itab_coll>-aufnr = w_mdpsx-delnr.
            <wa_itab_coll>-dat01 = w_mdpsx-dat01.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

    MOVE-CORRESPONDING t_itab_coll TO t_itab.
* ----------------------------------------------------------------- *
* extra information
* ----------------------------------------------------------------- *
    LOOP AT t_itab ASSIGNING <wa_itab>.

      IF <wa_itab>-kdauf IS INITIAL AND
         <wa_itab>-kdpos IS INITIAL.

        SELECT SINGLE eisbe FROM marc INTO <wa_itab>-stock_f
          WHERE matnr EQ <wa_itab>-matnr
            AND werks EQ <wa_itab>-werks.

      ENDIF.

* get material description
      SELECT SINGLE maktx FROM makt INTO <wa_itab>-maktx
        WHERE matnr EQ <wa_itab>-matnr
          AND spras EQ sy-langu.

* get old material number
      SELECT SINGLE zeinr bismt FROM mara
        INTO CORRESPONDING FIELDS OF <wa_itab>
         WHERE matnr EQ <wa_itab>-matnr.

* get customer name
      SELECT SINGLE name1 FROM kna1 INTO <wa_itab>-name1
        WHERE kunnr EQ <wa_itab>-kunnr.


* get stock Balance
      <wa_itab>-stock_bal =  <wa_itab>-stock + <wa_itab>-stock_e + <wa_itab>-open_pos +
                             <wa_itab>-prod_ord - <wa_itab>-open_so - <wa_itab>-deliver -
                             <wa_itab>-dep_req.

* get mrp
      <wa_itab>-mrp = <wa_itab>-stock_bal + <wa_itab>-open_prs + <wa_itab>-plan_ord -
                      <wa_itab>-stock_f.

* get expeptions
      DATA(lv_sum) = <wa_itab>-stock + <wa_itab>-stock_e -
                     <wa_itab>-open_so - <wa_itab>-deliver.

      IF lv_sum >= 0.
        MOVE icon_green_light TO <wa_itab>-icon .  " green

      ELSEIF <wa_itab>-stock_bal >= 0.
        MOVE icon_yellow_light TO <wa_itab>-icon .  " yellow

      ELSEIF <wa_itab>-stock_bal < 0.
        MOVE icon_red_light TO <wa_itab>-icon .  " red

      ENDIF.

* get base unit
      SELECT SINGLE meins FROM mara INTO <wa_itab>-meins
        WHERE matnr EQ <wa_itab>-matnr.

* set collors
      CLEAR lt_color.
      CLEAR ls_color.
      IF <wa_itab>-mrp < 0.

        ls_color-fname     = 'MRP'.
        ls_color-color-col = 7.
        ls_color-color-int = 1.
        ls_color-color-inv = 0.
        APPEND ls_color TO lt_color.

        <wa_itab>-t_color = lt_color.
      ENDIF.

    ENDLOOP.

    SORT t_itab BY icon DESCENDING.
*    IF me->uplog_tab IS NOT INITIAL.
*      CALL METHOD display_messages( ).
*    ENDIF.

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
* Get Details
* ----------------------------------------------------------------- *
  METHOD get_details.
    DATA lt_mdpsx              TYPE STANDARD TABLE OF mdps .

    DATA : lo_element TYPE REF TO cl_abap_elemdescr,
           li_values  TYPE ddfixvalues,
           ls_value   TYPE ddfixvalue.


    REFRESH t_details.

    CALL FUNCTION 'MD_STOCK_REQUIREMENTS_LIST_API'
      EXPORTING
*       PLSCN                    = PLSCN
        matnr                    = v_matnr
        werks                    = v_werks
*       BERID                    = BERID
*       ERGBZ                    = ERGBZ
*       AFIBZ                    = AFIBZ
*       INPER                    = INPER
*       DISPLAY_LIST_MDPSX       = DISPLAY_LIST_MDPSX
*       DISPLAY_LIST_MDEZX       = DISPLAY_LIST_MDEZX
*       DISPLAY_LIST_MDSUX       = DISPLAY_LIST_MDSUX
*       NOBUF                    = NOBUF
*       PLAUF                    = PLAUF
*       I_VRFWE                  = I_VRFWE
*       IS_SFILT                 = IS_SFILT
*       IS_AFILT                 = IS_AFILT
* IMPORTING
*       E_MT61D                  = E_MT61D
*       E_MDKP                   = E_MDKP
*       E_CM61M                  = E_CM61M
*       E_MDSTA                  = E_MDSTA
*       E_ERGBZ                  = E_ERGBZ
      TABLES
        mdpsx                    = lt_mdpsx
*       mdezx                    = t_mdezx
*       mdsux                    = t_mdsux
      EXCEPTIONS
        material_plant_not_found = 1
        plant_not_found          = 2
        OTHERS                   = 3.

    IF sy-subrc <> 0.
      CALL METHOD add_message
        EXPORTING
          i_msgid = sy-msgid
          i_msgty = sy-msgty
          i_msgno = sy-msgno
          i_msgv1 = sy-msgv1
          i_msgv2 = sy-msgv2
          i_msgv3 = sy-msgv3
          i_msgv4 = sy-msgv4.

      CALL METHOD display_messages( ).
      EXIT.
    ENDIF.

    IF v_delkz NE 'AR'.
      CLEAR: rt_delkz, rs_delkz.

      rs_delkz-sign   = 'I'.
      rs_delkz-option = 'EQ'.
      rs_delkz-low    = v_delkz.
      APPEND rs_delkz TO rt_delkz.

    ELSE.

      CLEAR: rt_delkz, rs_delkz.

      rs_delkz-sign   = 'I'.
      rs_delkz-option = 'EQ'.
      rs_delkz-low    = 'AR'.
      APPEND rs_delkz TO rt_delkz.

      rs_delkz-sign   = 'I'.
      rs_delkz-option = 'EQ'.
      rs_delkz-low    = 'SB'.
      APPEND rs_delkz TO rt_delkz.

    ENDIF.



    LOOP AT lt_mdpsx INTO DATA(wa_mdpsx) WHERE delkz IN rt_delkz.
      MOVE-CORRESPONDING wa_mdpsx TO w_details.

      lo_element ?= cl_abap_typedescr=>describe_by_data( wa_mdpsx-delkz ) .
      li_values =  lo_element->get_ddic_fixed_values( sy-langu ) .
      READ TABLE li_values INTO ls_value WITH KEY low = wa_mdpsx-delkz .
      IF sy-subrc = 0 .
        w_details-ddtext = ls_value-ddtext .
      ENDIF.

      IF w_details-lifnr IS NOT INITIAL.
        SELECT SINGLE name1 FROM lfa1 INTO w_details-name_l
          WHERE lifnr EQ w_details-lifnr.
      ENDIF.

      IF w_details-kunnr IS NOT INITIAL.
        SELECT SINGLE name1 FROM kna1 INTO w_details-name_k
          WHERE kunnr EQ w_details-kunnr.
      ENDIF.

      SELECT SINGLE meins FROM mara INTO w_details-meins
        WHERE matnr EQ v_matnr.

      w_details-matnr = v_matnr.

      APPEND w_details TO t_details.

    ENDLOOP.


  ENDMETHOD.


* ----------------------------------------------------------------- *
* Convertion
* ----------------------------------------------------------------- *
  METHOD convert_quantities.
    DATA i_out_me  TYPE mara-meins.
    DATA lv_out_me TYPE ekpo-meins.

    REFRESH table.

    LOOP AT me->t_itab INTO w_itab.

      select_unit( EXPORTING i_value = value
                             i_matnr = w_itab-matnr
                             i_werks = w_itab-werks
                   IMPORTING i_unit  = lv_out_me ) .

      IF lv_out_me IS NOT INITIAL.
        IF w_itab-stock IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-stock
                            IMPORTING e_menge  = w_itab-stock ) .
        ENDIF.

        IF w_itab-stock_e IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-stock_e
                            IMPORTING e_menge  = w_itab-stock_e ) .
        ENDIF.

        IF w_itab-stock_f IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-stock_f
                            IMPORTING e_menge  = w_itab-stock_f ) .
        ENDIF.

        IF w_itab-open_prs IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-open_prs
                            IMPORTING e_menge  = w_itab-open_prs ) .
        ENDIF.

        IF w_itab-open_pos IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-open_pos
                            IMPORTING e_menge  = w_itab-open_pos ) .
        ENDIF.

        IF w_itab-prod_ord IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-prod_ord
                            IMPORTING e_menge  = w_itab-prod_ord ) .
        ENDIF.

        IF w_itab-plan_ord IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-plan_ord
                            IMPORTING e_menge  = w_itab-plan_ord ) .
        ENDIF.

        IF w_itab-open_so IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-open_so
                            IMPORTING e_menge  = w_itab-open_so ) .
        ENDIF.

        IF w_itab-deliver IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-deliver
                            IMPORTING e_menge  = w_itab-deliver ) .
        ENDIF.

        IF w_itab-dep_req IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-dep_req
                            IMPORTING e_menge  = w_itab-dep_req ) .
        ENDIF.

        IF w_itab-stock_bal IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-stock_bal
                            IMPORTING e_menge  = w_itab-stock_bal ) .
        ENDIF.

        IF w_itab-mrp IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_itab-matnr
                                      i_in_me  = w_itab-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_itab-mrp
                            IMPORTING e_menge  = w_itab-mrp ) .
        ENDIF.

        w_itab-meins = lv_out_me.
      ENDIF.

      APPEND w_itab TO table.
    ENDLOOP.

  ENDMETHOD.

* ----------------------------------------------------------------- *
* Details Convertion
* ----------------------------------------------------------------- *
  METHOD convert_det_quant.

    DATA i_out_me  TYPE mara-meins.
    DATA lv_out_me TYPE ekpo-meins.


    LOOP AT table INTO w_details.

      select_unit( EXPORTING i_value = value
                             i_matnr = w_itab-matnr
                             i_werks = w_itab-werks
                   IMPORTING i_unit = lv_out_me ) .


      IF lv_out_me IS NOT INITIAL.
        IF w_details-mng01 IS NOT INITIAL.
          convert_mat_unit( EXPORTING i_matnr  = w_details-matnr
                                      i_in_me  = w_details-meins
                                      i_out_me = lv_out_me
                                      i_menge  = w_details-mng01
                            IMPORTING e_menge  = w_details-mng01 ) .
        ENDIF.

        w_details-meins = lv_out_me.
      ENDIF.

      MODIFY table FROM w_details.
    ENDLOOP.

  ENDMETHOD.

* ----------------------------------------------------------------- *
* Material Convertion
* ----------------------------------------------------------------- *
  METHOD convert_mat_unit.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = i_matnr
        i_in_me              = i_in_me
        i_out_me             = i_out_me
        i_menge              = i_menge
      IMPORTING
        e_menge              = e_menge
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      CALL METHOD add_message
        EXPORTING
          i_msgid = sy-msgid
          i_msgty = sy-msgty
          i_msgno = sy-msgno
          i_msgv1 = sy-msgv1
          i_msgv2 = sy-msgv2
          i_msgv3 = sy-msgv3
          i_msgv4 = sy-msgv4.

    ENDIF.


  ENDMETHOD.

* ----------------------------------------------------------------- *
* Select Unit
* ----------------------------------------------------------------- *
  METHOD select_unit.

    CASE i_value.
      WHEN 'SUNIT'.

        SELECT SINGLE * FROM tvkwz INTO @DATA(lv_tvkwz)
          WHERE werks EQ @i_werks.

        CLEAR i_unit.
        SELECT SINGLE vrkme FROM mvke INTO i_unit
          WHERE matnr EQ i_matnr
            AND vkorg EQ lv_tvkwz-vkorg
            AND vrkme NE ' '.

        IF sy-subrc EQ 0.
          SELECT SINGLE * FROM marm INTO @DATA(ls_marm)
                WHERE matnr EQ @i_matnr
                AND meinh EQ @i_unit.

          IF sy-subrc NE 0.
            CLEAR i_unit.
          ENDIF.
        ENDIF.

      WHEN 'PRUNIT'.
        CLEAR i_unit.
        SELECT SINGLE frtme FROM marc INTO i_unit
          WHERE matnr EQ i_matnr
            AND werks EQ i_werks.

        IF sy-subrc EQ 0.
          CLEAR ls_marm.
          SELECT SINGLE * FROM marm INTO ls_marm
            WHERE matnr EQ i_matnr
              AND meinh EQ i_unit.

          IF sy-subrc NE 0.
            CLEAR i_unit.
          ENDIF.
        ENDIF.

      WHEN 'OUNIT'.
        i_unit = v_unit.


      WHEN OTHERS.
        CLEAR i_unit.

    ENDCASE.

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

* ----------------------------------------------------------------- *
* Check and update ZTK_TG_PP_056
* ----------------------------------------------------------------- *
  METHOD check_update_z056.
    DATA : lo_element TYPE REF TO cl_abap_elemdescr,
           li_values  TYPE ddfixvalues,
           ls_value   TYPE ddfixvalue.

    DATA wa_ztk_tg_pp_056 TYPE ztk_tg_pp_056.


    CLEAR wa_ztk_tg_pp_056.

    lo_element ?= cl_abap_typedescr=>describe_by_data( i_delkz ) .
    li_values =  lo_element->get_ddic_fixed_values( sy-langu ) .
    READ TABLE li_values INTO ls_value WITH KEY low = i_delkz .
    IF sy-subrc = 0 .
      wa_ztk_tg_pp_056-ddtext = ls_value-ddtext .
    ENDIF.

*    wa_ztk_tg_pp_056-werks = i_werks.
    wa_ztk_tg_pp_056-delkz = i_delkz.

    MODIFY ztk_tg_pp_056 FROM wa_ztk_tg_pp_056.

    CALL METHOD add_message
      EXPORTING
        i_msgid = 'ZTK_PP'
        i_msgty = 'S'
        i_msgno = '079'
        i_msgv1 = i_delkz
        i_msgv2 = i_werks
        i_msgv3 = ''
        i_msgv4 = ''.

  ENDMETHOD.

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

    me->unit_ty = value.

* --------------------------------------------------------------- *
*   Creating the SALV Object
* --------------------------------------------------------------- *
    TRY.

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
*  COLUMN  modifications
* --------------------------------------------------------------- *
    fields_modification( ).


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
*    set_top_of_page( CHANGING t_salv = t_salv_dcl ).
*


* --------------------------------------------------------------- *
*   set footer
* --------------------------------------------------------------- *
**   Calling the End of Page method
*    CALL METHOD me->set_end_of_page
*      CHANGING
*        t_salv = t_salv_dcl.

  ENDMETHOD.

* ----------------------------------------------------------------- *
* Get Popup
* ----------------------------------------------------------------- *
*  METHOD get_popup.
*    DATA: lt_salv_dcl        TYPE REF TO cl_salv_table.
*    DATA: l_salv_funct_pop   TYPE REF TO cl_salv_functions_list.
*    DATA: l_salv_columns_pop TYPE REF TO cl_salv_columns_table.
*    DATA: l_salv_aggrs_pop   TYPE REF TO cl_salv_aggregations.
*    DATA: l_salv_display_pop TYPE REF TO cl_salv_display_settings.
*
*
** --------------------------------------------------------------- *
**   Creating the SALV Object
** --------------------------------------------------------------- *
*    TRY.
*
*        cl_salv_table=>factory( IMPORTING r_salv_table = lt_salv_dcl
*                                 CHANGING t_table = table ).
*
*      CATCH cx_salv_msg.
*    ENDTRY.
*
*    CHECK sy-subrc EQ 0.
*
*    l_salv_funct_pop = lt_salv_dcl->get_functions( ).
*    l_salv_funct_pop->set_all( if_salv_c_bool_sap=>true ).
*
*
*
*** --------------------------------------------------------------- *
***   In case you want to change some column attributes
*** --------------------------------------------------------------- *
*    l_salv_columns_pop  = lt_salv_dcl->get_columns( ).
*    l_salv_columns_pop->set_optimize( abap_true ).
**
*    l_salv_aggrs_pop    = lt_salv_dcl->get_aggregations( ).
*
**  TRY.
**  CALL METHOD gr_table->set_screen_status
**    EXPORTING
**      report        = 'SALV_DEMO_TABLE_EVENTS'
**      pfstatus      = 'D0100'
***      SET_FUNCTIONS = C_FUNCTIONS_NONE
*    .
**  ENDTRY.
*
*
** ----------------------------------------------------------------- *
** Display settings
** ----------------------------------------------------------------- *
*    l_salv_display_pop = lt_salv_dcl->get_display_settings( ).
** zebra stripes
*    l_salv_display_pop->set_striped_pattern( cl_salv_display_settings=>true ).
** Title
*    l_salv_display_pop->set_list_header( 'Details' ).
*
*
**... §4.1 set the size and position of the Popup via coordinates
*    lt_salv_dcl->set_screen_popup(
*                                start_column = 5
*                                end_column   = 100
*                                start_line   = 3
*                                end_line     = 20 ).
*
***... §4.2 set the selection mode of the Popup: multiple or single row selection
**    IF gs_test-selection EQ gc_true.
**      DATA: lr_selections TYPE REF TO cl_salv_selections.
**
**      lr_selections = gr_table->get_selections( ).
**      lr_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).
**    ENDIF.
*
**... §5 display the table
*    lt_salv_dcl->display( ).
**
**      lr_selections = gr_table->get_selections( ).
**      data: lt_sel type salv_t_row.
**      lt_sel = lr_selections->get_selected_rows( ).
*  ENDMETHOD.


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

    DATA wa_itab TYPE itab_ty.

    CLEAR wa_itab.
    READ TABLE go_data->t_itab INTO wa_itab INDEX row.

    IF sy-subrc = 0.
      CASE column.

        WHEN 'OPEN_PRS'.
          CHECK wa_itab-open_prs IS NOT INITIAL.

          go_data->get_details( EXPORTING v_matnr = wa_itab-matnr
                                           v_werks = wa_itab-werks
                                           v_delkz = 'BA' ).

          IF unit_ty IS NOT INITIAL.
            go_data->convert_det_quant( EXPORTING value = unit_ty
                                          CHANGING table = go_data->t_details ).
          ENDIF.

          go_popup->get_popup( CHANGING table = go_data->t_details ).

        WHEN 'OPEN_POS'.
          CHECK wa_itab-open_pos IS NOT INITIAL.

          go_data->get_details( EXPORTING v_matnr = wa_itab-matnr
                                          v_werks = wa_itab-werks
                                          v_delkz = 'BE' ).

          IF unit_ty IS NOT INITIAL.
            go_data->convert_det_quant( EXPORTING value = unit_ty
                                         CHANGING table = go_data->t_details ).
          ENDIF.

          go_popup->get_popup( CHANGING table = go_data->t_details ).

        WHEN 'PROD_ORD'.
          CHECK wa_itab-prod_ord IS NOT INITIAL.
          go_data->get_details( EXPORTING v_matnr = wa_itab-matnr
                                          v_werks = wa_itab-werks
                                          v_delkz = 'FE' ).

          IF unit_ty IS NOT INITIAL.
            go_data->convert_det_quant( EXPORTING value = unit_ty
                                         CHANGING table = go_data->t_details ).
          ENDIF.

          go_popup->get_popup( CHANGING table = go_data->t_details ).

        WHEN 'PLAN_ORD'.
          CHECK wa_itab-plan_ord IS NOT INITIAL.
          go_data->get_details( EXPORTING v_matnr = wa_itab-matnr
                                          v_werks = wa_itab-werks
                                          v_delkz = 'PA' ).

          IF unit_ty IS NOT INITIAL.
            go_data->convert_det_quant( EXPORTING value = unit_ty
                                         CHANGING table = go_data->t_details ).
          ENDIF.

          go_popup->get_popup( CHANGING table = go_data->t_details ).

        WHEN 'OPEN_SO' .
          CHECK wa_itab-open_so IS NOT INITIAL.
          go_data->get_details( EXPORTING v_matnr = wa_itab-matnr
                                          v_werks = wa_itab-werks
                                          v_delkz = 'VC' ).
          IF unit_ty IS NOT INITIAL.
            go_data->convert_det_quant( EXPORTING value = unit_ty
                                         CHANGING table = go_data->t_details ).
          ENDIF.

          go_popup->get_popup( CHANGING table = go_data->t_details ).

        WHEN 'DELIVER' .
          CHECK wa_itab-deliver IS NOT INITIAL.
          go_data->get_details( EXPORTING v_matnr = wa_itab-matnr
                                          v_werks = wa_itab-werks
                                          v_delkz = 'VJ' ).

          IF unit_ty IS NOT INITIAL.
            go_data->convert_det_quant( EXPORTING value = unit_ty
                                         CHANGING table = go_data->t_details ).
          ENDIF.

          go_popup->get_popup( CHANGING table = go_data->t_details ).

        WHEN 'DEP_REQ' .
          CHECK wa_itab-dep_req IS NOT INITIAL.
          go_data->get_details( EXPORTING v_matnr = wa_itab-matnr
                                           v_werks = wa_itab-werks
                                           v_delkz = 'AR' ).

          IF unit_ty IS NOT INITIAL.
            go_data->convert_det_quant( EXPORTING value = unit_ty
                                         CHANGING table = go_data->t_details ).
          ENDIF.


          go_popup->get_popup( CHANGING table = go_data->t_details ).

      ENDCASE.

      CLEAR unit_ty.

    ENDIF.
  ENDMETHOD .


  METHOD handle_on_user_command.
    DATA lv_index TYPE i.

    CASE e_salv_function.
* ------------------------------------------------------------------- *
* Transactions
* ------------------------------------------------------------------- *
      WHEN '&BUT1'.

        check_selected_row( IMPORTING v_success = f_success
                                      v_index   = lv_index ).

        IF f_success EQ 'X'.
          DATA(lv_matnr) = go_data->t_itab[ lv_index ]-matnr.
          DATA(lv_werks) = go_data->t_itab[ lv_index ]-werks.

          SET PARAMETER ID 'MAT' FIELD lv_matnr.
          SET PARAMETER ID 'WRK' FIELD lv_werks.
          CALL TRANSACTION 'MD04' AND SKIP FIRST SCREEN.
          SET PARAMETER ID 'MAT' FIELD space.
          SET PARAMETER ID 'WRK' FIELD space.

        ENDIF.

      WHEN '&BUT2'.

        check_selected_row( IMPORTING v_success = f_success
          v_index   = lv_index ).

        IF f_success EQ 'X'.
          lv_matnr = go_data->t_itab[ lv_index ]-matnr.
          lv_werks = go_data->t_itab[ lv_index ]-werks.

          SET PARAMETER ID 'MAT' FIELD lv_matnr.
          SET PARAMETER ID 'WRK' FIELD lv_werks.
          CALL TRANSACTION 'MB52' AND SKIP FIRST SCREEN.
          SET PARAMETER ID 'MAT' FIELD space.
          SET PARAMETER ID 'WRK' FIELD space.

        ENDIF.

* ------------------------------------------------------------------- *
* Convertion Unit
* ------------------------------------------------------------------- *
      WHEN '&BUNIT'.
        go_display->get_alv( EXPORTING value = ' '
                             CHANGING  table  = go_data->t_itab ).
        go_display->set_handlers( ).
        go_display->alv_display( ).

      WHEN '&SUNIT'.

        go_data->convert_quantities( EXPORTING value = 'SUNIT'
                                     CHANGING table    = go_data->t_itab_conv ).

        go_display->get_alv( EXPORTING value = 'SUNIT'
                             CHANGING  table  = go_data->t_itab_conv ).
        go_display->set_handlers( ).
        go_display->alv_display( ).

      WHEN '&PRUNIT'.
        go_data->convert_quantities(
                                    EXPORTING value = 'PRUNIT'
                                    CHANGING table    = go_data->t_itab_conv ).

        go_display->get_alv( EXPORTING value = 'PRUNIT'
                             CHANGING  table  = go_data->t_itab_conv ).
        go_display->set_handlers( ).
        go_display->alv_display( ).


      WHEN '&OUNIT'.
        CLEAR: v_unit, f_process.
        CALL SCREEN 0100 STARTING AT 45 5 ENDING AT 85 10.

        IF f_process EQ 'X'.

          go_data->convert_quantities(
                                      EXPORTING value = 'OUNIT'
                                      CHANGING table    = go_data->t_itab_conv ).

          go_display->get_alv( EXPORTING value = 'OUNIT'
                               CHANGING  table  = go_data->t_itab_conv ).
          go_display->set_handlers( ).
          go_display->alv_display( ).

        ENDIF.

* ------------------------------------------------------------------- *
* Messages
* ------------------------------------------------------------------- *
      WHEN '&LOG'.
        CHECK go_data->uplog_tab[] IS NOT INITIAL.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          TABLES
            i_message_tab = go_data->uplog_tab.

    ENDCASE.

* ------------------------------------------------------------------- *
* Using Function Keys
* ------------------------------------------------------------------- *
    CASE sy-ucomm .
      WHEN '&F03'.
        LEAVE TO SCREEN 0.
      WHEN '&F15' OR '&F12' .
        LEAVE PROGRAM .
      WHEN OTHERS.
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
    l_salv_display->set_list_header( 'ATP Report' ).

  ENDMETHOD.


* ----------------------------------------------------------------- *
* Selection mode
* ----------------------------------------------------------------- *
  METHOD set_selection_mode.
    l_salv_selections = t_salv_dcl->get_selections( ).

    l_salv_selections->set_selection_mode( if_salv_c_selection_mode=>row_column ).

  ENDMETHOD.

* ----------------------------------------------------------------- *
* Set status
* ----------------------------------------------------------------- *
  METHOD set_status.
    t_salv_dcl->set_screen_status(
    pfstatus      =  'STANDARD2'
    report        =  sy-repid
    set_functions =  t_salv_dcl->c_functions_all ).
  ENDMETHOD.

* ----------------------------------------------------------------- *
* Fields modification
* ----------------------------------------------------------------- *
  METHOD fields_modification.

** --------------------------------------------------------------- *
**   In case you want to change some column attributes
** --------------------------------------------------------------- *
    l_salv_columns  = t_salv_dcl->get_columns( ).
    l_salv_columns->set_optimize( abap_true ).
*
    l_salv_aggrs    = t_salv_dcl->get_aggregations( ).
*
***   Add TOTAL for COLUMN NETWR
**    TRY.
**        CALL METHOD lo_aggrs->add_aggregation
**          EXPORTING
**            columnname  = 'NETWR'
**            aggregation = if_salv_c_aggregation=>total.
**      CATCH cx_salv_data_error .                        "#EC NO_HANDLER
**      CATCH cx_salv_not_found .                         "#EC NO_HANDLER
**      CATCH cx_salv_existing .                          "#EC NO_HANDLER
**    ENDTRY.
****
***   Bring the total line to top
**    lo_aggrs->set_aggregation_before_items( ).
*
*
*    TRY.
** ------------------------------------------------------------------ *
*****set key
** ------------------------------------------------------------------ *
*    TRY.
*        l_salv_column ?= l_salv_columns->get_column( 'MATNR' ).
*        l_salv_column->set_key( ).
*      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
*    ENDTRY.
*    TRY.
*        l_salv_column ?= l_salv_columns->get_column( 'WERKS' ).
*        l_salv_column->set_key( ).
*      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
*    ENDTRY.

** ------------------------------------------------------------------ *
*****set exception column
** ------------------------------------------------------------------ *
*    TRY.
*        l_salv_columns->set_exception_column( 'STATUS' ).
*      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
*    ENDTRY.
*    TRY.
*        l_salv_column ?= l_salv_columns->get_column( 'STATUS' ).
*        l_salv_column->set_short_text( 'Status' ).
*        l_salv_column->set_medium_text( 'Status' ).
*        l_salv_column->set_long_text( 'Status' ).
*      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
*    ENDTRY.
*
** ------------------------------------------------------------------ *
*****set icon
** ------------------------------------------------------------------ *
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'ICON' ).
        l_salv_column->set_icon( if_salv_c_bool_sap=>true ).
        l_salv_column->set_short_text( 'Status' ).
        l_salv_column->set_medium_text( 'Status' ).
        l_salv_column->set_long_text( 'Status' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
** ------------------------------------------------------------------ *
*****set columns name
** ------------------------------------------------------------------ *
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'STOCK' ).
        l_salv_column->set_short_text( 'Unr. Stock' ).
        l_salv_column->set_medium_text( 'Unrestricted Stock' ).
        l_salv_column->set_long_text( 'Unrestricted Stock' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'STOCK_E' ).
        l_salv_column->set_short_text( 'Sales St.' ).
        l_salv_column->set_medium_text( 'Sales Stock' ).
        l_salv_column->set_long_text( 'Sales Stock' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'STOCK_F' ).
        l_salv_column->set_short_text( 'Safety St.' ).
        l_salv_column->set_medium_text( 'Safety Stock' ).
        l_salv_column->set_long_text( 'Safety Stock' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'OPEN_PRS' ).
        l_salv_column->set_short_text( 'Open PRs' ).
        l_salv_column->set_medium_text( 'Open PRs' ).
        l_salv_column->set_long_text( 'Open PRs' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'OPEN_POS' ).
        l_salv_column->set_short_text( 'Open POs' ).
        l_salv_column->set_medium_text( 'Open POs' ).
        l_salv_column->set_long_text( 'Open POs' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'PROD_ORD' ).
        l_salv_column->set_short_text( 'Prod. Ord' ).
        l_salv_column->set_medium_text( 'Product. Orders' ).
        l_salv_column->set_long_text( 'Production Orders' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'PLAN_ORD' ).
        l_salv_column->set_short_text( 'Plan. Ord' ).
        l_salv_column->set_medium_text( 'Planned Orders' ).
        l_salv_column->set_long_text( 'Planned Orders' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'OPEN_SO' ).
        l_salv_column->set_short_text( 'Open SO' ).
        l_salv_column->set_medium_text( 'Open SO' ).
        l_salv_column->set_long_text( 'Open Sales Order' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'DELIVER' ).
        l_salv_column->set_short_text( 'Deliv.' ).
        l_salv_column->set_medium_text( 'Deliveries' ).
        l_salv_column->set_long_text( 'Deliveries' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'DEP_REQ' ).
        l_salv_column->set_short_text( 'Dep. Req.' ).
        l_salv_column->set_medium_text( 'Dep. Requir.' ).
        l_salv_column->set_long_text( 'Dep. Requirements' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'STOCK_BAL' ).
        l_salv_column->set_short_text( 'Stock Bal.' ).
        l_salv_column->set_medium_text( 'Stock Balance.' ).
        l_salv_column->set_long_text( 'Stock Balance' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.
    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'MRP' ).
        l_salv_column->set_short_text( 'MRP' ).
        l_salv_column->set_medium_text( 'MRP' ).
        l_salv_column->set_long_text( 'MRP' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'AUFNR' ).
        l_salv_column->set_short_text( '1st Order' ).
        l_salv_column->set_medium_text( 'First Order' ).
        l_salv_column->set_long_text( 'First existing Order' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    TRY.
        l_salv_column ?= l_salv_columns->get_column( 'DAT01' ).
        l_salv_column->set_short_text( 'Finish Dat' ).
        l_salv_column->set_medium_text( 'Finish Date' ).
        l_salv_column->set_long_text( 'Finish Date' ).
      CATCH cx_salv_not_found.                          "#EC NO_HANDLER
    ENDTRY.

    CASE me->unit_ty.
      WHEN 'SUNIT'.
        TRY.
            l_salv_column ?= l_salv_columns->get_column( 'MEINS' ).
            l_salv_column->set_short_text( 'Sales Unit' ).
            l_salv_column->set_medium_text( 'Sales Unit' ).
            l_salv_column->set_long_text( 'Sales Unit' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

      WHEN 'PRUNIT'.
        TRY.
            l_salv_column ?= l_salv_columns->get_column( 'MEINS' ).
            l_salv_column->set_short_text( 'Prod. Unit' ).
            l_salv_column->set_medium_text( 'Production Unit' ).
            l_salv_column->set_long_text( 'Production Unit' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

      WHEN 'OUNIT'.
        TRY.
            l_salv_column ?= l_salv_columns->get_column( 'MEINS' ).
            l_salv_column->set_short_text( 'Other Unit' ).
            l_salv_column->set_medium_text( 'Other Unit' ).
            l_salv_column->set_long_text( 'Other Unit' ).
          CATCH cx_salv_not_found.                      "#EC NO_HANDLER
        ENDTRY.

    ENDCASE.

** ------------------------------------------------------------------ *
***   Set the HotSpot for Columns
** ------------------------------------------------------------------ *

*    set_hotspot( columnname = 'STOCK' ) .
*    set_hotspot( columnname = 'STOCK_E' ).
*    set_hotspot( columnname = 'STOCK_F' ).
    set_hotspot( columnname = 'OPEN_PRS' ).
    set_hotspot( columnname = 'OPEN_POS' ).
    set_hotspot( columnname = 'PROD_ORD' ).
    set_hotspot( columnname = 'PLAN_ORD' ).
    set_hotspot( columnname = 'OPEN_SO' ).
    set_hotspot( columnname = 'DELIVER' ).
    set_hotspot( columnname = 'DEP_REQ' ).
** ------------------------------------------------------------------ *
******set space for zero
** ------------------------------------------------------------------ *
    l_salv_column ?= l_salv_columns->get_column( 'STOCK' ).
    l_salv_column->set_zero( ' ' ).

    l_salv_column ?= l_salv_columns->get_column( 'STOCK' ).
    l_salv_column->set_zero( ' ' ).

    l_salv_column ?= l_salv_columns->get_column( 'STOCK_E' ).
    l_salv_column->set_zero( ' ' ).

    l_salv_column ?= l_salv_columns->get_column( 'STOCK_F' ).
    l_salv_column->set_zero( ' ' ).
*
    l_salv_column ?= l_salv_columns->get_column( 'OPEN_PRS' ).
    l_salv_column->set_zero( ' ' ).
*
    l_salv_column ?= l_salv_columns->get_column( 'OPEN_POS' ).
    l_salv_column->set_zero( ' ' ).
*
    l_salv_column ?= l_salv_columns->get_column( 'PROD_ORD' ).
    l_salv_column->set_zero( ' ' ).
*
    l_salv_column ?= l_salv_columns->get_column( 'PLAN_ORD' ).
    l_salv_column->set_zero( ' ' ).
*
    l_salv_column ?= l_salv_columns->get_column( 'OPEN_SO' ).
    l_salv_column->set_zero( ' ' ).

    l_salv_column ?= l_salv_columns->get_column( 'DELIVER' ).
    l_salv_column->set_zero( ' ' ).

    l_salv_column ?= l_salv_columns->get_column( 'DEP_REQ' ).
    l_salv_column->set_zero( ' ' ).

    l_salv_column ?= l_salv_columns->get_column( 'STOCK_BAL' ).
    l_salv_column->set_zero( ' ' ).

    l_salv_column ?= l_salv_columns->get_column( 'MRP' ).
    l_salv_column->set_zero( ' ' ).
*
** ------------------------------------------------------------------ *
******set collor
** ------------------------------------------------------------------ *
    DATA: color TYPE lvc_s_colo.

    color-col = '3'.
    color-int = '0'.
    color-inv = '0'.

    l_salv_column ?= l_salv_columns->get_column( 'STOCK' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'STOCK_E' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'STOCK_F' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'OPEN_PRS' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'OPEN_POS' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'PROD_ORD' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'PLAN_ORD' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'OPEN_SO' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'DELIVER' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'DEP_REQ' ).
    l_salv_column->set_color( color ).
*----------------------------------------------------------------*
    CLEAR color.

    color-col = '5'.
    color-int = '0'.
    color-inv = '0'.

    l_salv_column ?= l_salv_columns->get_column( 'STOCK_BAL' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'MRP' ).
    l_salv_column->set_color( color ).

*----------------------------------------------------------------*
    CLEAR color.

    color-col = '4'.
    color-int = '1'.
    color-inv = '0'.

    l_salv_column ?= l_salv_columns->get_column( 'MATNR' ).
    l_salv_column->set_color( color ).

    l_salv_column ?= l_salv_columns->get_column( 'WERKS' ).
    l_salv_column->set_color( color ).
*
*
** ------------------------------------------------------------------ *
***** set the color of a complete row
** ------------------------------------------------------------------ *
    TRY.
        l_salv_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.

** ------------------------------------------------------------------ *
*****set visible column
** ------------------------------------------------------------------ *
**    try.
**        l_salv_column ?= l_salv_columns->get_column( 'LGNUM' ).
**        l_salv_column->set_visible( if_salv_c_bool_sap=>false ).
*
*    CATCH cx_salv_not_found.
*    ENDTRY.
  ENDMETHOD.

* ----------------------------------------------------------------- *
* Check selected row
* ----------------------------------------------------------------- *
  METHOD check_selected_row .

    DATA t_rows  TYPE salv_t_row.
    DATA s_rows  TYPE i.
    DATA v_lines TYPE i.

    t_rows = l_salv_selections->get_selected_rows( ).

    DESCRIBE TABLE t_rows LINES v_lines.

    IF v_lines > 1 OR v_lines = 0.
      CLEAR v_success .
      MESSAGE 'You must choose one line' TYPE 'S' DISPLAY LIKE 'E'.

    ELSE.
      v_success = 'X'.

      READ TABLE t_rows INTO v_index INDEX 1.

    ENDIF.

  ENDMETHOD.

  METHOD set_hotspot.

    TRY.
        l_salv_column ?= l_salv_columns->get_column( columnname ).
      CATCH cx_salv_not_found.
    ENDTRY.

    TRY.
        CALL METHOD l_salv_column->set_cell_type
          EXPORTING
            value = if_salv_c_cell_type=>hotspot.
      CATCH cx_salv_data_error .
    ENDTRY.

  ENDMETHOD.
* ----------------------------------------------------------------- *
* Set top_of_page
* ----------------------------------------------------------------- *
  METHOD set_top_of_page.
*
*    DATA: lo_header  TYPE REF TO cl_salv_form_layout_grid,
*          lo_h_label TYPE REF TO cl_salv_form_label,
*          lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.
**
**   header object
*    CREATE OBJECT lo_header.
**
**   To create a Lable or Flow we have to specify the target
**     row and column number where we need to set up the output
**     text.
**
**   information in Bold
*    lo_h_label = lo_header->create_label( row = 1 column = 1 ).
*    lo_h_label->set_text( 'Header in Bold' ).
**
**   information in tabular format
*    lo_h_flow = lo_header->create_flow( row = 2  column = 1 ).
*    lo_h_flow->create_text( TEXT = 'This is text of flow' ).
**
*    lo_h_flow = lo_header->create_flow( row = 3  column = 1 ).
*    lo_h_flow->create_text( TEXT = 'Number of Records in the output' ).
**
*    lo_h_flow = lo_header->create_flow( row = 3  column = 2 ).
*    lo_h_flow->create_text( TEXT = 20 ).
**
**   set the top of list using the header for Online.
*    t_salv->set_top_of_list( lo_header ).
**
**   set the top of list using the header for Print.
*    t_salv->set_top_of_list_print( lo_header ).
*
  ENDMETHOD.                    "set_top_of_page


* ----------------------------------------------------------------- *
* Set end of page
* ----------------------------------------------------------------- *
  METHOD set_end_of_page.
*
*    DATA: lo_footer  TYPE REF TO cl_salv_form_layout_grid,
*          lo_f_label TYPE REF TO cl_salv_form_label,
*          lo_f_flow  TYPE REF TO cl_salv_form_layout_flow.
**
**   footer object
*    CREATE OBJECT lo_footer.
**
**   information in bold
*    lo_f_label = lo_footer->create_label( row = 1 column = 1 ).
*    lo_f_label->set_text( 'Footer .. here it goes' ).
**
**   tabular information
*    lo_f_flow = lo_footer->create_flow( row = 2  column = 1 ).
*    lo_f_flow->create_text( TEXT = 'This is text of flow in footer' ).
**
*    lo_f_flow = lo_footer->create_flow( row = 3  column = 1 ).
*    lo_f_flow->create_text( TEXT = 'Footer number' ).
**
*    lo_f_flow = lo_footer->create_flow( row = 3  column = 2 ).
*    lo_f_flow->create_text( TEXT = 1 ).
**
**   Online footer
*    t_salv->set_end_of_list( lo_footer ).
**
**   Footer in print
*    t_salv->set_end_of_list_print( lo_footer ).
*
  ENDMETHOD.                    "set_end_of_page
ENDCLASS.



CLASS lcl_display_popup IMPLEMENTATION.
* ----------------------------------------------------------------- *
*  PUBLIC SECTION.
* ----------------------------------------------------------------- *
* ----------------------------------------------------------------- *
* Methods
* ----------------------------------------------------------------- *
  METHOD get_popup.

* --------------------------------------------------------------- *
*   Creating the SALV Object
* --------------------------------------------------------------- *
    TRY.

        cl_salv_table=>factory( IMPORTING r_salv_table = lt_salv_dcl
        CHANGING t_table = table ).

      CATCH cx_salv_msg.
    ENDTRY.

    CHECK sy-subrc EQ 0.

    l_salv_funct_pop = lt_salv_dcl->get_functions( ).
    l_salv_funct_pop->set_all( if_salv_c_bool_sap=>true ).



** --------------------------------------------------------------- *
**   In case you want to change some column attributes
** --------------------------------------------------------------- *
    l_salv_columns_pop  = lt_salv_dcl->get_columns( ).
    l_salv_columns_pop->set_optimize( abap_true ).
*
    l_salv_aggrs_pop    = lt_salv_dcl->get_aggregations( ).

* ----------------------------------------------------------------- *
* Modification
* ----------------------------------------------------------------- *
    field_modification( ).
* ----------------------------------------------------------------- *
* Display settings
* ----------------------------------------------------------------- *
    set_display_settings( ).

* ----------------------------------------------------------------- *
* Set screen popup
* ----------------------------------------------------------------- *
    set_screen_popup( ).

    lt_salv_dcl->display( ).

  ENDMETHOD.

* ----------------------------------------------------------------- *
*  PRIVATE SECTION.
* ----------------------------------------------------------------- *

  METHOD set_screen_popup.
    lt_salv_dcl->set_screen_popup(  start_column = 5
                                    end_column   = 100
                                    start_line   = 3
                                    end_line     = 20 ).

  ENDMETHOD.


  METHOD set_display_settings.

    l_salv_display_pop = lt_salv_dcl->get_display_settings( ).
* zebra stripes
    l_salv_display_pop->set_striped_pattern( cl_salv_display_settings=>true ).
* Title
    l_salv_display_pop->set_list_header( 'Details' ).


  ENDMETHOD.

  METHOD field_modification.

** ------------------------------------------------------------------ *
*****set visible column
** ------------------------------------------------------------------ *
    DATA(lv_delkz) = go_data->t_details[ 1 ]-delkz.

    CASE lv_delkz.

      WHEN 'BA' OR 'BE'.

        TRY.
            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DEL12' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'AUFVR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'KUNNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_K' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'SOBKZ' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELNR' ).
            IF lv_delkz = 'BA'.
              l_salv_column_pop->set_short_text( 'PR no' ).
              l_salv_column_pop->set_medium_text( 'PR no' ).
              l_salv_column_pop->set_long_text( 'PR no' ).
            ELSE.
              l_salv_column_pop->set_short_text( 'PO no' ).
              l_salv_column_pop->set_medium_text( 'PO no' ).
              l_salv_column_pop->set_long_text( 'PO no' ).
            ENDIF.

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELPS' ).
            l_salv_column_pop->set_short_text( 'Item' ).
            l_salv_column_pop->set_medium_text( 'Item' ).
            l_salv_column_pop->set_long_text( 'Item' ).


          CATCH cx_salv_not_found.
        ENDTRY.

      WHEN 'FE'  .
        TRY.
            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELPS' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'AUFVR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'KUNNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_K' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'LIFNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_L' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'SOBKZ' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DEL12' ).
            l_salv_column_pop->set_short_text( 'Prod. ord.' ).
            l_salv_column_pop->set_medium_text( 'Production orders' ).
            l_salv_column_pop->set_long_text( 'Production orders' ).

          CATCH cx_salv_not_found.
        ENDTRY.

      WHEN 'PA'.
        TRY.
            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DEL12' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELPS' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'AUFVR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'KUNNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_K' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'LIFNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_L' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'SOBKZ' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELNR' ).
            l_salv_column_pop->set_short_text( 'Plan. ord.' ).
            l_salv_column_pop->set_medium_text( 'Planned orders' ).
            l_salv_column_pop->set_long_text( 'Planned orders' ).

          CATCH cx_salv_not_found.
        ENDTRY.

      WHEN 'VC' OR 'VJ'.
        TRY.
            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DEL12' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'AUFVR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'LIFNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_L' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELNR' ).
            IF lv_delkz = 'VC'.
              l_salv_column_pop->set_short_text( 'Open SO' ).
              l_salv_column_pop->set_medium_text( 'Open SO' ).
              l_salv_column_pop->set_long_text( 'Open SO' ).
            ELSE.
              l_salv_column_pop->set_short_text( 'Deliveries' ).
              l_salv_column_pop->set_medium_text( 'Deliveries' ).
              l_salv_column_pop->set_long_text( 'Deliveries' ).
            ENDIF.

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELPS' ).
            l_salv_column_pop->set_short_text( 'Item' ).
            l_salv_column_pop->set_medium_text( 'Item' ).
            l_salv_column_pop->set_long_text( 'Item' ).

          CATCH cx_salv_not_found.
        ENDTRY.

      WHEN 'AR' OR 'SB'.
        TRY.
            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DELPS' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).


            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'KUNNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_K' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'LIFNR' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'NAME_L' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'SOBKZ' ).
            l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

            l_salv_column_pop ?= l_salv_columns_pop->get_column( 'DEL12' ).
            l_salv_column_pop->set_short_text( 'Dep. Req.' ).
            l_salv_column_pop->set_medium_text( 'Dep. Requirements' ).
            l_salv_column_pop->set_long_text( 'Dep. Requirements' ).

          CATCH cx_salv_not_found.
        ENDTRY.
      WHEN OTHERS.

    ENDCASE.

    TRY .
        l_salv_column_pop ?= l_salv_columns_pop->get_column( 'MATNR' ).
        l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

        l_salv_column_pop ?= l_salv_columns_pop->get_column( 'MEINS' ).
        l_salv_column_pop->set_visible( if_salv_c_bool_sap=>false ).

      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*&      Module  STATUS_0100  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
  SET PF-STATUS 'NO_STATUS'.
  SET TITLEBAR 'TUNIT'.
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0100 INPUT.
  CLEAR f_process.

  CASE fcode.
    WHEN 'SAVE'.
      IF v_unit IS INITIAL.
        MESSAGE 'Please choose Unit' TYPE 'S' DISPLAY LIKE 'E'.
      ELSE.
        f_process = 'X'.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN 'CANC' OR '&CANCEL'.
      CLEAR f_process.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.

  ENDCASE.

ENDMODULE.
