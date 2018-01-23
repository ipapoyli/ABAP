PERFORM alv_display_summary_list. 

*&---------------------------------------------------------------------*
*&      Form alv_display_summary_list
*&---------------------------------------------------------------------*
FORM alv_display_summary_list.
  DATA:
    f_program_name LIKE sy-repid,
    f_inclname     LIKE trdir-name,
    f_repid        LIKE sy-repid.

  xtab[] = itab[].

* ====================
  f_repid = sy-repid.

  CLEAR layout.
  layout-colwidth_optimize    = 'X'.
  layout-zebra                = 'X'.
  layout-info_fieldname       = 'ROWCOLOR'.
  layout-box_fieldname        = 'SEL'.
  layout-box_tabname          = 'ITAB'.

* ====================
  PERFORM alv_header USING 'SUMM'.
  PERFORM alv_events USING 'ZALV_USERCOMM_SUMM'.

* ====================
  CLEAR: fieldcat[].
  PERFORM alv_merge  USING 'ITAB'  'XKUN'      'I'.
  PERFORM alv_merge  USING 'ITAB'  'AMOUNTS'   'I'.
  PERFORM alv_merge  USING 'ITAB'  'ITAB'      'I'.
  PERFORM alv_sort   USING 'ITAB'.

  PERFORM alv_modify_list USING 'ITAB' '00'.

  PERFORM alv_colpos.

* ====================
  variant-variant          = pa_vari.

*----------------------------------------------------------------------*
* Authorization for saving ALV layouts
  DATA lv_save.
  CLEAR lv_save.
  AUTHORITY-CHECK OBJECT 'S_ALV_LAYO'
           ID 'ACTVT' FIELD '23'.
  IF sy-subrc <> 0.
    lv_save = 'U'.
  ELSE.
    lv_save = 'A'.
  ENDIF.
*----------------------------------------------------------------------*

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = f_repid
*     i_callback_pf_status_set = 'ZALV_SET_STATUS'
      i_callback_user_command = 'ZALV_USERCOMM_SUMM'
      is_layout               = layout
      it_fieldcat             = fieldcat[]
      it_sort                 = sortcat[]
      i_save                  = lv_save
      it_events               = eventcat[]
      is_variant              = variant
    TABLES
      t_outtab                = xtab
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

  xtab[] = xtab[].

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form alv_header
*&---------------------------------------------------------------------*
FORM alv_header USING typ.
  DATA: wtext1(100).
  DATA: wtext2(100).
  DATA: wtext3(100).
  DATA: wtext4(100).

  REFRESH: listheader.
  CLEAR  : listheader.

* ========== Format ALV titles
  IF p_opn    = 'X'.
    alv_title_sub = TEXT-t02.
  ELSEIF p_chq    = 'X'.
    alv_title_sub = TEXT-t03.
  ELSEIF p_pym    = 'X'.
    alv_title_sub = TEXT-t07.
  ELSEIF p_atm = 'X'.
    alv_title_sub = TEXT-t08.
  ENDIF.

  IF typ          = 'DET'.
    alv_title     = TEXT-t04.
  ELSE.
    alv_title     = TEXT-t05.
  ENDIF.

* =============================
  PERFORM alv_head   USING 'H'  ''  alv_title  alv_title_sub ''.

* =============================
  PERFORM alv_head   USING 'S'  TEXT-a43 ':' p_bukrs t001-butxt.
  IF c_bldat = 'X'.
    PERFORM alv_head USING 'S'  TEXT-s28  TEXT-s29 '' ''.
  ELSEIF c_budat = 'X'.
    PERFORM alv_head USING 'S'  TEXT-s28  TEXT-s30 '' ''.
  ELSEIF c_zfbdt = 'X'.
    PERFORM alv_head USING 'S'  TEXT-s28  TEXT-s31 '' ''.
  ENDIF.

* =============================
  IF        c_lcurr = 'X'.

    IF      c_lcurl = 'X'.
      CONCATENATE '(' TEXT-s38    TEXT-s42 ')'  INTO wtext1
      SEPARATED BY space.
      PERFORM alv_head USING 'S'  TEXT-s26  TEXT-s10 wtext1 ''.
    ELSEIF  c_vcurr = 'X'.
      CONCATENATE '(' TEXT-s06    TEXT-s42 ')'  INTO wtext1
      SEPARATED BY space.
      PERFORM alv_head USING 'S'  TEXT-s26  TEXT-s10 wtext1 ''.
    ELSEIF  c_tcurr = 'X'.
      CONCATENATE '(' TEXT-s07    TEXT-s42 ')'  INTO wtext1
      SEPARATED   BY space.
      CONCATENATE TEXT-s08 ':' c_kurst          INTO wtext2
      SEPARATED   BY space.
      WRITE c_wwert TO wtext3 DD/MM/YYYY.
      CONCATENATE TEXT-s43 ':' wtext3          INTO wtext3
      SEPARATED   BY space.
      CONCATENATE TEXT-s46 ':' c_waers       INTO wtext4
      SEPARATED   BY space.
      PERFORM alv_head USING 'S'  TEXT-s26  TEXT-s10 wtext1 ''.
      PERFORM alv_head USING 'S'  wtext3 ''  ''  ''.
      PERFORM alv_head USING 'S'  wtext4 ''  ''  ''.
      PERFORM alv_head USING 'S'  wtext2 ''  ''  ''.
    ENDIF.

*  ELSEIF    c_gcurr = 'X'.
*    PERFORM   alv_head USING 'S'  text-s26  text-s27 '' ''.
  ELSEIF    c_dcurr = 'X'.
    PERFORM   alv_head USING 'S'  TEXT-s26  TEXT-s35 '' ''.
  ELSEIF    c_pcurr = 'X'.
    PERFORM   alv_head USING 'S'  TEXT-s26  TEXT-a28 '' ''.
  ELSEIF    c_ccurr = 'X'.
    PERFORM   alv_head USING 'S'  TEXT-s26  TEXT-s37 '' ''.
  ENDIF.

* =============================
  IF p_dpm     = 'X'.
    PERFORM   alv_head USING 'S'  TEXT-s47 ''  '' ''.
  ELSEIF p_cpm = 'X'.
    PERFORM   alv_head USING 'S'  TEXT-s44 ''  '' ''.
  ELSE.
*   PERFORM   alv_head USING 'S'  text-s45  p_mon   '' ''.
    PERFORM   alv_head USING 'S'  TEXT-s45 ''  '' ''.
  ENDIF.

* =============================
  PERFORM alv_head USING 'A'  ''  TEXT-t06 p_kdate ''.

ENDFORM.


*&---------------------------------------------------------------------*
*&      Form alv_events
*&---------------------------------------------------------------------*
FORM alv_events USING f_ev.
  CLEAR: eventcat[].

  CLEAR eventcat_ln.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = eventcat.

* ====================
* TOP-OF-PAGE
  READ TABLE eventcat WITH KEY name =  slis_ev_top_of_page
                          INTO eventcat_ln.
  IF sy-subrc = 0.
    MOVE 'TOP_OF_PAGE' TO eventcat_ln-form.
    APPEND eventcat_ln TO eventcat.
  ENDIF.

** ====================
** SET_PF_STATUS
*  READ TABLE eventcat WITH KEY name =  slis_ev_pf_status_set
*                          INTO eventcat_ln.
*  IF sy-subrc = 0.
*    MOVE 'ZALV_SET_STATUS' TO eventcat_ln-form.
*    APPEND eventcat_ln TO eventcat.
*  ENDIF.

* ====================
* AT_USER_COMMAND
  READ TABLE eventcat WITH KEY name =  slis_ev_user_command
                          INTO eventcat_ln.
  IF sy-subrc = 0.
    MOVE f_ev          TO eventcat_ln-form.
    APPEND eventcat_ln TO eventcat.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  alv_merge
*&---------------------------------------------------------------------*
FORM alv_merge USING f_tabname f_strname f_type .
  DATA: int_tab TYPE slis_tabname.
  DATA: int_str TYPE dd02l-tabname.
  DATA: f_repid LIKE sy-repid.

  CLEAR: fieldcat_x[].
  f_repid = sy-repid.
  int_str = f_strname.

  CASE f_type.

* ====================
    WHEN 'S'.

      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          i_program_name         = f_repid
          i_structure_name       = int_str
          i_inclname             = f_repid
          i_bypassing_buffer     = 'X'
          i_buffer_active        = space
        CHANGING
          ct_fieldcat            = fieldcat_x
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.

* ====================
    WHEN 'I'.

      CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
        EXPORTING
          i_program_name         = f_repid
          i_internal_tabname     = int_str
          i_inclname             = f_repid
          i_bypassing_buffer     = 'X'
          i_buffer_active        = space
        CHANGING
          ct_fieldcat            = fieldcat_x
        EXCEPTIONS
          inconsistent_interface = 1
          program_error          = 2
          OTHERS                 = 3.
  ENDCASE.


  APPEND LINES OF fieldcat_x TO fieldcat.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form alv_sort
*&---------------------------------------------------------------------*
FORM  alv_sort USING tabname.
  CLEAR: sortcat[].

  CASE tabname.

*   --------------------------------
    WHEN 'ITAB'.

      PERFORM alv_scat USING '1' 'KUNNR'   'X' '' ''   ''  ''.
      PERFORM alv_scat USING '2' 'TYPE'    'X' '' ' ' ' ' ' '.

*   --------------------------------
    WHEN 'ITAB2'.
      IF p_chq IS INITIAL.
        PERFORM alv_scat USING '1' 'KUNNR'  'X' '' 'X' ''  ''.
        PERFORM alv_scat USING '2' 'PBLDAT' 'X' '' ''  ''  ''.
        PERFORM alv_scat USING '3' 'PBELNR' 'X' '' ''  ''  ''.
        PERFORM alv_scat USING '4' 'BLDAT'  'X' '' ''  ''  ''.
      ELSE.
        PERFORM alv_scat USING '1' 'KUNNR'  'X' '' 'X' ''  ''.
        PERFORM alv_scat USING '2' 'UMSKZ'  'X' '' 'X' ''  ''.
        PERFORM alv_scat USING '3' 'PBLDAT' 'X' '' ''  ''  ''.
        PERFORM alv_scat USING '4' 'PBELNR' 'X' '' ''  ''  ''.
        PERFORM alv_scat USING '5' 'PBUZEI' 'X' '' ''  ''  ''.
        PERFORM alv_scat USING '6' 'BLDAT'  'X' '' ''  ''  ''.
      ENDIF.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form alv_modify_list.
*&---------------------------------------------------------------------*
FORM alv_modify_list USING f_tabname f_period.
  DATA: wtext(100) TYPE c.

  LOOP AT fieldcat INTO fieldcat_ln.
    fieldcat_ln-key                = ' '.
    fieldcat_ln-just               = 'C'.
    fieldcat_ln-ddictxt            = 'L'.
    fieldcat_ln-tabname            = f_tabname.
    fieldcat_ln-no_out             = ' '.

    CLEAR wtext.

* ====================
    CASE fieldcat_ln-fieldname.
*      WHEN 'BUSAB'.
*        fieldcat_ln-seltext_l = text-a25.
*        fieldcat_ln-no_out    = 'X'.
*      WHEN 'KATR1'.
*        fieldcat_ln-seltext_l = text-a24.
*      WHEN 'KATR2'.
*        fieldcat_ln-seltext_l = text-a27.
*        fieldcat_ln-no_out = 'X'.
*      WHEN 'KATR8'.
*        fieldcat_ln-seltext_l = text-a26.
*        fieldcat_ln-no_out    = 'X'.
*      WHEN 'KATR7'.
*        fieldcat_ln-seltext_l = text-a29.
*        fieldcat_ln-no_out    = 'X'.
*      WHEN 'KATR6'.
*        fieldcat_ln-seltext_l = text-a28.
*        fieldcat_ln-no_out    = 'X'.
*      WHEN 'SNAME'.
*        fill_text   text-a11    text-a25.
*      WHEN 'VTEXT1'.
*        fill_text   text-a11    text-a24.
*        fieldcat_ln-no_out    = 'X'.
*      WHEN 'VTEXT2'.
*        fieldcat_ln-seltext_l = text-a27.
*        fieldcat_ln-just      = 'L'.
*      WHEN 'VTEXT8'.
*        fill_text   text-a11    text-a26.
*      WHEN 'VTEXT7'.
*        fill_text   text-a11    text-a29.
*      WHEN 'VTEXT6'.
*        fill_text   text-a11    text-a28.
*        fieldcat_ln-no_out = 'X'.
      WHEN 'VTEXTZ'.
        fill_text   TEXT-a11    TEXT-a31.
      WHEN 'DVTEXTZ'.
        fill_text   TEXT-a11    TEXT-a41.
        IF p_opn = ' ' AND p_atm = ' '.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
*      WHEN 'ZWELT'.
*       fill_text   text-a33    text-a32.
      WHEN 'ZTERM'.
        fieldcat_ln-just      = 'L'.
        fill_text   ''          TEXT-a31.
        fieldcat_ln-no_out    = 'X'.
      WHEN 'DZTERM'.
        fieldcat_ln-just      = 'L'.
        fill_text   ''          TEXT-a41.
        fieldcat_ln-no_out    = 'X'.

* ====================
      WHEN 'KUNNR'.
        fieldcat_ln-hotspot   = 'X'.
      WHEN 'BEZEI'.
        fieldcat_ln-seltext_l = TEXT-a48.
        fieldcat_ln-no_out    = 'X'.
      WHEN 'TYPE'.
        fieldcat_ln-seltext_l = TEXT-a34.
        fieldcat_ln-no_out    = 'X'.
      WHEN 'TYPET'.
        fieldcat_ln-seltext_l = TEXT-a34.
        fieldcat_ln-just      = 'L'.
      WHEN 'JAMON'.

      WHEN 'BALANCE'.
        IF xdet IS INITIAL.
          fieldcat_ln-hotspot   = 'X'.
        ENDIF.
        fieldcat_ln-seltext_l = TEXT-s32.
        IF pa_det = 'X'.
          fieldcat_ln-no_out  = 'X'.
        ENDIF.
*        ----------------------------- ccc00252  22-11-2017
*               local balance
*        -----------------------------
      WHEN 'BALANCE_LC'.
        IF xdet IS INITIAL.
          fieldcat_ln-hotspot   = 'X'.
        ENDIF.
        fieldcat_ln-seltext_l = TEXT-s55.
        IF pa_det = 'X'.
          fieldcat_ln-no_out  = 'X'.
        ENDIF.
*        -----------------------------
      WHEN 'WAERS'.
        IF f_tabname = 'ITAB'.
          IF       c_lcurr = 'X'.
            fieldcat_ln-seltext_l = TEXT-a06.
*          ELSEIF   c_gcurr = 'X'.
*            fieldcat_ln-seltext_l = text-a08.
          ELSEIF   c_dcurr = 'X'.
            fieldcat_ln-seltext_l = TEXT-a07.
          ELSEIF   c_ccurr = 'X'.
            fieldcat_ln-seltext_l = TEXT-a09.
          ELSEIF   c_pcurr = 'X'.
            fieldcat_ln-seltext_l = TEXT-a28.
          ENDIF.
        ELSE.
          fieldcat_ln-seltext_l   = TEXT-a07.
          IF      c_dcurr = 'X'.
            fieldcat_ln-seltext_l = TEXT-a07.
          ELSEIF c_ccurr = 'X'.
            fieldcat_ln-seltext_l = TEXT-a23.
          ELSEIF c_pcurr = 'X'.
            fieldcat_ln-seltext_l = TEXT-a28.
          ENDIF.
        ENDIF.

* ====================
      WHEN 'BELNR'.
        fieldcat_ln-hotspot  = 'X'.

      WHEN 'VBELN'.
        fieldcat_ln-hotspot  = 'X'.
        IF p_opn = ' ' AND p_atm = ' '.
          fieldcat_ln-no_out = 'X'.
        ENDIF.

      WHEN 'BSTKD'.
        IF p_opn = ' ' AND p_atm = ' '.
          fieldcat_ln-no_out = 'X'.
        ENDIF.

      WHEN 'BOENO'.
        fieldcat_ln-hotspot  = 'X'.
        fieldcat_ln-just     = 'L'.
        IF p_opn = 'X' AND p_atm = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.

      WHEN 'KUNNR'   OR 'SHKZG'   OR 'GJAHR' OR
           'BLART'   OR 'KURSF'   OR 'BSCHL' OR
           'BLDAT'.

      WHEN 'BUDAT'.
        IF p_chq = 'X'.
          fieldcat_ln-seltext_l = TEXT-a46.
        ENDIF.

      WHEN 'WSTAT' OR 'WNAME' OR 'WBZOG' OR 'WDATE'.
        IF p_opn = 'X' AND p_atm = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
      WHEN 'BANKA' OR 'BRNCH' OR 'BANKN' OR 'IBAN'.
        fieldcat_ln-just      = 'L'.
        IF p_opn = 'X' AND p_atm = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.

      WHEN 'PORTF'.
        fieldcat_ln-seltext_l = TEXT-a42.
        fieldcat_ln-no_out    = 'X'.

      WHEN 'BUKRS' OR 'BUZEI' OR 'ZWELS'.
        fieldcat_ln-no_out    = 'X'.

      WHEN 'BUTXT'.
        fieldcat_ln-just      = 'L'.
        fieldcat_ln-no_out    = 'X'.

      WHEN 'LTEXT'   OR 'XBLNR'   OR
           'NAME1'   OR 'SGTXT'.
        fieldcat_ln-just      = 'L'.

      WHEN 'PNAME'.
        fieldcat_ln-just      = 'L'.
        IF p_opn = 'X' AND p_atm = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.

      WHEN 'UMSKZ'.
        IF p_opn = 'X' AND p_atm = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.

      WHEN 'XBLNR'.
        fieldcat_ln-just      = 'L'.
        IF p_opn = 'X' AND p_atm = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
      WHEN 'ZFBDT'.
        fieldcat_ln-seltext_l = TEXT-a05.
        IF p_chq = 'X'.
          fieldcat_ln-seltext_l = TEXT-a44.
        ENDIF.

* ====================
*      WHEN 'WRBTR_DB'.
*        fieldcat_ln-seltext_l = text-a12.
*        fieldcat_ln-no_zero   = 'X'.
*        IF c_dcurr IS INITIAL.
*          fieldcat_ln-no_out   = 'X'.
*        ENDIF.
*      WHEN 'WRBTR_CR'.
*        fieldcat_ln-seltext_l = text-a13.
*        fieldcat_ln-no_zero   = 'X'.
*        IF c_dcurr IS INITIAL.
*          fieldcat_ln-no_out   = 'X'.
*        ENDIF.

* ====================
      WHEN 'WRBTR'.
        fieldcat_ln-seltext_l = TEXT-a20.

      WHEN 'DMBTR'.
        MOVE TEXT-a21 TO wtext.
        REPLACE '&' WITH t001-waers INTO wtext.
        fieldcat_ln-seltext_l = wtext.
      WHEN 'CBBTR'.
        fieldcat_ln-seltext_l = TEXT-a47.

      WHEN 'DMBE2'.
        MOVE TEXT-a22 TO wtext.
        REPLACE '&' WITH waers_e2 INTO wtext.
        fieldcat_ln-seltext_l = wtext.
*       IF c_gcurr            = ' '.
        fieldcat_ln-no_out  = 'X'.
*        ENDIF.

      WHEN 'DMBE3'.
        IF waers_e3 <> waers_e2.
          MOVE TEXT-a23 TO wtext.
          REPLACE '&' WITH waers_e3 INTO wtext.
          fieldcat_ln-seltext_l = wtext.
        ELSE.
          fieldcat_ln-no_out    = 'X'.
        ENDIF.

      WHEN 'VMBTR'.
        IF     c_vcurr   = 'X'.
          MOVE TEXT-a38 TO wtext.
          REPLACE '&' WITH t001-waers INTO wtext.
        ELSEIF c_tcurr   = 'X'.
          MOVE TEXT-a39 TO wtext.
          REPLACE '&' WITH c_waers INTO wtext.
        ENDIF.
        fieldcat_ln-seltext_l = wtext.
        IF c_vcurr = 'X' OR c_tcurr = 'X'.
        ELSE.
          fieldcat_ln-tech  = 'X'.
        ENDIF.

      WHEN 'PMBTR'.
        fieldcat_ln-seltext_l = TEXT-a40.
        IF c_pcurr IS INITIAL.
          fieldcat_ln-tech    = 'X'.
        ENDIF.

* ====================
      WHEN 'DDSYMB'.
        fieldcat_ln-icon      = 'Χ'.
        fieldcat_ln-seltext_l = TEXT-s34.
      WHEN 'DDAYS'.
        IF c_bldat = 'X'.
          fieldcat_ln-seltext_l = TEXT-a35.
        ELSEIF c_zfbdt = 'X'.
          fieldcat_ln-seltext_l = TEXT-a36.
        ELSEIF c_budat = 'X'.
          fieldcat_ln-seltext_l = TEXT-a37.
        ENDIF.
        fieldcat_ln-just      = 'R'.
      WHEN 'PARTIAL'.
        fieldcat_ln-seltext_l = TEXT-a10.
        fieldcat_ln-icon      = 'Χ'.
        fieldcat_ln-hotspot   = 'Χ'.

* ====================
      WHEN 'PER_F'.
        IF p_fut IS INITIAL.
          fieldcat_ln-seltext_l = TEXT-s33.
        ELSE.
          fieldcat_ln-seltext_l = TEXT-s51.
        ENDIF.
        IF p_gt = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        IF p_chq = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        fieldcat_ln-hotspot     = ' '.
        IF xdet IS INITIAL.
          fieldcat_ln-hotspot   = 'X'.
        ENDIF.
      WHEN 'PER_O'.
        fieldcat_ln-seltext_l = TEXT-s34.
        IF p_gt = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        IF p_chq IS INITIAL.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        fieldcat_ln-hotspot     = ' '.
        IF xdet IS INITIAL.
          fieldcat_ln-hotspot   = 'X'.
        ENDIF.

*        ccc00252  05-01-2018
      WHEN 'PER_LF'.
        IF p_fut IS INITIAL.
          fieldcat_ln-seltext_l = TEXT-lf1.
        ELSE.
          fieldcat_ln-seltext_l = TEXT-lf2.
        ENDIF.
        IF p_gt = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        IF p_chq = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        fieldcat_ln-hotspot     = ' '.
        IF xdet IS INITIAL.
          fieldcat_ln-hotspot   = 'X'.
        ENDIF.

      WHEN 'PER_LO'.
        fieldcat_ln-seltext_l = TEXT-lo4.
        IF p_gt = 'X'.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        IF p_chq IS INITIAL.
          fieldcat_ln-no_out = 'X'.
        ENDIF.
        fieldcat_ln-hotspot     = ' '.
        IF xdet IS INITIAL.
          fieldcat_ln-hotspot   = 'X'.
        ENDIF.

*  --------------------------------------------------------- *

      WHEN 'PER01'.
        PERFORM alv_fldcat USING  c_001 s_day-y01_text f_period.
      WHEN 'PER02'.
        PERFORM alv_fldcat USING  c_002 s_day-y02_text f_period.
      WHEN 'PER03'.
        PERFORM alv_fldcat USING  c_003 s_day-y03_text f_period.
      WHEN 'PER04'.
        PERFORM alv_fldcat USING  c_004 s_day-y04_text f_period.
      WHEN 'PER05'.
        PERFORM alv_fldcat USING  c_005 s_day-y05_text f_period.
      WHEN 'PER06'.
        PERFORM alv_fldcat USING  c_006 s_day-y06_text f_period.
      WHEN 'PER07'.
        PERFORM alv_fldcat USING  c_007 s_day-y07_text f_period.
      WHEN 'PER08'.
        PERFORM alv_fldcat USING  c_008 s_day-y08_text f_period.
      WHEN 'PER09'.
        PERFORM alv_fldcat USING  c_009 s_day-y09_text f_period.
      WHEN 'PER10'.
        PERFORM alv_fldcat USING  c_010 s_day-y10_text f_period.
      WHEN 'PER11'.
        PERFORM alv_fldcat USING  c_011 s_day-y11_text f_period.
      WHEN 'PER12'.
        PERFORM alv_fldcat USING  c_012 s_day-y12_text f_period.
      WHEN 'PER13'.
        PERFORM alv_fldcat USING  c_013 s_day-y13_text f_period.

*        -----------------------  ccc00252  22-11-2017
*             Local Periods
*       ------------------------

      WHEN 'PER01_LC'.
        PERFORM alv_fldcat_lc USING  c_001 s_day-y01_text f_period.
      WHEN 'PER02_LC'.
        PERFORM alv_fldcat_lc USING  c_002 s_day-y02_text f_period.
      WHEN 'PER03_LC'.
        PERFORM alv_fldcat_lc USING  c_003 s_day-y03_text f_period.
      WHEN 'PER04_LC'.
        PERFORM alv_fldcat_lc USING  c_004 s_day-y04_text f_period.
      WHEN 'PER05_LC'.
        PERFORM alv_fldcat_lc USING  c_005 s_day-y05_text f_period.
      WHEN 'PER06_LC'.
        PERFORM alv_fldcat_lc USING  c_006 s_day-y06_text f_period.
      WHEN 'PER07_LC'.
        PERFORM alv_fldcat_lc USING  c_007 s_day-y07_text f_period.
      WHEN 'PER08_LC'.
        PERFORM alv_fldcat_lc USING  c_008 s_day-y08_text f_period.
      WHEN 'PER09_LC'.
        PERFORM alv_fldcat_lc USING  c_009 s_day-y09_text f_period.
      WHEN 'PER10_LC'.
        PERFORM alv_fldcat_lc USING  c_010 s_day-y10_text f_period.
      WHEN 'PER11_LC'.
        PERFORM alv_fldcat_lc USING  c_011 s_day-y11_text f_period.
      WHEN 'PER12_LC'.
        PERFORM alv_fldcat_lc USING  c_012 s_day-y12_text f_period.
      WHEN 'PER13_LC'.
        PERFORM alv_fldcat_lc USING  c_013 s_day-y13_text f_period.

*        -----------------------
*        ----------------------- ccc00252  05-01-2018
      WHEN 'VTWEG'.
        fieldcat_ln-no_sum = 'X'.
*        -----------------------
      WHEN 'KLIMK'.
        fieldcat_ln-no_sum = 'X'.

      WHEN 'CONT1'.
        fieldcat_ln-seltext_l = TEXT-s52.

      WHEN 'CTELF'.
        fieldcat_ln-seltext_l = TEXT-s53.

      WHEN 'DAYS_DELQ'.
        fieldcat_ln-seltext_l = TEXT-s54.

*     ccc00252   16-01-2018
      WHEN 'PRCTR'.
*        IF pa_det NE 'X'.
*          fieldcat_ln-no_out  = 'X'.
*        ENDIF.

      WHEN OTHERS.
        fieldcat_ln-no_out    = 'X'.
    ENDCASE.

* ====================
    CASE fieldcat_ln-fieldname.
      WHEN 'PDMBTR'    OR 'PDMBE2'    OR 'PDMBE3' OR
           'OWRBTR_DB' OR 'OWRBTR_CR' OR 'OWRBTR' OR
           'PWRBTR_DB' OR 'PWRBTR_CR' OR 'PWRBTR' OR
            'WRBTR_DB' OR  'WRBTR_CR'.
        fieldcat_ln-tech    = 'X'.
    ENDCASE.

* ====================
    CASE fieldcat_ln-datatype.
      WHEN 'CURR'.
        IF fieldcat_ln-fieldname NE 'KLIMK'.
          fieldcat_ln-do_sum    = 'X'.
          fieldcat_ln-just      = 'R'.
        ENDIF.
    ENDCASE.

* ====================
    fieldcat_ln-reptext_ddic  = fieldcat_ln-seltext_s
                              = fieldcat_ln-seltext_m
                              = fieldcat_ln-seltext_l.
    MODIFY fieldcat FROM fieldcat_ln.
  ENDLOOP.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form alv_colpos
*&---------------------------------------------------------------------*
FORM alv_colpos.
  CLEAR col_pos.

  LOOP    AT fieldcat INTO fieldcat_ln.
    ADD 1 TO col_pos.
    MOVE     col_pos    TO fieldcat_ln-col_pos.
    MODIFY fieldcat   FROM fieldcat_ln.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form alv_head
*&---------------------------------------------------------------------*
FORM alv_head USING f_typ
                    f_txt
                    f_var1
                    f_var2
                    f_var3.

  DATA: w_txt1(100), w_txt2(100),w_txt3(100), w_txt4(100), w_txt5(100).

  DATA: w_info LIKE listheader_ln-info.

  CLEAR: listheader_ln, w_info.

  WRITE: f_typ  TO w_txt1,
         f_txt  TO w_txt2,
         f_var1 TO w_txt3,
         f_var2 TO w_txt4,
         f_var3 TO w_txt5.

  CONCATENATE  w_txt2
               w_txt3
               w_txt4
               w_txt5
          INTO w_info SEPARATED BY space.

  listheader_ln-typ     = f_typ.
  listheader_ln-info    = w_info.
  APPEND listheader_ln TO listheader.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form alv_scat
*&---------------------------------------------------------------------*
FORM alv_scat USING f_spos f_fldn f_lup f_ldwn f_stot f_expa f_tonly.

  CLEAR sortcat_ln.
  sortcat_ln-spos            = f_spos.
  sortcat_ln-fieldname       = f_fldn.
  sortcat_ln-up              = f_lup.
  sortcat_ln-down            = f_ldwn.
  sortcat_ln-subtot          = f_stot.
  sortcat_ln-expa            = f_expa.
  layout-totals_only         = f_tonly.

  APPEND  sortcat_ln  TO sortcat.
ENDFORM.

*&--------------------------------------------------------------------*
*&      Form  ZALV_USERCOMM_SUMM
*&--------------------------------------------------------------------*
FORM zalv_usercomm_summ USING r_ucomm     LIKE sy-ucomm
                              rs_selfield TYPE slis_selfield.
  DATA: ztab LIKE itab.

  RANGES: r_budat FOR bkpf-budat.

  fieldcat_xtab[] = fieldcat[].
  xdet            = 'X'.

  CLEAR      ztab.
  READ TABLE xtab INTO ztab INDEX rs_selfield-tabindex.

  CASE r_ucomm.
    WHEN '&IC1'.

      CASE rs_selfield-fieldname.
*       --------------
        WHEN 'KUNNR'.
          SET PARAMETER ID 'KUN' FIELD ztab-kunnr.
          CALL TRANSACTION 'XD03' AND SKIP FIRST SCREEN .
      ENDCASE.

      CASE rs_selfield-fieldname+0(3).
*       --------------
        WHEN 'PER'.
          PERFORM alv_delete_docs  USING ztab
                                         rs_selfield-fieldname+3(2).
          PERFORM alv_display_detailed_list
                                   USING rs_selfield-fieldname+3(2).
*       --------------
        WHEN 'BAL'.
          PERFORM alv_delete_docs           USING ztab  '00'.
          PERFORM alv_display_detailed_list USING       '00'.
      ENDCASE.

  ENDCASE.

  fieldcat[] = fieldcat_xtab[].
  xdet       = ' '.

  rs_selfield-refresh    = 'X'.
  rs_selfield-col_stable = 'X'.
  rs_selfield-row_stable = 'X'.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form zalv_set_status
*&---------------------------------------------------------------------*
FORM zalv_set_status USING rt_extab TYPE slis_t_extab.

*  IF pa_det IS NOT INITIAL.
    SET PF-STATUS 'SUMSTATUS'.
*  ELSE.
*    SET PF-STATUS 'STANDARD_FULLSCREEN'.
*  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form top_of_page
*&---------------------------------------------------------------------*
FORM top_of_page.
  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = listheader.
ENDFORM.

