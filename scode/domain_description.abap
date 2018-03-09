* get from domain description 
  CALL FUNCTION 'DD_DOMVALUES_GET'
    EXPORTING
      domname        = 'ZZPRINTCODE'   "<-- Your Domain Here
      text           = 'X'
      langu          = sy-langu
    TABLES
      dd07v_tab      = idd07v
    EXCEPTIONS
      wrong_textflag = 1
      OTHERS         = 2.

  LOOP AT idd07v WHERE domvalue_l EQ p_prtc.
    p_prtc_descr = idd07v-ddtext.
    EXIT.
  ENDLOOP.
