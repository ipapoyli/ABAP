
*loop
*    ---------------------------------------------- Due Date
    CLEAR: bkpf , kna1 .
    SELECT SINGLE * FROM bkpf WHERE awkey = itab-vbeln
                                AND bukrs = itab-bukrs.
    SELECT SINGLE * FROM kna1 WHERE kunnr = itab-kunrg.

    IF sy-subrc = 0.
      CLEAR knvv.
      SELECT SINGLE * FROM knvv WHERE kunnr = itab-kunrg.

      l_date = bkpf-bldat.

      SELECT SINGLE zfbdt FROM bseg INTO l_date
                            WHERE bukrs = bkpf-bukrs
                              AND belnr = bkpf-belnr
                              AND gjahr = bkpf-gjahr
                              AND buzei = 1.

      PERFORM get_due_date USING knvv-zterm l_date kna1-katr4
                 CHANGING due_date.

      itab-due_date = due_date.

    ENDIF.
*    ----------------------------------------------
*endloop


*&---------------------------------------------------------------------*
*&      Form  GET_DUE_DATE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_KNVV_ZTERM  text
*      -->P_L_DATE  text
*      -->P_KNA1_KATR4  text
*      <--P_DUE_DATE  text
*----------------------------------------------------------------------*
FORM get_due_date  USING i_zterm
                         i_base_date
                         i_katr4
                   CHANGING due_date.

* calculate due date
  DATA : ls_t052 TYPE t052.
  DATA : ls_faede TYPE faede.
  DATA : l_reduce TYPE i.

  CHECK : i_zterm     IS NOT INITIAL,
          i_base_date IS NOT INITIAL.

  SELECT SINGLE * FROM  t052 INTO ls_t052
                  WHERE zterm  = i_zterm.

  CLEAR ls_faede.
  MOVE-CORRESPONDING ls_t052 TO ls_faede.
  MOVE: ls_t052-ztag1 TO ls_faede-zbd1t,
        ls_t052-ztag2 TO ls_faede-zbd2t,
        ls_t052-ztag3 TO ls_faede-zbd3t.

  MOVE i_base_date TO ls_faede-bldat.
  ls_faede-koart = 'D'.

  CALL FUNCTION 'DETERMINE_DUE_DATE'
    EXPORTING
      i_faede = ls_faede
    IMPORTING
      e_faede = ls_faede
    EXCEPTIONS
      OTHERS  = 1.

  CHECK sy-subrc EQ 0.
  due_date = ls_faede-netdt.

  IF i_katr4 IS NOT INITIAL.
    l_reduce = i_katr4.
    SUBTRACT l_reduce FROM due_date.
  ENDIF.


ENDFORM.


