*&---------------------------------------------------------------------*
*&      Form  CHECK_MM_STOCK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_mm_stock USING p_itab STRUCTURE itab CHANGING d_error.

  DATA: mm_available_qty TYPE menge_d,
        mm_meins         TYPE meins,
        waiting_qty      TYPE menge_d,
        waiting_meins    TYPE meins,
        l_lgtyp          TYPE lgtyp.

* < collect available quant
  CLEAR: mm_available_qty, waiting_qty,
  mm_meins,         waiting_meins.

  CASE p_itab-sobkz.
    WHEN ' '.
      IF p_itab-bestq = space.
        SELECT SINGLE mchb~clabs mara~meins
        FROM mchb
        JOIN mara ON mara~matnr = mchb~matnr
        INTO (mm_available_qty, mm_meins)
        WHERE mchb~werks = p_itab-werks
        AND mchb~lgort = p_itab-lgort
        AND mchb~matnr = p_itab-matnr
        AND mchb~charg = p_itab-charg.
      ELSE.
        SELECT SINGLE mchb~cspem mara~meins
        FROM mchb
        JOIN mara ON mara~matnr = mchb~matnr
        INTO (mm_available_qty, mm_meins)
        WHERE mchb~werks = p_itab-werks
        AND mchb~lgort = p_itab-lgort
        AND mchb~matnr = p_itab-matnr
        AND mchb~charg = p_itab-charg.
      ENDIF.
    WHEN 'E'.
      IF p_itab-bestq = space.
        SELECT SINGLE mska~kalab mara~meins
        FROM mska
        JOIN mara ON mara~matnr = mska~matnr
        INTO (mm_available_qty, mm_meins)
        WHERE mska~werks = p_itab-werks
        AND mska~lgort = p_itab-lgort
        AND mska~matnr = p_itab-matnr
        AND mska~charg = p_itab-charg.
      ELSE.
        SELECT SINGLE mska~kaspe mara~meins
        FROM mska
        JOIN mara ON mara~matnr = mska~matnr
        INTO (mm_available_qty, mm_meins)
        WHERE mska~werks = p_itab-werks
        AND mska~lgort = p_itab-lgort
        AND mska~matnr = p_itab-matnr
        AND mska~charg = p_itab-charg.
      ENDIF.
  ENDCASE.

  IF mm_available_qty < p_itab-verme + waiting_qty.
**    PERFORM message_error_screen USING
**          TEXT-199
**          TEXT-256
**          TEXT-257
**          TEXT-258
**          '' '' '' '' '' ''.
    d_error = 'X'.
  ENDIF.

ENDFORM.                    " CHECK_MM_STOCK
