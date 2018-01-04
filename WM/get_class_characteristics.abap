*---------------------------------------------------------------------*
*       FORM get_class_characteristics                                      *
*---------------------------------------------------------------------*
FORM get_class_characteristics USING p_klart p_class.

* extract the characteristic names for KLART/CLASS
  CLEAR: klah, ksml.
  FREE it_cabn.

  SELECT SINGLE clint FROM klah
  INTO CORRESPONDING FIELDS OF klah
  WHERE klart = p_klart
    AND class = p_class.

  IF syst-subrc = 0.
    SELECT * FROM ksml
    INTO CORRESPONDING FIELDS OF ksml
    WHERE clint = klah-clint.
      SELECT SINGLE * FROM cabn
      INTO CORRESPONDING FIELDS OF cabn
      WHERE atinn = ksml-imerk.
      IF syst-subrc = 0.
        it_cabn-atinn = cabn-atinn.
        it_cabn-atnam = cabn-atnam.
        it_cabn-atfor = cabn-atfor.
        APPEND it_cabn.
      ENDIF.
    ENDSELECT.
  ENDIF.

ENDFORM.
