DATA : GT_COUNTRIES LIKE T005T OCCURS 100 WITH HEADER LINE.

*&---------------------------------------------------------------------*
*&      Form  GET_COUNTRY
*&---------------------------------------------------------------------*
*       Get Countries
*----------------------------------------------------------------------*
FORM GET_COUNTRY  USING    PL_SPRAS
                           PL_LAND1
                  CHANGING PL_LANDX.

  CLEAR : GT_COUNTRIES.
  READ TABLE GT_COUNTRIES WITH KEY SPRAS = PL_SPRAS
                                   LAND1 = PL_LAND1.

  IF SY-SUBRC = 0.
*
  ELSE.
    SELECT SINGLE * FROM T005T INTO GT_COUNTRIES
      WHERE SPRAS = PL_SPRAS
      AND   LAND1 = PL_LAND1.
    IF SY-SUBRC = 0.
      APPEND GT_COUNTRIES.
    ENDIF.
  ENDIF.

  PL_LANDX = GT_COUNTRIES-LANDX.

ENDFORM.
