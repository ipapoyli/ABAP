DATA BEGIN OF ITAB.
DATA BUKRS LIKE EKKO-BUKRS.
DATA SPRAS LIKE LFA1-SPRAS.
DATA ADRN2 LIKE EKPO-ADRN2.
DATA EBELN LIKE EKPO-EBELN.
data ADRNR like EKPO-ADRNR.
data WERKS like EKPO-WERKS.
DATA END OF ITAB.


*&---------------------------------------------------------------------*
*&      Form  get_ship_to_address
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->IN_TAB     text
*      -->OUT_TAB    text
*----------------------------------------------------------------------*
FORM GET_SHIP_TO_ADDRESS TABLES IN_TAB STRUCTURE ITCSY
  OUT_TAB STRUCTURE ITCSY.

  DATA LV_WERKS TYPE EKPO-WERKS.
  DATA LS_ADRC  TYPE ADRC.
  DATA LV_ADRNR TYPE EKPO-ADRNR.
  DATA LV_ADRN2 TYPE EKPO-ADRN2.
  DATA LV_EBELN TYPE EKPO-EBELN.
  DATA LV_BEZEI TYPE T005U-BEZEI.

  LV_ADRN2 = ITAB-ADRN2.

  IF LV_ADRN2 IS NOT INITIAL.
    LV_ADRNR = LV_ADRN2.
  ELSE.

    LV_EBELN = iTAB-EBELN.

    SELECT SINGLE ADRN2 FROM EKPO INTO LV_ADRN2
    WHERE EBELN = LV_EBELN
    AND   ADRN2 NE SPACE.

    IF SY-SUBRC = 0.
      LV_ADRNR = LV_ADRN2.
    ELSE.


      LV_ADRNR = ITAB-adrnr.

      IF LV_ADRNR IS INITIAL .
        LV_WERKS = ITAB-WERKS.

        CLEAR LV_ADRNR.
        SELECT SINGLE ADRNR FROM T001W INTO LV_ADRNR
        WHERE WERKS = LV_WERKS.
      ENDIF.
    ENDIF.
  ENDIF.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
    EXPORTING
      INPUT  = LV_ADRNR
    IMPORTING
      OUTPUT = LV_ADRNR.

  SELECT SINGLE * FROM ADRC INTO LS_ADRC
    WHERE ADDRNUMBER = LV_ADRNR.

  SELECT SINGLE BEZEI FROM T005U INTO LV_BEZEI
    WHERE LAND1 = LS_ADRC-COUNTRY 
    AND   BLAND = LS_ADRC-REGION 
    AND   SPRAS = SY-LANGU .

ENDFORM.                    "get_ship_to_address
