DATA : BEGIN OF GT_SHIP_TO OCCURS 100,    
         KUNNR TYPE KNA1-KUNNR,
         LAND1 TYPE KNA1-LAND1,
         NAME1 TYPE KNA1-NAME1,
         STRAS TYPE KNA1-STRAS,
         ADRNR TYPE KNA1-ADRNR,
         SPRAS TYPE KNA1-SPRAS,
*        bland TYPE t005u-bland,  "Region
         BEZEI TYPE T005U-BEZEI,
         LANDX TYPE T005T-LANDX,
       END OF GT_SHIP_TO.


*&---------------------------------------------------------------------*
*&      Form  GET_SHIP_TO_DATA
*&---------------------------------------------------------------------*
*       Get Ship-To data
*----------------------------------------------------------------------*
FORM GET_SHIP_TO_DATA  USING    PL_KUNNR.
  DATA : LV_REGION LIKE ADRC-REGION.

  CLEAR : GT_SHIP_TO.
  READ TABLE GT_SHIP_TO WITH KEY KUNNR = PL_KUNNR.

  IF SY-SUBRC = 0.
*
  ELSE.
    CLEAR : KNA1.
    SELECT SINGLE * FROM KNA1 WHERE KUNNR = PL_KUNNR.

    IF SY-SUBRC = 0.

      MOVE-CORRESPONDING KNA1 TO GT_SHIP_TO.

      CLEAR : LV_REGION.

      SELECT SINGLE REGION FROM ADRC INTO LV_REGION
        WHERE ADDRNUMBER = GT_SHIP_TO-ADRNR
        AND   NATION = ''.
      IF SY-SUBRC = 0.
        PERFORM GET_REGION USING GT_SHIP_TO-SPRAS
                                 GT_SHIP_TO-LAND1
                                 LV_REGION
                           CHANGING GT_SHIP_TO-BEZEI.
      ENDIF.

      PERFORM GET_COUNTRY USING GT_SHIP_TO-SPRAS
                                GT_SHIP_TO-LAND1
                          CHANGING GT_SHIP_TO-LANDX.

      APPEND GT_SHIP_TO.

    ENDIF.
  ENDIF.

*  POSTAB-TG_SHIPTO_KUNNR = GT_SHIP_TO-KUNNR.
*  POSTAB-TG_SHIPTO_STRAS = GT_SHIP_TO-STRAS.
*  POSTAB-TG_SHIPTO_NAME1 = GT_SHIP_TO-NAME1.
*  POSTAB-TG_SHIPTO_REGION = GT_SHIP_TO-BEZEI.
*  POSTAB-TG_SHIPTO_COUNTRY = GT_SHIP_TO-LANDX.

ENDFORM.
