DATA : gt_regions LIKE t005u OCCURS 100 WITH HEADER LINE. 

*&---------------------------------------------------------------------*
*&      Form  GET_REGION          
*&---------------------------------------------------------------------*
*       Get Regions
*----------------------------------------------------------------------*
FORM get_region  USING    pl_spras
                          pl_land1
                          pl_region
                 CHANGING pl_bezei.

  CLEAR : gt_regions.
  READ TABLE gt_regions WITH KEY spras = pl_spras
                                 land1 = pl_land1
                                 bland = pl_region.

  IF sy-subrc = 0.
*    
  ELSE.
    SELECT SINGLE * FROM t005u INTO gt_regions
    WHERE spras = pl_spras
    AND   land1 = pl_land1
    AND   bland = pl_region.
    IF sy-subrc = 0.
      APPEND gt_regions.
    ENDIF.
  ENDIF.

  pl_bezei = gt_regions-bezei.

ENDFORM.
