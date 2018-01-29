TABLES sscrfields .

* function key data
DATA: functxt TYPE smp_dyntxt.

*----------------------------------------------------------------------*
* Selection screen                                                     *
*----------------------------------------------------------------------*
SELECTION-SCREEN : FUNCTION KEY 1.
SELECTION-SCREEN : FUNCTION KEY 2.

*----------------------------------------------------------------------*
INITIALIZATION.

  CLEAR functxt.
  functxt-icon_id = icon_table_settings.
  functxt-quickinfo = 'Batch Stock'.
  functxt-icon_text = 'Batch Stock'.
  sscrfields-functxt_01 = functxt.

  CLEAR functxt.
  functxt-icon_id = icon_table_settings.
  functxt-quickinfo = 'Material Stock'.
  functxt-icon_text = 'Material Stock'.
  sscrfields-functxt_02 = functxt.
  *----------------------------------------------------------------------*
  
  *----------------------------------------------------------------------*
  AT SELECTION-SCREEN.
  
  * Call Function Routine
  CASE sscrfields-ucomm.

    WHEN 'FC01'. "batch stock
      CALL TRANSACTION 'ZPPDL045TB'.

    WHEN 'FC02'. "Material stock
      CALL TRANSACTION 'ZPPDL045TM'.

  ENDCASE.
