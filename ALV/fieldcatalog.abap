**----------------------------------------------------------------------
** ALV Data first screen
**----------------------------------------------------------------------
DATA:
  GS_FIELDCATALOG TYPE LVC_S_FCAT OCCURS 0,
  GV_FCAT         LIKE LINE OF GS_FIELDCATALOG,
  GS_LAYOUT       TYPE LVC_S_LAYO.

DATA FC_COLPOS TYPE LVC_COLPOS.

* example
PERFORM FIELDCATALOG using fc_colpos 'FIELDNAME'  'REF_TABLE'  'REF_FIELD'
                                     'SCRTEXT_L'  'L'  'X' .


*&---------------------------------------------------------------------*
*&      Form  FIELDCATALOG
*&---------------------------------------------------------------------*
FORM FIELDCATALOG USING    FC_COLPOS
                           P_FIELDNAME  TYPE LVC_FNAME
                           P_REF_TABLE  TYPE LVC_RTNAME
                           P_REF_FIELD  TYPE LVC_RFNAME
                           P_SCRTEXT_L  TYPE SCRTEXT_L
                           P_COLDDICTXT TYPE LVC_DDICT
                           P_HOTSPOT    TYPE LVC_HOTSPT .

  CLEAR GV_FCAT.
  ADD 1 TO FC_COLPOS.
  GV_FCAT-COL_POS    = FC_COLPOS.
  GV_FCAT-FIELDNAME  = P_FIELDNAME.
  GV_FCAT-REF_TABLE  = P_REF_TABLE.
  GV_FCAT-REF_FIELD  = P_REF_FIELD.
  GV_FCAT-SCRTEXT_L  = P_SCRTEXT_L.
  GV_FCAT-COLDDICTXT = P_COLDDICTXT.
  GV_FCAT-HOTSPOT    = P_HOTSPOT.
  INSERT GV_FCAT INTO TABLE GS_FIELDCATALOG.

ENDFORM.
