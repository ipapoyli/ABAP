
PARAMETERS       p_lgnum LIKE  lagp-lgnum OBLIGATORY .

*----------------------------------------------------------------------*  

INITIALIZATION.
  g_repid = sy-repid.
  g_uname = sy-uname.
  g_langu = sy-langu.

  GET PARAMETER ID 'LGN' FIELD p_lgnum.
  
*----------------------------------------------------------------------*  
  
  SET PARAMETER ID 'LGN' FIELD P_LGNUM.
