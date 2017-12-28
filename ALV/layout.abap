data   gs_layout       TYPE lvc_s_layo.

FORM layout  CHANGING p_gs_layout TYPE lvc_s_layo.

  p_gs_layout-zebra = 'X'.
  p_gs_layout-box_fname = 'SEL'.
  p_gs_layout-sel_mode = 'A' .
  p_gs_layout-ctab_fname = 'CELLCOLOR'.
  p_gs_layout-col_opt = 'X'.  " optimize columns

ENDFORM.
