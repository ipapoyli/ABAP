DATA  gv_lines TYPE n.
DATA: t_rows   TYPE lvc_t_row.
DATA: s_row    TYPE lvc_s_row.

FORM select_rows .

  CLEAR: gv_lines,t_rows.
  CLEAR: t_rows , s_row .
  CALL METHOD g_grid->get_selected_rows
    IMPORTING
      et_index_rows = t_rows.
  DESCRIBE TABLE t_rows LINES gv_lines .

ENDFORM.
