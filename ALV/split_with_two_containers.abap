*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
data  gt_fieldcat_o TYPE lvc_t_fcat,
      gt_fieldcat_b TYPE lvc_t_fcat,
      gs_fieldcat   TYPE lvc_s_fcat,
      gs_layout_o   TYPE lvc_s_layo,
      gs_layout_b   TYPE lvc_s_layo.

DATA: alv_container TYPE REF TO cl_gui_docking_container,
      alv_grid_o    TYPE REF TO cl_gui_alv_grid,
      alv_grid_b    TYPE REF TO cl_gui_alv_grid.

DATA:
* Reference to split container
  dg_splitter      TYPE REF TO cl_gui_splitter_container,
* Reference to grid container Orders
  dg_parent_grid_o TYPE REF TO cl_gui_container,
* Reference to grid container Batches
  dg_parent_grid_b TYPE REF TO cl_gui_container.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
  CHECK alv_container IS INITIAL.

  CREATE OBJECT alv_container
    EXPORTING
      repid     = sy-repid
      dynnr     = sy-dynnr
      side      = alv_container->dock_at_bottom
      extension = 245.
*----------------------------------------------------------------------*
* Create Splitter for alv_container
  CREATE OBJECT dg_splitter
    EXPORTING
      parent  = alv_container
      rows    = 2
      columns = 1.
*----------------------------------------------------------------------*
* Split the alv_container to two containers
  CALL METHOD dg_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = dg_parent_grid_o.
*----------------------------------------------------------------------*
  CALL METHOD dg_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = dg_parent_grid_b.
*----------------------------------------------------------------------*
* Set splitter height
  CALL METHOD dg_splitter->set_row_height
    EXPORTING
      id     = 1
      height = 40.
*----------------------------------------------------------------------*
*Specify parent as splitter part for ALV grid creation

  CREATE OBJECT alv_grid_o
    EXPORTING
      i_parent = dg_parent_grid_o.

  CREATE OBJECT alv_grid_b
    EXPORTING
      i_parent = dg_parent_grid_b.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
    CALL METHOD alv_grid_o->set_table_for_first_display
      EXPORTING
        i_save          = 'A'
        is_layout       = gs_layout_o
      CHANGING
        it_fieldcatalog = gt_fieldcat_o
        it_outtab       = itab_o[].

    CALL METHOD alv_grid_b->set_table_for_first_display
      EXPORTING
        i_save               = 'A'
        is_layout            = gs_layout_b
        it_toolbar_excluding = lt_exclude
      CHANGING
        it_fieldcatalog      = gt_fieldcat_b
        it_outtab            = itab_b[].
        
        *----------------------------------------------------------------------*
