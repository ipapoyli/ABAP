PROCESS BEFORE OUTPUT.
  MODULE liste_initialisieren.
  LOOP AT extract WITH CONTROL
   tctrl_ztk_master_matnr CURSOR nextline.
    MODULE liste_show_liste.
  ENDLOOP.
*
PROCESS AFTER INPUT.
  MODULE liste_exit_command AT EXIT-COMMAND.
  MODULE liste_before_loop.
  LOOP AT extract.
    MODULE liste_init_workarea.
    CHAIN.
      FIELD ztk_master_matnr-master_material .
      FIELD ztk_master_matnr-material .
      FIELD ztk_master_matnr-description .
      FIELD ztk_master_matnr-descr .
      FIELD ztk_master_matnr-rate .
      MODULE set_update_flag ON CHAIN-REQUEST.

*     ccc00252  24-01-2018
      MODULE get_descriptions.

    ENDCHAIN.
    FIELD vim_marked MODULE liste_mark_checkbox.
    CHAIN.
      FIELD ztk_master_matnr-master_material .
      FIELD ztk_master_matnr-material .
      MODULE liste_update_liste.
    ENDCHAIN.
  ENDLOOP.
  MODULE liste_after_loop.
