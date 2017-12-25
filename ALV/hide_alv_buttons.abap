* hide Buttons
  IF p_rb1 = 'X'.
    ls_excl_tab-fcode = '&RLB'.
    APPEND ls_excl_tab TO lt_excl_tab.
    ls_excl_tab-fcode = '&PRSUL'.
    APPEND ls_excl_tab TO lt_excl_tab.
    ls_excl_tab-fcode = '&PRLMM'.
    APPEND ls_excl_tab TO lt_excl_tab.

  ELSEIF   p_rb2 = 'X'.
    ls_excl_tab-fcode = '&RLB'.
    APPEND ls_excl_tab TO lt_excl_tab.
    ls_excl_tab-fcode = '&PRSUL'.
    APPEND ls_excl_tab TO lt_excl_tab.
    ls_excl_tab-fcode = '&PRLMM'.
    APPEND ls_excl_tab TO lt_excl_tab.

  ELSEIF  p_rb3 = 'X'.
    ls_excl_tab-fcode = '&RLB'.
    APPEND ls_excl_tab TO lt_excl_tab.
    ls_excl_tab-fcode = '&PRSUL'.
    APPEND ls_excl_tab TO lt_excl_tab.
    ls_excl_tab-fcode = '&PRLMM'.
    APPEND ls_excl_tab TO lt_excl_tab.
    ls_excl_tab-fcode = '&CRT'.
    APPEND ls_excl_tab TO lt_excl_tab.

  ENDIF.
