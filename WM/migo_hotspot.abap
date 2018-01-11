*     Number of Material Document
      IF e_column_id-fieldname = 'MBLNR'.
        CHECK NOT tbp-mblnr IS INITIAL.
        CALL FUNCTION 'MIGO_DIALOG'
          EXPORTING
            i_action            = 'A04'
            i_refdoc            = 'R02'
            i_notree            = 'X'
            i_no_auth_check     = ' '
            i_deadend           = 'X'
            i_skip_first_screen = 'X'
            i_okcode            = 'OK_GO'
            i_mblnr             = tbp-mblnr
            i_mjahr             = tbp-mjahr.
*          i_zeile             = list-zeile.
      ENDIF.
