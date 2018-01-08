*PROCESS BEFORE OUTPUT.
*  MODULE status_0400.

*PROCESS AFTER INPUT.
*  MODULE user_command_0400.

*  MODULE exit_command AT EXIT-COMMAND.

*PROCESS ON VALUE-REQUEST.
*  FIELD t307-letyp MODULE set_values.



*&---------------------------------------------------------------------*
*&      Module  SET_VALUES  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE set_values INPUT.

  PERFORM search_help_letyp.

ENDMODULE.


*&---------------------------------------------------------------------*
*&      Form  SEARCH_HELP_LETYP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM search_help_letyp .

  DATA: BEGIN OF itab_letyp OCCURS 0,
          letyp TYPE t307-letyp,
          lgnum TYPE t307-lgnum, " Warehouse Number
        END OF itab_letyp.


  SELECT letyp lgnum FROM t307
    INTO CORRESPONDING FIELDS OF TABLE itab_letyp
    WHERE lgnum = itab_x-lgnum.

  IF sy-subrc NE 0.
*
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield        = 'LETYP'
      dynpprog        = sy-repid
      dynpnr          = sy-dynnr
      dynprofield     = 'T307-LETYP'
      value           = space
      value_org       = 'S'
      display         = 'F'
    TABLES
      value_tab       = itab_letyp
*     return_tab      = t_return
    EXCEPTIONS
      parameter_error = 1
      no_values_found = 2
      OTHERS          = 3.


  READ TABLE itab_letyp INDEX 1.
  IF sy-subrc EQ 0.
    MOVE itab_letyp-letyp TO t307-letyp.
  ENDIF.


ENDFORM. " search_help_letyp
