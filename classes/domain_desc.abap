

*----------------------------------------------------------------------*
*       CLASS cl_domain_desc DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_domain_desc  DEFINITION.
  PUBLIC SECTION.
    METHODS : get_domain_descr
      IMPORTING
        i_field       TYPE any
        i_langu       TYPE  syst-langu DEFAULT sy-langu
      RETURNING
        VALUE(return) TYPE ddtext .
ENDCLASS.                    "cl_domain_desc DEFINITION

*----------------------------------------------------------------------*
*       CLASS cl_domain_desc IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS cl_domain_desc IMPLEMENTATION .
  METHOD get_domain_descr.

    DATA : lo_element TYPE REF TO cl_abap_elemdescr,
           li_values  TYPE ddfixvalues,
           ls_value   TYPE ddfixvalue.

    lo_element ?= cl_abap_typedescr=>describe_by_data( i_field ) .
    li_values =  lo_element->get_ddic_fixed_values( i_langu ) .
    READ TABLE li_values INTO ls_value WITH KEY low = i_field .
    IF sy-subrc = 0 .
      return = ls_value-ddtext .
    ENDIF.
  ENDMETHOD.                    "GET_DOMAIN_DESCR
ENDCLASS .                    "cl_domain_desc IMPLEMENTATION




* ------------------------------------------------------------------- *
* data for domain desc
* ------------------------------------------------------------------- *
DATA : lo_domain_text_test TYPE REF TO cl_domain_desc .
DATA : lv_domain    TYPE koart      VALUE 'A' . "variable defined using domain
DATA : lv_description  TYPE ddtext .
* ------------------------------------------------------------------- *


* ------------------------------------------------------------------- *
START-OF-SELECTION.
* ------------------------------------------------------------------- *




* ------------------------------------------------------------------- *
*get domain description *
* ------------------------------------------------------------------- *
  CREATE OBJECT lo_domain_text_test .
  CLEAR lv_description .
  lv_description = lo_domain_text_test->get_domain_descr( lv_domain ) .
