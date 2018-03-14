import
IV_CUOBJ	TYPE	CUOBJ	                     	Configuration (internal object number)
IV_ATNAM	TYPE	ATNAM	                     	Characteristic Name


export 
ev_value




   DATA:   lt_cuval  TYPE TABLE OF conf_out .
        DATA:   ls_cuval  TYPE conf_out .


        CALL FUNCTION 'VELO01_GET_CONFIGURATION'
          EXPORTING
            cuobj_number = iv_cuobj
          TABLES
            cuval_et     = lt_cuval.

        READ TABLE lt_cuval INTO ls_cuval WITH KEY atnam = iv_atnam.


    DATA ls_bapicharactdetail TYPE bapicharactdetail.
    DATA lt_ret               TYPE TABLE OF bapiret2.
    DATA lo_exception         TYPE REF TO cx_root.
    DATA lv_val TYPE atwrt.
    DATA lv_float TYPE atflv.
    DATA lv_num_string TYPE c LENGTH 30.

    lv_val = ls_cuval-atwrt.

    CALL FUNCTION 'BAPI_CHARACT_GETDETAIL'
      EXPORTING
        charactname   = iv_atnam
      IMPORTING
        charactdetail = ls_bapicharactdetail
      TABLES
        return        = lt_ret.


    CASE ls_bapicharactdetail-data_type.
      WHEN 'NUM' OR 'DATE' OR 'TIME'.
        CALL FUNCTION 'CTBP_CONVERT_VALUE_EXT_TO_INT'
          EXPORTING
            charactname       = iv_atnam
            value_external    = lv_val
            charactdetail     = ls_bapicharactdetail
          IMPORTING
            value_from        = lv_float
*           VALUE_TO          =
*           VALUE_RELATION    =
          EXCEPTIONS
            no_authority      = 1
            charact_not_found = 2
            wrong_data_type   = 3
            wrong_value       = 4
            wrong_input       = 5
            OTHERS            = 6.
        ev_value = lv_float.
      WHEN 'CHAR' .
        ev_value = lv_val.
      WHEN OTHERS.

    ENDCASE.
