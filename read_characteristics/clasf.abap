import
IV_MATNR	TYPE	MATNR	                     	Material Number
IV_CHARG	TYPE	CHARG_D	                     	Batch Number
IV_CLASSTYPE	TYPE	KLASSENART	                     	Class Type
IV_ATNAM	TYPE	ATNAM	                     	Characteristic Name


export
EV_VALUE




FUNCTION ztk_xx_read_claf_single_val.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_MATNR) TYPE  MATNR
*"     VALUE(IV_CHARG) TYPE  CHARG_D OPTIONAL
*"     VALUE(IV_CLASSTYPE) TYPE  KLASSENART
*"     VALUE(IV_ATNAM) TYPE  ATNAM
*"  EXPORTING
*"     REFERENCE(EV_VALUE)
*"----------------------------------------------------------------------

  DATA: objectkey_imp   LIKE bapi1003_key-object,
        objecttable_imp LIKE bapi1003_key-objecttable,
        classtype_imp   LIKE bapi1003_key-classtype,
        alloclist       TYPE bapi1003_alloc_list
                             OCCURS 0 WITH HEADER LINE,
        return          TYPE bapiret2 OCCURS 0 WITH HEADER LINE.

  DATA: class        LIKE  klah-class,
        classtype    LIKE  klah-klart,
        object       LIKE  ausp-objek,
        objecttable  LIKE  tcla-obtab,
        t_class      TYPE  sclass OCCURS 0 WITH HEADER LINE,
        t_objectdata TYPE clobjdat OCCURS 0 WITH HEADER LINE.

  classtype_imp = classtype = iv_classtype.
  IF iv_classtype = '023'.
    CONCATENATE iv_matnr iv_charg INTO objectkey_imp RESPECTING BLANKS.
    objecttable_imp = objecttable = 'MCH1'.
  ELSE.
    objectkey_imp = iv_matnr.
    objecttable_imp = objecttable = 'MARA'.
  ENDIF.
  object = objectkey_imp.

  FREE: alloclist, return.
  CALL FUNCTION 'BAPI_OBJCL_GETCLASSES'
    EXPORTING
      objectkey_imp   = objectkey_imp
      objecttable_imp = objecttable_imp
      classtype_imp   = classtype_imp
    TABLES
      alloclist       = alloclist
      return          = return.

  READ TABLE alloclist INTO alloclist INDEX 1.
  class = alloclist-classnum.

  FREE: t_class, t_objectdata.
  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
    EXPORTING
      class              = class
      classtype          = classtype
      object             = object
      objecttable        = objecttable
    TABLES
      t_class            = t_class
      t_objectdata       = t_objectdata
    EXCEPTIONS
      no_classification  = 1
      no_classtypes      = 2
      invalid_class_type = 3
      OTHERS             = 4.

  DATA ls_bapicharactdetail TYPE bapicharactdetail.
  DATA lt_ret               TYPE TABLE OF bapiret2.
  DATA lo_exception         TYPE REF TO cx_root.
  DATA lv_val TYPE atwrt.
  DATA lv_float TYPE atflv.
  DATA lv_num_string TYPE c LENGTH 30.

  CLEAR t_objectdata.
  CLEAR lv_val.
  READ TABLE t_objectdata WITH KEY atnam = iv_atnam.
  IF sy-subrc = 0.
    CHECK t_objectdata-ausp1 <> '?'.
    lv_val = t_objectdata-ausp1.
  ENDIF.

  FREE lt_ret.
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
*         VALUE_TO          =
*         VALUE_RELATION    =
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



ENDFUNCTION.
