*---------------------------------------------------------------------*
*       FORM get_characteristics                                      *
*---------------------------------------------------------------------*
FORM get_characteristics
                         USING objectkey LIKE  bapi1003_key-object
                             objecttable LIKE  bapi1003_key-objecttable
                                classnum LIKE  bapi1003_key-classnum
                               classtype LIKE  bapi1003_key-classtype.

  CLEAR: values_num[],
         values_char[],
         values_curr[],
         return[].
         
  CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
    EXPORTING
      objectkey       = objectkey
      objecttable     = objecttable
      classnum        = classnum
      classtype       = classtype
      language        = sy-langu
    TABLES
      allocvaluesnum  = values_num
      allocvalueschar = values_char
      allocvaluescurr = values_curr
      return          = return.
      
      
  LOOP AT return INTO ls_return WHERE type = 'E'
                                   OR type = 'A'.
                                   
  ENDLOOP.

ENDFORM.
