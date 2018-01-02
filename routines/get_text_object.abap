*&---------------------------------------------------------------------*
*&      Form  GET_TEXT_OBJECT
*&---------------------------------------------------------------------*
FORM GET_TEXT_OBJECT  USING    P_VBELN
                               P_POSNR
                               P_KUNNR
                               P_OBJ
                               P_ID
                      CHANGING P_LINE.

  DATA : L_ID     TYPE THEAD-TDID.
  DATA : L_OBJ    TYPE THEAD-TDOBJECT.
  DATA : L_LANGU  TYPE THEAD-TDSPRAS.
  DATA : L_NAME   TYPE THEAD-TDNAME.
  DATA : LT_LINES TYPE STANDARD TABLE OF TLINE.
  DATA : LS_LINE TYPE TLINE.

  L_OBJ = P_OBJ.
  L_ID  = P_ID.
  SELECT SINGLE SPRAS FROM KNA1 INTO L_LANGU
  WHERE KUNNR = P_KUNNR.

  CASE P_OBJ.
    WHEN 'VBBK'.

      WRITE P_VBELN TO L_NAME.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = L_NAME(10)
        IMPORTING
          OUTPUT = L_NAME(10).


    WHEN 'VBBP'.

      CONCATENATE P_VBELN P_POSNR INTO L_NAME.
  ENDCASE.

  CALL FUNCTION 'READ_TEXT'
    EXPORTING
      ID                      = L_ID
      LANGUAGE                = L_LANGU
      NAME                    = L_NAME
      OBJECT                  = L_OBJ
    TABLES
      LINES                   = LT_LINES
    EXCEPTIONS
      ID                      = 1
      LANGUAGE                = 2
      NAME                    = 3
      NOT_FOUND               = 4
      OBJECT                  = 5
      REFERENCE_CHECK         = 6
      WRONG_ACCESS_TO_ARCHIVE = 7
      OTHERS                  = 8.

  IF SY-SUBRC <> 0.
    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        ID                      = L_ID
        LANGUAGE                = 'E'
        NAME                    = L_NAME
        OBJECT                  = L_OBJ
      TABLES
        LINES                   = LT_LINES
      EXCEPTIONS
        ID                      = 1
        LANGUAGE                = 2
        NAME                    = 3
        NOT_FOUND               = 4
        OBJECT                  = 5
        REFERENCE_CHECK         = 6
        WRONG_ACCESS_TO_ARCHIVE = 7
        OTHERS                  = 8.
  ENDIF.
  READ TABLE LT_LINES INTO LS_LINE INDEX 1.
  IF SY-SUBRC = 0.
    MOVE LS_LINE-TDLINE TO P_LINE.
  ENDIF.
ENDFORM.                    " GET_TEXT_OBJECT
