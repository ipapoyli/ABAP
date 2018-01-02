
*----------------------------------------------------------------------*
* Types                                                                *
*----------------------------------------------------------------------*
TYPES: BEGIN OF itab_type,
*        sel,
  ICON         LIKE ICON-ID,
  INDEX        TYPE lvc_index,
*----------------------------------------------------------------------*
  serial_no    LIKE afru-ltxa1,     "Transaction Serial No.
  sequence_no  TYPE afko-cy_seqnr,  "Production Order Number
  DATE         TYPE budat,          "Timestamp
  machine_time TYPE ru_ismng,       "Machine Time
  energy_kwh   TYPE ru_ismng,       "Energy
  work_center  TYPE crhd-arbpl,     "Work center
*----------------------------------------------------------------------*
  msg          TYPE string.
*----------------------------------------------------------------------*
TYPES: END OF itab_type.


*----------------------------------------------------------------------*
* Global Variables                                                     *
*----------------------------------------------------------------------*
DATA: itab    TYPE TABLE OF itab_type WITH HEADER LINE.
DATA: it_file LIKE alsmex_tabline OCCURS 0 WITH HEADER LINE.

************************************************************************
*                S E L E C T I O N   S C R E E N                       *
************************************************************************

SELECTION-SCREEN  BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: filepath  LIKE rlgrap-filename OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.


*----------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
AT SELECTION-SCREEN ON VALUE-REQUEST FOR FILEPATH.
*----------------------------------------------------------------------*
  DATA : FILE_TABLE TYPE FILETABLE.
  DATA : RC TYPE I.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    CHANGING
      FILE_TABLE              = FILE_TABLE
      RC                      = RC
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  CHECK RC = 1.
  READ TABLE FILE_TABLE INDEX 1 INTO FILEPATH.
*----------------------------------------------------------------------*

  PERFORM READ_FILE_FROM_LOCAL.
  PERFORM CONVERT_DATA.


*&---------------------------------------------------------------------*
*&      Form  READ_FILE_FROM_LOCAL
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_file_from_local .

REFRESH it_file.
CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
EXPORTING
  filename                = filepath
  i_begin_col             = 1
  i_begin_row             = 1
  i_end_col               = 10
  i_end_row               = 5000
TABLES
  intern                  = it_file
EXCEPTIONS
  inconsistent_parameters = 1
  upload_ole              = 2
  OTHERS                  = 3.

IF sy-subrc <> 0.
  MESSAGE s398(00) WITH 'Error Uploading XLS file'.
ENDIF.


ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CONVERT_DATA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM convert_data .

  DATA : ln_pos TYPE lvc_index.
  REFRESH: itab.

  LOOP AT it_file.

    CASE it_file-col.
    WHEN '0001'.
      MOVE it_file-VALUE TO itab-serial_no  .
    WHEN '0002'.
      MOVE it_file-VALUE TO itab-sequence_no.
    WHEN '0003'.
*        MOVE it_file-value TO itab-date .
      CONCATENATE it_file-VALUE+6(4) it_file-VALUE+3(2) it_file-VALUE(2) INTO itab-DATE.
    WHEN '0004'.
      MOVE it_file-VALUE TO itab-machine_time.
    WHEN '0005'.
      MOVE it_file-VALUE TO itab-energy_kwh.
    WHEN '0006'.
      MOVE it_file-VALUE TO itab-work_center.
    ENDCASE.
*
    AT END OF row.
      ADD 1 TO ln_pos.
      itab-INDEX = ln_pos.
      itab-ICON = icon_yellow_light.
      APPEND itab.
      CLEAR itab.
    ENDAT.

  ENDLOOP.

ENDFORM.


