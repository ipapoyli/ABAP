*----------------------------------------------------------------------*
* Selection screen data                                                   *
*----------------------------------------------------------------------*
DATA: f4_on_pc.
DATA:
  gv_path_ini TYPE string,
  gv_path_sel TYPE string.
DATA dir TYPE string.
DATA rc TYPE c.


*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE text-b03 .  "Destination specifications
SELECTION-SCREEN SKIP.
PARAMETERS  : p_locf  RADIOBUTTON GROUP rdg2 DEFAULT 'X',
              p_servf RADIOBUTTON GROUP rdg2.
PARAMETERS: p_path    LIKE btch0000-text80  OBLIGATORY.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN END OF BLOCK b03.

*----------------------------------------------------------------------*
*----------------------------------------------------------------------*

*----------------------------------------------------------------------
INITIALIZATION.
*----------------------------------------------------------------------
* Selection screen processing

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_path.


  perform is_local_file changing f4_on_pc .

  if f4_on_pc = 'X'.
    CALL METHOD cl_gui_frontend_services=>directory_browse
      EXPORTING
        initial_folder  = gv_path_ini
      CHANGING
        selected_folder = gv_path_sel
      EXCEPTIONS
        cntl_error      = 1
        error_no_gui    = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      CALL METHOD cl_gui_cfw=>flush( ).
      IF NOT gv_path_sel IS INITIAL.
        gv_path_ini = p_path = gv_path_sel.
      ENDIF.
    ENDIF.

  else.
    CALL FUNCTION '/SAPDMC/LSM_F4_SERVER_FILE'
      EXPORTING
*       DIRECTORY  = ' '
        FILEMASK   = '?'
      IMPORTING
        SERVERFILE = p_path
*    EXCEPTIONS
*       CANCELED_BY_USER       = 1
*       OTHERS     = 2
      .


  endif.


at selection-screen on p_path.


  clear rc.
  dir = p_path.
  perform is_local_file changing f4_on_pc .
  if  f4_on_pc = 'X'.
    if not ( p_path is initial ).
      call method cl_gui_frontend_services=>directory_exist
        exporting
          directory            = dir
        receiving
          result               = rc
        exceptions
          cntl_error           = 1
          error_no_gui         = 2
          wrong_parameter      = 3
          not_supported_by_gui = 4
          others               = 5.

      if sy-subrc = 0.
        if rc ne 'X'.
          message id '00' type 'E' number 398
               with text-e03.
        endif.
      endif.
    endif.
  else.




    CALL FUNCTION 'PFL_CHECK_DIRECTORY'
      EXPORTING
        DIRECTORY                   = p_path
*       WRITE_CHECK                 = ' '
*       FILNAME                     = ' '
*       DIRECTORY_LONG              =
      EXCEPTIONS
        PFL_DIR_NOT_EXIST           = 1
        PFL_PERMISSION_DENIED       = 2
        PFL_CANT_BUILD_DATASET_NAME = 3
        PFL_FILE_NOT_EXIST          = 4
        PFL_AUTHORIZATION_MISSING   = 5
        OTHERS                      = 6.
    IF SY-SUBRC <> 0.
      message id '00' type 'E' number 398
               with text-e03.
    ENDIF.
  endif.





*&---------------------------------------------------------------------*
*&      Form  is_local_file
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->F4_ON_PC   text
*----------------------------------------------------------------------*
form is_local_file changing f4_on_pc .

  "By Default Local.
  f4_on_pc = 'X' .

  data: scr_fields  like dynpread
        occurs 1 with header line .

  data dyname like d020s-prog value sy-repid.
  data dynumb like d020s-dnum value '1000' .

  dyname = sy-repid .
  clear : scr_fields , scr_fields[] .

  scr_fields-fieldname = 'P_LOCF' .
  append scr_fields.



  call function 'DYNP_VALUES_READ'
    exporting
      dyname               = dyname
      dynumb               = dynumb
    tables
      dynpfields           = scr_fields
    exceptions
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      others               = 9.
  read table scr_fields index 1.
  if sy-subrc = 0.
    f4_on_pc = scr_fields-fieldvalue.

  endif.

endform .                    "is_local_file
