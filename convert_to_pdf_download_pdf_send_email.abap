*----------------------------------------------------------------------*
*PDF variables
*----------------------------------------------------------------------*
DATA ls_control_param   TYPE ssfctrlop.
DATA ls_output_opt      TYPE ssfcompop.

DATA : st_job_output_options TYPE ssfcresop.
DATA : st_job_output_info    TYPE ssfcrescl.
DATA : l_binfile_size        TYPE i.
DATA : it_docs               TYPE STANDARD TABLE OF docs.
DATA : pdf_lines             TYPE TABLE OF tline WITH HEADER LINE.
DATA : g_file_name           TYPE string.
DATA : full_path             TYPE string.
DATA : pdf_string            TYPE string.
DATA : pdf_xstring           TYPE xstring.
DATA : g_path                TYPE btch0000-text80.
DATA : l_len                 TYPE i.
DATA : l_buffer              TYPE xstring.
    
    ls_output_opt-tdprinter    = 'PDFUC'.
    ls_control_param-no_dialog = 'X'.
    ls_control_param-getotf    = 'X'.
    
*----------------------------------------------------------------------*
* create smartform
*----------------------------------------------------------------------*
    CALL FUNCTION lf_fm_name
      EXPORTING
        control_parameters = ls_control_param
        output_options     = ls_output_opt
        user_settings      = space
        iv_bukrs           = lv_bukrs
        iv_kunnr           = lv_kunnr
        iv_matnr           = lv_matnr
        iv_datum           = lv_datum
        iv_attn            = lv_attn
        iv_attn_mail       = lv_attn_mail
      IMPORTING
*       DOCUMENT_OUTPUT_INFO       =
        job_output_info    = st_job_output_info
        job_output_options = st_job_output_options
*      TABLES
*       g_lines            = l_lines
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.
        
IF sy-subrc <> 0 AND reprint_flag IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
ENDIF.
        
*----------------------------------------------------------------------*   
* convert to pdf
*----------------------------------------------------------------------*
    clear: st_job_output_info-otfdata[],st_job_output_info-otfdata,
           it_docs[],it_docs,
           pdf_lines[],pdf_lines.

    CALL FUNCTION 'CONVERT_OTF_2_PDF'
*     EXPORTING
*       USE_OTF_MC_CMD               = 'X'
*       ARCHIVE_INDEX                =
      IMPORTING
        bin_filesize           = l_binfile_size
      TABLES
        otf                    = st_job_output_info-otfdata
        doctab_archive         = it_docs
        lines                  = pdf_lines
      EXCEPTIONS
        err_conv_not_possible  = 1
        err_otf_mc_noendmarker = 2
        OTHERS                 = 3.

    IF sy-subrc <> 0 AND reprint_flag IS INITIAL.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

*----------------------------------------------------------------------*
* Create file name and full path
*----------------------------------------------------------------------*
    clear pdf_lines.
    CONCATENATE gs_doc-prntsk '_' gs_doc-kunnr '_'
                gs_doc-matnr '_'  'Created on_' gs_doc-datum '_'
                '.pdf' INTO g_file_name.
    clear full_path.
    CONCATENATE g_path '\' g_file_name INTO full_path.      
    
*----------------------------------------------------------------------*    
 * Create binary form of PDF
 *----------------------------------------------------------------------*
    CLEAR : pdf_string, pdf_xstring.
    CONCATENATE LINES OF pdf_lines INTO pdf_string RESPECTING BLANKS.
    l_len = strlen( pdf_string ).

    EXPORT my_data = pdf_string(l_len) TO DATA BUFFER l_buffer.
    IMPORT my_data TO pdf_xstring FROM DATA BUFFER l_buffer
                                  IN CHAR-TO-HEX MODE.

*----------------------------------------------------------------------*
* download pdf
*----------------------------------------------------------------------*
    IF p_chkdl EQ 'X'.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          bin_filesize            = l_binfile_size
          filename                = full_path
          filetype                = 'BIN'
*         APPEND                  = ' '
*         WRITE_FIELD_SEPARATOR   = ' '
*         HEADER                  = '00'
*         TRUNC_TRAILING_BLANKS   = ' '
*         WRITE_LF                = 'X'
*         COL_SELECT              = ' '
*         COL_SELECT_MASK         = ' '
*         DAT_MODE                = ' '
*         CONFIRM_OVERWRITE       = ' '
*         NO_AUTH_CHECK           = ' '
*         CODEPAGE                = ' '
*         IGNORE_CERR             = ABAP_TRUE
*         REPLACEMENT             = '#'
*         WRITE_BOM               = ' '
*         TRUNC_TRAILING_BLANKS_EOL       = 'X'
*         WK1_N_FORMAT            = ' '
*         WK1_N_SIZE              = ' '
*         WK1_T_FORMAT            = ' '
*         WK1_T_SIZE              = ' '
*         WRITE_LF_AFTER_LAST_LINE        = ABAP_TRUE
*         SHOW_TRANSFER_STATUS    = ABAP_TRUE
*         VIRUS_SCAN_PROFILE      = '/SCET/GUI_DOWNLOAD'
* IMPORTING
*         FILELENGTH              =
        TABLES
          data_tab                = pdf_lines
*         FIELDNAMES              =
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

        EXIT.
      ENDIF.
    ENDIF.


*----------------------------------------------------------------------*
*  send email // using p_mail = email address , pdf_xstring
*----------------------------------------------------------------------*
    IF p_chkem EQ 'X'.
      CLEAR: lw_send_request, main_text, subject_line, att_name.

      TRY.
          lw_send_request = cl_bcs=>create_persistent( ).

          document = cl_document_bcs=>create_document(
                                  i_type    = 'RAW'
                                  i_text    = main_text
*                      i_length  = '500'
                                  i_subject = subject_line ).



          pdfmailcontent = cl_document_bcs=>xstring_to_solix(
                                  ip_xstring = pdf_xstring ).

          att_name = '123'.
          CALL METHOD document->add_attachment
            EXPORTING
              i_attachment_type    = 'PDF'
              i_attachment_subject = att_name
              i_att_content_hex    = pdfmailcontent.

          lw_send_request->set_document( document ).


* Add recipient
          lw_recipient = cl_cam_address_bcs=>create_internet_address(
                                                            p_mail ).
          TRY.
              CALL METHOD lw_recipient->set_address_type
                EXPORTING
                  i_address_type = 'INT'.
            CATCH cx_os_object_not_found .
              l_mail_error = 'X'.
          ENDTRY.

*     add recipient with its respective attributes to send request
          CALL METHOD lw_send_request->add_recipient
            EXPORTING
              i_recipient = lw_recipient
              i_express   = 'X'.

*
** Add CC if selected by user
*      IF NOT  cc_recipient IS INITIAL.
*        lw_recipient = cl_cam_address_bcs=>create_internet_address(
*        cc_recipient ).
*        TRY.
*          CALL METHOD lw_recipient->set_address_type
*          EXPORTING
*            i_address_type = c_rtype.
*        CATCH cx_os_object_not_found .
*          l_mail_error = 'X'.
*        ENDTRY.
*
**     add recipient with its respective attributes to send request
*        CALL METHOD lw_send_request->add_recipient
*        EXPORTING
*          i_recipient = lw_recipient
*          i_express   = 'X'
*          i_copy      = 'X'.
*      ENDIF.

** If sender is required
*          DATA: w_sender   TYPE REF TO if_sender_bcs.
*          w_sender = cl_cam_address_bcs=>create_internet_address(
*                                      i_address_string = 'ipapoulias@teka.vionet.gr'
*                                      ).
*          lw_send_request->set_sender(
*            i_sender = w_sender
*          ).

          lw_send_request->send_request->set_link_to_outbox( 'X' ).

          CALL METHOD lw_send_request->send(
            EXPORTING
              i_with_error_screen = 'X'
            RECEIVING
              result              = l_mail_error ).


        CATCH cx_bcs INTO bcs_exception.
          l_mail_error = 'X'.
*        l_error_text = bcs_exception->error_text.
          EXIT.
      ENDTRY.
    ENDIF.
  ENDIF.   
    
        
