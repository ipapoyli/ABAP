
*http://help.sap.com/saphelp_crm40/helpdata/FR/2d/1c5d3aebba4c38e10000000a114084/content.htm
*Send email with new class CL_BCS is very simple!

*&-------------------------------------------------------------*
*& Report  ZEMAIL_CL_BCS                                       *
*&-------------------------------------------------------------*
*& Mauricio Lauffer
*&-------------------------------------------------------------*

  REPORT  zemail_cl_bcs.


  CONSTANTS:
    gc_subject TYPE so_obj_des VALUE 'ABAP Email with CL_BCS',
    gc_raw     TYPE char03 VALUE 'RAW'.

  DATA:
    gv_mlrec         TYPE so_obj_nam,
    gv_sent_to_all   TYPE os_boolean,
    gv_email         TYPE adr6-smtp_addr,
    gv_subject       TYPE so_obj_des,
    gv_text          TYPE bcsy_text,
    gr_send_request  TYPE REF TO cl_bcs,
    gr_bcs_exception TYPE REF TO cx_bcs,
    gr_recipient     TYPE REF TO if_recipient_bcs,
    gr_sender        TYPE REF TO cl_sapuser_bcs,
    gr_document      TYPE REF TO cl_document_bcs.



  TRY.
      "Create send request
      gr_send_request = cl_bcs=>create_persistent( ).


      "Email FROM...
      gr_sender = cl_sapuser_bcs=>create( sy-uname ).
      "Add sender to send request
      CALL METHOD gr_send_request->set_sender
        EXPORTING
          i_sender = gr_sender.


      "Email TO...
      gv_email = 'frodo.baggins@outlook.com'.
      gr_recipient = cl_cam_address_bcs=>create_internet_address( gv_email ).
      "Add recipient to send request
      CALL METHOD gr_send_request->add_recipient
        EXPORTING
          i_recipient = gr_recipient
          i_express   = 'X'.


      "Email BODY
      APPEND 'Hello world! My first ABAP email!' TO gv_text.
      gr_document = cl_document_bcs=>create_document(
                      i_type    = gc_raw
                      i_text    = gv_text
                      i_length  = '12'
                      i_subject = gc_subject ).
      "Add document to send request
      CALL METHOD gr_send_request->set_document( gr_document ).


      "Send email
      CALL METHOD gr_send_request->send(
        EXPORTING
          i_with_error_screen = 'X'
        RECEIVING
          result              = gv_sent_to_all ).
      IF gv_sent_to_all = 'X'.
        WRITE 'Email sent!'.
      ENDIF.

      "Commit to send email
      COMMIT WORK.


      "Exception handling
    CATCH cx_bcs INTO gr_bcs_exception.
      WRITE:
        'Error!',
        'Error type:',
        gr_bcs_exception->error_type.
  ENDTRY.
