Log Messages  

*----------------------------------------------------------------------
TYPES : BEGIN OF uplog_typ,
          msgid  LIKE sy-msgid,
          msgty  LIKE sy-msgty,
          msgno  LIKE sy-msgno,
          msgv1  LIKE sy-msgv1,
          msgv2  LIKE sy-msgv2,
          msgv3  LIKE sy-msgv3,
          msgv4  LIKE sy-msgv4,
          lineno LIKE mesg-zeile,
        END OF uplog_typ.

DATA : uplog_tab TYPE TABLE OF uplog_typ.
DATA : uplog_wa TYPE uplog_typ.
*----------------------------------------------------------------------*

IF sy-subrc <> 0.
      CLEAR uplog_wa.
      uplog_wa-msgid = 'ZTK_TG_SD_01'.
      uplog_wa-msgty = 'E'.
      uplog_wa-msgno = '016'.
      uplog_wa-msgv1 = itab-altkn .
      APPEND uplog_wa TO uplog_tab.
*    --------------------------------------
    ENDIF.


*      ----------------------------------ccc00252   13-09-2017
    WHEN 'LOG'.
      CHECK uplog_tab[] IS NOT INITIAL.
      CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
        TABLES
          i_message_tab = uplog_tab.
*      ----------------------------------

