*wm_oo4*
*---------------------------------------------------*
** Data for the Popup Screen 500
DATA su_num_v TYPE lqua-lenum.
DATA fcode    TYPE sy-ucomm.

*---------------------------------------------------*
CALL SCREEN 0500 STARTING AT 45 5 ENDING AT 90 10.

*---------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_0500  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_0500 OUTPUT.
  SET PF-STATUS 'NO_STATUS'.
  SET TITLEBAR '007'.
ENDMODULE.


*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0500  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_0500 INPUT.

  CASE fcode.
    WHEN 'SAVE'.
        LEAVE TO SCREEN 0.
    WHEN 'CANCEL'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.
ENDMODULE.
