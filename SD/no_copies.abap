* Number of copies for TIM / DATIM / DA 

*program J_2GLPPSD

*create table table ztk_sd_pr_copynr 
*MANDT	      MANDT	        CLNT	3	0	Client
*BUKRS	      BUKRS	        CHAR	4	0	Company Code
*PRNTSK	    J_2GLPPRNTSK	CHAR	5	0	Print Task code
*COPYNR	    J_2GLPCOPYNR	NUMC	1	0	Number of print copies
*J_2GLPTITLE	J_2GLPTITLE 	CHAR	50	0	Title for Print task code
*COLUMN1	    TEXT40	      CHAR	40	0	Text, 40 Characters Long
*COLUMN2	    TEXT50	      CHAR	50	0	Text Field
*COLUMN3	    TEXT50	      CHAR	50	0	Text Field
*COLUMN4	    TEXT50	      CHAR	50	0	Text Field


* number of copies
DATA: wa_paips_copynr LIKE ztk_sd_pr_copynr.




************************************************************************
****  ccc00252
************************************************************************
*   gh_params-glopar-copynr = 'X'.

    PERFORM change_copy_nr CHANGING gh_params-glopar-copynr
                                    wa_paips_copynr.

************************************************************************



*&---------------------------------------------------------------------*
*& Form CHANGE_COPY_NR
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*      <--P_GH_PARAMS_GLOPAR_COPYNR  text
*      <--P_WA_PAIPS_COPYNR  text
*&---------------------------------------------------------------------*
FORM change_copy_nr  CHANGING p_copynr
                              ls_paips_copynr.

  CLEAR wa_paips_copynr.
  SELECT SINGLE * INTO wa_paips_copynr FROM ztk_sd_pr_copynr
    WHERE bukrs  eq s_bukrs
      AND prntsk eq s_prntsk.

  CHECK sy-subrc = 0.
  CHECK NOT wa_paips_copynr-copynr IS INITIAL.
  p_copynr = wa_paips_copynr-copynr.

ENDFORM.
