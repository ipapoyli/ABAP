* ----------------------------------------------------------- *
*Constants:
CONSTANTS: LC_MEMIDCPROG TYPE CHAR14 VALUE 'Z_PR_SELECT'.
* ----------------------------------------------------------- *
*Variables:
DATA: PRINTER TYPE ZTK_TL_PP_PR_SEL-PRINTER_LINQ.
* ----------------------------------------------------------- *
    FREE MEMORY ID LC_MEMIDCPROG.
      READ TABLE ITAB INDEX S_ROW-INDEX.
      PRINTER = ITAB-PRINTER_LINQ.
    EXPORT PRINTER TO MEMORY ID 'Z_PR_SELECT'.
* -----------------------------------------------------------*
* ----------------------------------------------------------- *
* ----------------------------------------------------------- *
* ----------------------------------------------------------- *
*    data l_printer type ZTK_TL_PP_PR_SEL-PRINTER_LINQ.
*    IMPORT printer TO l_printer FROM MEMORY ID 'Z_PR_SELECT'.
* -----------------------------------------------------------*
