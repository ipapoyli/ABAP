DATA BEGIN OF ITAB.
DATA BUKRS LIKE T001-BUKRS.
data kunnr like kna1-kunnr.

DATA END OF ITAB.

perform get_company_data.


FORM get_customer_data .

  DATA: v_kunnr  LIKE kna1-kunnr.
  DATA: v_bukrs  LIKE t001-bukrs.
  DATA: es_knvv  TYPE knvv.
  DATA: es_knvk  TYPE knvk.
  DATA: es_knkk  TYPE knkk.
  DATA: es_tvgrt TYPE tvgrt.
  DATA: es_t001cm TYPE t001cm.
  DATA: es_t052u  TYPE t052u.

  DATA: v_creditlimit TYPE p DECIMALS 2.
  DATA: v_creditavailable TYPE p DECIMALS 2.


  v_kunnr = itab-kunnr.
  v_bukrs = itab-bukrs.

  IF  v_bukrs = 'LU01' OR v_bukrs = 'TL01'.
    SET COUNTRY 'US'.
  ENDIF.

  CLEAR in_tab.
  READ TABLE in_tab WITH KEY 'RF130-SALDO'.
  REPLACE ALL OCCURRENCES OF ',' IN in_tab-VALUE WITH space.
  MOVE in_tab-VALUE TO v_creditlimit.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
  EXPORTING
    INPUT  = v_kunnr
  IMPORTING
    OUTPUT = v_kunnr.


  SELECT SINGLE * FROM knvv INTO es_knvv
  WHERE kunnr = v_kunnr.

* es_t052u-text1 "PTERMS
  SELECT SINGLE * FROM t052u INTO es_t052u
  WHERE zterm = es_knvv-zterm AND spras = 'E'.

* es_tvgrt-bezei.  "SALESPERSON
  SELECT SINGLE * FROM tvgrt INTO es_tvgrt
  WHERE spras = 'EN'
  AND vkgrp = es_knvv-vkgrp.

* es_knvk-name1.  "CONTACT
  SELECT SINGLE * FROM knvk INTO es_knvk
  WHERE kunnr = v_kunnr.

  SELECT SINGLE * FROM t001cm INTO es_t001cm
  WHERE bukrs = v_bukrs.

* es_knkk-klimk  "CREDITLIMIT
  SELECT SINGLE * FROM knkk INTO es_knkk
  WHERE kunnr = v_kunnr
  AND kkber = es_t001cm-kkber.

  v_creditavailable = es_knkk-klimk - v_creditlimit.  "CREDITAVAILABLE


ENDFORM.
