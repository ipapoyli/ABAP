*************************************************************************
*  sapscript 
*************************************************************************
/:	 	PERFORM GET_ADDRESSDATA IN PROGRAM ZTK_TG_MM_001_MEDRUCK
/:	 	USING &EKKO-BUKRS&/:	 	USING &LFA1-SPRAS&
/:	 	CHANGING &ADRC-NAME1&/:	 	CHANGING &ADRC-NAME2&
/:	 	CHANGING &ADRC-NAME3&/:	 	CHANGING &ADRC-CITY1&
/:	 	CHANGING &ADRC-CITY2&/:	 	CHANGING &ADRC-STREET&
/:	 	CHANGING &ADRC-HOUSE_NUM1&
/:	 	CHANGING &ADRC-POST_CODE1&
/:	 	CHANGING ®ION&
/:	 	CHANGING &COUNTRY&
/:	 	CHANGING &ADRC-TEL_NUMBER&
/:	 	CHANGING &ADRC-FAX_NUMBER&
/:	 	ENDPERFORM




*************************************************************************
*  program
*************************************************************************
FORM get_addressdata TABLES in_tab STRUCTURE itcsy
                            out_tab STRUCTURE itcsy.

  DATA es_adrc TYPE adrc.
  DATA es_t001 TYPE t001.
  DATA es_t618t TYPE t618t.
  DATA es_t005u TYPE t005u.
  DATA es_t005t TYPE t005t.
  DATA es_eikp TYPE eikp.

  DATA iv_bukrs LIKE t001-bukrs.
  DATA iv_spras TYPE c LENGTH 2.
  DATA iv_country TYPE landx.

  CLEAR in_tab.
  READ TABLE in_tab WITH KEY 'EKKO-BUKRS'.
  iv_bukrs = in_tab-value.

  CLEAR in_tab.
  READ TABLE in_tab WITH KEY 'LFA1-SPRAS'.
  iv_spras = in_tab-value.

  TRANSLATE iv_spras TO UPPER CASE.

  CLEAR es_t001.
  SELECT SINGLE * FROM t001 INTO es_t001 WHERE bukrs = iv_bukrs.

  CLEAR: es_adrc, es_t005t, es_t005u, es_eikp, es_t618t.


  SELECT SINGLE * FROM adrc INTO es_adrc
    WHERE addrnumber = es_t001-adrnr AND nation = ' '.
  SELECT SINGLE * FROM t005t INTO es_t005t
    WHERE spras = sy-langu AND land1 = es_adrc-country.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-NAME1'.
  out_tab-value = es_adrc-name1.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-NAME2'.
  out_tab-value = es_adrc-name2.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-NAME3'.
  out_tab-value = es_adrc-name3.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-CITY1'.
  out_tab-value = es_adrc-city1.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-CITY2'.
  out_tab-value = es_adrc-city2.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-STREET'.
  out_tab-value = es_adrc-street.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-HOUSE_NUM1'.
  out_tab-value = es_adrc-house_num1.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-POST_CODE1'.
  out_tab-value = es_adrc-post_code1.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'REGION'.
  out_tab-value = es_t005u-bezei.
*  PERFORM greek_uppercase_translation CHANGING out_tab-value.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'COUNTRY'.
  out_tab-value = es_t005t-landx.
*  PERFORM greek_uppercase_translation CHANGING out_tab-value.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-TEL_NUMBER'.
  out_tab-value = es_adrc-tel_number.
  MODIFY out_tab INDEX sy-tabix.

  CLEAR out_tab .
  READ TABLE out_tab WITH KEY 'ADRC-FAX_NUMBER'.
  out_tab-value = es_adrc-fax_number.
  MODIFY out_tab INDEX sy-tabix.

ENDFORM.                    "get_addressdata'
