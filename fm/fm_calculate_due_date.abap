FUNCTION ZVIC_CALC_PAYMENT_DATE.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_DATE_BEGIN) TYPE  DATUM
*"     REFERENCE(I_ZTERM) TYPE  DZTERM
*"     REFERENCE(I_CALC_WRK_DATE) TYPE  FLAG DEFAULT 'X'
*"     REFERENCE(I_CALENDAR) TYPE  WFCID DEFAULT 'GR'
*"  EXPORTING
*"     REFERENCE(E_PAYMENT_DATE) TYPE  SY-DATUM
*"----------------------------------------------------------------------
tables : t052.
*
data : faede like faede.
data: i_date like sy-datum.
data: e_date like sy-datum.
data: im_date type JVA_PROD_MONTH.
data: w1_date(8).
*
 clear: i_date, e_date.
*** concatenate I_DATE_BEGIN+6(4) I_DATE_BEGIN+3(2)
***                               I_DATE_BEGIN(2) into i_date.
*
 clear t052.
 SELECT SINGLE * FROM  t052
                 WHERE  zterm = i_zterm.
*
     if t052-zmona is not initial.
        CALL FUNCTION 'BKK_ADD_MONTH_TO_DATE'
          exporting
             MONTHS   = t052-zmona
             OLDDATE  = i_date_begin "i_date
          importing
             NEWDATE  = e_date.
     else.
       move i_date_begin to e_date.
     endif.
*
     move e_date(6) to im_date.
     if not t052-zfael is initial.
        if t052-zfael = '30' or t052-zfael = '31' or
           im_date+4(2) = '02'. "February
           call function 'JVA_LAST_DATE_OF_MONTH'
            exporting
               YEAR_MONTH         = im_date
            importing
               LAST_DATE_OF_MONTH = e_payment_date.
*
         else.
           concatenate e_date(6) t052-zfael into w1_date.
           move w1_date to e_payment_date.
         endif.
         move e_payment_date to i_date.
     else.
         move e_date to e_payment_date.
         move e_date to i_date.
     endif.
*
     if t052-ztag1 is not initial or t052-ztag2 is not initial or
        t052-ztag3 is not initial.
        CLEAR faede.
        MOVE-CORRESPONDING t052 TO faede.
        MOVE: t052-ztag1 TO faede-zbd1t,
              t052-ztag2 TO faede-zbd2t,
              t052-ztag3 TO faede-zbd3t.
*
        move i_date to faede-bldat.
        faede-koart = 'D'.
*
        CALL FUNCTION 'DETERMINE_DUE_DATE'
          EXPORTING
               i_faede = faede
          IMPORTING
               e_faede = faede
          EXCEPTIONS
               OTHERS  = 1.
*
           e_payment_date = faede-netdt.
      endif.

*     < set e_payment_date = next working date
      if I_calc_wrk_date is not initial.
        CALL FUNCTION 'DATE_CONVERT_TO_FACTORYDATE'
          EXPORTING
            correct_option               = '+'
            date                         = e_payment_date
            factory_calendar_id          = i_CALENDAR
          IMPORTING
            date                         = e_payment_date
*           FACTORYDATE                  = L_factorydat
*           WORKINGDAY_INDICATOR         = L_INDICATOR
          EXCEPTIONS
            calendar_buffer_not_loadable = 1
            correct_option_invalid       = 2
            date_after_range             = 3
            date_before_range            = 4
            date_invalid                 = 5
            factory_calendar_not_found   = 6
            OTHERS                       = 7.
        IF sy-subrc <> 0.
*       Implement suitable error handling here
        ENDIF.

      endif.
*     > set e_payment_date = next working date


ENDFUNCTION.
