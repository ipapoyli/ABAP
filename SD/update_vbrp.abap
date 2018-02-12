
* ------------------------------------------------- *
* read tables
* ------------------------------------------------- *
    CLEAR vbrk_i.
    SELECT SINGLE * FROM vbrk INTO vbrk_i
      WHERE vbeln EQ itab-vbeln.

    CALL FUNCTION 'RV_INVOICE_DOCUMENT_READ'
      EXPORTING
        konv_read    = 'X'
        vbrk_i       = vbrk_i
      TABLES
        xkomv        = xkomv
        xvbpa        = xvbpa
        xvbrk        = xvbrk
        xvbrp        = xvbrp
      EXCEPTIONS
        no_authority = 1
        OTHERS       = 2.

* ------------------------------------------------- *
* read item and change values
* ------------------------------------------------- *
    CLEAR vbrp_i.
    SELECT SINGLE * FROM vbrp INTO vbrp_i
      WHERE vbeln EQ vbrk_i-vbeln
        AND posnr EQ itab-posnr.

    vbrp_i-ntgew = itab-b_ntgew * itab-fklmg.
    vbrp_i-brgew = itab-b_brgew * itab-fklmg.

    CALL FUNCTION 'RV_INVOICE_ITEM_MAINTAIN'
      EXPORTING
        vbrp_i = vbrp_i
      TABLES
        xkomfk = xkomfk
        xkomv  = xkomv
        xthead = xthead
        xvbfs  = xvbfs
        xvbpa  = xvbpa
        xvbrk  = xvbrk
        xvbrp  = xvbrp
        xvbss  = xvbss.


* ------------------------------------------------- *
* update values
* ------------------------------------------------- *
    vbsk_i-mandt = sy-mandt.
    vbsk_i-ernam = vbrk_i-ernam.
    vbsk_i-erdat = vbrk_i-erdat.
    vbsk_i-uzeit = vbrk_i-erzet.

    CALL FUNCTION 'RV_INVOICE_DOCUMENT_ADD'
      EXPORTING
        vbsk_i = vbsk_i
      TABLES
        xkomfk = xkomfk
        xkomv  = xkomv
        xthead = xthead
        xvbfs  = xvbfs
        xvbpa  = xvbpa
        xvbrk  = xvbrk
        xvbrp  = xvbrp
        xvbss  = xvbss.
