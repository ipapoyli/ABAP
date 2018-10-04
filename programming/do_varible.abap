DATA BEGIN OF itab .
  DATA: a1 TYPE c LENGTH 1,
        a2 TYPE c LENGTH 1,
        a3 TYPE c LENGTH 1,
        a4 TYPE c LENGTH 1,
        a5 TYPE c LENGTH 1.

  DATA END OF itab.

  DATA: fldname1 TYPE fieldname,
        fldname2 TYPE fieldname.

  FIELD-SYMBOLS:
    <fld1> TYPE c,
    <fld2> TYPE c.

  itab-a1 = 'e'.
  itab-a2 = 'b'.
  itab-a3 = 'c'.
  itab-a4 = 'a'.
  itab-a5 = 'z'.

  DATA minval TYPE i .
  data num1 type i.

  DO ( 5 - 1 ) TIMES.
    IF num1 IS INITIAL .
      num1 = sy-index.
    else.
        num1 = minval.
    ENDIF.
    DATA(num2) = sy-index + 1 .


    fldname1 = |ITAB-A{ num1 }|.
    fldname2 = |ITAB-A{ num2 }|.

    ASSIGN (fldname1) TO <fld1>.
    ASSIGN (fldname2) TO <fld2>.

    IF <fld1> IS INITIAL .
      minval = num2.
    ELSEIF <fld2> IS INITIAL.
      minval = num1.
    ELSEIF <fld1> < <fld2>.
      minval = num1 .
    ELSEIF <fld1> > <fld2>.
      minval = num2 .
    ENDIF.


  ENDDO.
