DATA(text) = `ABC`.

LOOP AT itab INTO DATA(wa).   
  …
ENDLOOP.

SELECT * FROM dbtab
   INTO TABLE DATA(itab) 
        WHERE fld1 = @lv_fld1.

SELECT SINGLE f1 AS my_f1,
              F2 AS abc  
         FROM dbtab
         INTO DATA(ls_structure)
        WHERE …


If a table line is not found, the exception CX_SY_ITAB_LINE_NOT_FOUND is raised. No sy-subrc.
wa = itab[ idx ].
wa = itab[ KEY key INDEX idx ].
wa = itab[ col1 = … col2 = … ].
wa = itab[ KEY key col1 = … col2 = … ].

IF line_exists( itab[ … ] ).
…
ENDIF.

****
TYPES:
  BEGIN OF ty_line,
    col1 TYPE i,
    col2 TYPE i,
    col3 TYPE i,
  END OF ty_line,
  ty_tab TYPE STANDARD TABLE OF ty_line WITH EMPTY KEY.
DATA(gt_itab) = VALUE ty_tab( FOR j = 11 THEN j + 10 UNTIL j > 40
                            ( col1 = j col2 = j + 1 col3 = j + 2  ) ).

****
Count lines of table that meet a condition (field F1 contains “XYZ”).
DATA: lv_lines TYPE i.
LOOP AT gt_itab INTO ls_itab where F1 = ‘XYZ’.
  lv_lines = lv_lines + 1.
ENDLOOP.

DATA(lv_lines) = REDUCE i( INIT x = 0 FOR wa IN gt_itab
                    WHERE( F1 = ‘XYZ’ ) NEXT x = x + 1 ).

***
DATA(idx) = line_index( itab[ … ] ).

**********

CLEAR ls_line2.
MOVE-CORRESPONDING ls_line1        or     ls_line2 = CORRESPONDING #( ls_line1 ).
                TO ls_line2.

MOVE-CORRESPONDING ls_line1       or       ls_line2 = CORRESPONDING #( BASE ( ls_line2 ) ls_line1 ).
                TO ls_line2.

This creates a third and new structure (ls_line3) which is based on ls_line2 but overwritten by matching columns of ls_line1.
DATA(ls_line3) = CORRESPONDING line2   ( BASE ( ls_line2 ) ls_line1 ).

*******
DATA lv_output TYPE string.
CONCATENATE ‘Hello’ ‘world’ INTO lv_output SEPARATED BY space.       or   DATA(lv_out) = |Hello| & | | & |world|.

*********
DATA(gt_citys) = VALUE ty_citys( FOR ls_ship IN gt_ships ( ls_ship–city ) ).

DATA(gt_citys) = VALUE ty_citys( FOR ls_ship IN gt_ships WHERE ( route = ‘R0001’ ) ( ls_ship–city ) ).

DATA(gt_itab) = VALUE ty_tab( FOR j = 11 THEN j + 10 UNTIL j > 40 ( col1 = j col2 = j + 1 col3 = j + 2  ) ).

DATA(lv_lines) = REDUCE i( INIT x = 0 FOR wa IN gt_itab WHERE( F1 = ‘XYZ’ ) NEXT x = x + 1 ).

DATA(lv_sum) = REDUCE i( INIT x = 0 FOR wa IN itab NEXT x = x + wa ).

*******

