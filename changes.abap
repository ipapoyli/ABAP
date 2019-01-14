SELECT "lfa1~name1,
           cdhdr~objectid,
           cdhdr~changenr,
           cdhdr~username,
           cdhdr~udate,
           cdhdr~utime,
           cdpos~fname,
           cdpos~chngind,
           cdpos~value_new,
           cdpos~value_old
      FROM cdhdr
      INNER JOIN cdpos ON cdhdr~changenr EQ cdpos~changenr
                      AND cdhdr~objectid EQ cdpos~objectid
*        INNER JOIN lfa1  ON cdhdr~objectid EQ cdhdr~changenr
      INTO CORRESPONDING FIELDS OF TABLE @t_itab
      WHERE cdhdr~objectid IN @s_part
        AND cdpos~fname    EQ 'GROUP_FEATURE'
        AND cdpos~chngind  EQ 'U'
        AND cdhdr~udate    IN @s_date.
