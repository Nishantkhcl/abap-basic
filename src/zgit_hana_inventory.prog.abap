*&---------------------------------------------------------------------*
*& Report  ZHANASMART_INVENTORY
*&
*&---------------------------------------------------------------------*
*& Author: Dinesh Singh(51350672)
*&
*&---------------------------------------------------------------------*

REPORT zgit_hana_inventory.
TYPE-POOLS: slis.
TABLES: d010sinf,cuatexts,d020s.
********* Begin Declaration for Enhancement
TYPES : BEGIN OF t_content,
          line(255),     "Line size may exceed 72 in some cases
        END OF t_content.

TYPES : BEGIN OF t_keywords,
          key(20),
        END OF t_keywords.

TYPES : BEGIN OF t_level.
        INCLUDE STRUCTURE slevel.
TYPES : END OF t_level.

TYPES : BEGIN OF t_statements.
        INCLUDE STRUCTURE sstmnt.
TYPES : END OF t_statements.

TYPES : BEGIN OF t_token.
        INCLUDE STRUCTURE stokex.
TYPES : END OF t_token.

DATA:  BEGIN OF t_enh OCCURS 0,
         project  TYPE modact-name,
         name     TYPE modsap-name,
         typ      TYPE modsap-typ,
         member   TYPE modsap-member,
         status   TYPE modattr-status,
         modtext  TYPE modsapt-modtext,
         prog     TYPE d010sinf-prog,
         impl(15) TYPE c,
         stat(10) TYPE c,
         etyp(15) TYPE c,
       END OF t_enh,
       t_adira TYPE TABLE OF adiraccess.

FIELD-SYMBOLS: <fs1> LIKE LINE OF t_enh.
DATA :i_content    TYPE STANDARD TABLE OF t_content WITH HEADER LINE,
      i_keywords   TYPE STANDARD TABLE OF t_keywords WITH HEADER LINE,
      i_level      TYPE STANDARD TABLE OF t_level WITH HEADER LINE,
      i_statements TYPE STANDARD TABLE OF t_statements WITH HEADER LINE,
      i_token      TYPE STANDARD TABLE OF t_token WITH HEADER LINE.
DATA: w_overflow(10000).
********* End Declaration for Enhancement

********* Begin Declaration for Inteface
TABLES: tfdir,tadir.
TYPE-POOLS : slis.
*TYPES : BEGIN OF t_content,
*          line(500),
*        END OF t_content.
*TYPES : BEGIN OF t_keywords,
*          key(20),
*        END OF t_keywords.
*TYPES : BEGIN OF t_level.
*        INCLUDE STRUCTURE slevel.
*TYPES : END OF t_level.
*TYPES : BEGIN OF t_statements.
*        INCLUDE STRUCTURE sstmnt.
*TYPES : END OF t_statements.
*TYPES : BEGIN OF t_token.
*        INCLUDE STRUCTURE stokex.
*TYPES : END OF t_token.
TYPES:  BEGIN OF x_master,
          prog      LIKE d010sinf-prog,
          pdesc(72) TYPE c,
          tdesc(25) TYPE c,
          cat(40)   TYPE c ,
          devclass  LIKE tadir-devclass,
        END OF x_master.
TYPES: BEGIN OF x_prog,
         prog     LIKE d010sinf-prog,
         subc     LIKE d010sinf-subc,
         devclass LIKE tadir-devclass,
       END OF x_prog.
DATA:  " i_content TYPE STANDARD TABLE OF t_content WITH HEADER LINE,
  " i_keywords TYPE STANDARD TABLE OF t_keywords WITH HEADER LINE,
  "i_level TYPE STANDARD TABLE OF t_level WITH HEADER LINE,
  "i_statements TYPE STANDARD
  "   TABLE OF t_statements WITH HEADER LINE,
  "i_token TYPE STANDARD TABLE OF t_token WITH HEADER LINE,
  t_prog   TYPE STANDARD TABLE OF x_prog,
  t_master TYPE STANDARD TABLE OF x_master.

DATA:  t_fieldcat  TYPE slis_t_fieldcat_alv,
       wa_fieldcat TYPE slis_fieldcat_alv.

DATA:  wa_prog   TYPE x_prog,
       wa_master TYPE x_master.

DATA:  t_event  TYPE slis_t_event,
       wa_event TYPE slis_alv_event.
*DATA:  w_overflow(10000).

CONSTANTS: gc_comma      VALUE ''''.

DEFINE replace_all_commas.
  REPLACE GC_COMMA WITH SPACE INTO &1.
  REPLACE GC_COMMA WITH SPACE INTO &1.
  CONDENSE &1.
END-OF-DEFINITION.

DEFINE cat.
  IF &2 NS &1.
    IF NOT &2 IS INITIAL.
      CONCATENATE &1 ',' &2 INTO &2 SEPARATED BY SPACE.
    ELSE.
      &2 = &1.
    ENDIF.
  ENDIF.
END-OF-DEFINITION.
********* End Declaration for Inteface
********* Begin Declaration for Changes to Standard SAP Objects
TYPE-POOLS: slis. TABLES:vrsd.
TYPES: BEGIN OF x_objects,
         obj_type     TYPE smodilog-obj_type,
         obj_name     TYPE smodilog-obj_name,
         sub_type     TYPE smodilog-sub_type,
         sub_name     TYPE smodilog-sub_name,
         inactive     TYPE smodilog-inactive,
         operation    TYPE smodilog-operation,
         devc         TYPE tadir-devclass,
         ps_posid     TYPE df14l-ps_posid,
         objdes(60)   TYPE c,
         dev_desc(60) TYPE c,
       END OF x_objects.

DATA:  t_objects TYPE TABLE OF x_objects WITH HEADER LINE.

TYPES: BEGIN OF t_devct,
         devclass TYPE tdevct-devclass,
         ctext    TYPE tdevct-ctext,
       END   OF t_devct.

DATA: int_devct TYPE STANDARD TABLE OF t_devct,
      wa_devct  TYPE                   t_devct.

DATA: t_vrsd TYPE TABLE OF vrsd WITH HEADER LINE,
      t_vrsn TYPE TABLE OF vrsn WITH HEADER LINE.

*DATA: t_fieldcat  TYPE slis_t_fieldcat_alv,
*      wa_fieldcat TYPE slis_fieldcat_alv.

DATA:  index         TYPE i,
       indexto       TYPE i,
       g_counter     TYPE i,
       gv_object(10) TYPE c.

CONSTANTS :
  gc_aqqu(4)  TYPE c VALUE 'AQQU',
  gc_rept(4)  TYPE c VALUE 'REPT',
  gc_fugr(4)  TYPE c VALUE 'FUGR',
  gc_func(4)  TYPE c VALUE 'FUNC',
  gc_idoc(4)  TYPE c VALUE 'IDOC',
  gc_modu(4)  TYPE c VALUE 'MODU',
  gc_fpol(4)  TYPE c VALUE 'FPOL',
  gc_msag(4)  TYPE c VALUE 'MSAG',
  gc_bmgr(4)  TYPE c VALUE 'BMGR',
  gc_rewr(4)  TYPE c VALUE 'REWR',
  gc_scat(4)  TYPE c VALUE 'SCAT',
  gc_form(4)  TYPE c VALUE 'FORM',
  gc_styl(4)  TYPE c VALUE 'STYL',
  gc_subp(4)  TYPE c VALUE 'SUBP',
  gc_tran(4)  TYPE c VALUE 'TRAN',
  gc_usex(4)  TYPE c VALUE 'USEX',
  gc_incl(4)  TYPE c VALUE 'INCL',
  gc_badi(4)  TYPE c VALUE 'BADI',
  gc_work(4)  TYPE c VALUE 'WORK',
  gc_doma(4)  TYPE c VALUE 'DOMA',
  gc_dtel(4)  TYPE c VALUE 'DTEL',
  gc_tabl(4)  TYPE c VALUE 'TABL',
  gc_view(4)  TYPE c VALUE 'VIEW',
  gc_fldx(4)  TYPE c VALUE 'FLDX',
  gc_mcob(4)  TYPE c VALUE 'MCOB',
  gc_clas(4)  TYPE c VALUE 'CLAS',
  gc_tobj(4)  TYPE c VALUE 'TOBJ',
  gc_prog(4)  TYPE c VALUE 'PROG',
  gc_enqu(4)  TYPE c VALUE 'ENQU',
  gc_txt(3)   TYPE c VALUE 'TXT',
  gc_style(5) TYPE c VALUE 'STYLE',
  gc_sxci(4)  TYPE c VALUE 'SXCI',
  gc_pdws(4)  TYPE c VALUE 'PDWS',
  gc_r3tr(4)  TYPE c VALUE 'R3RT',
  gc_e        TYPE c VALUE 'E',
  gc_a        TYPE c VALUE 'A',
  gc_0(4)     TYPE c VALUE '0000'.
DATA: lv_ans.
********* End Declaration for Changes to Standard SAP Objects
********* Begin Custom Programs
TABLES : t777d.
TYPES : BEGIN OF x_object,
          object(5)    TYPE c,
          objtype(25)  TYPE c,
          obj_name(40) TYPE c,
          objdes(150)  TYPE c,
          type(5)      TYPE c,
          dev          TYPE tadir-devclass,
          devdesc(60)  TYPE c,
          pgmid(5)     TYPE c,
        END OF x_object.
TYPES : gt_itab TYPE STANDARD TABLE OF x_object.

TYPES :        BEGIN OF x_it,
                 obj_name LIKE tadir-obj_name,
                 devclass LIKE tadir-devclass,
               END OF x_it.

DATA : i_objects  TYPE STANDARD TABLE OF x_object,
       wa_objects LIKE LINE OF           i_objects.


TYPES : BEGIN OF x_sxc_attr,
          imp_name TYPE sxc_attr-imp_name,
          uname    TYPE sxc_attr-uname,
        END OF x_sxc_attr.

TYPES : BEGIN OF x_swdsheader,
          wfd_id   TYPE swdsheader-wfd_id,
          wfd_text TYPE swdstext-wfd_text,
        END OF x_swdsheader.

DATA : v_count  LIKE sy-tabix,
       v_atotal LIKE sy-tabix,
       v_htotal LIKE sy-tabix,
       v_dtotal LIKE sy-tabix,
       v_gtotal LIKE sy-tabix.

DATA: BEGIN OF i_devct OCCURS 0,
        devclass TYPE tdevct-devclass,
        spras    TYPE tdevct-spras,
        ctext    TYPE tdevct-ctext,
      END OF i_devct.

DATA: BEGIN OF i_lib OCCURS 0,
        lib LIKE t801k-lib,
        tab LIKE t801k-tab,
      END OF i_lib.

DATA: BEGIN OF i_object OCCURS 0,
        name(40) TYPE c,
        subc(1)  TYPE c,
      END OF i_object.

DATA: BEGIN OF i_estlist OCCURS 0,
        object(4) TYPE c,
        zhcltime  TYPE p DECIMALS 3,
      END OF i_estlist,
      wa_estlist LIKE LINE OF i_estlist.

DATA :BEGIN OF i_hr OCCURS 0,
        prog(40) TYPE c,
      END OF i_hr.
DATA: i_t582 LIKE t582a OCCURS 0 WITH HEADER LINE.
DATA: prog(40) TYPE c.
DATA: i_tfdir LIKE tfdir OCCURS 0 WITH HEADER LINE.
DATA: i_tadir LIKE tadir OCCURS 0 WITH HEADER LINE.
DATA: i_idocsyn LIKE idocsyn OCCURS 0 WITH HEADER LINE.
DATA: i_d010 LIKE d010sinf OCCURS 0 WITH HEADER LINE.

DATA: l_time TYPE p DECIMALS 3.

DATA: l_tot_time(5) TYPE p DECIMALS 3,
      v_atime(5)    TYPE p DECIMALS 3,
      v_htime(5)    TYPE p DECIMALS 3,
      v_dtime(5)    TYPE p DECIMALS 3,
      v_gtime(5)    TYPE p DECIMALS 3.

DATA : flag_color TYPE i VALUE 0.

DATA : i_sxc_attr    TYPE STANDARD TABLE OF x_sxc_attr,
       wa_sxc_attr   LIKE LINE OF           i_sxc_attr,
       i_swdsheader  TYPE STANDARD TABLE OF x_swdsheader,
       wa_swdsheader LIKE LINE OF           i_swdsheader.
********* End Custom Programs

TYPES: BEGIN OF ts_f4_runid,
         zrunid TYPE zghana_inventory-zrunid,
       END OF ts_f4_runid.

TYPES: BEGIN OF ts_f4_runid_hlp,
         date TYPE sydats,
         time TYPE sytime,
       END OF ts_f4_runid_hlp.

TYPES: BEGIN OF ts_f4_customer,
         zcustomer_name TYPE zghana_customer-zcustomer_name,
       END OF ts_f4_customer.

TYPES: tt_f4_runid     TYPE STANDARD TABLE OF ts_f4_runid,
       tt_f4_runid_hlp TYPE STANDARD TABLE OF ts_f4_runid_hlp,
       tt_f4_customer  TYPE STANDARD TABLE OF ts_f4_customer.

TYPES: BEGIN OF ts_zhana_inventory,
         mandt          TYPE zghana_inventory-mandt,
         zcustomer_name TYPE zghana_inventory-zcustomer_name,
         zrunid         TYPE zghana_inventory-zrunid,
         object_type    TYPE zghana_inventory-object_type,
         zcount         TYPE zghana_inventory-zcount,
       END OF ts_zhana_inventory,
       tt_zhana_inventory TYPE STANDARD TABLE OF ts_zhana_inventory.

DATA: gv_lines           TYPE i,
      gv_date(14)        TYPE c,
      gv_repid           TYPE sy-repid,
      gs_layout          TYPE slis_layout_alv,
      gt_fieldcat        TYPE slis_t_fieldcat_alv,

      gs_zhana_inventory TYPE ts_zhana_inventory,
      gt_zhana_inventory TYPE tt_zhana_inventory.

DATA: gd_ucomm TYPE sy-ucomm.

DATA: BEGIN OF gs_cust_runid,
        zcustomer_name TYPE zghana_inventory-zcustomer_name,
        zrunid         TYPE zghana_inventory-zrunid,
      END OF gs_cust_runid.

SELECTION-SCREEN BEGIN OF BLOCK b11 WITH FRAME TITLE text-011.
PARAMETERS: p_cust TYPE zgit_customer_name OBLIGATORY,
            p_runid TYPE zgrunid OBLIGATORY,
            p_nrun RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND upd, " New run
            p_orun RADIOBUTTON GROUP rb1.        " Old inventory Data for display
SELECTION-SCREEN END OF BLOCK b11.

SELECTION-SCREEN BEGIN OF BLOCK b22 WITH FRAME TITLE text-t22.
*PARAMETERS: " p_rid_d TYPE sydats MODIF ID atc,
" p_rid_t TYPE char08 MODIF ID atc,
*            p_runid TYPE zrunid.
SELECTION-SCREEN END OF BLOCK b22.
SELECTION-SCREEN SKIP 1.

INITIALIZATION.
*  LOOP AT SCREEN.
*    IF  screen-name CS 'P_RUNID'.
*      screen-input = 0.
*      screen-invisible = 1.
*      MODIFY SCREEN.
*    ENDIF.
*  ENDLOOP.

AT SELECTION-SCREEN OUTPUT.
*  CASE gd_ucomm.
*    WHEN 'UPD'. "radiobutton selection made
*      IF p_nrun = 'X'.
*        LOOP AT SCREEN.
*          IF  screen-name CS 'P_RUNID'.
*            screen-input = 0.
*            screen-invisible = 1.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDLOOP.
*      ELSEIF p_orun = 'X'.
*        LOOP AT SCREEN.
*          IF  screen-name CS 'P_RUNID'.
*            screen-input = 1.
*            screen-invisible = 0.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDLOOP.
*      ENDIF.
*  ENDCASE.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_cust.
  PERFORM f4_help_cust.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_runid.
  PERFORM f4_help_runid.

AT SELECTION-SCREEN.
  gd_ucomm = sy-ucomm.
  IF p_nrun = 'X'.
    SELECT SINGLE zcustomer_name zrunid INTO gs_cust_runid
    FROM zhana_inventory
    WHERE zcustomer_name = p_cust AND
          zrunid         = p_runid.
    IF sy-subrc EQ 0.
      MESSAGE text-020 TYPE 'E'.
    ENDIF.
  ENDIF.

START-OF-SELECTION.
  IF p_nrun = 'X'.
*    CONCATENATE sy-datum sy-uzeit INTO gv_date.
*    p_runid = gv_date.

    PERFORM enhancement_inventory.
    DESCRIBE TABLE t_enh LINES gv_lines.
    gs_zhana_inventory-mandt          = sy-mandt.
    gs_zhana_inventory-zcustomer_name = p_cust.
    gs_zhana_inventory-zrunid         = p_runid.
    gs_zhana_inventory-object_type    = 'Enhancement'.
    gs_zhana_inventory-zcount         = gv_lines.
    APPEND gs_zhana_inventory TO gt_zhana_inventory.
    CLEAR: gv_lines, gs_zhana_inventory.

    PERFORM interface_inventory.
    DESCRIBE TABLE t_master LINES gv_lines.
    gs_zhana_inventory-mandt          = sy-mandt.
    gs_zhana_inventory-zcustomer_name = p_cust.
    gs_zhana_inventory-zrunid         = p_runid.
    gs_zhana_inventory-object_type    = 'Interface'.
    gs_zhana_inventory-zcount         = gv_lines.
    APPEND gs_zhana_inventory TO gt_zhana_inventory.
    CLEAR: gv_lines, gs_zhana_inventory.

    PERFORM standard_obj_inventory.   " Changes to Standard SAP Objects
    DESCRIBE TABLE t_objects LINES gv_lines.
    gs_zhana_inventory-mandt          = sy-mandt.
    gs_zhana_inventory-zcustomer_name = p_cust.
    gs_zhana_inventory-zrunid         = p_runid.
    gs_zhana_inventory-object_type    = 'Standard Objects'.
    gs_zhana_inventory-zcount         = gv_lines.
    APPEND gs_zhana_inventory TO gt_zhana_inventory.
    CLEAR: gv_lines, gs_zhana_inventory.

    PERFORM custom_prog_inventory.
    DESCRIBE TABLE i_objects LINES gv_lines.
    gs_zhana_inventory-mandt          = sy-mandt.
    gs_zhana_inventory-zcustomer_name = p_cust.
    gs_zhana_inventory-zrunid         = p_runid.
    gs_zhana_inventory-object_type    = 'Reports/BDCs/Scripts/Module Pool'.
    gs_zhana_inventory-zcount         = gv_lines.
    APPEND gs_zhana_inventory TO gt_zhana_inventory.
    CLEAR: gv_lines, gs_zhana_inventory, gv_date.

    IF NOT gt_zhana_inventory IS INITIAL.
      INSERT zghana_inventory FROM TABLE gt_zhana_inventory.
      COMMIT WORK.
    ENDIF.
  ELSEIF p_orun = 'X'.
    SELECT mandt zcustomer_name zrunid object_type zcount INTO TABLE gt_zhana_inventory
    FROM zhana_inventory
      WHERE zcustomer_name = p_cust AND
            zrunid         = p_runid.
  ENDIF.

END-OF-SELECTION .
  PERFORM create_field_catelog.
  PERFORM alv_out_put.


*&---------------------------------------------------------------------*
*&      Form  ENHANCEMENT_INVENTORY
*&---------------------------------------------------------------------*
FORM enhancement_inventory .
  SELECT a~name a~member b~typ b~member c~status d~modtext
         FROM ( ( ( modact AS a INNER JOIN modsap AS b
                     ON a~member = b~name ) INNER JOIN modattr AS c
                     ON a~name = c~name ) LEFT OUTER JOIN modsapt AS d
                        ON d~sprsl = sy-langu
                        AND b~name = d~name )
          INTO TABLE t_enh
          WHERE a~member <>  space
  AND  b~member <>  space.
  SELECT * FROM adiraccess INTO TABLE t_adira.
  PERFORM modify_results.
  PERFORM find_other_exits.

ENDFORM.                    " ENHANCEMENT_INVENTORY

*&---------------------------------------------------------------------*
*&      Form  MODIFY_RESULTS
*&---------------------------------------------------------------------*
FORM modify_results.
  DATA: lt_smod_names TYPE TABLE OF smod_names,
        lw_smod_names TYPE          smod_names,
        lv_func_name  TYPE          rs38l-name,
        lv_table_name TYPE          ddobjname,
        lv_state      TYPE          ddgotstate.
  DATA: lv_len        TYPE i,
        lv_field1(20), lv_field2(20), lv_field3(20).

  LOOP AT t_enh ASSIGNING <fs1> .
    CLEAR: lv_field1,lv_field3,lv_field2 .
    CASE <fs1>-typ .
      WHEN 'E'.
        SPLIT <fs1>-member AT '_' INTO lv_field1 lv_field2 lv_field3.
        <fs1>-prog = lv_field2.
        <fs1>-etyp = 'Function Exit'.
        REFRESH: lt_smod_names. lw_smod_names-itype = 'C'.
        APPEND lw_smod_names TO lt_smod_names.
        lv_func_name = <fs1>-member.
        CALL FUNCTION 'MOD_FUNCTION_INCLUDE'
          EXPORTING
            funcname      = lv_func_name
          TABLES
            incl_names    = lt_smod_names
          EXCEPTIONS
            error_occured = 1
            OTHERS        = 2.
        IF sy-subrc = 0.
          READ TABLE lt_smod_names INTO lw_smod_names INDEX 1.
          IF sy-subrc = 0.
            SELECT SINGLE * FROM d010sinf
            WHERE prog = lw_smod_names-iname AND subc = 'I'.
            IF sy-subrc = 0.
              <fs1>-impl = 'Implemented'.
            ELSE.
              <fs1>-impl = 'Not Implemented'.
            ENDIF.
          ENDIF.
        ELSE.
          <fs1>-impl = 'Not Implemented'.
        ENDIF.
      WHEN 'C'.
        IF <fs1>-member CA '+'.
          SPLIT <fs1>-member AT '+' INTO lv_field1 lv_field2.
          CONCATENATE '+' lv_field2 INTO lv_field2.
          <fs1>-prog = lv_field1.
          SELECT SINGLE * FROM cuatexts
                             WHERE prog = lv_field1
          AND   code = lv_field2.
          IF sy-subrc = 0.
            <fs1>-impl = 'Implemented'.
          ELSE.
            <fs1>-impl = 'Not Implemented'.
          ENDIF.
        ENDIF.
        <fs1>-etyp = 'GUI code'.
      WHEN 'S'.
        SPLIT <fs1>-member AT '_' INTO lv_field1 lv_field2 lv_field3.
        lv_len = strlen( lv_field3 ).
        lv_len = lv_len - 4.
        <fs1>-prog = lv_field3+0(lv_len).
        lv_field2 = lv_field3+lv_len(4).
        SELECT SINGLE *  FROM d020s
                   WHERE prog = <fs1>-prog
        AND   dnum = lv_field2.
        IF sy-subrc <> 0.
          <fs1>-impl = 'Not Implemented'.
        ELSE.
          <fs1>-impl = 'Implemented'.
        ENDIF.
        <fs1>-etyp = 'Screens'.
      WHEN 'T'.
        <fs1>-etyp = 'Tables'.
        lv_table_name = <fs1>-member.
        CALL FUNCTION 'DDIF_STATE_GET'
          EXPORTING
            type     = 'TABL'
            name     = lv_table_name
            state    = 'M'
          IMPORTING
            gotstate = lv_state
          EXCEPTIONS
            OTHERS   = 2.
        IF sy-subrc = 0 AND NOT lv_state IS INITIAL.
          <fs1>-impl = 'Implemented'.
        ELSE.
          <fs1>-impl = 'Not Implemented'.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
    IF <fs1>-status = 'A'.
      <fs1>-stat = 'Active'.
    ELSE.
      <fs1>-stat = 'Inactive'.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " MODIFY_RESULTS
*&---------------------------------------------------------------------*
*&      Form  FIND_OTHER_EXITS
*&---------------------------------------------------------------------*
FORM find_other_exits .
  DATA: wa_adira TYPE adiraccess,
        lv_prog  TYPE d010sinf-prog,
        lv_tabix TYPE sy-tabix,
        lv_level TYPE d010sinf-prog,
        lv_rc    TYPE sy-subrc.
  REFRESH i_keywords[].
  i_keywords = 'FORM'.
  APPEND i_keywords.
  LOOP AT t_adira INTO wa_adira WHERE object = 'REPS'
       OR object = 'PROG' OR object = 'FUGR'.
    REFRESH: i_content,i_statements,i_token,i_level.
    IF wa_adira-object = 'PROG' OR wa_adira-object = 'REPS'.
      lv_prog = wa_adira-obj_name.
    ELSE.
      CONCATENATE 'SAPL' wa_adira-obj_name INTO lv_prog.
    ENDIF.
    READ REPORT lv_prog INTO i_content.
    PERFORM f_scan_source.
    LOOP AT i_statements.
      READ TABLE i_token INDEX i_statements-from.
      IF sy-subrc = 0 AND i_token-str = 'FORM'.
        lv_tabix = i_statements-from + 1.
        READ TABLE i_token INDEX lv_tabix.
        IF sy-subrc = 0 AND i_token-str CP '*USER*EXIT*'.
          CLEAR: lv_rc, lv_level.
          READ TABLE i_level INDEX i_statements-level.
          IF sy-subrc = 0.
            IF i_level-name IS INITIAL.
              lv_level = lv_prog.
            ELSE.
              lv_level = i_level-name.
            ENDIF.
            PERFORM lv_check_implementation
                  USING lv_level i_statements-trow CHANGING lv_rc.
            IF lv_rc = 0.
              t_enh-project = 'XX'.t_enh-name = 'XX'.
              t_enh-member = i_token-str. t_enh-prog = lv_level.
              t_enh-typ = 'U'. t_enh-status = 'A'.
              APPEND t_enh.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " FIND_OTHER_EXITS
*&---------------------------------------------------------------------*
*&      Form  LV_CHECK_IMPLEMENTATION
*&---------------------------------------------------------------------*
FORM lv_check_implementation  USING    lv_level TYPE d010sinf-prog
                                       p_lv_row TYPE i
                              CHANGING p_rc TYPE sy-subrc.
  DATA:lt_content TYPE STANDARD TABLE OF t_content WITH HEADER LINE,
       lv_row     TYPE                   i.
  p_rc = 4. lv_row = p_lv_row.
  READ REPORT lv_level INTO lt_content.
  lv_row = lv_row + 1.
  LOOP AT lt_content FROM lv_row.
    IF lt_content-line+0(1) <> '*'.
      IF lt_content-line CP '*ENDFORM*.*'.
        EXIT.
      ENDIF.
      p_rc = 0.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " LV_CHECK_IMPLEMENTATION
*&---------------------------------------------------------------------*
*&      Form  F_SCAN_SOURCE
*&---------------------------------------------------------------------*
FORM f_scan_source .
  SCAN ABAP-SOURCE i_content
      KEYWORDS FROM i_keywords[]
      TOKENS INTO i_token
      STATEMENTS INTO i_statements
      LEVELS INTO i_level
      OVERFLOW INTO w_overflow
      WITH ANALYSIS
      WITH INCLUDES.
ENDFORM.                    " f_scan_source
*&---------------------------------------------------------------------*
*&      Form  INTERFACE_INVENTORY
*&---------------------------------------------------------------------*
FORM interface_inventory .
  SELECT a~prog a~subc b~devclass INTO TABLE t_prog
        FROM ( d010sinf AS a INNER JOIN tadir AS b ON
         b~pgmid = 'R3TR'
          AND b~object = 'PROG'
           AND a~prog = b~obj_name )
      WHERE ( a~prog LIKE 'Z%'
             OR a~prog LIKE 'Y%'
             OR a~prog LIKE 'SAPMZ%'
             OR a~prog LIKE 'SAPDZ%'
             OR a~prog LIKE 'SAPDY%'
             OR a~prog LIKE 'SAPMY%' )
*        AND a~prog IN s_prog
        AND a~subc <> 'I'
  AND a~r3state = 'A'.
  SORT t_prog.
  DELETE ADJACENT DUPLICATES FROM t_prog.
  IF NOT t_prog[] IS INITIAL.
    PERFORM find_interfaces.
  ENDIF.
ENDFORM.                    " INTERFACE_INVENTORY
*&---------------------------------------------------------------------*
*&      Form  FIND_INTERFACES
*&---------------------------------------------------------------------*
FORM find_interfaces .
  DATA: lv_index TYPE sy-tabix.
  DATA: lv_in TYPE x_master-cat.
  i_keywords = 'CALL'.APPEND i_keywords.
  i_keywords = 'OPEN'.APPEND i_keywords.

  LOOP AT t_prog INTO wa_prog.
    CLEAR: lv_in.
    READ REPORT wa_prog-prog INTO i_content.
    PERFORM f_scan_source.
    LOOP AT i_statements.
      lv_index = i_statements-from.
      READ TABLE i_token INDEX lv_index.
      IF sy-subrc = 0.
        IF i_token-str = 'CALL'.
          lv_index = i_statements-from + 1.
          READ TABLE i_token INDEX lv_index.
          IF sy-subrc = 0.
            IF i_token-str = 'FUNCTION'.
              lv_index = i_statements-from + 2.
              READ TABLE i_token INDEX lv_index.
              replace_all_commas i_token-str.
              IF sy-subrc = 0.
                IF i_token-str CS 'BDC_INSERT' OR
                           i_token-str CS 'BDC_OPEN_GROUP'.
                  cat 'BDC' lv_in.
                ELSE.
                  SELECT SINGLE * FROM tfdir
                  WHERE funcname = i_token-str AND fmode = 'R'.
                  IF sy-subrc = 0.
                    IF tfdir-funcname CP '*BAPI*'.
                      cat 'BAPI' lv_in.
                    ELSEIF tfdir-funcname CP '*FTP*'.
                      cat 'FTP' lv_in.
                    ENDIF.
                  ENDIF.
                ENDIF.
              ENDIF.
            ELSEIF i_token-str = 'TRANSACTION'.
              LOOP AT i_token FROM lv_index TO i_statements-to
                             WHERE str = 'USING'.
                cat 'BDC' lv_in. EXIT.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ELSEIF i_token-str = 'OPEN'.
          lv_index = i_statements-from + 1.
          READ TABLE i_token INDEX lv_index.
          IF sy-subrc = 0 AND i_token-str = 'DATASET'.
            LOOP AT i_token FROM lv_index TO i_statements-to
                    WHERE str = 'INPUT' OR str = 'OUTPUT'.
              IF i_token-str = 'INPUT'.
                cat 'DATASET INPUT' lv_in.
              ENDIF
.
              IF i_token-str = 'OUTPUT'.
                cat 'DATASET OUTPUT' lv_in.
              ENDIF.
              EXIT.
            ENDLOOP.
            IF sy-subrc <> 0.
              cat 'OPEN DATASET' lv_in.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    IF NOT lv_in IS INITIAL.
      PERFORM build_master USING wa_prog lv_in.
    ENDIF.
    CLEAR: wa_prog.
    REFRESH:i_level, i_content, i_statements, i_token.
  ENDLOOP.
ENDFORM.                    " FIND_INTERFACES
*&---------------------------------------------------------------------*
*&      Form  BUILD_MASTER
*&---------------------------------------------------------------------*
FORM build_master USING    wa_prog TYPE x_prog
                           VALUE(p_cat).
  DATA: lv_text TYPE trdirt-text.
  CLEAR: wa_master.
  wa_master-prog = wa_prog-prog.
  CASE wa_prog-subc.
    WHEN '1'.
      wa_master-tdesc = 'Executable'.
    WHEN 'I'.
      wa_master-tdesc = 'Include'.
    WHEN 'M'.
      wa_master-tdesc = 'Module Pool'.
    WHEN 'F'.
      wa_master-tdesc = 'Function Group'.
    WHEN 'S'.
      wa_master-tdesc = 'Subroutine Pool'.
    WHEN 'J'.
      wa_master-tdesc = 'Interface Pool'.
    WHEN 'K'.
      wa_master-tdesc = 'Class Pool'.
  ENDCASE.
  SELECT SINGLE text FROM trdirt INTO lv_text
  WHERE name = wa_master-prog AND sprsl = sy-langu.
  IF sy-subrc <> 0.
    SELECT SINGLE text FROM trdirt INTO lv_text
    WHERE name = wa_master-prog AND sprsl = 'E'.
  ENDIF.
  IF sy-subrc = 0.
    wa_master-pdesc = lv_text.
  ENDIF.

  wa_master-cat = p_cat.
  wa_master-devclass = wa_prog-devclass.
  APPEND wa_master TO t_master.
ENDFORM.                    " BUILD_MASTER
*&---------------------------------------------------------------------*
*&      Form  STANDARD_OBJ_INVENTORY
*&---------------------------------------------------------------------*
FORM standard_obj_inventory .
  DATA: lv_index TYPE sy-tabix.
  SELECT a~obj_type a~obj_name a~sub_type a~sub_name
             a~inactive a~operation b~devclass d~ps_posid
    FROM ( ( smodilog AS a INNER JOIN tadir AS b
      ON a~obj_type = b~object AND
         a~obj_name = b~obj_name AND
         b~pgmid = 'R3TR' ) INNER JOIN tdevc AS c
        ON b~devclass = c~devclass ) INNER JOIN df14l AS d
          ON c~component = d~fctr_id
  INTO TABLE t_objects
    WHERE ( a~operation <> 'TRSL' OR a~prot_only = space ) AND
           ( a~operation <> 'IMP'  OR a~prot_only = 'X' ) AND
           ( a~int_type <> 'APPD' OR a~prot_only = 'X' ) AND
          b~devclass NOT LIKE 'Z%' AND
  b~devclass NOT LIKE 'Y%'.
  SORT t_objects. DELETE ADJACENT DUPLICATES FROM t_objects.
  PERFORM get_devc_desc.
  PERFORM collect_fianl_data.

ENDFORM.                    " STANDARD_OBJ_INVENTORY
*&---------------------------------------------------------------------*
*&      Form  GET_DEVC_DESC
*&---------------------------------------------------------------------*
FORM get_devc_desc .
  SELECT devclass ctext FROM tdevct INTO TABLE int_devct
  WHERE spras = sy-langu.
  SORT int_devct.

ENDFORM.                    " GET_DEVC_DESC
*&---------------------------------------------------------------------*
*&      Form  COLLECT_FIANL_DATA
*&---------------------------------------------------------------------*
FORM collect_fianl_data .
  FIELD-SYMBOLS: <fs> LIKE LINE OF t_objects.
  LOOP AT t_objects ASSIGNING <fs>.
    PERFORM get_description USING <fs>-obj_type <fs>-obj_name
                           CHANGING <fs>-objdes.

    CLEAR wa_devct.
    READ TABLE int_devct INTO wa_devct
         WITH KEY devclass = <fs>-devc BINARY SEARCH.
    IF sy-subrc EQ 0.
      <fs>-dev_desc = wa_devct-ctext.
    ENDIF.
  ENDLOOP.
ENDFORM.                    " COLLECT_FIANL_DATA
*&---------------------------------------------------------------------*
*&      Form  GET_DESCRIPTION
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
FORM get_description  USING VALUE(object) VALUE(obj_name)
                     CHANGING objdes.
  DATA : lv_workid(40) TYPE c,                              "#EC NEEDED
         lv_name(8)    TYPE c,                              "#EC NEEDED
         lv_secobj(8)  TYPE c,

         lv_group      TYPE aqgtq-num,
         lv_query(20)  TYPE c.  "should be more since for leading space

  RANGES:lr_lang FOR sy-langu.

  DATA:  lv_lang(50) TYPE c,
         lv_strl     TYPE i,
         lv_ofset    TYPE i.
  DO 4 TIMES.
    REFRESH: lr_lang.
    CASE sy-index.
      WHEN '1'.
        lr_lang-sign = 'I'. lr_lang-option = 'EQ'.
        lr_lang-low = sy-langu. APPEND lr_lang.
      WHEN '2'.
        lr_lang-sign = 'I'. lr_lang-option = 'EQ'.
        lr_lang-low = 'E'. APPEND lr_lang.
      WHEN '3'.
        lr_lang-sign = 'I'. lr_lang-option = 'EQ'.
        lr_lang-low = 'D'. APPEND lr_lang.
      WHEN '4'.
        CALL FUNCTION 'SYSTEM_INSTALLED_LANGUAGES'
          IMPORTING
            languages       = lv_lang
          EXCEPTIONS
            sapgparam_error = 1
            OTHERS          = 2.
        IF sy-subrc <> 0.
          EXIT.
        ENDIF.
        lv_strl = strlen( lv_lang ).
        DO lv_strl TIMES.
          lv_ofset = sy-index - 1.
          lr_lang-sign = 'I'. lr_lang-option = 'EQ'.
          lr_lang-low = lv_lang+lv_ofset(1). APPEND lr_lang.
          CLEAR: lr_lang.
        ENDDO.
    ENDCASE.
    CASE object.
      WHEN gc_aqqu.
        CLEAR: lv_group, lv_query.
        SPLIT obj_name AT space INTO lv_group lv_query.
        CONDENSE: lv_group, lv_query.
        SELECT SINGLE text FROM aqgtq INTO objdes
              WHERE sprsl IN lr_lang
               AND  num = lv_group
               AND  qnum = lv_query
        AND  tindx = '00001'.
      WHEN gc_rept. "Reports
        SELECT SINGLE text FROM trdirt INTO objdes
          WHERE name = obj_name AND
        sprsl IN lr_lang.
      WHEN gc_fugr.          "Function Group
        SELECT  SINGLE areat FROM tlibt INTO objdes
          WHERE area = obj_name
        AND   spras IN lr_lang.
      WHEN gc_func.            "Function Modules
        SELECT SINGLE stext FROM tftit INTO objdes
          WHERE funcname = obj_name
        AND spras IN lr_lang.
      WHEN gc_idoc.          "IDOC types
        SELECT  SINGLE descrp FROM edbast INTO objdes
          WHERE idoctyp = obj_name AND
        langua IN lr_lang.
      WHEN gc_msag.            "Message Class
        SELECT  SINGLE stext FROM t100t INTO objdes
          WHERE arbgb = obj_name
        AND sprsl IN lr_lang.
      WHEN gc_bmgr.            "Report Groups
        SELECT SINGLE jtext FROM t803t INTO objdes
         WHERE rgjnr = obj_name AND
        langu IN lr_lang.
      WHEN gc_rewr.            "Rep writers
        SELECT SINGLE sec_object  FROM t803z INTO lv_secobj
        WHERE pri_object = obj_name.
        IF sy-subrc = 0.
          SELECT SINGLE  rtext FROM t800t INTO objdes
          WHERE rname = lv_secobj
          AND langu IN lr_lang.
        ENDIF.
      WHEN gc_scat.            "CATT procedures
        SELECT SINGLE twb_title FROM tnode02_at
               INTO objdes
        WHERE tc_name = obj_name AND spras IN lr_lang.
      WHEN gc_form.    "Layout sets
        SELECT SINGLE tdtitle FROM stxh INTO objdes
           WHERE tdname = obj_name AND
            tdspras IN lr_lang AND
            tdobject = gc_form AND
        tdid = gc_txt.
      WHEN gc_styl.     " Styles
        SELECT SINGLE tdtitle FROM stxh INTO objdes
           WHERE tdname = obj_name AND
             tdspras IN lr_lang AND
             tdobject = gc_style AND
        tdid = gc_txt.
      WHEN gc_subp.      "subroutine pool
        SELECT SINGLE text FROM trdirt INTO objdes
           WHERE name = obj_name AND
        sprsl IN lr_lang.
      WHEN gc_tran.        "Transaction
        SELECT SINGLE ttext FROM tstct INTO objdes
        WHERE tcode = obj_name AND
        sprsl IN lr_lang.
      WHEN gc_incl.     " includes
        SELECT SINGLE text FROM trdirt INTO objdes
          WHERE name = obj_name
        AND sprsl IN lr_lang.
      WHEN gc_badi.   "BADI's
        SELECT SINGLE text FROM sxc_attrt INTO objdes
        WHERE imp_name = obj_name AND
        sprsl IN lr_lang.
      WHEN gc_work.         "workflows
        CLEAR : lv_workid.
        lv_workid = obj_name.
        SPLIT lv_workid AT ':' INTO lv_workid objdes.
      WHEN gc_doma.      "domains
        SELECT SINGLE  ddtext FROM dd01t INTO objdes
        WHERE domname = obj_name AND
              as4local = gc_a AND
              ddlanguage IN lr_lang AND
        as4vers = gc_0.
      WHEN gc_dtel.      "Data elements
        SELECT  SINGLE ddtext FROM dd04t INTO objdes
          WHERE rollname = obj_name AND
              as4local = gc_a AND
              ddlanguage IN lr_lang AND
        as4vers = gc_0.
      WHEN gc_tabl.     "Tables
        SELECT  SINGLE ddtext FROM dd02t INTO objdes
            WHERE tabname = obj_name AND
                  as4local = gc_a AND
                  ddlanguage IN lr_lang AND
        as4vers = gc_0.
      WHEN gc_view.      "Views
        SELECT  SINGLE ddtext FROM dd25t INTO objdes
            WHERE viewname = obj_name AND
                  as4local = gc_a AND
                  ddlanguage IN lr_lang AND
        as4vers = gc_0.
      WHEN gc_fldx.      "field exits
        SELECT  SINGLE stext FROM tftit INTO objdes
         WHERE funcname = obj_name AND
        spras IN lr_lang.
      WHEN gc_mcob.      "matchcode objects.
        SELECT SINGLE  mctext FROM dd20t INTO objdes
            WHERE mconame = obj_name AND
                  as4local = gc_a AND
                  ddlanguage IN lr_lang AND
        as4vers = gc_0.
      WHEN gc_clas OR 'INTF'.    "class
        SELECT SINGLE descript FROM seoclasstx
            INTO objdes
             WHERE clsname = obj_name AND
        langu IN lr_lang.
      WHEN gc_enqu.   "enque deque
        SELECT  SINGLE ddtext FROM dd25t INTO objdes
             WHERE viewname = obj_name AND
                   as4local = gc_a AND
                   ddlanguage IN lr_lang AND
        as4vers = gc_0.
      WHEN gc_prog.
        SELECT SINGLE text FROM trdirt INTO objdes
             WHERE name = obj_name AND
        sprsl IN lr_lang.
      WHEN gc_tobj.   "Transport objects
        lv_workid =  obj_name.
        SPLIT lv_workid AT space INTO lv_workid lv_name.
        SELECT  SINGLE ddtext FROM dd02t INTO objdes
           WHERE tabname = lv_workid AND
                 as4local = gc_a AND
                 ddlanguage IN lr_lang AND
        as4vers = gc_0.
      WHEN 'INFO'.
        SELECT SINGLE itext FROM t582s INTO objdes
          WHERE infty = obj_name AND
        sprsl IN lr_lang.
      WHEN 'STRU'.
        SELECT SINGLE ddtext FROM dd02t INTO objdes
         WHERE tabname = obj_name AND
        ddlanguage IN lr_lang.
      WHEN 'HRTB'.
        SELECT SINGLE ddtext FROM dd02t INTO objdes
          WHERE tabname = obj_name AND
        ddlanguage IN lr_lang.
      WHEN 'POOL'.
        SELECT SINGLE text FROM trdirt INTO objdes
           WHERE name = obj_name AND
        sprsl IN lr_lang.
      WHEN 'SOBJ'.
        SELECT SINGLE ntext FROM tojtt INTO objdes
           WHERE name = obj_name AND
        language IN lr_lang.
      WHEN 'WAPA'.
        SELECT SINGLE text FROM o2applt INTO objdes
           WHERE applname = obj_name AND
        langu IN lr_lang.
    ENDCASE.
    IF NOT objdes IS INITIAL.
      EXIT.
    ENDIF.
  ENDDO.
ENDFORM.                    " GET_DESCRIPTION
*&---------------------------------------------------------------------*
*&      Form  CUSTOM_PROG_INVENTORY
*&---------------------------------------------------------------------*
FORM custom_prog_inventory .
  SELECT a~obj_name b~subc
    FROM tadir AS a INNER JOIN d010sinf AS b
          ON a~obj_name = b~prog
                 INTO  TABLE i_object
                 WHERE pgmid     =  'R3TR'  AND
                       object    =  'PROG'  AND
                   ( devclass LIKE 'Z%'
                    OR devclass LIKE 'Y%' ) AND
*                    OR devclass = '$TMP' ) AND
                    b~r3state = 'A'.

  SELECT * FROM t582a INTO TABLE i_t582
  WHERE infty GE '9000'.

  SELECT a~obj_name b~subc
      FROM tadir AS a INNER JOIN d010sinf AS b
            ON a~obj_name = b~prog
                   INTO  TABLE i_object
                   WHERE pgmid     =  'R3TR'  AND
                         object    =  'PROG'  AND
                     ( devclass LIKE 'Z%'
                      OR devclass LIKE 'Y%' ) AND
*                      OR devclass = '$TMP' ) AND
                         b~r3state = 'A'.

  PERFORM get_objects :
          USING 'R3TR'  'CLAS'  'Class(Abap Objects)',
          USING 'R3TR'  'WAPA'  'BSP Applications',
          USING 'R3TR'  'INTF'  'Interfaces',
          USING 'R3TR'  'SOBJ'  'Business Objects',
          USING 'R3TR'  'AQQU'  'ABAP Queries',
          USING 'R3TR'  'REPT'  'Reports/BDCs/Scripts',
          USING 'R3TR'  'FUGR'  'Function Groups',
          USING 'R3TR'  'FUNC'  'Function Modules',
          USING 'R3TR'  'FLDX'  'Field Exits',
          USING 'R3TR'  'IDOC'  'IDOC Types',
          USING 'R3TR'  'MODU'  'Module Pools',
          USING 'R3TR'  'MSAG'  'Message Classes',
          USING 'R3TR'  'BMGR'  'Report Groups',
          USING 'R3TR'  'REWR'  'Rep.Writers/Painters',
          USING 'R3TR'  'SCAT'  'CATT Procedures',
          USING 'R3TR'  'FORM'  'Layout Sets',
          USING 'R3TR'  'STYL'  'SAP Styles',
          USING 'R3TR'  'SUBP'  'Subroutine Pools',
          USING 'R3TR'  'TRAN'  'Transactions',
          USING 'R3TR'  'INCL'  'INCLUDES',
          USING 'R3TR'  'BADI'  'BADI',
          USING 'R3TR'  'WORK'  'Work Flow',
          USING 'R3TR'  'IASP'  'IAC Services',
          USING 'R3TR'  'INFO'  'Infotypes',
          USING 'R3TR'  'STRU'  'Infotype Structures',
          USING 'R3TR'  'HRTB'  'Infotype Tables',
          USING 'R3TR'  'POOL'  'Infotype Module Pool',
          USING 'R3TR'  'DOMA'  'Domains',
          USING 'R3TR'  'DTEL'  'Data Elements',
          USING 'R3TR'  'MCOB'  'Matchcode Objects',
          USING 'R3TR'  'TABL'  'Tables',
          USING 'R3TR'  'VIEW'  'Views'.

  FIELD-SYMBOLS: <fs> TYPE x_object.
  LOOP AT i_objects ASSIGNING <fs>.
    PERFORM get_description USING <fs>-object <fs>-obj_name
                            CHANGING <fs>-objdes.
    PERFORM get_devclass_desc USING <fs>-object <fs>-obj_name
                            CHANGING <fs>-dev <fs>-devdesc.
  ENDLOOP.
*  IF p_junk IS INITIAL.
*    DELETE i_objects WHERE dev = 'ZHCLTOOL'.
*
*    IF p_temp IS INITIAL.
*      DELETE i_objects WHERE dev = '$TMP'.
*    ENDIF.
*
*    DELETE i_objects WHERE dev NP 'Z*' AND dev NP 'Y*'
*              AND dev <> 'NONE' AND dev <> space AND dev <> '$TMP'.
*  ENDIF.
  DELETE i_objects WHERE object NE 'REPT' AND object NE 'MODU'.

ENDFORM.                    " CUSTOM_PROG_INVENTORY
*&---------------------------------------------------------------------*
*&      Form  GET_OBJECTS
*&---------------------------------------------------------------------*
FORM get_objects USING
                         p_pgmid      LIKE tadir-pgmid
                         p_object     LIKE tadir-object
                         p_descr      LIKE progtab-line.
  DATA : lv_repgp(4) TYPE c.
  CASE p_object.
*   Function Modules
    WHEN 'FUNC'.

      SELECT * FROM tfdir INTO TABLE i_tfdir
                        WHERE ( funcname LIKE 'Y%' OR
      funcname LIKE 'Z%' ).

      IF sy-subrc = 0.
        LOOP AT i_tfdir.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'FUNC'.
          wa_objects-obj_name = i_tfdir-funcname.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDLOOP.
      ENDIF.

*   Field Exits
    WHEN 'FLDX'.
      SELECT * FROM tfdir INTO TABLE i_tfdir
      WHERE funcname LIKE 'FIELD_EXIT%'.
      IF sy-subrc = 0.
        LOOP AT i_tfdir.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'FLDX'.
          wa_objects-obj_name = i_tfdir-funcname.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDLOOP.
      ENDIF.
*   IDOC Types
    WHEN 'IDOC'.
      SELECT DISTINCT idoctyp INTO TABLE i_idocsyn
                         FROM idocsyn
                         WHERE idoctyp LIKE 'Z%' OR
      idoctyp LIKE 'Y%'.
      IF sy-subrc = 0.
        LOOP AT i_idocsyn.

          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'IDOC'.
          wa_objects-obj_name = i_idocsyn-idoctyp.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDLOOP.
      ENDIF.
*   Module Pools
    WHEN 'MODU'.
      LOOP AT i_object WHERE subc = 'M'.
        wa_objects-pgmid = 'R3TR'.wa_objects-object = 'MODU'.
        wa_objects-obj_name = i_object-name.
        wa_objects-objtype = p_descr.
        APPEND wa_objects TO i_objects.CLEAR wa_objects.
      ENDLOOP.
*   Subroutine Pools
    WHEN 'SUBP'.
      LOOP AT i_object WHERE subc = 'S'.
        wa_objects-pgmid = 'R3TR'.wa_objects-object = 'SUBP'.
        wa_objects-obj_name = i_object-name.
        wa_objects-objtype = p_descr.
        APPEND wa_objects TO i_objects.CLEAR wa_objects.
      ENDLOOP.
*   Whe Reports
    WHEN 'REPT'.
      LOOP AT i_object WHERE subc = '1'.
        wa_objects-pgmid = 'R3TR'.wa_objects-object = 'REPT'.
        wa_objects-obj_name = i_object-name.
        wa_objects-objtype = p_descr.
        APPEND wa_objects TO i_objects.CLEAR wa_objects.
      ENDLOOP.
*   Includes
    WHEN 'INCL'.

      SELECT * FROM  d010sinf
                        INTO  TABLE i_d010
                        WHERE (  prog LIKE  '_Z%'     OR
                                 prog LIKE  '_Y%'     OR
                                 prog LIKE  'Z%'      OR
                                 prog LIKE  'Y%'      OR
                                 prog LIKE  'SAP_Z%'  OR
                                 prog LIKE  'SAP_Y%'  )  AND
                                 r3state =  'A'          AND  "Mod N
                            NOT  cnam LIKE  'SAP%'       AND
                            NOT  cnam LIKE  'DDIC%'      AND
      subc EQ    'I'.

      IF sy-subrc = 0.
        LOOP AT i_d010.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'INCL'.
          wa_objects-obj_name = i_d010-prog.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDLOOP.
      ENDIF.
*   Report Groups
    WHEN 'BMGR'.
      SELECT lib tab FROM  t801k
                 INTO  TABLE i_lib
      WHERE unam  NE 'SAP'.
      LOOP AT i_lib.
        SELECT SINGLE rgjnr FROM t803j INTO lv_repgp
        WHERE lib = i_lib-lib.
        IF sy-subrc = 0.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'BMGR'.
          wa_objects-obj_name = lv_repgp .
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.
        ENDIF.
        CLEAR wa_objects.
      ENDLOOP.
*   Report Writers/Painters
    WHEN 'REWR'.
      DATA : BEGIN OF i_pri_object OCCURS 0,
               name LIKE t803z-pri_object,
             END OF i_pri_object.
      LOOP AT i_lib.
        REFRESH i_pri_object.
        CLEAR   i_pri_object.
        SELECT  pri_object FROM  t803z
                           INTO TABLE i_pri_object
        WHERE lib = i_lib-lib.
        LOOP AT i_pri_object.
          IF i_pri_object-name+0(1) CN '1234567890'.
            wa_objects-pgmid = 'R3TR'.wa_objects-object = 'REWR'.
            wa_objects-obj_name = i_pri_object-name.
            wa_objects-objtype = p_descr.
            APPEND wa_objects TO i_objects.CLEAR wa_objects.
          ENDIF.
        ENDLOOP.
      ENDLOOP.
*   For BADI
    WHEN 'BADI'.
      SELECT imp_name FROM sxc_attr INTO
            TABLE  i_sxc_attr
       WHERE active = 'X' AND
      uname <> 'SAP'.
      IF sy-subrc = 0.
        LOOP AT i_sxc_attr INTO wa_sxc_attr.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'BADI'.
          wa_objects-obj_name = wa_sxc_attr-imp_name.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDLOOP.
      ENDIF.
*   For Workflows
    WHEN 'WORK'.
      SELECT wfd_id
      FROM swdsheader
      INTO wa_swdsheader-wfd_id
      WHERE version = '0000' AND
            exetyp = 'S' AND
            created_by <> 'SAP'.
        IF sy-subrc = 0.
          SELECT SINGLE wfd_text
          FROM swdstext
          INTO wa_swdsheader-wfd_text
          WHERE wfd_id = wa_swdsheader-wfd_id AND
                version = '0000' AND
                exetyp = 'S' AND
          language = sy-langu.
          IF sy-subrc <> 0.
            SELECT SINGLE wfd_text
         FROM swdstext
         INTO wa_swdsheader-wfd_text
         WHERE wfd_id = wa_swdsheader-wfd_id AND
               version = '0000' AND
               exetyp = 'S' AND
            language = 'E'.
          ENDIF.
          APPEND wa_swdsheader TO i_swdsheader.
        ENDIF.
      ENDSELECT.

      DELETE ADJACENT DUPLICATES FROM i_swdsheader COMPARING wfd_id.
      LOOP AT i_swdsheader INTO wa_swdsheader.
        CONCATENATE wa_swdsheader-wfd_id ':' wa_swdsheader-wfd_text INTO
                  wa_swdsheader-wfd_text.
        wa_objects-pgmid = 'R3TR'.wa_objects-object = 'WORK'.
        wa_objects-obj_name = wa_swdsheader-wfd_text.
        wa_objects-objtype = p_descr.
        APPEND wa_objects TO i_objects.CLEAR wa_objects.
      ENDLOOP.

*   Additions for HR
    WHEN 'INFO'.
      REFRESH i_hr.
      LOOP AT i_t582.
        APPEND i_t582-infty TO i_hr.
      ENDLOOP.
      SORT i_hr BY prog.
      LOOP AT i_hr.
        wa_objects-pgmid = 'R3TR'.wa_objects-object = 'INFO'.
        wa_objects-obj_name = i_hr-prog.
        wa_objects-objtype = p_descr.
        APPEND wa_objects TO i_objects.CLEAR wa_objects.
      ENDLOOP.
    WHEN 'STRU'.
      LOOP AT i_t582.
        CONCATENATE 'PS' i_t582-infty INTO prog.
        APPEND prog TO i_hr.
        CONCATENATE 'P' i_t582-infty INTO prog.
        APPEND prog TO i_hr.
      ENDLOOP.
      LOOP AT i_hr.
        CLEAR tadir-obj_name.
        SELECT SINGLE obj_name FROM tadir
                      INTO tadir-obj_name
                      WHERE pgmid  = 'R3TR' AND
                            object = 'TABL' AND
        obj_name = i_hr-prog.
        IF sy-subrc = 0.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'STRU'.
          wa_objects-obj_name = tadir-obj_name.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDIF.
      ENDLOOP.
    WHEN 'HRTB'.
      REFRESH i_hr.
      LOOP AT i_t582.
        CONCATENATE 'PA' i_t582-infty INTO prog.
        APPEND prog TO i_hr.
      ENDLOOP.
      LOOP AT i_hr.
        CLEAR tadir-obj_name.
        SELECT SINGLE obj_name FROM tadir
                      INTO tadir-obj_name
                      WHERE pgmid  = 'R3TR' AND
                            object = 'TABL' AND
        obj_name = i_hr-prog.
        IF sy-subrc = 0.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'HRTB'.
          wa_objects-obj_name = tadir-obj_name.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDIF.
      ENDLOOP.
    WHEN 'POOL'.
      LOOP AT i_t582.
        CLEAR t777d-repid.

        SELECT SINGLE repid
                FROM t777d
                INTO t777d-repid
        WHERE  infty = i_t582-infty .

        IF sy-subrc = 0.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = 'POOL'.
          wa_objects-obj_name = t777d-repid.
          wa_objects-objtype = p_descr.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDIF.
      ENDLOOP.
*---------------------------------------------------------------
*   All others
    WHEN  OTHERS.
*      IF p_temp = 'X'.
      SELECT * FROM tadir
                    INTO TABLE i_tadir
                    WHERE  pgmid    =  p_pgmid   AND
                           object   =  p_object  AND
                          ( devclass LIKE 'Z%'
                           OR devclass LIKE 'Y%' ).
*      OR devclass = '$TMP' ).

*      ELSE.
*        SELECT * FROM tadir
*                      INTO TABLE i_tadir
*                      WHERE  pgmid    =  p_pgmid   AND
*                             object   =  p_object  AND
*                            ( devclass LIKE 'Z%'
*                             OR devclass LIKE 'Y%' ).
*      ENDIF.
      IF sy-subrc = 0.
        IF p_object EQ 'TABL'.
          DELETE i_tadir WHERE obj_name CP 'P*'.
        ENDIF.
        LOOP AT i_tadir.
          wa_objects-pgmid = 'R3TR'.wa_objects-object = p_object.
          wa_objects-obj_name = i_tadir-obj_name.
          wa_objects-objtype = p_descr.
          wa_objects-dev = i_tadir-devclass.
          APPEND wa_objects TO i_objects.CLEAR wa_objects.
        ENDLOOP.
      ENDIF.
  ENDCASE.
ENDFORM.                               " GET_OBJECTS
*&---------------------------------------------------------------------*
*&      Form  GET_DEVCLASS_DESC
*&---------------------------------------------------------------------*
FORM get_devclass_desc USING VALUE(object) VALUE(obj_name)
                     CHANGING devc devdesc.
  DATA : lv_workid(40)      TYPE c,                         "#EC NEEDED
         lv_pname           LIKE tfdir-pname,
         lv_split_dummy(50) TYPE c.
  IF devc IS INITIAL.
    CASE object.
      WHEN gc_aqqu.
        SELECT SINGLE devclass FROM tadir INTO devc
           WHERE pgmid = gc_r3tr AND
             object = gc_aqqu AND
        obj_name = obj_name .
      WHEN gc_rept OR gc_modu OR gc_subp.    " Reports
        SELECT SINGLE devclass FROM tadir INTO devc
           WHERE pgmid = gc_r3tr AND
             object = gc_prog AND
        obj_name = obj_name .
      WHEN gc_fugr.              "Function Group
        IF obj_name+0(1) <> 'X'.
          SELECT SINGLE devclass FROM tadir INTO devc
             WHERE pgmid = gc_r3tr AND
               object = gc_fugr       AND
          obj_name = obj_name.
        ELSE.
          SELECT SINGLE devclass FROM tadir INTO devc
             WHERE pgmid = gc_r3tr AND
               object = 'FUGS' AND
          obj_name = obj_name.
          IF sy-subrc <> 0.
            SELECT SINGLE devclass FROM tadir INTO devc
               WHERE pgmid = gc_r3tr AND
                 object = 'FUGX' AND
            obj_name = obj_name.
          ENDIF.
        ENDIF.
      WHEN gc_func OR gc_fldx.            "Function Modules
        SELECT SINGLE pname FROM tfdir INTO lv_pname
        WHERE funcname = obj_name.
        IF sy-subrc = 0.
*         Condense is better than shift since we may have something like
*         /VIRSA/SAPL...
          REPLACE 'SAPL' WITH space INTO lv_pname.CONDENSE lv_pname.
          PERFORM get_devclass_desc USING gc_fugr lv_pname
                                    CHANGING devc devdesc.
          EXIT. "Since devclass will already been have fetched
        ENDIF.
      WHEN gc_idoc.          "IDOC types
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_idoc       AND
        obj_name = obj_name.
      WHEN gc_msag.            "Message Class
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_msag AND
        obj_name = obj_name.
      WHEN gc_bmgr.            "Report Groups
        devc = 'NONE'.
      WHEN gc_rewr.            "Rep writers
        devc = 'NONE'.
      WHEN gc_scat.            "CATT procedures
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_scat       AND
        obj_name = obj_name .
      WHEN gc_form.    "Layout sets
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_form  AND
        obj_name = obj_name .
      WHEN gc_styl.     " Styles
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_styl   AND
        obj_name = obj_name .
      WHEN gc_tran.        "Transaction
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_tran    AND
        obj_name = obj_name .
      WHEN gc_incl.     " includes
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_prog  AND
        obj_name = obj_name .
        IF sy-subrc <> 0.
          DATA: lv_f,lv_t,lv_c,
                lv_fugr  TYPE rs38l-area,
                lv_type  TYPE ddtypet-typegroup,
                lv_class TYPE seoclsname.
          CALL FUNCTION 'RS_PROGNAME_SPLIT'
            EXPORTING
              progname_with_namespace = obj_name
            IMPORTING
              fugr_is_name            = lv_f
              fugr_group              = lv_fugr
*             sldb_is_reserved_name   = lv_l
*             sldb_logdb_name         = lv_ldba
*             MST_IS_RESERVED_NAME    = lv_mst
              type_is_reserved_name   = lv_t
              type_name               = lv_type
*             menu_is_reserved_name   = lv_m
*             menu_name               = lv_menu
              class_is_name           = lv_c
              class_name              = lv_class
*             CNTX_IS_RESERVED_NAME   = lv_cntx
            EXCEPTIONS
              delimiter_error         = 1
              OTHERS                  = 2.
          IF sy-subrc = 0.
            IF NOT lv_f IS INITIAL.
              PERFORM get_devclass_desc USING gc_fugr lv_fugr
                                        CHANGING devc devdesc.
              EXIT. "Since devclass will already been have fetched
            ENDIF.
            IF NOT lv_c IS INITIAL.
              PERFORM get_devclass_desc USING gc_clas lv_class
                                        CHANGING devc devdesc.
              EXIT. "Since devclass will already been have fetched
            ENDIF.
            IF NOT lv_t IS INITIAL.
              SELECT SINGLE devclass FROM tadir INTO devc
              WHERE pgmid = gc_r3tr AND
                object = 'TYPE' AND
              obj_name = lv_type.
              IF sy-subrc = 0. EXIT. ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
        IF devc IS INITIAL.
*         Brute force
          SPLIT obj_name AT '=' INTO lv_class lv_split_dummy.
          PERFORM get_devclass_desc USING gc_clas lv_class
                          CHANGING devc devdesc.
          IF NOT devc IS INITIAL. EXIT. ENDIF.
        ENDIF.
      WHEN gc_badi.   "BADI's
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_sxci       AND
        obj_name = obj_name.
      WHEN gc_work.         "workflows
        CLEAR : lv_workid.
        lv_workid = obj_name.
        SPLIT lv_workid AT ':' INTO lv_workid lv_split_dummy.
        IF sy-subrc = 0.
          lv_workid = lv_workid+2.     "to remove ws from it
          SELECT SINGLE devclass FROM tadir INTO devc
           WHERE pgmid = gc_r3tr AND
                 object = gc_pdws       AND
          obj_name = lv_workid.
        ENDIF.
      WHEN gc_doma.      "domains
        SELECT SINGLE devclass FROM tadir INTO devc
          WHERE pgmid = gc_r3tr AND
                object = gc_doma AND
        obj_name = obj_name.
      WHEN gc_dtel.      "Data elements
        SELECT SINGLE devclass FROM tadir INTO devc
          WHERE pgmid = gc_r3tr AND
               object = gc_dtel AND
        obj_name = obj_name.
      WHEN gc_tabl.     "Tables
        SELECT SINGLE devclass FROM tadir INTO devc
           WHERE pgmid = gc_r3tr AND
               object = gc_tabl       AND
        obj_name = obj_name.
      WHEN gc_view.      "Views
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_view AND
        obj_name = obj_name.
      WHEN gc_mcob.      "matchcode objects.
        SELECT SINGLE devclass FROM tadir INTO devc
          WHERE pgmid = gc_r3tr AND
                object = gc_mcob  AND
        obj_name = obj_name.
      WHEN gc_clas.    "class
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
           object = gc_clas AND
        obj_name = obj_name.
        IF sy-subrc <> 0.
          SELECT SINGLE devclass FROM tadir INTO devc
           WHERE pgmid = gc_r3tr AND
             object = 'INTF' AND
          obj_name = obj_name.
        ENDIF.
      WHEN gc_enqu.   "enque deque
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_enqu AND
        obj_name = obj_name.
      WHEN gc_prog.
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_prog AND
        obj_name = obj_name.
      WHEN gc_tobj.   "Transport objects
        SELECT SINGLE devclass FROM tadir INTO devc
         WHERE pgmid = gc_r3tr AND
               object = gc_tobj  AND
        obj_name = lv_workid.
      WHEN 'INFO'.
        CONCATENATE 'P' obj_name INTO obj_name.
        SELECT SINGLE devclass FROM tadir INTO devc
            WHERE pgmid = gc_r3tr AND
                  object = 'TABL'       AND
        obj_name = obj_name .
      WHEN 'STRU'.
        SELECT SINGLE devclass FROM tadir INTO devc
            WHERE pgmid = gc_r3tr AND
                  object = 'TABL'       AND
        obj_name = obj_name .
      WHEN 'HRTB'.
        SELECT SINGLE devclass FROM tadir INTO devc
            WHERE pgmid = gc_r3tr AND
                  object = 'TABL'       AND
        obj_name = obj_name .
      WHEN 'POOL'.
        SELECT SINGLE devclass FROM tadir INTO devc
            WHERE pgmid = gc_r3tr AND
                  object = gc_prog      AND
        obj_name = obj_name .
      WHEN 'IASP'.
        SELECT SINGLE devclass FROM tadir INTO devc
            WHERE pgmid = gc_r3tr AND
                  object = 'IASP'       AND
        obj_name = obj_name .
    ENDCASE.
  ENDIF.
  IF NOT devc IS INITIAL.
    READ TABLE i_devct WITH KEY devclass = devc
          spras = sy-langu BINARY SEARCH.
    IF sy-subrc <> 0.
      READ TABLE i_devct WITH KEY devclass = devc
            spras = 'E' BINARY SEARCH.
      IF sy-subrc <> 0.
        READ TABLE i_devct WITH KEY devclass = devc
              BINARY SEARCH.
      ENDIF.
    ENDIF.
    IF sy-subrc = 0.
      devdesc = i_devct-ctext.
    ENDIF.
  ENDIF.
ENDFORM.                    " GET_DEVCLASS_DESC
*&---------------------------------------------------------------------*
*&      Form  F4_HELP_CUST
*&---------------------------------------------------------------------*
FORM f4_help_cust .
*-- Local data declaration
  DATA: lt_f4_customer TYPE          tt_f4_customer,
        ls_return      TYPE          ddshretval,
        lt_return      TYPE TABLE OF ddshretval.

*  SELECT DISTINCT zcustomer_name INTO TABLE lt_f4_customer
*  FROM zhana_customer.
  SELECT DISTINCT zcustomer_name INTO TABLE lt_f4_customer
  FROM zhana_upl_syslog.


  IF sy-subrc EQ 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'P_CUST'
        value_org       = 'S'
      TABLES
        value_tab       = lt_f4_customer
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    READ TABLE lt_return
    INTO ls_return INDEX 1.
    IF sy-subrc = 0.
      p_cust = ls_return-fieldval.
    ENDIF.
  ELSE.
  ENDIF.
ENDFORM.                    " F4_HELP_CUST
*&---------------------------------------------------------------------*
*&      Form  CREATE_FIELD_CATELOG
*&---------------------------------------------------------------------*
FORM create_field_catelog .
  DATA: ls_fieldcat TYPE LINE OF slis_t_fieldcat_alv.

  ls_fieldcat-fieldname   = 'ZCUSTOMER_NAME'.
  ls_fieldcat-seltext_m   = text-030.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'ZRUNID'.
  ls_fieldcat-seltext_m   = text-031.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'OBJECT_TYPE'.
  ls_fieldcat-seltext_m   = text-032.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR  ls_fieldcat.

  ls_fieldcat-fieldname   = 'ZCOUNT'.
  ls_fieldcat-seltext_m   = text-033.
  APPEND ls_fieldcat TO gt_fieldcat.
  CLEAR  ls_fieldcat.

ENDFORM.                    " CREATE_FIELD_CATELOG
*&---------------------------------------------------------------------*
*&      Form  ALV_OUT_PUT
*&---------------------------------------------------------------------*
FORM alv_out_put .
  CLEAR: gs_layout.
  gs_layout-zebra = abap_true.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program = gv_repid
      is_layout          = gs_layout
      it_fieldcat        = gt_fieldcat
    TABLES
      t_outtab           = gt_zhana_inventory
    EXCEPTIONS
      program_error      = 1
      OTHERS             = 2.
ENDFORM.                    " ALV_OUT_PUT
*&---------------------------------------------------------------------*
*&      Form  F4_HELP_RUNID
*&---------------------------------------------------------------------*
FORM f4_help_runid .

  DATA: lt_f4_runid TYPE          tt_f4_runid,
        ls_return   TYPE          ddshretval,
        lt_return   TYPE TABLE OF ddshretval.

*  SELECT DISTINCT zrunid INTO TABLE lt_f4_runid
*  FROM zhana_inventory
*  WHERE zcustomer_name EQ p_cust.
  SELECT DISTINCT zrunid INTO TABLE lt_f4_runid
  FROM zhana_upl_syslog
  WHERE zcustomer_name EQ p_cust.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'P_RUNID'
        value_org       = 'S'
      TABLES
        value_tab       = lt_f4_runid
        return_tab      = lt_return
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.

    READ TABLE lt_return INTO ls_return INDEX 1.
    IF sy-subrc = 0.
      p_runid = ls_return-fieldval.
    ENDIF.
  ELSE.
  ENDIF.

ENDFORM.                    " F4_HELP_RUNID
