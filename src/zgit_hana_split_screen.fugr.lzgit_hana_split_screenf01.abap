*----------------------------------------------------------------------*
***INCLUDE LZHANA_SPLIT_SCREENF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  RS_SYNTAX_CHECK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM rs_syntax_check .
  DATA: lv_funcname TYPE rs38l_fnam.
  DATA   modification_mode TYPE c.
  DATA   transport_key TYPE trkey.
  DATA:  l_korrnum   TYPE trkorr,
         lv_devclass TYPE devclass.
  DATA:    lv_pgmid    TYPE e071-pgmid,
           lv_object   TYPE e071-object,
           lv_obj_name TYPE e071-obj_name,
           ls_mtdkey   TYPE seocpdkey.

  DATA:lv_we_lock_order      TYPE e070-trkorr,
       lv_we_lock_order_user TYPE e070-as4user,
       lv_we_lock_task       TYPE e070-trkorr,
       lv_we_lock_task_user  TYPE e070-as4user,
       lv_we_object_editable TYPE trpari-s_checked.


  IF ls_objname <> ls_sobjname. "both are not in same include
    CALL FUNCTION 'RS_SYNTAX_CHECK'
      EXPORTING
        i_global_check   = 'X'
        i_global_program = ls_objname
        i_program        = ls_sobjname
      IMPORTING
        o_error_line     = lv_error_line
        o_error_message  = lv_error_msg
        o_error_offset   = lv_error_offset
      TABLES
        i_source         = ls_final_program1.
  ELSE.
    CALL FUNCTION 'RS_SYNTAX_CHECK'
      EXPORTING
*       i_global_check   = 'X'
*       i_global_program = ls_objname
        i_global_program = ls_sobjname
        i_program        = ls_sobjname
      IMPORTING
        o_error_line     = lv_error_line
        o_error_message  = lv_error_msg
        o_error_offset   = lv_error_offset
      TABLES
        i_source         = ls_final_program1.

  ENDIF.
  IF lv_error_line IS NOT INITIAL.
    lt_final = gt_results.
    WRITE icon_red_light AS ICON TO ls_final-status.
    MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
    MESSAGE  lv_error_msg  TYPE 'E' DISPLAY LIKE 'E'.
    lv_flag = 'X'.
  ELSE.

    lt_final = gt_results.
    WRITE icon_green_light AS ICON TO ls_final-status.
*    IF ls_final-devclass NE '$TMP'.
*      ls_final-trkorr = gv_trno.
*    ENDIF.

    MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
    lv_flag = 'X'.

    INSERT REPORT ls_sobjname FROM ls_final_program1 UNICODE ENABLING 'X'.
    IF sy-subrc = 0 AND gv_trno IS NOT INITIAL.
      IF ls_final-objtype EQ 'FUGR'.
        CALL FUNCTION 'FUNCTION_PRUEF_TRDIR'
          EXPORTING
            im_include_name         = ls_final-sobjname
          IMPORTING
            ex_funcname             = lv_funcname
          EXCEPTIONS
            include_not_found_trdir = 1
            report_source_not_found = 2
            OTHERS                  = 3.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'RS_CORR_INSERT'
            EXPORTING
*             object                   = lv_funcname
              object                   = ls_final-sobjname
*             object_class             = 'FUNC'
              object_class             = 'PROG'
              mode                     = space
              devclass                 = ls_final-devclass
              object_class_supports_ma = 'X'
              korrnum                  = gv_trno
*             suppress_dialog          = 'X'
            IMPORTING
              ordernum                 = gv_trno
            EXCEPTIONS
              cancelled                = 1
              permission_failure       = 2
              unknown_objectclass      = 3
              OTHERS                   = 4.
          IF sy-subrc EQ 0.
            ls_final-trkorr = gv_trno.
            MODIFY lt_final FROM ls_final TRANSPORTING trkorr WHERE sobjname EQ ls_final-sobjname.
          ELSEIF sy-subrc EQ 1
                 AND
                 sy-ucomm NE 'PF12'.
            CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
              EXPORTING
                wi_kurztext       = 'HANA Smart tool corresctions'
                wi_trfunction     = 'S'
                iv_username       = sy-uname
                wi_strkorr        = gv_trno
                wi_client         = sy-mandt
              IMPORTING
                we_trkorr         = gv_trno
              EXCEPTIONS
                no_systemname     = 1
                no_systemtype     = 2
                no_authorization  = 3
                db_access_error   = 4
                file_access_error = 5
                enqueue_error     = 6
                number_range_full = 7
                invalid_input     = 8
                OTHERS            = 9.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'RS_CORR_INSERT'
                EXPORTING
*                 object                   = lv_funcname
                  object                   = ls_final-sobjname
*                 object_class             = 'FUNC'
                  object_class             = 'PROG'
                  mode                     = space
                  devclass                 = ls_final-devclass
                  object_class_supports_ma = 'X'
                  korrnum                  = gv_trno
*                 suppress_dialog          = 'X'
                IMPORTING
                  ordernum                 = gv_trno
                EXCEPTIONS
                  cancelled                = 1
                  permission_failure       = 2
                  unknown_objectclass      = 3
                  OTHERS                   = 4.
              IF sy-subrc EQ 0.
                ls_final-trkorr = gv_trno.
                MODIFY lt_final FROM ls_final TRANSPORTING trkorr WHERE sobjname EQ ls_final-sobjname.
              ELSE.
                WRITE icon_yellow_light AS ICON TO ls_final-status.
                INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
                MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
                lv_flag = 'X'.
              ENDIF.
            ENDIF.
          ELSEIF sy-subrc EQ 1
                 AND
                 sy-ucomm EQ 'PF12'.
            WRITE icon_yellow_light AS ICON TO ls_final-status.
            MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
            INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
            lv_flag = 'X'.
          ELSE.
            WRITE icon_yellow_light AS ICON TO ls_final-status.
            MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
            INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
            lv_flag = 'X'.
          ENDIF.
        ENDIF.
      ELSEIF lw_result-objtype EQ 'PROG'.
        CALL FUNCTION 'RS_CORR_INSERT'
          EXPORTING
            object                   = lw_result-sobjname
            object_class             = lw_result-objtype
            mode                     = space
            global_lock              = 'X'
            devclass                 = lw_result-devclass
            object_class_supports_ma = 'X'
            korrnum                  = gv_trno
*           suppress_dialog          = 'X'
          IMPORTING
            ordernum                 = gv_trno
          EXCEPTIONS
            cancelled                = 1
            permission_failure       = 2
            unknown_objectclass      = 3
            OTHERS                   = 4.
        IF sy-subrc EQ 0.
          ls_final-trkorr = gv_trno.
          MODIFY lt_final FROM ls_final TRANSPORTING trkorr WHERE sobjname EQ ls_final-sobjname.
        ELSEIF sy-subrc EQ 1
               AND
               sy-ucomm NE 'PF12'.
          CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
            EXPORTING
              wi_kurztext       = 'HANA Smart tool corresctions'
              wi_trfunction     = 'S'
              iv_username       = sy-uname
              wi_strkorr        = gv_trno
              wi_client         = sy-mandt
            IMPORTING
              we_trkorr         = gv_trno
            EXCEPTIONS
              no_systemname     = 1
              no_systemtype     = 2
              no_authorization  = 3
              db_access_error   = 4
              file_access_error = 5
              enqueue_error     = 6
              number_range_full = 7
              invalid_input     = 8
              OTHERS            = 9.
          IF sy-subrc EQ 0.
            CALL FUNCTION 'RS_CORR_INSERT'
              EXPORTING
                object                   = lw_result-sobjname
                object_class             = lw_result-objtype
                mode                     = space
                global_lock              = 'X'
                devclass                 = lw_result-devclass
                object_class_supports_ma = 'X'
                korrnum                  = gv_trno
*               suppress_dialog          = 'X'
              IMPORTING
                ordernum                 = gv_trno
              EXCEPTIONS
                cancelled                = 1
                permission_failure       = 2
                unknown_objectclass      = 3
                OTHERS                   = 4.
            IF sy-subrc EQ 0.
              ls_final-trkorr = gv_trno.
              MODIFY lt_final FROM ls_final TRANSPORTING trkorr WHERE sobjname EQ ls_final-sobjname.
            ELSE.
              WRITE icon_yellow_light AS ICON TO ls_final-status.
              INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
              MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
              lv_flag = 'X'.
            ENDIF.
          ENDIF.
        ELSEIF sy-subrc EQ 1
               AND
               sy-ucomm EQ 'PF12'.
          WRITE icon_yellow_light AS ICON TO ls_final-status.
          INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
          MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
          lv_flag = 'X'.
        ELSE.
          WRITE icon_yellow_light AS ICON TO ls_final-status.
          INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
          MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
          lv_flag = 'X'.
        ENDIF.
      ELSEIF lw_result-objtype EQ 'CLAS'.

        CALL FUNCTION 'SEO_METHOD_GET_NAME_BY_INCLUDE'
          EXPORTING
            progname = ls_final-sobjname
          IMPORTING
            mtdkey   = ls_mtdkey.
        IF sy-subrc EQ 0.
          CALL FUNCTION 'RS_CORR_INSERT'
            EXPORTING
              object                   = ls_mtdkey
              object_class             = 'METH'
              mode                     = space
              global_lock              = 'X'
              devclass                 = lw_result-devclass
              object_class_supports_ma = 'X'
              korrnum                  = gv_trno
*             suppress_dialog          = 'X'
            IMPORTING
              ordernum                 = gv_trno
            EXCEPTIONS
              cancelled                = 1
              permission_failure       = 2
              unknown_objectclass      = 3
              OTHERS                   = 4.
          IF sy-subrc EQ 0.
            ls_final-trkorr = gv_trno.
            MODIFY lt_final FROM ls_final TRANSPORTING trkorr WHERE trkorr IS INITIAL.

          ELSEIF sy-subrc EQ 1
                 AND
                 sy-ucomm NE 'PF12'.
            CALL FUNCTION 'TRINT_INSERT_NEW_COMM'
              EXPORTING
                wi_kurztext       = 'HANA Smart tool corresctions'
                wi_trfunction     = 'S'
                iv_username       = sy-uname
                wi_strkorr        = gv_trno
                wi_client         = sy-mandt
              IMPORTING
                we_trkorr         = gv_trno
              EXCEPTIONS
                no_systemname     = 1
                no_systemtype     = 2
                no_authorization  = 3
                db_access_error   = 4
                file_access_error = 5
                enqueue_error     = 6
                number_range_full = 7
                invalid_input     = 8
                OTHERS            = 9.
            IF sy-subrc EQ 0.
              CALL FUNCTION 'RS_CORR_INSERT'
                EXPORTING
                  object                   = ls_mtdkey
                  object_class             = 'METH'
                  mode                     = space
                  global_lock              = 'X'
                  devclass                 = lw_result-devclass
                  object_class_supports_ma = 'X'
                  korrnum                  = gv_trno
*                 suppress_dialog          = 'X'
                IMPORTING
                  ordernum                 = gv_trno
                EXCEPTIONS
                  cancelled                = 1
                  permission_failure       = 2
                  unknown_objectclass      = 3
                  OTHERS                   = 4.
              IF sy-subrc EQ 0.
                ls_final-trkorr = gv_trno.
                MODIFY lt_final FROM ls_final TRANSPORTING trkorr WHERE trkorr IS INITIAL.
              ELSE.
                WRITE icon_yellow_light AS ICON TO ls_final-status.
                INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
                MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
                lv_flag = 'X'.
              ENDIF.
            ENDIF.
          ELSEIF sy-subrc EQ 1
                 AND
                 sy-ucomm EQ 'PF12'.
            WRITE icon_yellow_light AS ICON TO ls_final-status.
            INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
            MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
            lv_flag = 'X'.
          ELSE.
            WRITE icon_yellow_light AS ICON TO ls_final-status.
            INSERT REPORT ls_sobjname FROM ls_old_program UNICODE ENABLING 'X'.
            MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
            lv_flag = 'X'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
    PERFORM free_continer.
  ENDIF.
ENDFORM.                    " RS_SYNTAX_CHECK
*&---------------------------------------------------------------------*
*&      Form  CHECK_FUNCTION_GROUP
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM check_function_group .
  DATA:lt_objname1  TYPE STANDARD TABLE OF syrepid,"sobj_name.
       ls_groupname TYPE                   rs38l_area,
       ls_objname1  TYPE                   pname.
  ls_groupname = ls_objname.
  CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
    IMPORTING
      pname               = ls_objname1
    CHANGING
      group               = ls_groupname
    EXCEPTIONS
      function_not_exists = 1
      include_not_exists  = 2
      group_not_exists    = 3
      no_selections       = 4
      no_function_include = 5
      OTHERS              = 6.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.
  IF ls_objname1 IS NOT INITIAL.
    ls_objname = ls_objname1.
  ENDIF.
ENDFORM.                    " CHECK_FUNCTION_GROUP
*&---------------------------------------------------------------------*
*&      Form  FREE_CONTINER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM free_continer .
  CLEAR: count, index, ls_line_number_old , ls_line_number_new.
  CALL METHOD g_container_1->free.
  CALL METHOD g_container_2->free.

  CLEAR: g_container_1.
  CLEAR: g_container_2.

  PERFORM call_screen IN PROGRAM (sy-cprog).
ENDFORM.                    " FREE_CONTINER
*&---------------------------------------------------------------------*
*&      Form  CREATE_TR_FOR_SAVE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM create_tr_for_save CHANGING lw_result TYPE zghana_output.
  DATA: lv_funcname TYPE rs38l_fnam.
  DATA: ls_mtdkey TYPE seocpdkey.
  IF gv_trno IS INITIAL.
    IF lw_result-objtype EQ 'FUGR'.
*      CALL FUNCTION 'FUNCTION_PRUEF_TRDIR'
*        EXPORTING
*          im_include_name         = lw_result-sobjname
*        IMPORTING
*          ex_funcname             = lv_funcname
*        EXCEPTIONS
*          include_not_found_trdir = 1
*          report_source_not_found = 2
*          OTHERS                  = 3.
*      IF sy-subrc EQ 0.
        CALL FUNCTION 'RS_CORR_INSERT'
          EXPORTING
*            object                   = lv_funcname
            object                   = lw_result-sobjname
*            object_class             = 'FUNC'
            object_class             = 'PROG'
            mode                     = space
            devclass                 = lw_result-devclass
            object_class_supports_ma = 'X'
            korrnum                  = gv_trno
          IMPORTING
            ordernum                 = gv_trno
          EXCEPTIONS
            cancelled                = 1
            permission_failure       = 2
            unknown_objectclass      = 3
            OTHERS                   = 4.
        IF sy-subrc EQ 0.
          lw_result-trkorr = gv_trno.
          MODIFY gt_results FROM lw_result TRANSPORTING trkorr WHERE sobjname EQ lw_result-sobjname.
        ELSEIF sy-subrc EQ 1.
          MESSAGE 'Please select TR for saving the program' TYPE 'E'.
          EXIT.
        ENDIF.
*      ENDIF.
    ELSEIF lw_result-objtype EQ 'PROG'.
      CALL FUNCTION 'RS_CORR_INSERT'
        EXPORTING
          object                   = lw_result-sobjname
          object_class             = lw_result-objtype
          mode                     = space
          global_lock              = 'X'
          devclass                 = lw_result-devclass
          object_class_supports_ma = 'X'
          korrnum                  = gv_trno
        IMPORTING
          ordernum                 = gv_trno
        EXCEPTIONS
          cancelled                = 1
          permission_failure       = 2
          unknown_objectclass      = 3
          OTHERS                   = 4.
      IF sy-subrc EQ 0.
        lw_result-trkorr = gv_trno.
        MODIFY gt_results FROM lw_result TRANSPORTING trkorr WHERE sobjname EQ lw_result-sobjname.
      ELSEIF sy-subrc EQ 1.
        MESSAGE 'Please select TR for saving the program' TYPE 'E'.
        EXIT.
      ENDIF.
    ELSEIF lw_result-objtype EQ 'CLAS'.

      CALL FUNCTION 'SEO_METHOD_GET_NAME_BY_INCLUDE'
        EXPORTING
          progname = ls_final-sobjname
        IMPORTING
          mtdkey   = ls_mtdkey.
      IF sy-subrc EQ 0.
        CALL FUNCTION 'RS_CORR_INSERT'
          EXPORTING
            object                   = ls_mtdkey
            object_class             = 'METH'
            mode                     = space
            global_lock              = 'X'
            devclass                 = lw_result-devclass
            object_class_supports_ma = 'X'
            korrnum                  = gv_trno
*           suppress_dialog          = 'X'
          IMPORTING
            ordernum                 = gv_trno
          EXCEPTIONS
            cancelled                = 1
            permission_failure       = 2
            unknown_objectclass      = 3
            OTHERS                   = 4.
        IF sy-subrc EQ 0.
          lw_result-trkorr = gv_trno.
          MODIFY gt_results FROM lw_result TRANSPORTING trkorr WHERE sobjname EQ lw_result-sobjname.
        ELSE.
          MESSAGE 'Please select TR for saving the program' TYPE 'E'.
          EXIT.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
ENDFORM.                    " CREATE_TR_FOR_SAVE
