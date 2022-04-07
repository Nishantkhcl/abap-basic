FUNCTION ZGIT_HANA_SPLIT_SCREEN.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IM_TRNO) TYPE  TRKORR OPTIONAL
*"     REFERENCE(IM_FLAG_CLAS) TYPE  FLAG OPTIONAL
*"  TABLES
*"      LT_FINAL STRUCTURE  ZGIT_HANA_OUTPUT
*"      GI_CONTENT_FNL STRUCTURE  ABAPTXT255
*"      GI_CONTENT_OLD_FNL STRUCTURE  ABAPTXT255
*"      GI_INDEX_NEW STRUCTURE  ZGIT_HANA_GI_INDEX
*"      IT_RETURN STRUCTURE  ZGIT_HANA_OUTPUT OPTIONAL
*"----------------------------------------------------------------------
  LOOP AT gi_index_new INTO gs_index_new.
    ls_index_new-sobjname = gs_index_new-sobjname.
    ls_index_new-old_line = gs_index_new-old_line.
    ls_index_new-new_line = gs_index_new-new_line.
    APPEND ls_index_new TO it_index_new.
    CLEAR ls_index_new.
  ENDLOOP.
  READ TABLE lt_final INDEX 1 INTO ls_final.
  IF sy-subrc = 0.
    IF im_flag_clas = abap_true."called from ORDERBY remediation program
      IF ls_final-objtype = 'CLAS'.
        ls_final-sobjname = ls_final-sobjname_d.
      ENDIF.
    ENDIF.
    ls_objtype = ls_final-objtype.
    ls_objname = ls_final-objname.
    ls_sobjname = ls_final-sobjname.

  ENDIF.
  ls_old_program[] = gi_content_old_fnl[].
  ls_final_program[] = gi_content_fnl[].
  gt_results[] = lt_final[].
  READ TABLE it_index_new INDEX 1 INTO ls_index_new.
  gv_trno = im_trno.
  CALL SCREEN 9010.
ENDFUNCTION.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9010 OUTPUT.
  DATA:pgm_name      TYPE                   sobj_name,
       old_pgm_name  TYPE                   sobj_name,
       g_container_1 TYPE REF TO            cl_gui_custom_container,
       g_container_2 TYPE REF TO            cl_gui_custom_container,
       textid1       TYPE REF TO            cl_gui_abapedit,
       textid2       TYPE REF TO            cl_gui_abapedit,
       it_report     TYPE STANDARD TABLE OF abaptxt255,
       it_report2    TYPE STANDARD TABLE OF abaptxt255,
       gv_flag       TYPE                   flag VALUE space.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'TESTBAR'.
*Set pgm name to i/o field
  IF ls_sobjname IS NOT INITIAL.
    pgm_name = ls_sobjname.
  ELSE.
    pgm_name = ls_objname.
  ENDIF.

  IF g_container_1 IS INITIAL.
*creating object reference for container
**Set pgm name to i/o field
*    IF ls_objname IS NOT INITIAL.
*      pgm_name = ls_sobjname.
*    ENDIF.
*    old_pgm_name = pgm_name.
*    CLEAR pgm_name.
    CREATE OBJECT g_container_1
      EXPORTING
        container_name = 'CONTAINER_1'. "Pass name of the container created in screen 9010

*text editor control
    CREATE OBJECT textid1
      EXPORTING
        parent           = g_container_1
        max_number_chars = 72.

*To read only mode
    CALL METHOD textid1->set_readonly_mode
      EXPORTING
        readonly_mode          = '1'
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        error_dp               = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
*    Implement suitable error handling here
    ENDIF..

*To fill some text in text editor
    CALL METHOD textid1->set_selected_text_as_table
      EXPORTING
        table    = ls_old_program
      EXCEPTIONS
        error_dp = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    IF gv_flag EQ abap_false.
      CALL METHOD textid1->set_selection_pos_in_line
        EXPORTING
          line                   = 1
*         pos                    = 0
        EXCEPTIONS
          error_cntl_call_method = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      gv_flag = abap_true.
    ELSE.
      CALL METHOD textid1->set_first_visible_line
        EXPORTING
          line                   = ls_line_number_old
        EXCEPTIONS
          error_cntl_call_method = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
*     Implement suitable error handling here
      ENDIF.
    ENDIF.
  ENDIF.

*creating object reference for container2
*Set pgm name to i/o field
  IF g_container_2 IS INITIAL.
    CREATE OBJECT g_container_2
      EXPORTING
        container_name = 'CONTAINER_2'. "Pass name of the container created in screen 9010
**text editor control
    CREATE OBJECT textid2
      EXPORTING
        parent           = g_container_2
        max_number_chars = 72.

*To read only mode
    CALL METHOD textid2->set_readonly_mode
      EXPORTING
        readonly_mode          = '1'
      EXCEPTIONS
        error_cntl_call_method = 1
        invalid_parameter      = 2
        error_dp               = 3
        OTHERS                 = 4.
    IF sy-subrc <> 0.
*    Implement suitable error handling here
    ENDIF.

*Preety Printer
    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo                  = 'X'
      IMPORTING
        indentation_maybe_wrong = lv_line_number
      TABLES
        ntext                   = ls_final_program1
        otext                   = ls_final_program
      EXCEPTIONS
        enqueue_table_full      = 1
        include_enqueued        = 2
        include_readerror       = 3
        include_writeerror      = 4
        OTHERS                  = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

    CALL METHOD textid2->set_selected_text_as_table
      EXPORTING
        table    = ls_final_program1
      EXCEPTIONS
        error_dp = 1
        OTHERS   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
    IF gv_flag EQ abap_false.
      CALL METHOD textid2->set_selection_pos_in_line
        EXPORTING
          line                   = 1
*         pos                    = 0
        EXCEPTIONS
          error_cntl_call_method = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
      gv_flag = abap_true.
    ELSE.
      CALL METHOD textid2->set_first_visible_line
        EXPORTING
          line                   = ls_line_number_new
        EXCEPTIONS
          error_cntl_call_method = 1
          OTHERS                 = 2.
      IF sy-subrc <> 0.
*     Implement suitable error handling here
      ENDIF.
    ENDIF.
  ENDIF.

  IF pgm_name NE old_pgm_name.
    CLEAR lv_flag.
  ENDIF.
  old_pgm_name = pgm_name.
ENDMODULE.                 " STATUS_9010  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit INPUT.
*  DATA:ok_code TYPE sy-ucomm.
* free the container memory when exit
*  IF ok_code = '&F03' OR ok_code = '&F15' OR ok_code = '&F12'..
*    CALL METHOD g_container_1->free.
*    CALL METHOD g_container_2->free.
*    CLEAR: g_container_1.
*    CLEAR: g_container_2.
*    PERFORM call_screen IN PROGRAM zhcl_impact_hana_remediation.
**  LEAVE PROGRAM.
*  ENDIF.
ENDMODULE.                 " EXIT  INPUT
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9010  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9010 INPUT.
  DATA:ok_code    TYPE sy-ucomm,
       save       TYPE sy-ucomm,
       next       TYPE sy-ucomm,
       count      TYPE i,
       answer,
       index      TYPE sy-index,
       lv_index   TYPE sy-index,
       lv_index1  TYPE sy-index,
       gs_results TYPE zgit_hana_output.
  DATA: lw_result TYPE zgit_hana_output.
  line = lines( gt_results ).
  CALL METHOD cl_gui_cfw=>dispatch.

  CASE ok_code.
    WHEN '&F03' OR '&F15' OR '&F12'.
      IF lv_flag IS NOT INITIAL.
        PERFORM call_screen IN PROGRAM sy-cprog.
      ELSE.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Save'(004)
            text_question         = 'Remediated code is not saved Do You want to Save?'(003)
            text_button_1         = 'Yes'(001)
            text_button_2         = 'No'(002)
            default_button        = '1'
            display_cancel_button = ' '
          IMPORTING
            answer                = answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.
        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.
        IF answer = '1'.
          CLEAR lw_result.
          READ TABLE gt_results INTO lw_result INDEX 1.
          IF sy-subrc EQ 0.
            IF lw_result-trkorr IS INITIAL
               AND
               lw_result-devclass NE '$TMP'
               AND
               gv_trno IS INITIAL.
              PERFORM create_tr_for_save CHANGING lw_result.
            ELSEIF ( lw_result-trkorr IS NOT INITIAL
                     AND
                     lw_result-devclass NE '$TMP').
              gv_trno = lw_result-trkorr.
            ENDIF.

          ENDIF.
          IF ( gv_trno IS NOT INITIAL
             AND
             lw_result-devclass NE '$TMP')
             OR
             lw_result-devclass EQ '$TMP'.
            IF ls_objtype = gc_fugr.
              PERFORM check_function_group.
              PERFORM rs_syntax_check.
            ELSE.
              PERFORM rs_syntax_check.
            ENDIF.
          ENDIF.
        ENDIF.

        IF answer = '2'.
*          lt_final = gt_results.
          LOOP AT lt_final INTO ls_final.
            IF ls_final-status EQ icon_yellow_light.
              WRITE icon_yellow_light AS ICON TO ls_final-status.
*            ELSE.
*              ls_final-status = ' '.
            ENDIF.
            MODIFY lt_final FROM ls_final TRANSPORTING status WHERE sobjname = ls_final-sobjname.
          ENDLOOP.
          PERFORM free_continer.


        ENDIF.
      ENDIF.
    WHEN 'CANCEL'.
      PERFORM free_continer.
    WHEN 'EXIT'.
      PERFORM free_continer.
    WHEN 'SCROLL'.
      count = lines( it_index_new ).
      index = lv_index + 1.
      IF index LE count.
        CLEAR ls_index_new.
        READ TABLE it_index_new INDEX index INTO ls_index_new.
        IF sy-subrc = 0.
          ls_line_number_old = ls_index_new-old_line.
          ls_line_number_new = ls_index_new-new_line.

          CALL METHOD textid1->set_first_visible_line
            EXPORTING
              line                   = ls_line_number_old
            EXCEPTIONS
              error_cntl_call_method = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
*     Implement suitable error handling here
          ENDIF.

          CALL METHOD textid2->set_first_visible_line
            EXPORTING
              line                   = ls_line_number_new
            EXCEPTIONS
              error_cntl_call_method = 1
              OTHERS                 = 2.
          IF sy-subrc <> 0.
*     Implement suitable error handling here
          ENDIF.
        ENDIF.
        lv_index = index.
      ENDIF.
    WHEN 'SAVE'.
      CLEAR lw_result.
      READ TABLE gt_results INTO lw_result INDEX 1.
      IF sy-subrc EQ 0.
        IF lw_result-trkorr IS INITIAL
           AND
           lw_result-devclass NE '$TMP'.

          PERFORM create_tr_for_save CHANGING lw_result.

        ELSEIF lw_result-trkorr IS NOT INITIAL
               AND
               lw_result-devclass NE '$TMP'.
          gv_trno = lw_result-trkorr.
        ENDIF.
      ENDIF.
*      PERFORM action.
      IF ( gv_trno IS NOT INITIAL
           AND
           lw_result-devclass NE '$TMP')
          OR
          lw_result-devclass EQ '$TMP'.
        IF ls_objtype = gc_fugr.
          PERFORM check_function_group.
          PERFORM rs_syntax_check.
        ELSE.
          PERFORM rs_syntax_check.
        ENDIF.
      ENDIF.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_9010  INPUT
