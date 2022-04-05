FUNCTION-POOL ZGIT_HANA_SPLIT_SCREEN.           "MESSAGE-ID ..
TYPES : BEGIN OF t_content,
          line(255),
        END OF t_content.
DATA:it_index_new       TYPE STANDARD TABLE OF zhana_gi_index,
     ls_index_new       TYPE                   zhana_gi_index,
     gs_index_new       TYPE                   zhana_gi_index,
     ls_final           TYPE                   zhana_output,
     gt_results         TYPE STANDARD TABLE OF zhana_output,
     lt_final           TYPE STANDARD TABLE OF zhana_output,
     gv_report          TYPE                   sobj_name,
     it_save_report     TYPE STANDARD TABLE OF abaptxt255,
     it_error           TYPE STANDARD TABLE OF swoterror,
     lv_error_line      TYPE                   sy-staco,
     lv_error_offset    TYPE                   sy-fdpos,
     lv_error_msg(240)  TYPE                   c,
     it_source          TYPE STANDARD TABLE OF t_content,"abaptxt255,
     ls_old_program     TYPE STANDARD TABLE OF abaptxt255,
     ls_final_program   TYPE STANDARD TABLE OF abaptxt255,
     ls_final_program1  TYPE STANDARD TABLE OF abaptxt255,
     ls_return          TYPE                   bapireturn,
     it_return          TYPE STANDARD TABLE OF bapireturn,
     ls_line_number_new TYPE                   i,
     ls_line_number_old TYPE                   i,
     lv_line_number     TYPE                   i VALUE '1',
     ls_objtype         TYPE                   sci_typid,
     ls_objname         TYPE                   sobj_name,
     lv_final_preety    TYPE                   sobj_name,
     lv_final_pgname    TYPE                   sobj_name,
     ls_sobjname        TYPE                   sobj_name,"zhcl_hana_sub_object.
     lv_flag            TYPE                   flag VALUE space,
     gv_msg_line        TYPE                   sci_line,
     line               TYPE                   i,
     gv_trno            TYPE                   trkorr.
FIELD-SYMBOLS:<fs_results> TYPE zhana_output.
CONSTANTS:gc_fugr TYPE trobjtype VALUE 'FUGR'.
* INCLUDE LZHANA_SPLIT_SCREEND...            " Local class definition
