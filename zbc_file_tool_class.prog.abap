*&---------------------------------------------------------------------*
*&  Include           ZBC_FILE_TOOL_CLASS
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_handle_events DEFINITION.

  PUBLIC SECTION.
    METHODS: on_user_command FOR EVENT added_function OF cl_salv_events
                             IMPORTING e_salv_function,
             on_double_click FOR EVENT double_click OF cl_salv_events_table "#EC NEEDED
                             IMPORTING row column.          "#EC NEEDED

  PRIVATE SECTION.
    METHODS: handle_user_command IMPORTING is_function TYPE sy-ucomm.

ENDCLASS. "lcl_handle_events DEFINITION
*----------------------------------------------------------------------*
*       CLASS lcl_doc DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_doc DEFINITION.
*-Public section - Methods
  PUBLIC SECTION.

    CONSTANTS: c_yes(1) TYPE c VALUE 'X',
               c_no(1)  TYPE c VALUE ' '.
*---Selection criteria
    DATA: gd_path     TYPE dxfields-longpath,
          gd_mask     TYPE dxfields-filemask,
          gd_type     TYPE eseftftype,
          gr_user     TYPE bgrfc_user_name_range,
          gd_usr      TYPE boolean,
          gd_len      TYPE char1,
          gd_dir      TYPE char1,
          gd_locl     TYPE char1,
          gr_cred     TYPE date_t_range.
*---Methods
    METHODS: constructor.
    METHODS: display_result  IMPORTING id_repid TYPE sy-repid.
    METHODS: read_files.

    CLASS-METHODS: initialization EXPORTING et_erdat TYPE shp_erdat_range_t. "#EC NEEDED

*-Private section - Attributes and methods
  PRIVATE SECTION.
*---Methods
    METHODS: fill_attributes IMPORTING  id_repid TYPE sy-repid
                             EXCEPTIONS fill_failed.

ENDCLASS. "lcl_idoc DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_handle_events IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_handle_events IMPLEMENTATION.

  METHOD on_user_command.

    CALL METHOD handle_user_command( EXPORTING is_function = e_salv_function ).

  ENDMETHOD.                    "on_user_command

  METHOD on_double_click.

    CHECK sy-uname = sy-uname.                              "#EC *

  ENDMETHOD.                    "on_double_click


  METHOD handle_user_command.
*---Local data declarations
    DATA: lo_selections  TYPE REF TO cl_salv_selections,
          lt_rows        TYPE salv_t_row,
          ld_row         TYPE i.
    DATA: ld_dataset     TYPE string.
    DATA: ld_answer      TYPE char1.
    DATA: ld_local       TYPE string.
    DATA: ld_delete_ok   TYPE boolean.
    DATA: ls_s_stable    TYPE lvc_s_stbl.

    CASE is_function.
      WHEN 'DELALL'.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Deletion of files'
            text_question         = 'Delete ALL ! ?'
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            display_cancel_button = ' '
          IMPORTING
            answer                = ld_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF ld_answer = '1'.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = 'Deletion all files'
              text_question         = 'Are you very sure ?'
              text_button_1         = 'Definitely!'
              text_button_2         = 'No way!'
              display_cancel_button = ' '
            IMPORTING
              answer                = ld_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2.

          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

          IF ld_answer = '1'.
            CLEAR ld_delete_ok.
            LOOP AT gt_output INTO gs_output.
              CONCATENATE gs_output-dirname gs_output-name INTO ld_dataset.
              DELETE DATASET ld_dataset.
              IF sy-subrc NE 0.
                MESSAGE s368(00) WITH 'Deletion failed for 1 or more files'.
              ELSE.
                ld_delete_ok = 'X'.
              ENDIF.
            ENDLOOP.
          ENDIF.
        ENDIF.

        IF ld_delete_ok = abap_true.
          CALL METHOD go_doc->read_files( ).
          gt_output[] = gt_file_list[].
          ls_s_stable-row = abap_true.
          go_table->refresh( EXPORTING s_stable     = ls_s_stable
                                       refresh_mode = if_salv_c_refresh=>full ).
        ENDIF.

      WHEN 'DEL'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar              = 'Deletion of files'
            text_question         = 'Delete files?'
            text_button_1         = 'Yes'
            text_button_2         = 'No'
            display_cancel_button = ' '
          IMPORTING
            answer                = ld_answer
          EXCEPTIONS
            text_not_found        = 1
            OTHERS                = 2.

        IF sy-subrc <> 0.
          MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
        ENDIF.

        IF ld_answer = '1'.

          REFRESH: lt_rows.
          lo_selections = go_table->get_selections( ).
          lt_rows       = lo_selections->get_selected_rows( ).

          CLEAR ld_delete_ok.
          LOOP AT lt_rows INTO ld_row.
            READ TABLE gt_output INTO gs_output INDEX ld_row.
            IF sy-subrc = 0.
              CONCATENATE gs_output-dirname gs_output-name INTO ld_dataset.
              DELETE DATASET ld_dataset.
              IF sy-subrc NE 0.
                MESSAGE s368(00) WITH 'Deletion failed for 1 or more files'.
              ELSE.
                ld_delete_ok = 'X'.
              ENDIF.
            ENDIF.
          ENDLOOP.

        ENDIF.

        IF ld_delete_ok = abap_true.
          CALL METHOD go_doc->read_files( ).
          gt_output[] = gt_file_list[].


          ls_s_stable-row = abap_true.
          go_table->refresh( EXPORTING s_stable     = ls_s_stable
                                       refresh_mode = if_salv_c_refresh=>full ).
        ENDIF.
      WHEN OTHERS.
    ENDCASE.

  ENDMETHOD.                    "handle_user_command


ENDCLASS. "lcl_handle_events IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_doc IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_doc IMPLEMENTATION.

*-Constructor
  METHOD constructor.
*---Get selection screen info
    CALL METHOD me->fill_attributes( id_repid = sy-repid ).

    me->read_files( ).
    gt_output[] = gt_file_list[].

  ENDMETHOD.                    "constructor

  METHOD read_files.
*---Local data declaration
    DATA: l_errcnt(2)            TYPE p VALUE 0.
    DATA: g_rec_level_120 TYPE i VALUE 0.
    DATA: ls_color               TYPE lvc_s_scol,
          lt_color               TYPE lvc_t_scol.

    REFRESH: gt_file_list, gt_output.

    gs_file-rec_level = g_rec_level_120.

    CALL 'C_DIR_READ_FINISH'             " just to be sure
        ID 'ERRNO'  FIELD gs_file-errno
        ID 'ERRMSG' FIELD gs_file-errmsg.

    CALL 'C_DIR_READ_START' ID 'DIR'    FIELD gd_path
                            ID 'FILE'   FIELD gd_mask
                            ID 'ERRNO'  FIELD gs_file-errno
                            ID 'ERRMSG' FIELD gs_file-errmsg.
    IF sy-subrc <> 0.
      MESSAGE e204(s_dx_bapi) WITH gs_file-errmsg gs_file-errmsg.
    ENDIF.

    DO.
      CLEAR gs_file.
      REFRESH lt_color.
      gs_file-rec_level = g_rec_level_120.
      CALL 'C_DIR_READ_NEXT'  ID 'TYPE'   FIELD gs_file-type
                              ID 'NAME'   FIELD gs_file-name
                              ID 'LEN'    FIELD gs_file-len
                              ID 'OWNER'  FIELD gs_file-owner
                              ID 'MTIME'  FIELD gs_file-mtime
                              ID 'MODE'   FIELD gs_file-mode
                              ID 'ERRNO'  FIELD gs_file-errno
                              ID 'ERRMSG' FIELD gs_file-errmsg.
      gs_file-dirname = gd_path.
      MOVE sy-subrc TO gs_file-subrc.

      CASE sy-subrc.

        WHEN 0.
*---------Should user ID be taken into account
          IF gd_usr = ' '.
            CHECK gs_file-owner IN gr_user.
          ENDIF.
*---------Add color and icon
          CLEAR: gs_file-errno, gs_file-errmsg.
          CASE gs_file-type(1).

            WHEN 'F' OR 'f'.
              MOVE c_yes  TO gs_file-usable.
              gs_file-icon_dir = icon_edit_file.

              ls_color-color-col = col_positive. ls_color-color-int = 0.  ls_color-color-inv = 0.
              ls_color-fname     = 'NAME'.       APPEND ls_color TO lt_color.

            WHEN 'D' OR 'd'.                    " directory
              IF gd_dir = abap_true. " ignore.
                CONTINUE.
              ENDIF.
              MOVE c_no  TO gs_file-usable.
              gs_file-dir_flag = c_yes.
              gs_file-icon_dir = icon_object_folder.

              ls_color-color-col = col_total.    ls_color-color-int = 0.  ls_color-color-inv = 0.
              ls_color-fname     = 'NAME'.       APPEND ls_color TO lt_color.

            WHEN OTHERS. " device, fifo, socket,...
              MOVE c_no TO gs_file-usable.
              gs_file-icon_dir = icon_deceased_patient.

              ls_color-color-col = col_group.    ls_color-color-int = 0.  ls_color-color-inv = 0.
              ls_color-fname     = 'NAME'.       APPEND ls_color TO lt_color.

          ENDCASE.

          IF gs_file-len = 0.
            IF gd_len = abap_true. " ignore
              CONTINUE.
            ENDIF.
            MOVE c_no TO gs_file-usable.
            gs_file-icon_dir = icon_deceased_patient.
          ENDIF.

        WHEN 1.
          EXIT.

        WHEN OTHERS.
          ADD 1 TO l_errcnt.
          IF l_errcnt > 10.
            EXIT.
          ENDIF.
          IF sy-subrc = 5.
            MOVE: '???' TO gs_file-type,
                  '???' TO gs_file-owner,
                  '???' TO gs_file-mode.
          ENDIF.
          MOVE c_no TO gs_file-usable.
      ENDCASE.

      PERFORM p6_to_date_time_tz IN PROGRAM rstr0400 USING gs_file-mtime
                                                 gs_file-mod_time
                                                 gs_file-mod_date.

      CHECK gs_file-mod_date IN gr_cred.
*-----Set color to field
      gs_file-t_color[] = lt_color[].

      APPEND gs_file TO gt_file_list.
    ENDDO.

    CALL 'C_DIR_READ_FINISH'
        ID 'ERRNO'  FIELD gs_file-errno
        ID 'ERRMSG' FIELD gs_file-errmsg.

    SORT gt_file_list STABLE BY rec_level DESCENDING
                                name      ASCENDING
                                mtime     DESCENDING.

  ENDMETHOD.                    "read_files


  METHOD display_result.
*---Local data declaration
    DATA: lo_functions  TYPE REF TO cl_salv_functions_list,
          lo_events     TYPE REF TO cl_salv_events_table,
          lo_selections TYPE REF TO cl_salv_selections,
          lo_display    TYPE REF TO cl_salv_display_settings,
          lo_sort       TYPE REF TO cl_salv_sorts,
          lo_layout     TYPE REF TO cl_salv_layout,
          ls_layout     TYPE salv_s_layout,                 "#EC NEEDED
          lo_columns    TYPE REF TO cl_salv_columns_table,
          lo_columns2   TYPE REF TO cl_salv_column_table,
          lo_column     TYPE REF TO cl_salv_column.
*          ld_name       TYPE lvc_fname.

*---Create ALV table instance
    TRY.
        cl_salv_table=>factory( IMPORTING r_salv_table = go_table
                                 CHANGING t_table      = gt_output ).
      CATCH cx_salv_msg .                               "#EC NO_HANDLER
      CATCH cx_salv_object_not_found.                   "#EC NO_HANDLER
    ENDTRY.
*---Set menu status ( copied from standard SALV_STANDARD )
    go_table->set_screen_status( pfstatus      = 'SALV_STANDARD'
                                 report        = id_repid
                                 set_functions = go_table->c_functions_all ).
*---Default ALV buttons
    lo_functions = go_table->get_functions( ).
    lo_functions->set_default( abap_true ).
*---Register events
    lo_events = go_table->get_event( ).
    CREATE OBJECT go_events.
    SET HANDLER go_events->on_double_click FOR lo_events.
    SET HANDLER go_events->on_user_command FOR lo_events.
*---Selection possibilities
    lo_selections = go_table->get_selections( ).
    lo_selections->set_selection_mode( if_salv_c_selection_mode=>multiple ).

    lo_display = go_table->get_display_settings( ).
    lo_display->set_striped_pattern( value = c_yes ).
    lo_display->set_fit_column_to_table_size( value = c_yes ).

    lo_sort = go_table->get_sorts( ).
    lo_sort->set_group_active( value = c_yes ).
    lo_sort->add_sort( 'DIRNAME' ).
    lo_sort->add_sort( 'MOD_DATE' ).

    lo_layout = go_table->get_layout( ).
    ls_layout = lo_layout->get_default_layout( ).
*---Get all columns
    lo_columns = go_table->get_columns( ).
    lo_columns->set_optimize( ).
    lo_columns->set_column_position( columnname = 'ICON_DIR' position   = 2 ).

    lo_columns2 ?= lo_columns->get_column( 'ICON_DIR' ).
    lo_columns2->set_icon( if_salv_c_bool_sap=>true ).

    TRY.
        lo_columns->set_color_column( 'T_COLOR' ).
      CATCH cx_salv_data_error.                         "#EC NO_HANDLER
    ENDTRY.
*---Hide columns
    lo_column = lo_columns->get_column( 'REC_LEVEL' ). lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'CHANGED' ).   lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'SEEN' ).      lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'ERRMSG' ).    lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'ERRNO' ).     lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'SUBRC' ).     lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'USABLE' ).    lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'MODE' ).      lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'MTIME' ).     lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'DIR_FLAG' ).  lo_column->set_technical( abap_true ).
    lo_column = lo_columns->get_column( 'TYPE' ).      lo_column->set_visible( abap_false ).
*---Set remaining titles
    lo_column = lo_columns->get_column( 'DIRNAME' ).   lo_column->set_short_text( 'Directory' ).
    lo_column = lo_columns->get_column( 'NAME' ).      lo_column->set_short_text( 'Filename' ).
    lo_column = lo_columns->get_column( 'TYPE' ).      lo_column->set_short_text( 'Filetype' ).
    lo_column = lo_columns->get_column( 'LEN' ).       lo_column->set_short_text( 'Length' ).
    lo_column = lo_columns->get_column( 'OWNER' ).     lo_column->set_short_text( 'Owner' ).
    lo_column = lo_columns->get_column( 'MOD_DATE' ).  lo_column->set_short_text( 'Date' ).
    lo_column = lo_columns->get_column( 'MOD_TIME' ).  lo_column->set_short_text( 'Time' ).
    lo_column = lo_columns->get_column( 'ICON_DIR' ).  lo_column->set_short_text( 'FileType' ).
*---Set table on the screen
    go_table->display( ).

  ENDMETHOD.                    "display_result

  METHOD initialization.

    CHECK sy-uname = sy-uname.                              "#EC *

  ENDMETHOD.                    "initialization
*-Fill up attributes based on selection screen
  METHOD fill_attributes.
*--Local data declarations
    DATA: lt_seltab    TYPE rsparams_tt,
          ls_seltab    TYPE rsparams,
          lf_field_in  TYPE text20,
          lf_field_out TYPE text20.

    FIELD-SYMBOLS: <fs_field_in>  TYPE any,
                   <fs_field_out> TYPE any.
*--Retrieve selection screen fields
    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report     = id_repid
      TABLES
        selection_table = lt_seltab
      EXCEPTIONS
        not_found       = 1
        no_report       = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
      RAISE fill_failed.
    ENDIF.
*--Assign the selection screen fields to the attributes
    LOOP AT lt_seltab INTO ls_seltab.
      lf_field_in  = ls_seltab-selname.
      lf_field_out = ls_seltab-selname.
      CASE ls_seltab-selname(2).
        WHEN 'P_'.
          REPLACE FIRST OCCURRENCE OF 'P_' IN lf_field_out WITH 'GD_'.
        WHEN 'S_'.
          REPLACE FIRST OCCURRENCE OF 'S_' IN lf_field_out WITH 'GR_'.
          CONCATENATE lf_field_in '[]' INTO lf_field_in. " Needed to avoid short dump
      ENDCASE.
      ASSIGN (lf_field_in) TO <fs_field_in>.
      IF <fs_field_in> IS ASSIGNED.
        ASSIGN (lf_field_out) TO <fs_field_out>.
        IF <fs_field_out> IS ASSIGNED.
          <fs_field_out> = <fs_field_in>.
          UNASSIGN <fs_field_out>.
          UNASSIGN <fs_field_in>.
        ELSE.
          UNASSIGN <fs_field_in>.
          RAISE fill_failed.
        ENDIF.
      ELSE.
        RAISE fill_failed.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "fill_attributes


ENDCLASS. "lcl_doc IMPLEMENTATION
