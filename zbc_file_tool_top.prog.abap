*&---------------------------------------------------------------------*
*&  Include           ZBC_FILE_TOOL_TOP
*&---------------------------------------------------------------------*

*----------------------------------------------------------------------*
*    T A B L E S                                                       *
*----------------------------------------------------------------------*
INCLUDE <color>.
INCLUDE <icon>.
*----------------------------------------------------------------------*
*    T Y P E S                                                         *
*----------------------------------------------------------------------*
TYPES: BEGIN OF x_file,
        dirname(255) TYPE c, " name of directory. (possibly truncated.)
        name(255)    TYPE c, " name of entry. (possibly truncated.)
        type(10)     TYPE c,           " type of entry.
        len(8)       TYPE p,           " length in bytes.
        owner(8)     TYPE c,           " owner of the entry.
        mtime(6)     TYPE p, " last modification date,seconds since 1970
        mode(9)      TYPE c, " like "rwx-r-x--x": protection mode.
        usable(1)    TYPE c,
        subrc(4)     TYPE c,
        errno(3)     TYPE c,
        errmsg(40)   TYPE c,
        mod_date     TYPE d,
        mod_time(8)  TYPE c,           " hh:mm:ss
        seen(1)      TYPE c,
        changed(1)   TYPE c,
        rec_level    TYPE i,
        dir_flag     TYPE xflag,
        t_color      TYPE lvc_t_scol,
        icon_dir     TYPE icon_d,
      END OF x_file.
*----------------------------------------------------------------------*
*    C L A S S E S                                                     *
*----------------------------------------------------------------------*
CLASS lcl_doc DEFINITION DEFERRED.
CLASS lcl_handle_events DEFINITION DEFERRED.
*----------------------------------------------------------------------*
*    G L O B A L   D A T A   D E C L A R A T I O N                     *
*----------------------------------------------------------------------*
DATA: gi_rows TYPE salv_t_row.    "#EC NEEDED
DATA: gs_row TYPE i.              "#EC NEEDED
DATA: g_repid TYPE syrepid.       "#EC NEEDED

DATA: gt_output TYPE STANDARD TABLE OF x_file,
      gs_output TYPE x_file.
DATA: gd_locl TYPE string.
DATA: gr_user TYPE bgrfc_user_name_range.

DATA: gt_file_list TYPE STANDARD TABLE OF x_file,
      gs_file      TYPE x_file.

DATA: gf_report TYPE char3.
DATA: gf_repid TYPE sy-repid.
*----------------------------------------------------------------------*
*    O B J E C T   D E C L A R A T I O N S                             *
*----------------------------------------------------------------------*
DATA : go_table TYPE REF TO cl_salv_table,
       go_events     TYPE REF TO lcl_handle_events.

DATA: go_doc TYPE REF TO lcl_doc.
*----------------------------------------------------------------------*
*    S E L E C T I O N - S C R E E N                                   *
*----------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b0 WITH FRAME TITLE text-ti1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-ti3.
PARAMETERS: p_path TYPE dxfields-longpath DEFAULT '/' LOWER CASE,
            p_mask TYPE dxfields-filemask DEFAULT '*.*' LOWER CASE NO-DISPLAY.
PARAMETERS: p_type TYPE eseftftype DEFAULT 'ASC' OBLIGATORY.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(26) text-010.
SELECT-OPTIONS: s_user FOR sy-uname NO-EXTENSION NO INTERVALS LOWER CASE.
SELECTION-SCREEN COMMENT 50(13) text-011.
PARAMETERS: p_usr AS CHECKBOX.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-ti4.
SELECT-OPTIONS: s_cred FOR sy-datum.
PARAMETERS: p_len AS CHECKBOX.
PARAMETERS: p_dir AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b3.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-ti2.
PARAMETERS: p_locl TYPE string DEFAULT 'C:\Temp\' LOWER CASE NO-DISPLAY.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN END OF BLOCK b0.
*----------------------------------------------------------------------*
*    I N I T I A L I Z A T I O N                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  CONCATENATE sy-sysid 'adm' INTO s_user-low.
  TRANSLATE s_user-low TO LOWER CASE.
  s_user-sign = 'I'. s_user-option = 'EQ'. APPEND s_user.
