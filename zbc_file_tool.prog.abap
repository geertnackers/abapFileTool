************************************************************************
* PROGRAM INFORMATION
************************************************************************
* PROGRAM....... ZBC_FILE_TOOL
* TITLE......... Clean up files on application folder
* AUTHOR........ Nackers Geert
* DATE WRITTEN..
* R/3 RELEASE... <Release>
* TRANSPORTNR... <Transport number>
* COPIED FROM...
*----------------------------------------------------------------------*
* PROGRAM FUNCTION:
* Delete files that were copied to the application folder but were not
* cleaned up
************************************************************************

************************************************************************
* CHANGE HISTORY
************************************************************************
* DATE CHANGE... <Date of change written>
* AUTHOR........ <Author name>
* CHANGE DESCR.. <Description of change>
* MARKER........ <Transport number OR Solution Manager Id if available>
************************************************************************

************************************************************************
* CONVENTIONS
************************************************************************
* data            --> xd_*
* structure       --> xs_*
* internal table  --> xt_*
* class           --> xcl_*
* object          --> xo_*
* table type      --> ty_*
* constants       --> c_*
* parameters      --> p_*
* select-options  --> so_*
* ranges          --> r_*
* field symbol    --> fs_*
* <x> needs to be replaced by 'g' for global declarations and with 'l'
* for local declarations
************************************************************************
REPORT ZBC_FILE_TOOL LINE-SIZE 132
                     LINE-COUNT 65
                     NO STANDARD PAGE HEADING.
*----------------------------------------------------------------------*
*    I N C L U D E S  &  T Y P E   P O O L S                           *
*----------------------------------------------------------------------*
INCLUDE ZBC_FILE_TOOL_TOP.
INCLUDE ZBC_FILE_TOOL_CLASS.
*----------------------------------------------------------------------*
*    S T A R T   O F   S E L E C T I O N                               *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CREATE OBJECT go_doc.
  IF sy-subrc NE 0.
    MESSAGE i001(00) WITH 'No data selected or not authorized'.
  ENDIF.
*----------------------------------------------------------------------*
*    E N D   O F   S E L E C T I O N                                   *
*----------------------------------------------------------------------*
END-OF-SELECTION.

  IF go_doc IS BOUND.
    CALL METHOD go_doc->display_result( EXPORTING id_repid = sy-repid ).
  ENDIF.
