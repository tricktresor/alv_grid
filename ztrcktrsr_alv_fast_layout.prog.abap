*&---------------------------------------------------------------------*
*& Report ZTRCKTRSR_ALV_FAST_LAYOUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztrcktrsr_alv_fast_layout.

"http://www.tricktresor.de/blog/grid-mit-variantenschnellzugriff-menue/

* BESCHREIBUNG
* ============
* Das Programm enthält eine Klasse, mit deren Hilfe vier durch einen
* Splitter getrennte ALV-Grids angezeigt werden
* Es können Varianten abgespeichert werden, die dann als Schnellzugriff
* mit einem Menü in der Toolbar aktiviert werden.


*** Class Application
CLASS lcl_main DEFINITION DEFERRED.

DATA gv_matnr    TYPE matnr.
DATA gt_mara     TYPE STANDARD TABLE OF mara.
DATA gt_marc     TYPE STANDARD TABLE OF marc.


SELECT-OPTIONS s_matnr FOR gv_matnr.

*----------------------------------------------------------------------*
*       CLASS lcl_main DEFINITION
*----------------------------------------------------------------------*
CLASS lcl_main DEFINITION.

  PUBLIC SECTION.

    DATA gr_grid    TYPE REF TO            cl_gui_alv_grid.
    DATA gr_cont    TYPE REF TO            cl_gui_container.
    DATA gs_variant TYPE                   disvariant.
    DATA gs_layout  TYPE                   lvc_s_layo.
    DATA gv_table   TYPE                   text80.

    DATA ls_toolbar       TYPE stb_button.
    DATA ls_variant       TYPE disvariant.

*** Variants
    DATA lt_ltdx          TYPE STANDARD TABLE OF ltdx.

*** Variant texts
    DATA lt_ltdxt         TYPE STANDARD TABLE OF ltdxt.

    METHODS:

*** constructor
      constructor
        IMPORTING i_handle    TYPE c
                  i_container TYPE REF TO cl_gui_container
                  i_title     TYPE lvc_title
                  i_table     TYPE c,

*** event for menu button
      handle_menu_button
                    FOR EVENT menu_button OF cl_gui_alv_grid
        IMPORTING e_object e_ucomm sender,

*** event for double click
      handle_double_click
                    FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row e_column es_row_no sender,

*** event for adding buttons to toolbar
      handle_toolbar
                    FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING e_object e_interactive sender,

*** event for pressed buttons
      handle_user_command
                    FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING e_ucomm sender.

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_main IMPLEMENTATION
*----------------------------------------------------------------------*
CLASS lcl_main IMPLEMENTATION.

  METHOD constructor.

    FIELD-SYMBOLS <outtab> TYPE STANDARD TABLE.

    gv_table = i_table.
    ASSIGN (gv_table) TO <outtab>.
    CHECK sy-subrc = 0.

*** Create ALV Grid
    CREATE OBJECT gr_grid
      EXPORTING
        i_parent = i_container.

*** Set variant
    CLEAR gs_variant.
    gs_variant-report    = sy-repid.
    gs_variant-handle    = i_handle.
    gs_variant-username  = sy-uname.

*** Layout
    gs_layout-grid_title = i_title.
    gs_layout-no_toolbar = ' '.

*** Set display
    CALL METHOD gr_grid->set_table_for_first_display
      EXPORTING
        i_structure_name = i_handle
        is_variant       = gs_variant
        i_save           = 'A'
        i_default        = 'X'
        is_layout        = gs_layout
      CHANGING
        it_outtab        = <outtab>
      EXCEPTIONS
        OTHERS           = 4.

**** Create event handler for grid
    SET HANDLER handle_user_command
    handle_toolbar
    handle_double_click
    handle_menu_button
    FOR gr_grid.

*** set toolbar interactive
    CALL METHOD gr_grid->set_toolbar_interactive.

  ENDMETHOD.                    "constructor

  METHOD handle_double_click.

    FIELD-SYMBOLS <outtab> TYPE STANDARD TABLE.
    FIELD-SYMBOLS <outwa>  TYPE any.
    FIELD-SYMBOLS <value>  TYPE any.

    ASSIGN (gv_table) TO <outtab>.
    CHECK sy-subrc = 0.

    CASE e_column.
      WHEN 'MATNR'.
        READ TABLE <outtab> ASSIGNING <outwa> INDEX es_row_no-row_id.
        IF sy-subrc = 0.
          ASSIGN COMPONENT e_column OF STRUCTURE <outwa> TO <value>.
          IF sy-subrc = 0.
            SET PARAMETER ID 'MAT' FIELD <value>.
            CALL TRANSACTION 'MM03' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
    ENDCASE.

  ENDMETHOD.                    "handle_double_click

  METHOD handle_toolbar.

    FIELD-SYMBOLS <ltdx>  TYPE ltdx.
    FIELD-SYMBOLS <ltdxt> TYPE ltdxt.

**** Get actual variant
    CALL METHOD sender->get_variant
      IMPORTING
        es_variant = ls_variant.

*** Read existing variants
    SELECT * FROM ltdx INTO TABLE lt_ltdx UP TO 10 ROWS
    WHERE relid  = 'LT'
    AND report = gs_variant-report
    AND handle = gs_variant-handle
    AND srtf2  = 0
    AND ( username = space
    OR  username = sy-uname )
    ORDER BY username variant.

    IF sy-subrc = 0.
*** add Separator to toolbar
      ls_toolbar-function  = 'DUMMY'.
      ls_toolbar-butn_type = '3'.
      APPEND ls_toolbar TO e_object->mt_toolbar.

*** Get variant texts
      SELECT * FROM ltdxt INTO TABLE lt_ltdxt
      FOR ALL ENTRIES IN lt_ltdx
      WHERE relid  = 'LT'
      AND report = gs_variant-report
      AND handle = gs_variant-handle
      AND variant = lt_ltdx-variant
      AND langu = sy-langu.

*** Check if there are global variants...
      READ TABLE lt_ltdx TRANSPORTING NO FIELDS
      WITH KEY username = space.
      IF sy-subrc = 0.
*** add menu "GLOBAL" to toolbar
        CLEAR ls_toolbar.
        IF ls_variant-variant IS NOT INITIAL AND
        ls_variant-variant(1) = '/'.
*** If variant is actually set: Display name of variant
          READ TABLE lt_ltdxt ASSIGNING <ltdxt>
          WITH KEY username = space
          variant  = ls_variant-variant.
          IF sy-subrc = 0.
            ls_toolbar-text = <ltdxt>-text.
          ENDIF.
        ENDIF.
        ls_toolbar-function  = 'GLOBAL'.
        ls_toolbar-icon      = icon_bw_gis.
        ls_toolbar-butn_type = '2'.
        APPEND ls_toolbar TO e_object->mt_toolbar.
      ENDIF.

*** Check if there are user specific variants...
      READ TABLE lt_ltdx TRANSPORTING NO FIELDS
      WITH KEY username = sy-uname.
      IF sy-subrc = 0.
*** add menu "USER SPECIFIC" to toolbar
        CLEAR ls_toolbar.
        IF ls_variant-variant IS NOT INITIAL AND
        ls_variant-variant(1) <> '/'.
*** If variant is actually set: Display name of variant
          READ TABLE lt_ltdxt ASSIGNING <ltdxt>
          WITH KEY username = sy-uname
          variant  = ls_variant-variant.
          IF sy-subrc = 0.
            ls_toolbar-text = <ltdxt>-text.
          ENDIF.
        ENDIF.
        ls_toolbar-function  = 'USER'.
        ls_toolbar-icon      = icon_usergroup.
        ls_toolbar-butn_type = '2'.
        APPEND ls_toolbar TO e_object->mt_toolbar.
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "handle_toolbar

  METHOD handle_user_command.
*** Set chosen variant

    FIELD-SYMBOLS <ltdx>  TYPE ltdx.
    FIELD-SYMBOLS <ltdxt> TYPE ltdxt.

*** data
    DATA lv_user_specific TYPE c.
    DATA ls_variant       TYPE disvariant.
    DATA ls_stable        TYPE lvc_s_stbl.
    DATA lt_fieldcat      TYPE  lvc_t_fcat.
    DATA lt_sort          TYPE  lvc_t_sort.
    DATA lt_filter        TYPE  lvc_t_filt.
    DATA ls_layout        TYPE  lvc_s_layo.

*** Variant
    CLEAR ls_variant.
    ls_variant-variant = e_ucomm.
    SHIFT ls_variant-variant LEFT BY 1 PLACES.
    ls_variant-report    = gs_variant-report.
    ls_variant-handle    = gs_variant-handle.
    IF ls_variant-variant(1) <> '/'.
      ls_variant-username  = sy-uname.
      lv_user_specific     = 'X'.
    ENDIF.
    CALL METHOD sender->get_frontend_fieldcatalog
      IMPORTING
        et_fieldcatalog = lt_fieldcat.

    CALL FUNCTION 'LVC_VARIANT_SELECT'
      EXPORTING
        i_dialog            = space
        i_user_specific     = lv_user_specific
        it_default_fieldcat = lt_fieldcat
      IMPORTING
        et_fieldcat         = lt_fieldcat
        et_sort             = lt_sort
        et_filter           = lt_filter
      CHANGING
        cs_variant          = ls_variant
      EXCEPTIONS
        wrong_input         = 1
        fc_not_complete     = 2
        not_found           = 3
        program_error       = 4
        data_missing        = 5
        OTHERS              = 6.
    IF sy-subrc = 0.
      CALL METHOD sender->set_variant( ls_variant ).
      CALL METHOD sender->set_frontend_fieldcatalog( lt_fieldcat ).
      CALL METHOD sender->set_filter_criteria( lt_filter ).
      CALL METHOD sender->set_sort_criteria( lt_sort ).
      ls_stable = 'XX'.
      CALL METHOD sender->refresh_table_display
        EXPORTING
          is_stable      = ls_stable
          i_soft_refresh = space.
    ENDIF.

  ENDMETHOD.                           "handle_user_command

  METHOD handle_menu_button.

    DATA lv_fcode         TYPE ui_func.
    DATA lv_text          TYPE gui_text.
    DATA lv_disable       TYPE c.
    DATA lv_checked       TYPE c.
    DATA lv_username      TYPE syuname.

    FIELD-SYMBOLS <ltdx>  TYPE ltdx.
    FIELD-SYMBOLS <ltdxt> TYPE ltdxt.

    CASE e_ucomm.
      WHEN 'GLOBAL'.
        lv_username = space.
      WHEN 'USER'.
        lv_username = sy-uname.
      WHEN OTHERS.
        EXIT.
    ENDCASE.

*** Get actual variant
    CALL METHOD sender->get_variant
      IMPORTING
        es_variant = ls_variant.

    LOOP AT lt_ltdx ASSIGNING <ltdx> WHERE username = lv_username.
*... get variant text
      READ TABLE lt_ltdxt ASSIGNING <ltdxt>
      WITH KEY variant = <ltdx>-variant.

*... set menu entry
      CLEAR ls_toolbar.
      lv_fcode  = '$'.
      lv_fcode+1 = <ltdx>-variant.

*... set text
      IF <ltdxt> IS ASSIGNED.
        lv_text      = <ltdxt>-text.
      ELSE.
        lv_text      = <ltdx>-variant.
      ENDIF.

*... mark and disable actual variant
      IF ls_variant-variant = <ltdx>-variant.
        lv_disable = 'X'.
        lv_checked = 'X'.
      ELSE.
        lv_disable = space.
        lv_checked = space.
      ENDIF.

*... add menu entry to menu
      CALL METHOD e_object->add_function
        EXPORTING
          disabled = lv_disable
          checked  = lv_checked
          fcode    = lv_fcode
          text     = lv_text.
    ENDLOOP.
  ENDMETHOD.                    "lcl_my_event_handler

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION


INITIALIZATION.


  DATA(docker) = NEW cl_gui_docking_container( ratio = 80 side = cl_gui_docking_container=>dock_at_bottom ).

  DATA(gr_splitter) = NEW cl_gui_splitter_container(
                               parent  = docker
                               rows    = 1
                               columns = 2 ).

  DATA(gr_cont1) = gr_splitter->get_container( row = 1 column = 1 ).
  DATA(gr_cont2) = gr_splitter->get_container( row = 1 column = 2 ).

  DATA(gr_grid1) = NEW lcl_main(
        i_handle    = 'MARA'
        i_container = gr_cont1
        i_title     = 'Materialstamm'
        i_table     = 'GT_MARA' ).

  DATA(gr_grid2) = NEW lcl_main(
        i_handle    = 'MARC'
        i_container = gr_cont2
        i_title     = 'Tabelle MARC'
        i_table     = 'GT_MARC' ).


AT SELECTION-SCREEN.

  SELECT * FROM mara INTO TABLE gt_mara UP TO 30 ROWS
   WHERE matnr IN s_matnr.
  gr_grid1->gr_grid->refresh_table_display( ).

  SELECT * FROM marc INTO TABLE gt_marc UP TO 30 ROWS
   WHERE matnr IN s_matnr.
  gr_grid2->gr_grid->refresh_table_display( ).