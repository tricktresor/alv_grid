*&---------------------------------------------------------------------*
*& Report ZTRCKTRSR_ALV_AUTOSUMME2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztrcktrsr_alv_autosumme2.

PARAMETERS p_total TYPE p DECIMALS 2.

CLASS lcl_autosumme DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_alv_rm_grid_friend .
    CLASS-METHODS register IMPORTING ir_grid TYPE REF TO cl_gui_alv_grid.
  PROTECTED SECTION.
    CLASS-METHODS handle_delayed_selection
      FOR EVENT delayed_changed_sel_callback
                  OF cl_gui_alv_grid
      IMPORTING sender.
ENDCLASS.

CLASS lcl_autosumme IMPLEMENTATION.
  METHOD register.
    "Set handler
    SET HANDLER handle_delayed_selection FOR ir_grid.
    "set delayed selection time
    ir_grid->set_delay_change_selection( time = 100 ). " Time in Milliseconds
    "register event for delayed selection
    ir_grid->register_delayed_event( ir_grid->mc_evt_delayed_change_select ).

  ENDMETHOD.

  METHOD handle_delayed_selection.

    "Local data
    DATA lt_cells TYPE lvc_t_cell.
    DATA ls_cell LIKE LINE OF lt_cells.
    DATA lv_total TYPE p DECIMALS 2.
    DATA lv_val_type TYPE c.
    DATA lv_index TYPE n LENGTH 2.
    DATA lv_tablename TYPE string.
    DATA lt_grouplevels TYPE lvc_t_grpl.
    DATA ls_grouplevel LIKE LINE OF lt_grouplevels.

    FIELD-SYMBOLS <ref_data> TYPE REF TO data.
    FIELD-SYMBOLS <table> TYPE table.
    FIELD-SYMBOLS <warea> TYPE any.
    FIELD-SYMBOLS <val> TYPE any.

    "data references to sub totals tables
    DATA ld_ct01 TYPE REF TO data.
    DATA ld_ct02 TYPE REF TO data.
    DATA ld_ct03 TYPE REF TO data.
    DATA ld_ct04 TYPE REF TO data.
    DATA ld_ct05 TYPE REF TO data.
    DATA ld_ct06 TYPE REF TO data.
    DATA ld_ct07 TYPE REF TO data.
    DATA ld_ct08 TYPE REF TO data.
    DATA ld_ct09 TYPE REF TO data.

    "get selected cells (selection via CTRL + Y)
    sender->get_selected_cells( IMPORTING et_cell = lt_cells ).

    "If there is only one cell selected, we do not need to sum that...
    CHECK lines( lt_cells ) > 1.

    "Read all cell values
    LOOP AT lt_cells INTO ls_cell.

      "in case of rowtype (normal cell, total or subtotal) assign correct data table
      CASE ls_cell-row_id-rowtype(1).
          "Total sum of all
        WHEN 'T'.
          ASSIGN sender->mt_ct00 TO <ref_data>.
          ls_cell-row_id-index = 1.
          "subtotals
        WHEN 'S'.
          sender->get_subtotals( IMPORTING
          ep_collect01 = ld_ct01
          ep_collect02 = ld_ct02
          ep_collect03 = ld_ct03
          ep_collect04 = ld_ct04
          ep_collect05 = ld_ct05
          ep_collect06 = ld_ct06
          ep_collect07 = ld_ct07
          ep_collect08 = ld_ct08
          ep_collect09 = ld_ct09
          et_grouplevels = lt_grouplevels ).

          lv_index = ls_cell-row_id-rowtype+4(2).
          lv_tablename = 'LD_CT' && lv_index.
          ASSIGN (lv_tablename) TO <ref_data>.

          READ TABLE lt_grouplevels INTO ls_grouplevel INDEX ls_cell-row_id-index.
          IF sy-subrc = 0.
            ls_cell-row_id-index = ls_grouplevel-cindx_from.
          ENDIF.
          "Normal cell value
        WHEN space.
          ASSIGN sender->mt_outtab TO <ref_data>.
      ENDCASE.

      "assign specified data table
      ASSIGN <ref_data>->* TO <table>.

      "Only read table line when index changes
      AT NEW row_id.
        READ TABLE <table> ASSIGNING <warea> INDEX ls_cell-row_id-index.
      ENDAT.
      "Assign selected fieldname of workarea
      ASSIGN COMPONENT ls_cell-col_id OF STRUCTURE <warea> TO <val>.
      IF sy-subrc = 0.
        "check correct type of field: Only numeric fields will be taken
        DESCRIBE FIELD <val> TYPE lv_val_type.
        CASE lv_val_type.
          WHEN 'P' "Packed
          OR 'N' "Numchar
          OR 'b' "Integer
          OR 'a' "decfloat
          OR 'e' "decfloat
          OR 'F'. "Float?
            "add cell value to total
            ADD <val> TO lv_total.
        ENDCASE.
      ENDIF.
    ENDLOOP.

    IF lv_total IS NOT INITIAL.
      "There were numeric fields selected and therefor we have a total to show:
      MESSAGE s000(oo) WITH 'TOTAL:' space lv_total.
      p_total = lv_total.
    ENDIF.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_main DEFINITION.

  PUBLIC SECTION.
* INTERFACES if_alv_rm_grid_friend .
    METHODS start.
  PROTECTED SECTION.
    DATA mr_grid TYPE REF TO cl_gui_alv_grid.
    DATA mt_data TYPE STANDARD TABLE OF spfli.
    DATA mv_data_table TYPE tabname VALUE 'SPFLI'.
    DATA mr_dock TYPE REF TO cl_gui_docking_container.
    METHODS create_docker.
    METHODS create_grid.
    METHODS select_data.
    METHODS register_autosumme.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.

  METHOD start.
    select_data( ).
    create_docker( ).
    create_grid( ).
    register_autosumme( ).
  ENDMETHOD.

  METHOD create_docker.
    "Create Docking container at bottom
    CREATE OBJECT mr_dock
      EXPORTING
        side                    = cl_gui_docking_container=>dock_at_bottom
        ratio                   = 90
        no_autodef_progid_dynnr = abap_false.

  ENDMETHOD.

  METHOD create_grid.
    "Create ALV-Grid
    CREATE OBJECT mr_grid
      EXPORTING
        i_appl_events = abap_true
        i_parent      = mr_dock.

    "and display data
    mr_grid->set_table_for_first_display(
    EXPORTING
    i_structure_name = mv_data_table
    CHANGING
    it_outtab = mt_data ).

    "Set focus on grid so user can directly scroll and select cells via CTRL+Y
    cl_gui_container=>set_focus( mr_grid ).

  ENDMETHOD.

  METHOD select_data.
    "Select data
    SELECT * FROM (mv_data_table) INTO TABLE mt_data UP TO 100 ROWS.
  ENDMETHOD.

  METHOD register_autosumme.
    lcl_autosumme=>register( mr_grid ).
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  DATA(gr_main) = NEW lcl_main( ).
  gr_main->start( ).