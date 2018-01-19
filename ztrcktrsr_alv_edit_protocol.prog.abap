REPORT ztrcktrsr_alv_edit_protocol.



CLASS lcl_demo DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS go
      IMPORTING io_container_grid TYPE REF TO cl_gui_container
                io_container_prot TYPE REF TO cl_gui_container.
    CLASS-METHODS set_message
      IMPORTING i_message TYPE clike.

    TYPES: BEGIN OF ts_data,
             key   TYPE char10,
             value TYPE text40,
           END OF ts_data,
           tt_data TYPE STANDARD TABLE OF ts_data WITH DEFAULT KEY.
  PROTECTED SECTION.
    CLASS-DATA mo_alv  TYPE REF TO cl_gui_alv_grid.
    CLASS-DATA mo_message_container TYPE REF TO cl_gui_container.
    CLASS-DATA mo_prot TYPE REF TO cl_alv_changed_data_protocol.
    CLASS-DATA mt_data TYPE tt_data.
    CLASS-METHODS handle_data_changed
                  FOR EVENT data_changed  OF  cl_gui_alv_grid
      IMPORTING er_data_changed sender.

ENDCLASS.                    "lcl_events DEFINITION

CLASS lcl_demo IMPLEMENTATION.

  METHOD go.

    DATA lt_fcat  TYPE lvc_t_fcat.

    "Make sure alv will be started only once
    CHECK mo_alv IS INITIAL.

    "Remember given container for protocol
    mo_message_container = io_container_prot.


    "create alv-grid
    CREATE OBJECT mo_alv
      EXPORTING
        i_parent       = io_container_grid
        i_applogparent = io_container_prot
        i_appl_events  = 'X'
      EXCEPTIONS
        OTHERS         = 5.

    "Register edit event
    mo_alv->register_edit_event( cl_gui_alv_grid=>mc_evt_enter ).

    "enable edit mode
    mo_alv->set_ready_for_input( 1 ).

    "set handler for changing data
    SET HANDLER handle_data_changed FOR mo_alv.

    "Build simple field catalog
    APPEND LINES OF VALUE lvc_t_fcat(
                  ( fieldname = 'KEY'
                    tabname   = 'XX'
                    reptext   = 'Key'
                    datatype  = 'CHAR'
                    inttype   = 'C'
                    outputlen = 10
                    edit      = abap_true )
                  ( fieldname = 'VALUE'
                    tabname   = 'XX'
                    reptext   = 'Value'
                    datatype  = 'CHAR'
                    inttype   = 'C'
                    outputlen = 40
                    lowercase = abap_true
                    edit      = abap_true )
                            ) TO lt_fcat.

    "Make sure user can enter data
    DO 5 TIMES.
      APPEND INITIAL LINE TO mt_data.
    ENDDO.

    " First Display
    mo_alv->set_table_for_first_display(
      CHANGING
        it_fieldcatalog = lt_fcat
        it_outtab       = mt_data
      EXCEPTIONS
        OTHERS          = 4 ).

    "Set focus on grid so user can immediately enter data in grid
    cl_gui_alv_grid=>set_focus( mo_alv ).


  ENDMETHOD.

  METHOD set_message.

    IF mo_prot IS INITIAL.
      "create extra protocol
      mo_prot = NEW #( i_calling_alv = mo_alv i_container = mo_message_container ).
    ELSE.
      "clear older protocol entries
      mo_prot->refresh_protocol( ).
    ENDIF.
    "add user message
    mo_prot->add_protocol_entry( i_msgty = 'E'
                                 i_msgno = '000'
                                 i_msgid = 'OO'
                                 i_msgv1 = i_message
                                 i_fieldname = space ).
    "and display
    mo_prot->display_protocol( ).

  ENDMETHOD.

  METHOD handle_data_changed.

    DATA: ls_good                TYPE lvc_s_modi.

    IF mo_prot IS BOUND.
      "clear extra protocol
      mo_prot->refresh_protocol( ).
      mo_prot->free( ).
      CLEAR mo_prot.
    ENDIF.

    "Loop at all changed fields
    LOOP AT er_data_changed->mt_good_cells INTO ls_good.
      "Read data table
      READ TABLE mt_data ASSIGNING FIELD-SYMBOL(<data>) INDEX ls_good-row_id.
      IF sy-subrc = 0.
        IF ls_good-fieldname = 'KEY' AND ls_good-value CN 'ABC '.
          "Field does not contain A, B or C - That's bad!
          er_data_changed->add_protocol_entry( i_msgty     = 'E'
                                               i_msgno     = '000'
                                               i_msgid     = 'OO'
                                               i_msgv1     = 'Must be ABC!'
                                               i_fieldname = 'KEY'
                                               i_row_id    = ls_good-row_id ).
        ENDIF.

        "Change field
        ASSIGN COMPONENT ls_good-fieldname OF STRUCTURE <data> TO FIELD-SYMBOL(<field>).
        IF sy-subrc = 0.
          <field> = ls_good-value.
        ENDIF.
      ENDIF.
    ENDLOOP.

    "Set focus
    cl_gui_control=>set_focus( mo_alv ).

  ENDMETHOD.                    "handle_data_changed

ENDCLASS.                    "lcl_events IMPLEMENTATION

PARAMETERS p_test TYPE c LENGTH 10.

INITIALIZATION.
  DATA(docker_prot) = NEW cl_gui_docking_container( ratio = 20 side = cl_gui_docking_container=>dock_at_bottom ).
  DATA(docker_grid) = NEW cl_gui_docking_container( ratio = 60 side = cl_gui_docking_container=>dock_at_bottom ).
  lcl_demo=>go( io_container_grid = docker_grid
                io_container_prot = docker_prot ).


AT SELECTION-SCREEN.

  IF p_test IS NOT INITIAL.
    "Demo for showing how to use the extra protocol outside alv data handling
    lcl_demo=>set_message( 'Not allowed to enter something in parameter!!' ).
  ENDIF.