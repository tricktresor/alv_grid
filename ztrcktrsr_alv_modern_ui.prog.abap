REPORT.

"Dummy parameter do display docker
PARAMETERS p.

CLASS main DEFINITION.
  PUBLIC SECTION.
    INCLUDE <cl_alv_control>.
    TYPES: BEGIN OF ty_param,
             name   TYPE string,
             text   TYPE string,
             status TYPE boolean,
           END OF ty_param,
           ty_params TYPE STANDARD TABLE OF ty_param WITH NON-UNIQUE DEFAULT KEY.

    METHODS init_grid IMPORTING parent TYPE REF TO cl_gui_container.
    METHODS add_parameter
      IMPORTING name   TYPE clike
                text   TYPE clike OPTIONAL
                status TYPE boolean OPTIONAL.
    METHODS get_params
      RETURNING VALUE(parameters) TYPE ty_params.

    "Settings color
    CONSTANTS color_on TYPE i VALUE col_positive.
    CONSTANTS color_off TYPE i VALUE col_negative.

    "Settings icons
* CONSTANTS status_icon_on TYPE icon_text VALUE icon_businav_szenario.
* CONSTANTS status_icon_off TYPE icon_text VALUE icon_businav_szenario.

* CONSTANTS status_icon_on TYPE icon_text VALUE icon_led_green.
* CONSTANTS status_icon_off TYPE icon_text VALUE icon_led_red.

* CONSTANTS status_icon_on TYPE icon_text VALUE ICON_oo_class.
* CONSTANTS status_icon_off TYPE icon_text VALUE ICON_oo_class.

* CONSTANTS status_icon_on TYPE icon_text VALUE ICON_oo_object.
* CONSTANTS status_icon_off TYPE icon_text VALUE ICON_oo_class.

    CONSTANTS status_icon_on  TYPE icon_text VALUE icon_ps_network_activity.
    CONSTANTS status_icon_off TYPE icon_text VALUE icon_ps_network_activity.

* CONSTANTS status_icon_on TYPE icon_text VALUE ICON_add_row.
* CONSTANTS status_icon_off TYPE icon_text VALUE ICON_remove_row.

* CONSTANTS status_icon_on TYPE icon_text VALUE icon_org_unit.
* CONSTANTS status_icon_off TYPE icon_text VALUE icon_org_unit.

    TYPES: BEGIN OF ty_ui_param,
             name       TYPE string,
             text       TYPE string,
             status_on  TYPE icon_text,
             status_off TYPE icon_text,
             t_color    TYPE lvc_t_scol,
             t_style    TYPE lvc_t_styl,
           END OF ty_ui_param,
           ty_ui_params TYPE STANDARD TABLE OF ty_ui_param.

  PROTECTED SECTION.
    DATA grid TYPE REF TO cl_gui_alv_grid.
    DATA params TYPE ty_params.
    DATA ui_params TYPE ty_ui_params.

    METHODS set_ui.
    METHODS set_color
      IMPORTING status       TYPE boolean
      RETURNING VALUE(color) TYPE lvc_t_scol.
    METHODS handle_click FOR EVENT hotspot_click OF cl_gui_alv_grid
      IMPORTING e_row_id.

ENDCLASS.

CLASS main IMPLEMENTATION.

  METHOD init_grid.

    "Local data
    DATA fieldcat TYPE lvc_t_fcat.
    DATA field TYPE lvc_s_fcat.
    DATA layout TYPE lvc_s_layo.

    "Transform parameter data to display UI
    set_ui( ).

    "Create grid in given container
    CREATE OBJECT grid
      EXPORTING
        i_parent = parent.

    "Set fields
    CLEAR field.
    field-fieldname  = 'NAME'.
    field-outputlen  = 20.
    field-colddictxt = 'Parameter'.
    field-style      = alv_style_font_bold.
    APPEND field TO fieldcat.

    CLEAR field.
    field-fieldname  = 'TEXT'.
    field-outputlen  = 40.
    field-colddictxt = 'Description'.
    APPEND field TO fieldcat.

    CLEAR field.
    field-fieldname  = 'STATUS_ON'.
    field-outputlen  = 4.
    field-colddictxt = 'On'.
    field-hotspot    = abap_true.
    field-icon       = abap_true.
    field-fix_column = abap_true.
    APPEND field TO fieldcat.

    CLEAR field.
    field-fieldname  = 'STATUS_OFF'.
    field-outputlen  = 4.
    field-colddictxt = 'Off'.
    field-hotspot    = abap_true.
    field-icon       = abap_true.
    field-fix_column = abap_true.
    APPEND field TO fieldcat.

    "Layout
    layout-stylefname = 'T_STYLE'.
    layout-ctab_fname = 'T_COLOR'.
    layout-no_toolbar = abap_true.
    layout-no_headers = abap_true.

    "Display Grid
    grid->set_table_for_first_display(
      EXPORTING
        is_layout = layout
      CHANGING
        it_outtab = ui_params
        it_fieldcatalog = fieldcat
      EXCEPTIONS
        OTHERS = 4 ).

    "Set handler
    SET HANDLER handle_click FOR grid.

  ENDMETHOD.

  METHOD get_params.
    "return current settings
    parameters = params.
  ENDMETHOD.

  METHOD add_parameter.
    "add parameter to parameter table
    APPEND VALUE #( name   = name
                    text   = text
                    status = status ) TO params.
  ENDMETHOD.

  METHOD set_color.

    CASE status.
      WHEN abap_true.
        "set color for switched on
        color = VALUE #( ( fname = 'STATUS_OFF' color-col = color_on )
                         ( fname = 'STATUS_ON' color-col = color_on ) ).
      WHEN abap_false.
        "set color for switched off
        color = VALUE #( ( fname = 'STATUS_OFF' color-col = color_off )
                         ( fname = 'STATUS_ON' color-col = color_off ) ).
    ENDCASE.

  ENDMETHOD.


  METHOD set_ui.

    "for each parameter
    LOOP AT params INTO DATA(param).

      "check if ui entry exists
      READ TABLE ui_params ASSIGNING FIELD-SYMBOL(<ui_param>) WITH KEY name = param-name.
      IF sy-subrc > 0.

        CASE param-status.
          WHEN abap_true.
            "set parameter switched on
            APPEND VALUE #( name = param-name
                            text = param-text
                            status_on = status_icon_on
                            status_off = 'ON'
                            t_color = set_color( abap_true )
                            t_style = VALUE #( ( fieldname = 'STATUS_ON'  style2 = alv_style2_no_border_right )
                                               ( fieldname = 'STATUS_OFF' style2 = alv_style2_no_border_left ) )
                          ) TO ui_params ASSIGNING <ui_param>.

          WHEN abap_false.
            "set parameter switched off
            APPEND VALUE #( name = param-name
                            text = param-text
                            status_on = 'OFF'
                            status_off = status_icon_off
                            t_color = set_color( abap_false )
                            t_style = VALUE #( ( fieldname = 'STATUS_ON'  style2 = alv_style2_no_border_right )
                                               ( fieldname = 'STATUS_OFF' style2 = alv_style2_no_border_left ) )
                          ) TO ui_params ASSIGNING <ui_param>.

          WHEN abap_undefined.
            "Set parameter not yet defined
            APPEND VALUE #( name = param-name
                            text = param-text
                            status_on = space
                            status_off = space
                            t_style = VALUE #( ( fieldname = 'STATUS_ON' style2 = alv_style2_no_border_right )
                                               ( fieldname = 'STATUS_OFF' style2 = alv_style2_no_border_left ) )
                          ) TO ui_params ASSIGNING <ui_param>.

        ENDCASE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD handle_click.
    "read parameter entries
    READ TABLE ui_params ASSIGNING FIELD-SYMBOL(<ui_param>) INDEX e_row_id-index.
    READ TABLE params ASSIGNING FIELD-SYMBOL(<param>) WITH KEY name = <ui_param>-name.

    IF <ui_param>-status_on = status_icon_on
    OR <ui_param>-status_on = status_icon_off.
      "set switch to OFF
      <ui_param>-status_on  = 'OFF'.
      <ui_param>-status_off = status_icon_off.
      <ui_param>-t_color    = set_color( abap_false ).
      <param>-status = abap_false.
    ELSE.
      "Set switch to ON
      <ui_param>-status_on  = status_icon_on.
      <ui_param>-status_off = 'ON'.
      <ui_param>-t_color    = set_color( abap_true ).
      <param>-status = abap_true.
    ENDIF.
    "Make changes visible
    grid->refresh_table_display( i_soft_refresh = abap_true ).
  ENDMETHOD.

ENDCLASS.

INITIALIZATION.

  DATA(main) = NEW main( ).

  main->add_parameter( name = 'DISPLAY_TECH_DESCR' text = 'Display technical description' status = abap_true ).
  main->add_parameter( name = 'DISPLAY_VALUES'     text = 'Display values'                status = abap_false ).
  main->add_parameter( name = 'AUTOSAVE'           text = 'Autosave'                      status = abap_true ).
  main->add_parameter( name = 'INST_CALC'          text = 'Instant calculation'           status = abap_undefined ).

  main->init_grid( NEW cl_gui_docking_container( ratio = 60 side = cl_gui_docking_container=>dock_at_bottom ) ).

AT SELECTION-SCREEN.
  "Enter on selection screen displays current parameters
  DATA(params) = main->get_params( ).
  cl_demo_output=>display_data( params ).