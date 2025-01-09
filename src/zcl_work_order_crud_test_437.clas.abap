CLASS zcl_work_order_crud_test_437 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    METHODS fill_tables_for_test.
    METHODS test_create_work_order IMPORTING iv_customer_id       TYPE zde_customer_id_437
                                             iv_technician_id     TYPE zde_technician_id_437
                                             iv_priority          TYPE zde_priority_437
                                             iv_description       TYPE zdt_work_ord_437-description
                                   RETURNING VALUE(rv_sql_result) TYPE string.

    METHODS test_read_work_order IMPORTING iv_work_order_id     TYPE zde_work_order_id_437
                                 RETURNING VALUE(rs_work_order) TYPE zdt_work_ord_437.

    METHODS test_read_work_orders RETURNING VALUE(rt_work_order) TYPE ztt_work_ord_437.

    METHODS test_update_work_order IMPORTING iv_work_order_id     TYPE zde_work_order_id_437
                                             iv_status            TYPE zde_status_437
                                   RETURNING VALUE(rv_sql_result) TYPE string.

    METHODS test_delete_work_order IMPORTING iv_work_order_id     TYPE zde_work_order_id_437
                                   RETURNING VALUE(rv_sql_result) TYPE string.

    CONSTANTS: BEGIN OF mc_valid_status,
                 Pending   TYPE zde_status_437 VALUE 'PE',
                 Completed TYPE zde_status_437 VALUE 'CO',
               END OF mc_valid_status,

               BEGIN OF mc_valid_priority,
                 High TYPE zde_priority_437 VALUE 'A',
                 Low  TYPE zde_priority_437 VALUE 'B',
               END OF mc_valid_priority.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.

CLASS zcl_work_order_crud_test_437 IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
**********Prepare and Fill Customer and Technician Records*******************************
* Set records in important tables
    me->fill_tables_for_test( ).

********************Create new work order**************************************************
* Test create invalid work order
*    out->write( me->test_create_work_order(
*                  iv_customer_id   = '1000000006'
*                  iv_technician_id = '20000006'
*                  iv_priority      = 'z'
*                  iv_description   = 'Test create invalid work order'
*                ) ).
*
*    out->write( cl_abap_char_utilities=>newline ).

* Test create valid work order
    out->write( me->test_create_work_order( iv_customer_id   = '1000000005'
                                            iv_technician_id = '20000005'
                                            iv_priority      = mc_valid_priority-low
                                            iv_description   = 'Test create valid work order' ) ).

    out->write( cl_abap_char_utilities=>newline ).

* Test create valid work order
    out->write( me->test_create_work_order( iv_customer_id   = '1000000004'
                                            iv_technician_id = '20000004'
                                            iv_priority      = mc_valid_priority-high
                                            iv_description   = 'Test create valid work order' ) ).

    out->write( cl_abap_char_utilities=>newline ).

*******************Read work order**************************************************
* Test Get Work Order
    out->write( data = me->test_read_work_order( iv_work_order_id = 2 )
                name = 'Work Order consulted:' ).

    out->write( cl_abap_char_utilities=>newline ).

********************Update work order**************************************************
* Test Update work order
    out->write( me->test_update_work_order( iv_work_order_id = 2
                                            iv_status        = mc_valid_status-completed ) ).
*
    out->write( cl_abap_char_utilities=>newline ).


* Get Work Order after of update operation
    out->write( data = me->test_read_work_orders( )
                name = 'Work Orders after of update operation:' ).
*
    out->write( cl_abap_char_utilities=>newline ).
*
********************Delete work order**************************************************
* Get Work Orders before of delete operation
    out->write( data = me->test_read_work_orders( )
                name = 'Work Orders before of delete operation:' ).

    out->write( cl_abap_char_utilities=>newline ).

* Test Delete work order
    out->write( me->test_delete_work_order( iv_work_order_id = 2 ) ).
    out->write( me->test_delete_work_order( iv_work_order_id = 1 ) ).

    out->write( cl_abap_char_utilities=>newline ).

* Get Work Orders after of delete operation
    out->write( data = me->test_read_work_orders( )
                name = 'Work Orders after of delete operation:' ).

    out->write( cl_abap_char_utilities=>newline ).
  ENDMETHOD.

  METHOD fill_tables_for_test.
    DATA lv_test TYPE i.

* Prepare tables for new Test
    DELETE FROM zdt_customer_437.
    DELETE FROM zdt_technici_437.
    DELETE FROM zdt_work_ord_437.
    DELETE FROM zdt_work_o_h_437.

* Fill Customer Data
    INSERT zdt_customer_437 FROM TABLE @( VALUE #( FOR i = 1 UNTIL i > 5
                                                   ( customer_id = |100000000{ i }|
                                                     name = |Customer{ i }|
                                                     address = |City{ i }|
                                                     phone = |100000{ i * 10 }| )
                                                      ) ).

* Fill Technician Data
    INSERT zdt_technici_437 FROM TABLE @( VALUE #( FOR i = 1 UNTIL i > 5
                                                   ( technician_id = |2000000{ i }|
                                                     name = |Technician{ i }|
                                                     specialty = |Specialty{ i }| )
                                                      ) ).
  ENDMETHOD.

  METHOD test_create_work_order.
** Check authorization for Create records
*    AUTHORITY-CHECK OBJECT 'ZAO_WO_437'
*    ID 'ACTVT' FIELD '01'. " Create
*    IF sy-subrc EQ 0.
    DATA(lr_work_ord_crud) = NEW zcl_work_ord_crud_handler_437( ).

    lr_work_ord_crud->create_work_order(
      EXPORTING
        iv_customer_id   = iv_customer_id
        iv_technician_id = iv_technician_id
        iv_priority      = iv_priority
        iv_description   = iv_description
      RECEIVING
        rv_sql_result    = rv_sql_result
    ).
*    ENDIF.
  ENDMETHOD.

  METHOD test_read_work_order.
** Check authorization for Display records
*    AUTHORITY-CHECK OBJECT 'ZAO_WO_437'
*    ID 'ACTVT' FIELD '03'. " Display
*    if sy-subrc eq 0.
* Read Work Order
    DATA(lr_work_ord_crud) = NEW zcl_work_ord_crud_handler_437( ).

    lr_work_ord_crud->read_work_order(
      EXPORTING
        iv_work_order_id = iv_work_order_id
      RECEIVING
        rs_work_order    = rs_work_order ).
*    endif.
  ENDMETHOD.

  METHOD test_read_work_orders.
** Check authorization for Display records
*    AUTHORITY-CHECK OBJECT 'ZAO_WO_437'
*    ID 'ACTVT' FIELD '03'. " Display
*    if sy-subrc eq 0.
* Read Work Order
    DATA(lr_work_ord_crud) = NEW zcl_work_ord_crud_handler_437( ).

    lr_work_ord_crud->read_work_orders(
      RECEIVING
        rt_work_order    = rt_work_order ).
*    endif.
  ENDMETHOD.

  METHOD test_update_work_order.
** Check authorization for Change records
*    AUTHORITY-CHECK OBJECT 'ZAO_WO_437'
*    ID 'ACTVT' FIELD '02'. " Change
*    if sy-subrc eq 0.
    DATA(lr_work_ord_crud) = NEW zcl_work_ord_crud_handler_437( ).

    lr_work_ord_crud->update_work_order(
      EXPORTING
        iv_work_order_id = iv_work_order_id
        iv_status        = iv_status
      RECEIVING
        rv_sql_result    = rv_sql_result ).
*    endif.
  ENDMETHOD.

  METHOD test_delete_work_order.
** Check authorization for Delete records
*    AUTHORITY-CHECK OBJECT 'ZAO_WO_437'
*    ID 'ACTVT' FIELD '06'. " Delete
*    if sy-subrc eq 0.
    DATA(lr_work_ord_crud) = NEW zcl_work_ord_crud_handler_437( ).

    lr_work_ord_crud->delete_work_order(
      EXPORTING
        iv_work_order_id = iv_work_order_id
      RECEIVING
        rv_sql_result    = rv_sql_result ).
*    endif.
  ENDMETHOD.
ENDCLASS.
