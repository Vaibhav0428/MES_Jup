CLASS zcl_http_product DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .
    CLASS-METHODS :
      read_posts
        IMPORTING
          VALUE(request) TYPE REF TO if_web_http_request
        RETURNING
          VALUE(rv_json) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_http_product IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>get ).
*        response->set_text( read_posts( request ) ).
        DATA(req) = request->get_form_fields( ).

        response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
        response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).

        DATA(lv_production) = VALUE #( req[ name = 'productionorder' ]-value OPTIONAL ).
        DATA(lv_plant) = VALUE #( req[ name = 'planningplant' ]-value OPTIONAL ).
        DATA(lv_date) = VALUE #( req[ name = 'orderplannedstartdate' ]-value OPTIONAL ).


        TYPES: BEGIN OF ty_hdr,
                 productionorder         TYPE i_productionordertp-productionorder,
                 planningplant           TYPE i_productionordertp-planningplant,
                 orderplannedstartdate   TYPE i_productionordertp-orderplannedstartdate,
                 orderactualreleasedate  TYPE i_productionordertp-orderactualreleasedate,
                 product                 TYPE i_productionordertp-product,
                 productname             TYPE i_producttext-productname,
                 productionunit          TYPE i_productionordertp-productionunit,
                 batch                   TYPE i_productionordertp-batch,
                 billofmaterial          TYPE i_productionordertp-billofmaterial,
                 storagelocation         TYPE i_productionordertp-storagelocation,
                 orderplannedtotalqty    TYPE i_productionordertp-orderplannedtotalqty,
                 actualdeliveredquantity TYPE i_productionordertp-actualdeliveredquantity,
                 productionversion       TYPE i_productionordertp-productionversion,
                 workcenter              TYPE i_productionorderoperationtp-workcenter,
                 status                  TYPE   string,
               END OF ty_hdr.

        TYPES: BEGIN OF ty_items,
                 reservation              TYPE i_productionorderopcomponenttp-reservation,
                 reservationitem          TYPE i_productionorderopcomponenttp-reservationitem,
                 productionorder          TYPE i_productionorderopcomponenttp-productionorder,
                 material                 TYPE i_productionorderopcomponenttp-material,
                 productname              TYPE i_producttext-productname,
                 producttype              TYPE i_product-producttype,
                 requiredquantity         TYPE i_productionorderopcomponenttp-requiredquantity,
                 entryunit                TYPE i_productionorderopcomponenttp-entryunit,
                 goodsmovementtype        TYPE i_productionorderopcomponenttp-goodsmovementtype,
                 billofmaterialitemnumber TYPE i_productionorderopcomponenttp-billofmaterialitemnumber,


               END OF ty_items.

        DATA it_items TYPE TABLE OF ty_items.
        DATA it_items2 TYPE TABLE OF ty_items.
        DATA wa_items TYPE ty_items.

        TYPES: BEGIN OF ty_final,
                 wa_hdr   TYPE ty_hdr,
                 it_items LIKE it_items,
               END OF ty_final.

        DATA it_final TYPE TABLE OF ty_final.
        DATA wa_final TYPE ty_final.

        SELECT FROM i_productionordertp AS a
        LEFT JOIN i_producttext AS b ON a~product = b~product AND b~language = 'E'
        INNER JOIN i_mfgorderwithstatus AS c ON a~productionorder = c~manufacturingorder AND c~orderisreleased IS NOT INITIAL
        FIELDS a~productionorder, a~planningplant, a~orderscheduledstartdate, a~product, a~batch,
        a~billofmaterial, a~orderactualreleasedate, a~storagelocation, a~orderplannedtotalqty,
        a~actualdeliveredquantity, a~productionversion, a~productionunit, b~productname ,c~OrderIsReleased , c~OrderIsDelivered ,c~OrderIsPartiallyDelivered
        WHERE a~orderconfirmedyieldqty IS INITIAL
*         a~technicalcompletiondate IS INITIAL
*          AND
*        a~iscompletelydelivered IS INITIAL AND
        AND a~planningplant = @lv_plant AND a~orderscheduledstartdate = @lv_date
        INTO TABLE @DATA(it_table) PRIVILEGED ACCESS.

        LOOP AT it_table INTO DATA(wa).

          SELECT SINGLE FROM i_productionorderoperationtp AS a
          FIELDS a~workcenter
          WHERE a~productionorder = @wa-productionorder
          INTO @wa_final-wa_hdr-workcenter PRIVILEGED ACCESS.

          wa_final-wa_hdr-productionorder = wa-productionorder.
          wa_final-wa_hdr-product = wa-product.
          IF wa-productionunit = 'ST'.
            wa_final-wa_hdr-productionunit = 'PC'.
          ELSE.
            wa_final-wa_hdr-productionunit = wa-productionunit.
          ENDIF.
          wa_final-wa_hdr-productname       = wa-productname.
          wa_final-wa_hdr-planningplant     = wa-planningplant.
          wa_final-wa_hdr-batch             = wa-batch.
          wa_final-wa_hdr-billofmaterial    = wa-billofmaterial.
          wa_final-wa_hdr-storagelocation   = wa-storagelocation.
          wa_final-wa_hdr-productionversion = wa-productionversion.
          wa_final-wa_hdr-orderplannedtotalqty    = wa-orderplannedtotalqty.
          wa_final-wa_hdr-actualdeliveredquantity = wa-actualdeliveredquantity.
          wa_final-wa_hdr-orderplannedstartdate   = wa-orderscheduledstartdate.
          wa_final-wa_hdr-orderactualreleasedate  = wa-orderactualreleasedate.
          IF wa-orderisdelivered  IS NOT INITIAL.
            wa_final-wa_hdr-status  = 'COMPLETE DELIVERED'.
            ELSEIF wa-OrderIsPartiallyDelivered IS NOT INITIAL.
            wa_final-wa_hdr-status = 'PARTIAL DELIVERED'.
          ELSEIF   wa-orderisreleased  IS NOT INITIAL.
            wa_final-wa_hdr-status  = 'RELEASED'.

          ENDIF.


          SELECT FROM i_productionorderopcomponenttp AS b
          LEFT JOIN i_producttext AS c ON b~material = c~product AND c~language = 'E'
          LEFT JOIN i_product AS d ON b~material = d~product
          FIELDS b~reservation, b~reservationitem, b~productionorder, b~material, b~requiredquantity, b~entryunit,
          c~productname, b~goodsmovementtype, b~billofmaterialitemnumber, d~producttype
          WHERE b~productionorder = @wa-productionorder
          INTO TABLE @DATA(it_item) PRIVILEGED ACCESS.

          LOOP AT it_item INTO DATA(wa_item).
            wa_items-reservation      = wa_item-reservation.
            wa_items-reservationitem  = wa_item-reservationitem.
            wa_items-productionorder  = wa_item-productionorder.
            wa_items-material         = wa_item-material.
            wa_items-productname      = wa_item-productname.
            wa_items-requiredquantity = wa_item-requiredquantity.
            wa_items-entryunit        = wa_item-entryunit.
            wa_items-producttype      = wa_item-producttype.
            wa_items-billofmaterialitemnumber = wa_item-billofmaterialitemnumber.
            wa_items-goodsmovementtype        = wa_item-goodsmovementtype.
            IF wa_items-entryunit = 'ST'.
              wa_items-entryunit = 'PC'.
            ENDIF.
            APPEND wa_items TO it_items2.
            CLEAR wa_items.
          ENDLOOP.

          wa_final-it_items = it_items2.

          APPEND wa_final TO it_final.
          CLEAR: wa_final, it_items2.
        ENDLOOP.

        /ui2/cl_json=>serialize(
          EXPORTING
            data        = it_final
            pretty_name = /ui2/cl_json=>pretty_mode-camel_case
            format_output = abap_true
          RECEIVING
            r_json      = DATA(lv_json2)
        ).

        response->set_text( lv_json2 ).
    ENDCASE.
  ENDMETHOD.


  METHOD read_posts.

    DATA: lv_product TYPE string,
          lv_plant   TYPE string,
          lv_date    TYPE string.

    lv_product = request->get_form_field( 'productionorder' ).
    lv_plant      = request->get_form_field( 'planningplant' ).
    lv_date       = request->get_form_field( 'orderplannedstartdate' ).

  ENDMETHOD.
ENDCLASS.
