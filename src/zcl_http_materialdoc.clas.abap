CLASS zcl_http_materialdoc DEFINITION
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



CLASS zcl_http_materialdoc IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.

    CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>get ).
*        response->set_text( read_posts( request ) ).
        DATA(req) = request->get_form_fields( ).

        response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
        response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).

        DATA(lv_materialdocument) = VALUE #( req[ name = 'materialdocument' ]-value OPTIONAL ).
        DATA(lv_materialdocumentyear) = VALUE #( req[ name = 'materialdocumentyear' ]-value OPTIONAL ).

        TYPES: BEGIN OF ty_material,
                 materialdocument     TYPE i_materialdocumentheader_2-materialdocument,
                 materialdocumentyear TYPE i_materialdocumentheader_2-materialdocumentyear,
                 supplier             TYPE i_materialdocumentitem_2-supplier,
                 suppliername         TYPE i_supplier-suppliername,
                 addresssearchterm1   TYPE i_supplier-addresssearchterm1,
                 material             TYPE i_materialdocumentitem_2-material,
                 batch                TYPE i_materialdocumentitem_2-batch,
                 quantityinentryunit  TYPE i_materialdocumentitem_2-quantityinentryunit,
                 entryunit            TYPE i_materialdocumentitem_2-entryunit,
                 productdescription   TYPE i_productdescription-productdescription,
                 reversedocument      TYPE i_materialdocumentheader_2-materialdocument,
                 reversedocumentyear  TYPE i_materialdocumentheader_2-materialdocumentyear,

               END OF ty_material.

        SELECT FROM i_materialdocumentheader_2 AS a
        LEFT JOIN i_materialdocumentitem_2 AS b ON a~materialdocument = b~materialdocument AND b~debitcreditcode = 'S'
        LEFT JOIN i_supplier AS c ON b~supplier = c~supplier
        LEFT JOIN i_productdescription  AS d ON b~material = d~product
        LEFT JOIN i_materialdocumentitem_2 AS f ON f~reversedmaterialdocument = b~materialdocument AND f~reversedmaterialdocumentyear = b~materialdocumentyear
                                                AND f~reversedmaterialdocumentitem = b~materialdocumentitem
        FIELDS a~materialdocument, a~materialdocumentyear, b~supplier, c~suppliername, c~addresssearchterm1, b~material, b~batch, b~quantityinentryunit,b~entryunit, d~productdescription,
               b~goodsmovementtype,b~plant,b~issuingorreceivingstorageloc AS from_location,b~storagelocation AS to_location,b~issuingorreceivingplant,
               f~materialdocument AS reversedmaterialdocument , f~materialdocumentyear AS reversedmaterialdocumentyear,
        CASE
          WHEN b~entryunit = 'ST' THEN 'PC'
          ELSE b~entryunit
        END AS entry_unit,
        a~documentdate,
        a~postingdate
        WHERE a~materialdocument = @lv_materialdocument
        AND a~materialdocumentyear = @lv_materialdocumentyear
        INTO TABLE @DATA(material) PRIVILEGED ACCESS.

*        DATA(rv_json) = xco_cp_json=>data->from_abap( material )->to_string( ).
        /ui2/cl_json=>serialize(
            EXPORTING
              data        = material " Internal table to serialize
              pretty_name = /ui2/cl_json=>pretty_mode-camel_case " Optional: for camelCase field names
            RECEIVING
              r_json      = DATA(lv_json) " Resulting JSON string
          ).


        response->set_text( lv_json ).

      WHEN CONV string( if_web_http_client=>post ).

        response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
        response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).


        DATA(req2) = request->get_form_fields( ).
        DATA(lv_body) = request->get_text( ).

        TYPES: BEGIN OF ty_body,
                 plant                        TYPE i_materialdocumentitem_2-plant,
                 material                     TYPE i_materialdocumentitem_2-material,
                 storagelocation              TYPE i_materialdocumentitem_2-storagelocation,
                 issuingorreceivingstorageloc TYPE i_materialdocumentitem_2-issuingorreceivingstorageloc,
                 quantityinentryunit          TYPE i_materialdocumentitem_2-quantityinentryunit,
                 batch                        TYPE i_materialdocumentitem_2-batch,
                 issgorrcvgbatch              TYPE i_materialdocumentitem_2-issgorrcvgbatch,
                 entryunit                    TYPE i_materialdocumentitem_2-entryunit,
               END OF ty_body.

        DATA wa_body TYPE ty_body.

        /ui2/cl_json=>deserialize(
      EXPORTING
        json = lv_body
      CHANGING
        data = wa_body
    ).

        IF wa_body IS NOT INITIAL.

          MODIFY ENTITIES OF i_materialdocumenttp
          ENTITY materialdocument
          CREATE FROM VALUE #( ( %cid = 'CID_001'
          goodsmovementcode = '04'
          postingdate = cl_abap_context_info=>get_system_date( )
          documentdate = cl_abap_context_info=>get_system_date( )
          %control-goodsmovementcode = cl_abap_behv=>flag_changed
          %control-postingdate = cl_abap_behv=>flag_changed
          %control-documentdate = cl_abap_behv=>flag_changed
          ) )
          ENTITY materialdocument                           "4900001414
          CREATE BY \_materialdocumentitem
          FROM VALUE #( (
          %cid_ref = 'CID_001'
          %target = VALUE #( ( %cid = 'CID_ITM_001'
          plant = wa_body-plant
          material = wa_body-material
          goodsmovementtype = '311'
          storagelocation = wa_body-storagelocation
          issuingorreceivingstorageloc = wa_body-issuingorreceivingstorageloc
          quantityinentryunit = wa_body-quantityinentryunit
          batch = wa_body-batch
          issgorrcvgbatch = wa_body-issgorrcvgbatch
          entryunit = wa_body-entryunit
          %control-plant = cl_abap_behv=>flag_changed
          %control-material = cl_abap_behv=>flag_changed
          %control-goodsmovementtype = cl_abap_behv=>flag_changed
          %control-storagelocation = cl_abap_behv=>flag_changed
          %control-issuingorreceivingstorageloc = cl_abap_behv=>flag_changed
          %control-quantityinentryunit = cl_abap_behv=>flag_changed
          %control-batch = cl_abap_behv=>flag_changed
          %control-issgorrcvgbatch = cl_abap_behv=>flag_changed
          %control-entryunit = cl_abap_behv=>flag_changed
          ) )
          ) )
          MAPPED DATA(ls_create_mapped)
          FAILED DATA(ls_create_failed)
          REPORTED DATA(ls_create_reported).

          COMMIT ENTITIES BEGIN
         RESPONSE OF i_materialdocumenttp
          FAILED DATA(commit_failed)
          REPORTED DATA(commit_reported).

          COMMIT ENTITIES END.
          IF commit_failed IS INITIAL AND ls_create_failed IS INITIAL.
            DATA(lv_matdoc) = commit_reported-materialdocument.
            /ui2/cl_json=>serialize(
            EXPORTING
              data        = lv_matdoc
              pretty_name = /ui2/cl_json=>pretty_mode-camel_case
           RECEIVING
              r_json      = DATA(lv_docnumber)
            ).

            response->set_text( lv_docnumber ).
          ELSE.
            response->set_text( 'Failed to create.' ).
          ENDIF.

        ENDIF.

    ENDCASE.

  ENDMETHOD.


  METHOD read_posts.


  ENDMETHOD.
ENDCLASS.
