


CLASS zcl_http_savetexturization DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.

  PROTECTED SECTION.
  PRIVATE SECTION.

     CLASS-METHODS getCID RETURNING VALUE(cid) TYPE abp_behv_cid.

    CLASS-METHODS savedata
      IMPORTING
        request TYPE REF TO if_web_http_request
      RETURNING
        VALUE(message) TYPE string.

ENDCLASS.



CLASS ZCL_HTTP_SAVETEXTURIZATION IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    CASE request->get_method( ).
      WHEN 'POST'.
        response->set_text( savedata( request ) ).
      WHEN OTHERS.
        response->set_status( i_code = 405 i_reason = 'Method Not Allowed' ).
    ENDCASE.
  ENDMETHOD.


  METHOD savedata.
    TRY.
        "Step 1: Define JSON structures"
        TYPES: BEGIN OF ty_header,
                 productionorder     TYPE string,
                 productiondate      TYPE string,
                 shift               TYPE string,
                 toolid              TYPE string,
                 id                  TYPE string,
                 srno                TYPE i,
                 batch               TYPE string,
                 item                TYPE string,
                 transactionid       TYPE string,
                 materialdescription TYPE string,
                 uom                 TYPE string,
                 quantity            TYPE p LENGTH 15 DECIMALS 3,
                 movementtype        TYPE string,
                 tolocation          TYPE c LENGTH 4,
                 storagelocation     TYPE c LENGTH 4,
                 refdocno            TYPE string,
               END OF ty_header.

        TYPES: BEGIN OF ty_response,
                 success TYPE c LENGTH 1,
                 id      TYPE string,
               END OF ty_response.

        DATA: resquest_data     TYPE TABLE OF ty_header,
              ls_response       TYPE ty_response,
              lt_response       TYPE TABLE OF ty_response,
              ls_textturization TYPE STRUCTURE FOR CREATE zr_texturization.

        "Step 2: Parse JSON"
        DATA(lv_body) = request->get_text( ).
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = lv_body
          CHANGING
            data = resquest_data ).



        LOOP AT resquest_data INTO DATA(ls_json).

         DATA lv_matnr TYPE c LENGTH 18.
         lv_matnr = |{ ls_json-item ALPHA = IN }|.
         ls_textturization-batch = |{ ls_json-batch ALPHA = IN }|.

          select single from i_batch as a
           fields a~Batch,a~Material
           where a~Material = @lv_matnr and a~Batch = @ls_textturization-batch
           into @data(wa_batch)
           PRIVILEGED ACCESS.

          ls_textturization-%cid = getcid(  ).
         if wa_batch is initial.
          ls_textturization-batch =  ls_json-batch.
         endif.
          ls_textturization-productionorder = ls_json-productionorder.
          ls_textturization-orderdate = ls_json-productiondate.
          ls_textturization-shift = ls_json-shift.
          ls_textturization-toolid = ls_json-toolid.
          ls_textturization-sno = ls_json-srno.
          ls_textturization-materialdescription = ls_json-materialdescription.
          ls_textturization-tolocation = ls_json-tolocation.
          ls_textturization-type = ls_json-movementtype.
          ls_textturization-storagelocation = ls_json-storagelocation.
          ls_textturization-transid = ls_json-transactionid.
          ls_textturization-refdocno = ls_json-refdocno.


          IF ls_textturization-type = '311'.
            SELECT SINGLE FROM i_unitofmeasure
            FIELDS unitofmeasure
            WHERE unitofmeasure_e = @ls_json-uom
            INTO @DATA(lv_uom).

            ls_textturization-uom = lv_uom.
          ELSE.
            ls_textturization-uom = ls_json-uom.
          ENDIF.

          ls_textturization-quantity = ls_json-quantity.



          ls_textturization-material = lv_matnr.

          MODIFY ENTITIES OF zr_texturization
          ENTITY zrtexturization
          CREATE FIELDS (
            batch
            productionorder
            orderdate
            shift
            toolid
            sno
            material
            materialdescription
            uom
            quantity
            storagelocation
            tolocation
            transid
            refdocno
            type )
          WITH VALUE #( ( ls_textturization ) )
            REPORTED DATA(ls_po_reported)
              FAILED   DATA(ls_po_failed)
              MAPPED   DATA(ls_po_mapped).

          COMMIT ENTITIES BEGIN
             RESPONSE OF zr_texturization
             FAILED DATA(ls_save_failed)
             REPORTED DATA(ls_save_reported).

          ls_response-id = ls_json-id.
          IF ls_po_failed IS NOT INITIAL OR ls_save_failed IS NOT INITIAL.
            ls_response-success = 'N'.
          ELSE.
            ls_response-success = 'Y'.
          ENDIF.

          COMMIT ENTITIES END.

          APPEND ls_response TO lt_response.
          CLEAR: ls_textturization, ls_response,wa_batch,lv_matnr.
        ENDLOOP.

        DATA:json TYPE REF TO if_xco_cp_json_data.

        xco_cp_json=>data->from_abap(
          EXPORTING
            ia_abap      = lt_response
          RECEIVING
            ro_json_data = json   ).
        json->to_string(
          RECEIVING
            rv_string =   message ).


        REPLACE ALL OCCURRENCES OF '"SUCCESS"' IN message WITH '"Success"'.

      CATCH cx_root INTO DATA(lx).
        message = |Error: { lx->get_text( ) }|.
    ENDTRY.


  ENDMETHOD.


        METHOD getCID.
        TRY.
            cid = to_upper( cl_uuid_factory=>create_system_uuid( )->create_uuid_x16( ) ).
          CATCH cx_uuid_error.
            ASSERT 1 = 0.
        ENDTRY.
      ENDMETHOD.
ENDCLASS.
