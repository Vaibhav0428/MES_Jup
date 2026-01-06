CLASS zcl_http_transferjournal DEFINITION
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



CLASS ZCL_HTTP_TRANSFERJOURNAL IMPLEMENTATION.


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
                 material                     TYPE matnr,
                 plant                        TYPE werks_d,
                 storagelocation              TYPE c LENGTH 4,
                 id                           TYPE string,
                 batch                        TYPE c LENGTH 10,
                 goodsmovementtype            TYPE string,
                 entryunit                    TYPE c LENGTH 3,
                 quantityinentryunit          TYPE string,
                 issgorrcvgbatch              TYPE string,
                 issuingorreceivingstorageloc TYPE c LENGTH 4,

               END OF ty_header.


        TYPES: BEGIN OF ty_results,
                 results TYPE TABLE OF ty_header WITH EMPTY KEY,
               END OF ty_results.

        TYPES: BEGIN OF ty_requests,
                 to_materialdocumentitem TYPE ty_results,
               END OF ty_requests.


        TYPES: BEGIN OF ty_response,
                 success TYPE c LENGTH 1,
                 id      TYPE string,
               END OF ty_response.



        DATA: resquest_data      TYPE ty_requests,
              ls_response        TYPE ty_response,
              ls_transferjournal TYPE STRUCTURE FOR CREATE zr_transferjournal,
              lt_transferjournal TYPE TABLE FOR CREATE zr_transferjournal.

        "Step 2: Parse JSON"
        DATA(lv_body) = request->get_text( ).
        /ui2/cl_json=>deserialize(
          EXPORTING
            json = lv_body
          CHANGING
            data = resquest_data ).




        LOOP AT resquest_data-to_materialdocumentitem-results INTO DATA(ls_json).
          ls_transferjournal-%cid = getcid(  ).
          ls_transferjournal-material = ls_json-material.
          ls_transferjournal-plant = ls_json-plant.
          ls_transferjournal-fromlocation = ls_json-storagelocation.
          ls_transferjournal-tolocation = ls_json-issuingorreceivingstorageloc.
          ls_transferjournal-frombatch = ls_json-batch.
          ls_transferjournal-tobatch = ls_json-issgorrcvgbatch.
          ls_transferjournal-entryunit = ls_json-entryunit.
          ls_transferjournal-quantity = ls_json-quantityinentryunit.
          ls_transferjournal-headerid = ls_json-id.
          ls_transferjournal-sno = sy-tabix.

          SELECT SINGLE FROM i_unitofmeasure
            FIELDS unitofmeasure
            WHERE unitofmeasure_e = @ls_json-entryunit
            INTO @DATA(lv_uom).

          ls_transferjournal-PostingUOM = lv_uom.

          APPEND ls_transferjournal TO lt_transferjournal.
          CLEAR: ls_transferjournal.
        ENDLOOP.

        MODIFY ENTITIES OF zr_transferjournal
           ENTITY zrtransferjournal
           CREATE FIELDS (
             headerid
             material
             plant
             fromlocation
             tolocation
             frombatch
             tobatch
             quantity
             entryunit
             sno
             postinguom
           )
           WITH VALUE #( FOR wa IN lt_transferjournal ( wa ) )
             REPORTED DATA(ls_po_reported)
               FAILED   DATA(ls_po_failed)
               MAPPED   DATA(ls_po_mapped).

        COMMIT ENTITIES BEGIN
           RESPONSE OF zr_transferjournal
           FAILED DATA(ls_save_failed)
           REPORTED DATA(ls_save_reported).

        ls_response-id = ls_transferjournal-headerid.
        IF ls_po_failed IS NOT INITIAL OR ls_save_failed IS NOT INITIAL.
          ls_response-success = 'N'.
        ELSE.
          ls_response-success = 'Y'.
        ENDIF.

        COMMIT ENTITIES END.

        ls_response-id = lt_transferjournal[ 1 ]-headerid.

        DATA:json TYPE REF TO if_xco_cp_json_data.
        xco_cp_json=>data->from_abap(
          EXPORTING
            ia_abap      = ls_response
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
