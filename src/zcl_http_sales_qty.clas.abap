class ZCL_HTTP_SALES_QTY definition
  public
  create public .

public section.

  interfaces IF_HTTP_SERVICE_EXTENSION .
protected section.
private section.
ENDCLASS.



CLASS ZCL_HTTP_SALES_QTY IMPLEMENTATION.


  method IF_HTTP_SERVICE_EXTENSION~HANDLE_REQUEST.
  CASE request->get_method(  ).
     WHEN CONV string( if_web_http_client=>get ).
        DATA(req) = request->get_form_fields( ).

        response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
        response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).

        DATA(lv_materialdocument) = VALUE #( req[ name = 'materialdocument' ]-value OPTIONAL ).
        DATA(lv_materialdocumentyear) = VALUE #( req[ name = 'materialdocumentyear' ]-value OPTIONAL ).

       select from I_MaterialDocumentItem_2 as a
       inner join I_Product as b on a~Material = b~Product  and b~ProductType = 'ZFRT'
       fields a~MaterialDocument,a~MaterialDocumentYear,a~Material,a~Batch,sum( a~QuantityInEntryUnit ) as TotalQty,a~Plant,
       a~StorageLocation
       where a~MaterialDocument = @lv_materialdocument and a~MaterialDocumentYear = @lv_materialdocumentyear and a~GoodsMovementType = '601'
       group by a~MaterialDocument , a~MaterialDocumentYear , a~Material , a~Batch, a~Plant,a~StorageLocation
       into table @data(it_qty)
       PRIVILEGED ACCESS.

        /ui2/cl_json=>serialize(
            EXPORTING
              data        = it_qty
              pretty_name = /ui2/cl_json=>pretty_mode-camel_case
            RECEIVING
              r_json      = DATA(lv_json)
          ).

        response->set_text( lv_json ).
   ENDCASE.
  endmethod.
ENDCLASS.
