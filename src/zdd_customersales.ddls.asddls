@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Data of Customer'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType: {
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity zdd_customersales
  as select from    I_CustomerSalesArea as a
    left outer join I_Customer          as b on a.Customer = b.Customer
    left outer join I_SalesOrder        as c on b.Customer = c.SoldToParty
    left outer join I_SalesOrderItem    as d on  c.SalesOrder             =  d.SalesOrder
                                             and d.SalesOrderItemCategory <> 'TADN'
//    left outer join I_UnitOfMeasure     as f on d.OrderQuantityUnit = f.UnitOfMeasure
    left outer join zdd_custsalesdelagg as e on  d.SalesOrder     = e.SalesOrder
                                             and d.SalesOrderItem = e.SalesOrderItem
{
  key a.Customer,
  key c.SalesOrder,
  key d.SalesOrderItem,
      b.CustomerName,
      b.AddressSearchTerm1                                         as CustShortName,
      @Semantics.quantity.unitOfMeasure: 'OrderQuantityUnit'
      cast( d.OrderQuantity as abap.dec(15,3) )                    as OrderQuantity,
      d.OrderQuantityUnit as OrderQuantityUnit,  
//      cast( f.UnitOfMeasure_E as abap.unit(3) )                    as OrderQuantityUnit,
      @Semantics.quantity.unitOfMeasure: 'Deliveryunit'
      coalesce( cast( e.DeliveredQuantity as abap.dec(15,3) ), 0 ) as DeliveredQuantity,
      d.OrderQuantityUnit as Deliveryunit,  
//      cast( f.UnitOfMeasure_E as abap.unit(3) )                    as Deliveryunit,
      @Semantics.quantity.unitOfMeasure: 'Remainingunit'
      d.OrderQuantity - coalesce( e.DeliveredQuantity, 0 )         as RemainingQuantity,
      d.OrderQuantityUnit as Remainingunit
//      cast( f.UnitOfMeasure_E as abap.unit(3) )                    as Remainingunit

}
where
  a.Division = 'SL'
group by
  a.Customer,
  c.SalesOrder,
  d.SalesOrderItem,
  b.CustomerName,
  b.AddressSearchTerm1,
  d.OrderQuantity,
  e.DeliveredQuantity,
  d.OrderQuantityUnit
