@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTEXTURIZATION'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_TEXTURIZATION
  as select from ztexturization
{
  key productionorder as Productionorder,
  key orderdate as Orderdate,
  key shift as Shift,
  key type as Type,
  key sno as Sno,
  key transid as Transid,
  toolid as Toolid,
  refdocno as Refdocno,
  material as Material,
  materialdescription as Materialdescription,
  uom as UOM,
  quantity as Quantity,
  batch as Batch,
  storagelocation as Storagelocation,
  tolocation as Tolocation,
  errorlog as Errorlog,
  posted as Posted,
  document as Document, 
  sendtomes as Sendtomes,
  @Semantics.user.createdBy: true
  created_by as CreatedBy,
  @Semantics.systemDateTime.createdAt: true
  created_at as CreatedAt,
  @Semantics.user.localInstanceLastChangedBy: true
  last_changed_by as LastChangedBy,
  @Semantics.systemDateTime.localInstanceLastChangedAt: true
  last_changed_at as LastChangedAt,
  @Semantics.systemDateTime.lastChangedAt: true
  local_last_changed_at as LocalLastChangedAt
}
