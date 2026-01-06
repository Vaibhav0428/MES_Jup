@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@ObjectModel.sapObjectNodeType.name: 'ZTRANSFERJOURNAL'
@EndUserText.label: '###GENERATED Core Data Service Entity'
define root view entity ZR_TRANSFERJOURNAL
  as select from ztransferjournal
{
  key header_id as HeaderID,
  key material as Material,
  key plant as Plant,
  key sno as Sno,
  from_location as FromLocation,
  to_location as ToLocation,
  from_batch as FromBatch,
  to_batch as ToBatch,
  quantity as Quantity,
  entry_unit as EntryUnit,
  errorlog as Errorlog,
  posted as Posted,
  posting_uom as PostingUOM,
  document_number as DocumentNumber,
  document_year as DocumentYear,
  sendtomes as SendToMES,
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
