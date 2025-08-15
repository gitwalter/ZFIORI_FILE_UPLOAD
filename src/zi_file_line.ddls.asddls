@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'File Line'
@Metadata.allowExtensions: true
define view entity ZI_File_Line
  as select from zfile_line
  association to parent ZI_FILE_BUFFER as _FileBuffer on $projection.Uuid = _FileBuffer.Uuid
{
  key uuid            as Uuid,
  key line_uuid       as LineUuid,
      line_number     as LineNumber,
      line_content    as LineContent,
      last_changed_at as LastChangedAt,
      last_changed_by as LastChangedBy,
      _FileBuffer
}
