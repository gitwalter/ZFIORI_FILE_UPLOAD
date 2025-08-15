@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Root view for file buffer'
define root view entity ZI_FILE_BUFFER
  as select from zfile_buffer
  composition [0..*] of ZI_File_Line as _FileLine
{
  key uuid                as Uuid,
      filename            as FileName,
      @Semantics.largeObject:
      { mimeType: 'MimeType',
      fileName: 'FileName',
      contentDispositionPreference: #INLINE }
      content             as Content,
      @Semantics.mimeType: true
      mimetype            as MimeType,

      @Consumption.valueHelpDefinition: [ { entity: { name: 'ZI_File_Upload_Process', element: 'FileUploadProcess' } } ]
      @Consumption.filter.mandatory: true
      @Consumption.filter.selectionType: #SINGLE
      file_upload_process as FileUploadProcess,

      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at     as LastChangedAt,
      @Semantics.user.lastChangedBy: true
      last_changed_by     as LastChangedBy,
      _FileLine
}
