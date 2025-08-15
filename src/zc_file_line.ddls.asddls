@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Projection ZI_File_Line'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_File_Line
  as projection on ZI_File_Line
{
  key Uuid,
  key LineUuid,
      LineNumber,
      LineContent,
      /* Associations */
      _FileBuffer : redirected to parent ZC_FILE_BUFFER
}
