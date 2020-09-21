unit BoldPMString25;

{ This examples shows how to implement a persistence mapper that
  creates a column of 25 characters worth in the DB }

interface
uses
  BoldDefs,
  BoldPMappersAttributeDefault;

type
  {Forward declaration of classes}
  TBoldPMString25 = class;

  {---TBoldPMShortString---}
  TBoldPMString25 = class(TBoldPMString)
  public
    function GetColumnTypeAsSQL(columnIndex: Integer): string; override;
    function GetColumnSize(columnIndex: Integer): Integer; override;
  end;

implementation
uses
  SysUtils,
  BoldPMapperLists;

{---TBoldPMShortString---}
function TBoldPMString25.GetColumnTypeAsSQL(columnIndex: Integer): string;
begin
  Result := Format('CHAR(%d)', [GetColumnSize(columnIndex)]);
end;

function TBoldPMString25.GetColumnSize(columnIndex: Integer): Integer;
begin
  Result := 25;
end;

initialization
  BoldMemberPersistenceMappers.AddDescriptor(TBoldPMString25, alConcrete);

finalization
  if BoldMemberPersistenceMappersAssigned then
    BoldMemberPersistenceMappers.RemoveDescriptorByClass(TBoldPMString25);

end.

