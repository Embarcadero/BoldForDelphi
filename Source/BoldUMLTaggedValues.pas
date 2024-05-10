
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLTaggedValues;

interface

uses
  BoldTaggedValueList;

const
  TAG_DOCUMENTATION: String ='documentation';
  TAG_DERIVED: String = 'derived';

  ENUM_TAG_PERSISTENCE: String = 'PersistenceEnum';
  TAG_PERSISTENCE: String = 'persistence';
    TV_PERSISTENCE_PERSISTENT: String = 'persistent';
    TV_PERSISTENCE_TRANSIENT: String = 'transient';


function UMLTaggedValueList: TBoldTaggedValuePerClassList;

implementation

uses
  SysUtils,
  BoldDefaultTaggedValues;

var
 G_UMLTaggedValues: TBoldTaggedValuePerClassList = nil;

procedure AddDefaultTaggedValues;
begin
  with G_UMLTaggedValues.ListForClassName['Class'] do
  begin
    Add(ENUM_TAG_PERSISTENCE,     TAG_PERSISTENCE,                    TV_PERSISTENCE_PERSISTENT);
  end;
  with G_UMLTaggedValues.ListForClassName['Association'] do
  begin
    Add('Boolean',                TAG_DERIVED,                        TV_FALSE);
    Add(ENUM_TAG_PERSISTENCE,     TAG_PERSISTENCE,                    TV_PERSISTENCE_PERSISTENT);
  end;
  with G_UMLTaggedValues.ListForClassName['Attribute'] do
  begin
    Add('Boolean',                TAG_DERIVED,                        TV_FALSE);
    Add(ENUM_TAG_PERSISTENCE,     TAG_PERSISTENCE,                    TV_PERSISTENCE_PERSISTENT);
  end;
end;

function UMLTaggedValueList: TBoldTaggedValuePerClassList;
begin
  if not Assigned(G_UMLTaggedValues) then
  begin
    G_UMLTaggedValues := TBoldTaggedValuePerClassList.Create;
    AddDefaultTaggedValues;
  end;
  Result := G_UMLTaggedValues
end;

initialization

finalization
  FreeAndNil(G_UMLTaggedValues);

end.
