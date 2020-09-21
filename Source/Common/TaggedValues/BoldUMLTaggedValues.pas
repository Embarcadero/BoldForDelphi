unit BoldUMLTaggedValues;

interface

uses
  BoldDefs,
  BoldTaggedValueList,
  BoldDefaultTaggedValues;

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
  BoldUtils;

var
 G_UMLTaggedValues: TBoldTaggedValuePerClassList = nil;

procedure AddDefaultTaggedValues;
begin
// Tagged values for Class
  with G_UMLTaggedValues.ListForClassName['Class'] do // do not localize
  begin
    Add(ENUM_TAG_PERSISTENCE,     TAG_PERSISTENCE,                    TV_PERSISTENCE_PERSISTENT);
  end;

// Tagged values for Association
  with G_UMLTaggedValues.ListForClassName['Association'] do // do not localize
  begin // do not localize
    Add('Boolean',                TAG_DERIVED,                        TV_FALSE); // do not localize
    Add(ENUM_TAG_PERSISTENCE,     TAG_PERSISTENCE,                    TV_PERSISTENCE_PERSISTENT);
  end;

// Tagged values for Attribute
  with G_UMLTaggedValues.ListForClassName['Attribute'] do // do not localize
  begin
    Add('Boolean',                TAG_DERIVED,                        TV_FALSE); // do not localize
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
