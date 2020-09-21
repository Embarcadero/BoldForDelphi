unit BoldRose98TaggedValues;

interface

uses
  BoldUMLTypes,
  BoldTaggedValueList;

const

  TAG_CHANGEABILITY: String = 'Changeability';
  TV_CHANGEABILITY_ADDONLY: String = 'addOnly';
  TV_CHANGEABILITY_CHANGEABLE: String = 'changeable';
  TV_CHANGEABILITY_FROZEN: String = 'frozen';

  TAG_CONSTRAINTS: String = 'Constraints';

  TAG_MODELNAME: String = 'ModelName';

  TAG_ISCLASSMETHOD: String = 'IsClassMethod';

  TAG_PTY_VERSION = 'PTYVersion';

type
  TBoldRose98TaggedValueSupport = class
  public
    class function StringToChangeableKind(const Value: String): TChangeableKind;
    class function ChangeableKindToString(Value: TChangeableKind): String;
   end;

function Rose98TaggedValueList: TBoldTaggedValuePerClassList;

implementation

uses
  SysUtils,
  BoldDefaultTaggedValues,
  BoldDefs,
  UMLConsts,
  BoldUtils;

var
 G_Rose98TaggedValues: TBoldTaggedValuePerClassList = nil;

procedure AddDefaultTaggedValues;
begin
// Tagged values for Model
  with G_Rose98TaggedValues.ListForClassName['Model'] do // do not localize
  begin
    Add('String',   TAG_MODELNAME,                       'BusinessClasses'); // do not localize
    Add('Text',     TAG_CONSTRAINTS,                     ''); // do not localize
    Add('PTYVersionSet', // do not localize
                    'PTYVersion',                      BOLDTVREV); // do not localize
  end;

// Tagged values for Class
  with G_Rose98TaggedValues.ListForClassName['Class'] do // do not localize
  begin
    Add('Text',     TAG_CONSTRAINTS,                    ''); // do not localize
  end;

// Tagged values for Association
  with G_Rose98TaggedValues.ListForClassName['Association'] do // do not localize
  begin
  end;

// Tagged values for Attribute
  with G_Rose98TaggedValues.ListForClassName['Attribute'] do // do not localize
  begin
    Add('Text',    TAG_CONSTRAINTS,               ''); // do not localize
  end;

// Tagged values for AssociationEnd
  with G_Rose98TaggedValues.ListForClassName['AssociationEnd'] do // do not localize
  begin
     Add('ChangeabilityKind', // do not localize
                   TAG_CHANGEABILITY,              TV_CHANGEABILITY_CHANGEABLE);
     Add('Text',    TAG_CONSTRAINTS,                ''); // do not localize
  end;

// Tagged values for Operation
  with G_Rose98TaggedValues.ListForClassName['Operation'] do // do not localize
  begin
    Add('Text',  TAG_CONSTRAINTS,                    ''); // do not localize
    Add('Boolean', TAG_ISCLASSMETHOD,                  TV_FALSE); // do not localize
  end;
end;

function Rose98TaggedValueList: TBoldTaggedValuePerClassList;
begin
  if not Assigned(G_Rose98TaggedValues) then
  begin
    G_Rose98TaggedValues := TBoldTaggedValuePerClassList.Create;
    AddDefaultTaggedValues;
  end;
  Result := G_Rose98TaggedValues
end;

{ TBoldRose98TaggedValueSupport }

class function TBoldRose98TaggedValueSupport.ChangeableKindToString(
  Value: TChangeableKind): String;
begin
  case Value of
    ckChangeable:
      Result := TV_CHANGEABILITY_CHANGEABLE;
    ckFrozen:
      Result := TV_CHANGEABILITY_FROZEN;
    ckAddOnly:
      Result := TV_CHANGEABILITY_ADDONLY;
    else
      raise EBold.CreateFmt(sWrongValue, [ClassName, 'ChangeableKindToString']); // do not localize
  end;
end;

class function TBoldRose98TaggedValueSupport.StringToChangeableKind(
  const Value: String): TChangeableKind;
begin
  if SameText(Value, TV_CHANGEABILITY_CHANGEABLE) then
    Result := ckChangeable
  else if SameText(Value, TV_CHANGEABILITY_FROZEN) then
    Result := ckFrozen
  else if SameText(Value, TV_CHANGEABILITY_ADDONLY) then
    Result := ckAddOnly
  else
    result := ckChangeable;
end;

initialization

finalization
  FreeAndNil(G_Rose98TaggedValues);

end.
