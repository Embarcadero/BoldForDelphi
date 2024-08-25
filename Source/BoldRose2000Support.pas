
{ Global compiler directives }
{$include bold.inc}
unit BoldRose2000Support;

interface

uses
  Classes,
  RationalRose2000_TLB,
  BoldUMLTypes;

type
  { forward declarations }
  TBoldRose2000Support = class;
  TBoldRose2000Properties = class;
  TBoldRose2000AddIns = class;

  { TBoldRose2000Support }
  TBoldRose2000Support = class
  public
    class function BooleanToString(Value: Boolean): string;
    class function FindClassByName(RoseModel: IRoseModel; const Name: string): IRoseClass;
    class function GetApplication: IRoseApplication;
    class function GetCalculatedAssociationName(RoseAssociation: IRoseAssociation; const PluralSuffix: string): string;
    class function GetCalculatedRoleName(RoseRole: IRoseRole; const PluralSuffix: string): string;
    class function GetEffectiveAssociationName(RoseAssociation: IRoseAssociation; const PluralSuffix: string): string;
    class function GetEffectiveRoleName(RoseRole: IRoseRole; const PluralSuffix: string): string;
    class function GetEffectiveMultiplicity(RoseRole: IRoseRole): String;
    class function ParametersToSignature(Parameters: IRoseParameterCollection): string;
    class function RoleIsMandatory(RoseRole: IRoseRole): Boolean;
    class function RoleIsMulti(RoseRole: IRoseRole): Boolean;
    class function RoseExportControlToVisibility(ExportControl: IRoseRichType): TVisibilityKind;
    class procedure SetExportControl(Visibility: TVisibilityKind; ExportControl: IRoseRichType);
    class procedure SetContainment(Aggregation: TAggregationKind; Containment: IRoseRichType);
    class function RoseContainmentToAggregationKind(Containment: IRoseRichType): TAggregationKind;
  end;

  { TBoldRose2000Properties }
  TBoldRose2000Properties = class
  private
    FToolName: string;
  public
    constructor Create(const ToolName: string);
    function GetBoolean(RoseItem: IRoseItem; const Name: string; DefaultValue: Boolean): Boolean;
    function GetInteger(RoseItem: IRoseItem; const Name: string; DefaultValue: Integer): Integer;
    function GetString(RoseItem: IRoseItem; const Name, DefaultValue: string): string;
    function GetText(RoseItem: IRoseItem; const Name, DefaultValue: string): string;
    procedure SetBoolean(RoseItem: IRoseItem; const Name: string; DefaultValue: Boolean; Value: Boolean; const LoggString: string);
    procedure SetInteger(RoseItem: IRoseItem; const Name: string; DefaultValue: Integer; Value: Integer; const LoggString: string);
    procedure SetString(RoseItem: IRoseItem; const Name, DefaultValue, Value: string; const LoggString: string);
    procedure SetText(RoseItem: IRoseItem; const Name, DefaultValue, Value: string; const LoggString: string);
    procedure SetDefaultPropertyString(RoseModel: IRoseModel; PropName, PropValue: String);
    function GetDefaultPropertyString(RoseModel: IRoseModel; PropName: String): String;
    procedure SetDefaultPropertyBoolean(RoseModel: IRoseModel; PropName: String; PropValue: Boolean);
    function GetDefaultPropertyBoolean(RoseModel: IRoseModel; PropName: String): Boolean;
    function GetBooleanString(RoseItem: IRoseItem; const Name: string; DefaultValue: String): String;
    procedure SetBooleanString(RoseItem: IRoseItem; const Name: string; DefaultValue, Value: String; const LoggString: string);
    property ToolName: string read FToolName write FToolName;
  end;

  { TBoldRose2000AddIns }
  TBoldRose2000AddIns = class
  public
    class procedure GetPlugInNames(PlugInsNameList: TStringList);
  end;

implementation

uses
  ActiveX,
  SysUtils,

  BoldCoreConsts,
  BoldLogHandler,
  BoldDefs,
  BoldUtils,
  BoldDefaultTaggedValues;

function StringToBoolean(inString: String): Boolean;
begin
  Result := False;
  if (UpperCase(inString)= 'Y') or (UpperCase(inString) = 'T') or (UpperCase(inString) = 'TRUE') then
    Result := True;
end;

function BooleanToString(inValue: Boolean): String;
begin
  if inValue then
    Result := 'True' // do not localize
  else
    Result := 'False'; // do not localize
end;

class function TBoldRose2000Support.BooleanToString(Value: Boolean): string;
begin
  Result := BooleanToString(Value);
end;

class function TBoldRose2000Support.FindClassByName(RoseModel: IRoseModel; const Name: string): IRoseClass;
var
  ClsCol: IRoseClassCollection;
begin
  ClsCol := RoseModel.FindClasses(name);
  if ClsCol.Count = 0 then
    Result := nil
  else
  begin
    Result := ClsCol.GetAt(1);
    if ClsCol.Count > 1 then
      raise EBoldImport.CreateFmt(sClassNameNotUnique, [Name]);
  end;
end;

class function TBoldRose2000Support.GetApplication: IRoseApplication;
var
  Unk: IUnknown;
begin
  try
    GetActiveObject(Class_RoseApplication, nil, Unk);
    if Assigned(Unk) then
      Result := Unk as IRoseApplication
    else
    begin
      Result := CoRoseApplication.Create;
      Result.Visible := True;
    end;
  except
    on Exception do
      Result := nil;
  end;
end;

class function TBoldRose2000Support.GetCalculatedAssociationName(RoseAssociation: IRoseAssociation; const PluralSuffix: string): string;
begin
  Result := Format('%s%s',
    [GetEffectiveRoleName(RoseAssociation.Role1,PluralSuffix),
     GetEffectiveRoleName(RoseAssociation.Role2,PluralSuffix)]);
end;

class function TBoldRose2000Support.GetCalculatedRoleName(RoseRole: IRoseRole; const PluralSuffix: string): string;
var
  OtherRole: IRoseRole;
begin
  if not RoseRole.Navigable then
  begin
    if RoseRole = RoseRole.Association.Role1 then
      otherRole := RoseRole.Association.Role2
    else
      otherRole := RoseRole.Association.Role1;
    if assigned(RoseRole) and assigned(RoseRole.Class_) then
      result := 'x_' + otherRole.Name + '_' + RoseRole.Class_.Name
    else
      result := 'x_' + otherRole.Name + '_unknown';
  end
  else
  begin
    Result := RoseRole.Class_.Name;
    if result <> '' then
      result[1] := lowercase(result[1])[1];
  end;
  if RoleIsMulti(RoseRole) then
    Result := Concat(Result,PluralSuffix);
end;

class function TBoldRose2000Support.GetEffectiveAssociationName(RoseAssociation: IRoseAssociation; const PluralSuffix: string): string;
begin
  Result := RoseAssociation.Name;
  if Trim(Result) = '' then
    Result := GetCalculatedAssociationName(RoseAssociation,PluralSuffix);
end;

class function TBoldRose2000Support.GetEffectiveRoleName(RoseRole: IRoseRole; const PluralSuffix: string): string;
begin
  Result := RoseRole.Name;

  if Trim(Result) = '' then
    Result := GetCalculatedRoleName(RoseRole,PluralSuffix);
end;

class function TBoldRose2000Support.GetEffectiveMultiplicity(RoseRole: IRoseRole): String;
begin
  Result := RoseRole.Cardinality;
  Trim(Result);
  if Result = '' then
    Result := '0..1'
  else
    Result := StringReplace(Result, 'n', '*', [rfReplaceAll]);

  if Length(Result) < 2 then
    Result := '0..' + Result;
end;

class function TBoldRose2000Support.ParametersToSignature(Parameters: IRoseParameterCollection): string;
var
  P: Integer;
  First: Boolean;
  Parameter: IRoseParameter;
begin
  First := True;
  Result := '';
  for P := 1 to Parameters.Count do
  begin
    if First then
      First := False
    else
      Result := Result + '; ';
    Parameter := Parameters.GetAt(P) as IRoseParameter;
    Result := Result + Format('%s: %s',[Parameter.Name, Parameter.Type_]);
  end;
end;

class function TBoldRose2000Support.RoleIsMandatory(RoseRole: IRoseRole): Boolean;
begin
  Result := GetLowerLimitForMultiplicity(RoseRole.Cardinality) > 0;
end;

class function TBoldRose2000Support.RoleIsMulti(RoseRole: IRoseRole): Boolean;
begin
  Result := GetUpperLimitForMultiplicity(RoseRole.Cardinality) > 1;
end;

{ TBoldRose2000Properties }

constructor TBoldRose2000Properties.Create(const ToolName: string);
begin
  inherited Create;
  FToolName := ToolName;
end;

function TBoldRose2000Properties.GetBooleanString(RoseItem: IRoseItem; const Name: string; DefaultValue: String): String;
var
  Value: string;
begin
  if Assigned(RoseItem) then
    Value := RoseItem.GetPropertyValue(ToolName, Name)
  else
    Value := '';
  if Value = '' then
    Result := DefaultValue
  else
  begin
    if AnsiCompareText(Value, 'True') = 0 then
      Result := TV_TRUE
    else
      Result := TV_FALSE;
  end;
end;

function TBoldRose2000Properties.GetBoolean(RoseItem: IRoseItem; const Name: string; DefaultValue: Boolean): Boolean;
var
  Value: string;
begin
  if Assigned(RoseItem) then
    Value := RoseItem.GetPropertyValue(ToolName, Name)
  else
    Value := '';
  if Value = '' then
    Result := DefaultValue
  else
    Result := (AnsiCompareText(Value, 'True') = 0);
end;

function TBoldRose2000Properties.GetDefaultPropertyBoolean(RoseModel: IRoseModel; PropName: String): Boolean;
var
  DefaultProps: IRoseDefaultModelProperties;
  PropCollection: IRosePropertyCollection;
  Index: Integer;
  Prop: IRoseProperty;
begin
  DefaultProps := RoseModel.DefaultProperties;
  PropCollection := DefaultProps.GetDefaultPropertySet(RoseModel.GetPropertyClassName, ToolName, 'default');

  Index := PropCollection.FindFirst(PropName);
  if Index > 0 then
    Prop := PropCollection.GetAt(Index)
  else
  begin
    Result := False;
    Exit;
  end;
  Result := StringToBoolean(Prop.Value);
end;

function TBoldRose2000Properties.GetDefaultPropertyString(RoseModel: IRoseModel; PropName: String): String;
var
  DefaultProps: IRoseDefaultModelProperties;
  PropCollection: IRosePropertyCollection;
  Index: Integer;
  Prop: IRoseProperty;
begin
  DefaultProps := RoseModel.DefaultProperties;
  PropCollection := DefaultProps.GetDefaultPropertySet(RoseModel.GetPropertyClassName, ToolName, 'default');

  Index := PropCollection.FindFirst(PropName);
  if Index > 0 then
    Prop := PropCollection.GetAt(Index)
  else
    Exit;

  Result :=  Prop.Value;
end;

procedure TBoldRose2000Properties.SetDefaultPropertyBoolean(RoseModel: IRoseModel; PropName: String; PropValue: Boolean);
var
  DefaultProps: IRoseDefaultModelProperties;
  PropCollection: IRosePropertyCollection;
  Index: Integer;
  Prop: IRoseProperty;
begin
  DefaultProps := RoseModel.DefaultProperties;
  PropCollection := DefaultProps.GetDefaultPropertySet(RoseModel.GetPropertyClassName, ToolName, 'default');

  Index := PropCollection.FindFirst(PropName);
  if Index > 0 then
    Prop := PropCollection.GetAt(Index)
  else
    Exit;

  Prop.Value := BooleanToString(PropValue);
end;

procedure TBoldRose2000Properties.SetDefaultPropertyString(RoseModel: IRoseModel; PropName, PropValue: String);
var
  DefaultProps: IRoseDefaultModelProperties;
  PropCollection: IRosePropertyCollection;
  Index: Integer;
  Prop: IRoseProperty;
begin
  DefaultProps := RoseModel.DefaultProperties;
  PropCollection := DefaultProps.GetDefaultPropertySet(RoseModel.GetPropertyClassName, ToolName, 'default');

  Index := PropCollection.FindFirst(PropName);
  if Index > 0 then
    Prop := PropCollection.GetAt(Index)
  else
    Exit;

  Prop.Value := PropValue;
end;

function TBoldRose2000Properties.GetInteger(RoseItem: IRoseItem; const Name: string; DefaultValue: Integer): Integer;
var
  Value: string;
begin
  if Assigned(RoseItem) then
    Value := RoseItem.GetPropertyValue(ToolName, Name)
  else
    Value := '';
  if Value = '' then
    Result := DefaultValue
  else
    Result := StrToInt(Value);
end;

function TBoldRose2000Properties.GetString(RoseItem: IRoseItem; const Name, DefaultValue: string): string;
begin
  if Assigned(RoseItem) then
    Result := RoseItem.GetPropertyValue(ToolName, Name)
  else
    Result := '';
  if Result = '' then
    Result := DefaultValue;
end;

function TBoldRose2000Properties.GetText(RoseItem: IRoseItem; const Name, DefaultValue: string): string;
begin
  if Assigned(RoseItem) then
    Result := RoseItem.GetPropertyValue(ToolName,Name)
  else
    Result := '';
  if Result = '' then
    Result := DefaultValue;
end;

procedure TBoldRose2000Properties.SetBooleanString(RoseItem: IRoseItem; const Name: string; DefaultValue, Value: String; const LoggString: string);
begin
  if GetBooleanString(RoseItem, Name, DefaultValue) <> Value then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, Value]);
    RoseItem.OverrideProperty(ToolName, Name, Value);
  end;
end;

procedure TBoldRose2000Properties.SetBoolean(RoseItem: IRoseItem;
                                             const Name: string;
                                             DefaultValue: Boolean;
                                             Value: Boolean;
                                             const LoggString: string);
begin
  if GetBoolean(RoseItem, Name, DefaultValue) <> Value then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, TBoldRose2000Support.BooleanToString(Value)]);
    RoseItem.OverrideProperty(ToolName, Name, TBoldRose2000Support.BooleanToString(Value));
  end;
end;

procedure TBoldRose2000Properties.SetInteger(RoseItem: IRoseItem;
                                             const Name: string;
                                             DefaultValue: Integer;
                                             Value: Integer;
                                             const LoggString: string);
begin
  if GetInteger(RoseItem, Name, DefaultValue) <> Value then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, IntToStr(Value)]);
    RoseItem.OverrideProperty(ToolName, Name, IntToStr(Value));
  end
end;

procedure TBoldRose2000Properties.SetString(RoseItem: IRoseItem; const Name, DefaultValue, Value: string;
  const LoggString: string);
begin
  if AnsiCompareText(GetString(RoseItem, Name, DefaultValue),Value) <> 0 then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, Value]);
    RoseItem.OverrideProperty(ToolName, Name, Value);
  end;
end;

procedure TBoldRose2000Properties.SetText(RoseItem: IRoseItem; const Name, DefaultValue, Value: string;
  const LoggString: string);
begin
  if AnsiCompareText(GetString(RoseItem, Name, DefaultValue),Value) <> 0 then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, Value]);
    RoseItem.OverrideProperty(ToolName, Name, Value);
  end;
end;

{ TBoldRose2000AddIns }

class procedure TBoldRose2000AddIns.GetPlugInNames(PlugInsNameList: TStringList);
var AddIns: IRoseAddInCollection;
    Index: Integer;
begin
  AddIns := TBoldRose2000Support.GetApplication.AddInManager.AddIns;
  PlugInsNameList.Clear;
  for Index := 1 to AddIns.Count do
  begin
    PlugInsNameList.Add(AddIns.GetAt(Index).Name);
  end;
end;

class function TBoldRose2000Support.RoseContainmentToAggregationKind(Containment: IRoseRichType): TAggregationKind;
begin
  Result := akNone;
  case Containment.Value of
    0: Result := akNone;
    1: Result := akComposite;
    2: Result := akAggregate;
  end;
end;

class procedure TBoldRose2000Support.SetContainment(Aggregation: TAggregationKind; Containment: IRoseRichType);
begin
  Containment.Value := 0;
  case Aggregation of
    akNone: Containment.Value := 0;
    akComposite: Containment.Value := 1;
    akAggregate: Containment.Value := 2;
  end;
end;

class function TBoldRose2000Support.RoseExportControlToVisibility(ExportControl: IRoseRichType): TVisibilityKind;
begin
  Result := vkPublic;
  case ExportControl.Value of
    0: Result := vkPublic;
    1: Result := vkProtected;
    2: Result := vkPrivate;
    else
      BoldLog.Log(sUnknownVisibility);
  end;
end;

class procedure TBoldRose2000Support.SetExportControl(Visibility: TVisibilityKind; ExportControl: IRoseRichType);
begin
  ExportControl.Value := 0;
  case Visibility of
    vkPublic: ExportControl.Value := 0;
    vkProtected: ExportControl.Value := 1;
    vkPrivate: ExportControl.Value := 2;
  end;
end;

end.
