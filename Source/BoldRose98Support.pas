
{ Global compiler directives }
{$include bold.inc}
unit BoldRose98Support;

interface

uses
  RationalRose98_TLB,
  Classes,
  BoldUMLTypes;

type
  { forward declarations }
  TBoldRose98Support = class;
  TBoldRose98Properties = class;
  TBoldRose98AddIns = class;
                                     
  { TBoldRose98Support }
  TBoldRose98Support = class
  public
    class function FindClassByName(RoseModel: IRoseModel; const Name: string): IRoseClass;
    class function GetApplication: IRoseApplication;
    class function GetVersion: Double;
    class function GetVersionString: String;
    class function RoseExportControlToVisibility(ExportControl: IRoseRichType): TVisibilityKind;
    class procedure SetExportControl(Visibility: TVisibilityKind; ExportControl: IRoseRichType; const LoggString: string);
    class procedure SetContainment(Aggregation: TAggregationKind; RoseRole, OtherRole: IRoseRole; const LoggString: string);
    class function GetContainment(RoseRole, OtherRole: IRoseRole): TAggregationKind;
  end;

  { TBoldRose98Properties }
  TBoldRose98Properties = class
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

  { TBoldRose98AddIns }
  TBoldRose98AddIns = class
  public
    class procedure GetPlugInNames(PlugInsNameList: TStringList);
  end;

implementation

uses
  ActiveX,
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldUtils,
  BoldDefaultTaggedValues,
  BoldLogHandler;

class function TBoldRose98Support.FindClassByName(RoseModel: IRoseModel; const Name: string): IRoseClass;
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

class function TBoldRose98Support.GetApplication: IRoseApplication;
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

{ TBoldRose98Properties }

constructor TBoldRose98Properties.Create(const ToolName: string);
begin
  inherited Create;
  FToolName := ToolName;
end;

function TBoldRose98Properties.GetBooleanString(RoseItem: IRoseItem; const Name: string; DefaultValue: String): String;
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

function TBoldRose98Properties.GetBoolean(RoseItem: IRoseItem; const Name: string; DefaultValue: Boolean): Boolean;
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

function TBoldRose98Properties.GetDefaultPropertyBoolean(RoseModel: IRoseModel; PropName: String): Boolean;
var DefaultProps: IRoseDefaultModelProperties;
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

function TBoldRose98Properties.GetDefaultPropertyString(RoseModel: IRoseModel; PropName: String): String;
var DefaultProps: IRoseDefaultModelProperties;
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

procedure TBoldRose98Properties.SetDefaultPropertyBoolean(RoseModel: IRoseModel; PropName:String; PropValue: Boolean);
var DefaultProps: IRoseDefaultModelProperties;
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

procedure TBoldRose98Properties.SetDefaultPropertyString(RoseModel: IRoseModel; PropName, PropValue: String);
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

function TBoldRose98Properties.GetInteger(RoseItem: IRoseItem; const Name: string; DefaultValue: Integer): Integer;
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

function TBoldRose98Properties.GetString(RoseItem: IRoseItem; const Name, DefaultValue: string): string;
begin
  if Assigned(RoseItem) then
    Result := RoseItem.GetPropertyValue(ToolName,Name)
  else
    Result := '';
  if Result = '' then Result := DefaultValue;
end;

function TBoldRose98Properties.GetText(RoseItem: IRoseItem; const Name, DefaultValue: string): string;
begin
  if Assigned(RoseItem) then
    Result := RoseItem.GetPropertyValue(ToolName,Name)
  else
    Result := '';
  if Result = '' then Result := DefaultValue;
end;

procedure TBoldRose98Properties.SetBooleanString(RoseItem: IRoseItem; const Name: string; DefaultValue, Value: String; const LoggString: string);
begin
  if GetBooleanString(RoseItem, Name, DefaultValue) <> Value then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, Value]);
    RoseItem.OverrideProperty(ToolName, Name, Value);
  end;
end;

procedure TBoldRose98Properties.SetBoolean(RoseItem: IRoseItem; const Name: string; DefaultValue: Boolean; Value: Boolean;
  const LoggString: string);
begin
  if GetBoolean(RoseItem, Name, DefaultValue) <> Value then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, BooleanToString(Value)]);
    RoseItem.OverrideProperty(ToolName, Name, BooleanToString(Value));
  end;
end;

procedure TBoldRose98Properties.SetInteger(RoseItem: IRoseItem; const Name: string; DefaultValue: Integer; Value: Integer;
  const LoggString: string);
begin
  if GetInteger(RoseItem, Name, DefaultValue) <> Value then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, IntToStr(Value)]);
    RoseItem.OverrideProperty(ToolName, Name, IntToStr(Value));
  end
end;

procedure TBoldRose98Properties.SetString(RoseItem: IRoseItem; const Name, DefaultValue, Value: string;
  const LoggString: string);
begin
  if AnsiCompareText(GetString(RoseItem, Name, DefaultValue),Value) <> 0 then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, Value]);
    RoseItem.OverrideProperty(ToolName, Name, Value);
  end;
end;

procedure TBoldRose98Properties.SetText(RoseItem: IRoseItem; const Name, DefaultValue, Value: string;
  const LoggString: string);
begin
  if AnsiCompareText(GetString(RoseItem, Name, DefaultValue),Value) <> 0 then
  begin
    BoldLog.LogFmt(sSettingValue, [LoggString, Name, Value]);
    RoseItem.OverrideProperty(ToolName, Name, Value);
  end;
end;

{ TBoldRose98AddIns }

class procedure TBoldRose98AddIns.GetPlugInNames(PlugInsNameList: TStringList);
var
  AddIns: IRoseAddInCollection;
  Index: Integer;
begin
  AddIns := TBoldRose98Support.GetApplication.AddInManager.AddIns;
  PlugInsNameList.Clear;
  for Index := 1 to AddIns.Count do
    PlugInsNameList.Add(AddIns.GetAt(Index).Name);
end;

class function TBoldRose98Support.GetContainment(RoseRole, OtherRole: IRoseRole): TAggregationKind;
begin
  Result := akNone;
  if OtherRole.Aggregate then
    case RoseRole.Containment.Value of
      0: Result := akAggregate;
      1: Result := akComposite;
      2: Result := akAggregate;
    end
  else if RoseRole.Containment.Value = 2 then
    BoldLog.Log(sContainmentByValueButNotAggregate);
end;

class procedure TBoldRose98Support.SetContainment(Aggregation: TAggregationKind; RoseRole, OtherRole: IRoseRole; const LoggString: string);
var
  ContainmentName: string;
begin
  if GetContainment(RoseRole, OtherRole) <> Aggregation then
  begin
    case Aggregation of
      akNone:
      begin
        OtherRole.Aggregate := false;
        RoseRole.Containment.Value := 0;
        ContainmentName := 'None'; // do not localize
      end;
      akComposite:
      begin
        OtherRole.Aggregate := true;
        RoseRole.Containment.Value := 1;
        ContainmentName := 'Composite'; // do not localize
      end;
      akAggregate:
      begin
        OtherRole.Aggregate := true;
        RoseRole.Containment.Value := 2;
        ContainmentName := 'Aggregate'; // do not localize
      end;
    end;
    BoldLog.LogFmt(sSettingValue, [LoggString, 'containment', ContainmentName]); // do not localize
    BoldLog.LogFmt(sSettingValue, [LoggString, 'Aggregate', BooleanToString(aggregation <> aknone)]); // do not localize
  end;
end;

class function TBoldRose98Support.RoseExportControlToVisibility(ExportControl: IRoseRichType): TVisibilityKind;
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

class procedure TBoldRose98Support.SetExportControl(Visibility: TVisibilityKind; ExportControl: IRoseRichType; const LoggString: string);
var
 VisibilityName: string;
begin
  if RoseExportControlToVisibility(ExportControl) <> Visibility then
  begin
    case Visibility of
      vkPublic:
      begin
        ExportControl.Value := 0;
        VisibilityName := 'public'; // do not localize
      end;
      vkProtected:
      begin
        ExportControl.Value := 1;
        VisibilityName := 'protected'; // do not localize
      end;
      vkPrivate:
      begin
       ExportControl.Value := 2;
       VisibilityName := 'private'; // do not localize
      end;
    end;
    BoldLog.LogFmt(sSettingValue, [LoggString, 'Visibility', VisibilityName]); // do not localize
  end;
end;

class function TBoldRose98Support.GetVersion: Double;
var
  versionStr: String;
  OldDecimalSeparator: char;
  start, stop, i: integer;
  DotCount: integer;
begin
  VersionStr := GetVersionString;
  start := -1;
  stop := -1;
  DotCount := 0;
  for i := 1 to length(VersionStr) do
  begin
    case VersionStr[i] of
      '0'..'9': if Start = -1 then
        Start := i;
      '.': begin
        inc(DotCount);
        if (start <> -1) and (DotCount = 2) then
          stop := i;
      end
      else if (start <> -1) and (stop = -1) then
        stop := i;
    end;
  end;
  if start = -1 then
    result := -1
  else
  begin
    if stop = -1 then
      stop := length(VersionStr);
    OldDecimalSeparator := {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DecimalSeparator;
    {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DecimalSeparator := '.';
    try
      result := StrToFloat(copy(VersionStr, start, stop-start));
    finally
      {$IFDEF BOLD_DELPHI16_OR_LATER}FormatSettings.{$ENDIF}DecimalSeparator := OldDecimalSeparator;
   end;
  end;
end;

class function TBoldRose98Support.GetVersionString: String;
begin
  result := GetApplication.Version;
end;

end.
