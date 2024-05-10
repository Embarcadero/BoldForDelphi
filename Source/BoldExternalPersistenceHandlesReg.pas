
{ Global compiler directives }
{$include bold.inc}
unit BoldExternalPersistenceHandlesReg;

interface

procedure Register;

implementation

{$R BoldExternalPersistenceHandles.res}

uses
  Classes,
  Controls,
  DesignEditors,
  DesignIntf,
  SysUtils,
  TypInfo,

  BoldCoreConsts,
  BoldMeta,
  BoldUtils,
  BoldDefs,
  BoldExternalPersistenceHandleSQLPropEditor,
  BoldExternalPersistenceHandleEventDriven,
  BoldExternalPersistenceControllerConfig,
  BoldExternalPersistenceHandleDataSet,
  BoldExternalPersistenceHandleSQL,
  BoldAbstractPropertyEditors,
  BoldIDEConsts;

type
  { forward declarations }
  TBoldExternalPersistenceHandleEditor = class;
  TBoldExternalPersistenceHandleEventProperty = class;
  TBoldExternalPersistenceHandleSQLPropEdit = class;


  TBoldExternalPersistenceHandleSQLPropEdit = class(TBoldPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

  { TBoldColumnsEditor }
  TBoldExternalPersistenceHandleEditor = class(TComponentEditor)
  private
    procedure EditConfig(const PropertyEditor: IProperty);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure Edit; override;
  end;

  { TBoldExternalPersistenceHandleEventProperty }
  TBoldExternalPersistenceHandleEventProperty = class(TMethodProperty)
  protected
    function GetFormMethodName: string; override;
  end;

procedure RegisterComponentsOnPalette;
begin
  RegisterComponents(BOLDPAGENAME_PERSISTENCE, [
    TBoldExternalPersistenceHandleEventDriven,
    TBoldExternalPersistenceHandleSQL,
    TBoldExternalPersistenceHandleDataSet]);
end;

procedure RegisterComponentEditors;
begin
  RegisterComponentEditor(TBoldExternalPersistenceHandleEventDriven, TBoldExternalPersistenceHandleEditor);
end;

procedure RegisterPropertyEditors;
begin
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceFetchEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceCreateEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceDeleteEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceUpdateEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceGetListEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistencePrepareFetchEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistencePostFetchEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceReadMemberEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceGetExistsEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceAssignKeyToObjectEvent), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TBoldExternalPersistenceGetKeyFromObject), nil, '', TBoldExternalPersistenceHandleEventProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TBoldExternalPersistenceHandleSQL, 'ClassesToHandle', TBoldExternalPersistenceHandleSQLPropEdit);
end;

procedure Register;
begin
  RegisterComponentsOnPalette;
  RegisterComponentEditors;
  RegisterPropertyEditors;
end;

{ TBoldExternalPersistenceHandleEventProperty }

function TBoldExternalPersistenceHandleEventProperty.GetFormMethodName: string;
var
  I: Integer;
begin
  if GetComponent(0) = Designer.GetRoot then
  begin
    Result := Designer.GetRootClassName;
    if (Result <> '') and (Result[1] = 'T') then // do not localize
      Delete(Result, 1, 1);
  end
  else
  begin
    if GetComponent(0) is TBoldExternalPersistenceConfigItem then
      result := (GetComponent(0) as TBoldExternalPersistenceConfigItem).ExpressionName
    else
      Result := Designer.GetObjectName(GetComponent(0));
    for I := Length(Result) downto 1 do
      if CharInSet(Result[I], ['.', '[', ']', '-', '>']) then
        Delete(Result, I, 1);
  end;
  if Result = '' then
    raise Exception.Create(sCannotCreateNameNow);
  Result := Result + GetTrimmedEventName;
end;

{ TBoldExternalPersistenceHandle }

procedure TBoldExternalPersistenceHandleEditor.Edit;
var
  Components: IDesignerSelections;
begin
  Components := TDesignerSelections.Create;
  try
    Components.Add(Component);
    GetComponentProperties(Components,
                           tkProperties,
                           Designer,
                           EditConfig
                           , nil);
  finally
    Components := nil;
  end;
end;

procedure TBoldExternalPersistenceHandleEditor.EditConfig(const PropertyEditor: IProperty);
begin
  if SameText(PropertyEditor.GetName, 'Config') then // do not localize
    PropertyEditor.Edit;
end;

procedure TBoldExternalPersistenceHandleEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TBoldExternalPersistenceHandleEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: result := sEditConfiguration;
  end;
end;

function TBoldExternalPersistenceHandleEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

{ TBoldExternalPersistenceHandleSQLPropEdit }

procedure TBoldExternalPersistenceHandleSQLPropEdit.Edit;

var
  i: integer;
  Handle: TBoldExternalPersistenceHandleSQL;
  Form: TBoldExternalPersistenceHandleSQLPropEditorForm;
begin
  Handle := GetComponent(0) as TBoldExternalPersistenceHandleSQL;

  if not Assigned(Handle.BoldModel) then
    raise Exception.Create(sBoldModelNotAssigned);

  Form := TBoldExternalPersistenceHandleSQLPropEditorForm.Create(nil);
  Form.Initialize(Handle.BoldModel.MoldModel, Handle.ClassesToHandle);
  if Form.ShowModal = mrOk then
  begin
    Handle.ClassesToHandle.Clear;
    for i := 0 to Form.CheckListBox1.Items.Count-1 do
      if Form.CheckListBox1.Checked[i] then
        Handle.ClassesToHandle.Add(Form.CheckListBox1.Items[i]);
  end;
  Form.Free;
end;

function TBoldExternalPersistenceHandleSQLPropEdit.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly, paDialog];
end;

function TBoldExternalPersistenceHandleSQLPropEdit.GetValue: string;

  function IsExternal(MoldClass: TMoldClass): Boolean;
  begin
    Result := MoldClass.Storage in [bsPartiallyExternal, bsExternal];
  end;

  function CheckIfAll: String;
  var
    i: integer;
    All: Boolean;
  begin
    Result := '';
    All := True;
    with (GetComponent(0) as TBoldExternalPersistenceHandleSQL) do
    begin
      for i := 0 to BoldModel.MoldModel.Classes.Count-1 do
        if IsExternal(BoldModel.MoldModel.Classes[i]) then
          if ClassesToHandle.IndexOf(BoldModel.MoldModel.Classes[i].ExpandedExpressionName) <> -1 then
            Result := Result + BoldModel.MoldModel.Classes[i].ExpandedExpressionName + ', '
          else
            All := False;
    end;
    if All then
      Result := '(All)'
    else
      if Result <> '' then
        SetLength(Result, Length(Result)-2)
      else
        Result := '(None)';
  end;

var
  i: integer;
begin
  Result := '';
  if PropCount <> 1 then
    Result := '(Multi)'
  else
  with (GetComponent(0) as TBoldExternalPersistenceHandleSQL) do
  begin
    if Assigned(BoldModel) then
      Result := CheckIfAll
    else
    begin
      for i := 0 to ClassesToHandle.Count-1 do
        Result := Result + ClassesToHandle[i] + ', ';
      if Result <> '' then
        SetLength(Result, Length(Result)-2)
      else
        Result := '(None)';
    end;
  end;
end;

end.
