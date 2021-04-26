
{ Global compiler directives }
{$include bold.inc}
unit BoldPropertyEditors;

interface

uses
  Classes,
  ExtCtrls,
  TypInfo,
  DesignIntf,
  DesignEditors,
  BoldElements,
  BoldSystemRt,
  BoldAbstractPropertyEditors,
  BoldModelAwareComponentEditor;

type
  TBoldStringSelectionProperty = class;
  TBoldComponentDblClickEditor = class;
  TBoldStringListEditor = class;
  TBoldFileNameProperty = class;

  {TBoldStringSelectionProperty}
  TBoldStringSelectionProperty = class(TBoldStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure GetValueList(List: TStrings); virtual; abstract;
  end;

  { TBoldOclExpressionProperty }
  TBoldOclExpressionProperty = class(TBoldStringProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; virtual; abstract;
    function GetVariableList(Component: TPersistent): TBoldExternalVariableList; virtual;
    procedure EnsureComponentType(Component: TPersistent; ComponentClass: TClass);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TBoldOclExpressionForOclComponent }
  TBoldOclExpressionForOclComponent = class(TBoldOclExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
    function GetVariableList(Component: TPersistent): TBoldExternalVariableList; override;
  end;

  {---TfrmBoldStaticTypeName property---}
  TBoldTypeNameSelectorProperty = class(TBoldStringProperty)
  protected
    function GetApprovedTypes: TBoldValueTypes; virtual; abstract;
    function GetContextType(Component: TPersistent): TBoldSystemTypeInfo; virtual; abstract;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  { TBoldTypeNameSelectorForHandles }
  TBoldTypeNameSelectorForHandles = class(TBoldTypeNameSelectorProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldSystemTypeInfo; override;
  end;

  { TBoldTypeNameSelectorForSQLHandle }
  TBoldTypeNameSelectorForSQLHandle = class (TBoldTypeNameSelectorForHandles)
  protected
    function GetApprovedTypes: TBoldValueTypes; override;
  end;

  { TBoldTypeNameSelectorPropertyForAllTypes }
  TBoldTypeNameSelectorPropertyForAllTypes = class(TBoldTypeNameSelectorForHandles)
  protected
    function GetApprovedTypes: TBoldValueTypes; override;
  end;

  { TBoldTypeNameSelectorPropertyForVariableHandle }
  TBoldTypeNameSelectorPropertyForVariableHandle = class(TBoldTypeNameSelectorForHandles)
  protected
    function GetApprovedTypes: TBoldValueTypes; override;
  end;

  { TBoldDesignTimeContextEditor }
  TBoldDesignTimeContextEditor = class(TBoldStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(proc: TGetStrProc); override;
  end;

  { TBoldGUIOclEditor }
  TBoldOclEditor = class(TBoldModelAwareComponentEditor)
  protected
    function GetContextType: TBoldElementTypeInfo; virtual; abstract;
    function GetVariablesFromComponent: TBoldExternalVariableList; virtual;
    function GetExpressionFromComponent: string; virtual; abstract;
    procedure SetExpressionInComponent(Expression: String); virtual; abstract;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TBoldOclComponentEditor }
  TBoldOclComponentEditor = class(TBoldOclEditor)
  protected
    function GetOclComponent: IBoldOclComponent;
    function GetContextType: TBoldElementTypeInfo; override;
    function GetVariablesFromComponent: TBoldExternalVariableList; override;
    function GetExpressionFromComponent: string; override;
    procedure SetExpressionInComponent(Expression: String); override;
  end;

  { TBoldComponentDblClickEditor }
  TBoldComponentDblClickEditor = class(TDefaultEditor)
  protected
    function GetDefaultMethodName: string; virtual; abstract;
  public
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    property DefaultMethodName: string read GetDefaultMethodName;
  end;

  { TBoldHandleEditor }
  TBoldHandleEditor = class(TBoldComponentDblClickEditor)
  protected
    procedure EditHandleOcl;
  public
    function GetDefaultMethodName: string; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  { TBoldStringListEditor }
  TBoldStringListEditor = class(TDefaultEditor)
  protected
    fTimer: TTimer;
    procedure StartTimer;
    procedure TimerTick(Sender: TObject);
    function DerivedFrom(TypeInfo: PTypeInfo; const aClass: TClass): boolean;
  public
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  end;

  {---TBoldFileNameProperty---}
  TBoldFileNameProperty = class(TBoldStringProperty)
  protected
    function FileFilter: string; virtual; abstract;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  {---TBoldElementSubscribeMethodProperty---}
  TBoldElementSubscribeMethodProperty = class(TBoldOneLinerWithEvalMethodProperty)
  public
    function TextToInsert: string; override;
  end;

  {---TBoldElementFilterMethodProperty---}
  TBoldElementFilterMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function TextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

  {---TBoldElementCompareMethodProperty---}
  TBoldElementCompareMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function TextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  Forms,
  StdCtrls,
  Controls,
  Dialogs,
  BoldHandles,
  BoldIndex,
  BoldIndexableList,
  BoldMetaElementList,
  BoldDefs,
  BoldDefsDT,
  BoldOclPropEditor,
  BoldTypeNameSelector,
  BoldCommonConst;

{ TBoldStringSelectionProperty }
function TBoldStringSelectionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TBoldStringSelectionProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do
      Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

procedure TBoldComponentDblClickEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
  if CompareText(PropertyEditor.GetName, DefaultMethodName) = 0 then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

{---TBoldStringListEditor---}
function TBoldStringListEditor.DerivedFrom(TypeInfo: PTypeInfo;
  const aClass: TClass): boolean;
var
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(TypeInfo);
  Result := //(TypeData^.TypeKind = tkClass) and
            aClass.InheritsFrom(TypeData^.ClassType);
end;

procedure TBoldStringListEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
  inherited;
  // Note:
  //  Other possiblilities include name mapping the property using
  //  PropertyEditor.GetName
  if DerivedFrom(PropertyEditor.GetPropType, TStringList) then
//  if PropertyEditor.GetPropType^.Name = 'TStringList' then
  begin
    StartTimer;
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TBoldStringListEditor.StartTimer;
begin
  fTimer := TTimer.Create(nil);
  with fTimer do
  begin
    Interval := 50;
    OnTimer := TimerTick;
    Enabled := True;
  end;
end;

procedure TBoldStringListEditor.TimerTick(Sender: TObject);
var
  Button: TButton;
begin
  if Screen.ActiveForm.Name = 'StrEditDlg' then // do not localize
  begin
    Button := TButton(Screen.ActiveForm.FindChildControl('CodeWndBtn')); // do not localize
    if Assigned(Button) then
      Button.Click;
    FreeAndNil(fTimer);
  end;
end;

{ TBoldFileNameProperty }

procedure TBoldFileNameProperty.Edit;
begin
  with TOpenDialog.Create(Application) do
  begin
    Filename := GetValue;
    Filter := FileFilter + '|' + ALLFILESFILTER;
    Options := Options + [ofShowHelp, ofPathMustExist, ofFileMustExist];
    try
      if Execute then SetValue(FileName);
    finally
      Free;
    end;
  end;
end;

function TBoldFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

{---TBoldElementSubscribeMethodProperty---}
function TBoldElementSubscribeMethodProperty.TextToInsert: string;
begin
  Result := Format('  Element%sSubscribeToExpression(%s%s, Subscriber, False);', [BOLDSYM_POINTERDEREFERENCE, BOLDSYM_QUOTECHAR, BOLDSYM_QUOTECHAR]); // do not localize
end;

{---TBoldElementFilterMethodProperty---}
function TBoldElementFilterMethodProperty.TextToInsert: string;
begin
  Result := '';
{$IFDEF BOLD_DELPHI}
  Result :=                 '  Result := false;' + BOLDCRLF; // do not localize
{$ENDIF}
  Result := result + Format('  if %s(Element) %s', [BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF; // do not localize
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF; // do not localize
  Result := Result + Format('  %s', [BOLDSYM_END]); // do not localize
end;

function TBoldElementFilterMethodProperty.GetDeltaLines: integer;
begin
  Result := -1;
end;

function TBoldElementFilterMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;

{---TBoldElementCompareMethodProperty---}
function TBoldElementCompareMethodProperty.TextToInsert: string;
begin
  Result :=          Format('  %sResult %s 0;', [BOLDSYM_TYPEINTEGER, BOLDSYM_ASSIGNMENT]) + BOLDCRLF; // do not localize
  Result := Result + Format('  if %s(item1) %s %s(item2) %s', [BOLDSYM_ASSIGNED, BOLDSYM_AND, BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF; // do not localize
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF; // do not localize
  Result := Result + Format('  %s', [BOLDSYM_END]); // do not localize
  Result := Result + BOLDSYM_RETURNRESULT;
end;

function TBoldElementCompareMethodProperty.GetDeltaLines: integer;
begin
  Result := -1 - CharCount(BOLDLF, BOLDSYM_RETURNRESULT);
end;

function TBoldElementCompareMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;

{---TBoldHandleEditor---}
procedure TBoldHandleEditor.EditHandleOcl;
var
  ExpressionPropInfo,
  PropInfo: PPropInfo;
  BoldHandle: TBoldElementHandle;
  i: integer;
  oclComponent: IBoldOclComponent;
  ComponentList: IDesignerSelections;
  c: TComponent;
begin
  // Can we access a boldhandle?
  Propinfo := TypInfo.GetPropInfo(Component.ClassInfo, 'BoldHandle'); // do not localize
  if Assigned(PropInfo) then
  begin
    with TBoldOclPropEditForm.Create(nil) do
    try
      ExpressionPropInfo := TypInfo.GetPropInfo(Component.ClassInfo, 'Expression'); // do not localize
      OclExpr := GetStrProp(Component, ExpressionPropinfo);
      BoldHandle := TBoldElementHandle(GetOrdProp(Component, PropInfo));
      if assigned(BoldHandle) then
      begin
        Context := BoldHandle.StaticBoldType;

        if BoldHandle.GetInterface(IBoldOCLComponent, OclComponent) then
          variables := OclComponent.VariableList;
      end
      else
        Context := nil;
      if ShowModal = mrOK then
        //Original line
//        SetStrProp(Component, ExpressionPropInfo, OclExpr);
      begin
        ComponentList := TDesignerSelections.Create;
        (Designer as IDesigner).GetSelections(ComponentList);
        for i := 0 to ComponentList.Count - 1 do
        begin
          if ComponentList[i] is TComponent then
            c := ComponentList[i] as TComponent
          else
            c := nil;
          if Assigned(c) then
          begin
            ExpressionPropInfo := TypInfo.GetPropInfo(c.ClassInfo, 'Expression'); // do not localize
            if Assigned(ExpressionPropInfo) then
            begin
              SetStrProp(c, ExpressionPropInfo, OclExpr);
              Designer.Modified;
            end;
          end;
        end;
      end;
    finally
      Free;
    end;
  end;
end;

function TBoldHandleEditor.GetDefaultMethodName: string;
begin
  Result := 'Expression'; // do not localize
end;

procedure TBoldHandleEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: EditHandleOcl;
  end;
end;

function TBoldHandleEditor.GetVerb(Index: Integer): string;
begin
  Result := sEditOCL;
end;

function TBoldHandleEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBoldDesignTimeContextEditor }

function TBoldDesignTimeContextEditor.GetAttributes: TPropertyAttributes;
begin
  result := [paMultiSelect, paValueList, paRevertable]
end;

procedure TBoldDesignTimeContextEditor.GetValues(proc: TGetStrProc);
var
  ValueTypeNameList: TBoldElementTypeInfoList;
  i: integer;
  SystemTypeInfo: TBoldSystemTypeInfo;
begin
  Assert (GetComponent(0) is TBoldElementHandle);
  SystemTypeInfo := TBoldElementHandle(GetComponent(0)).StaticSystemTypeInfo;
  if Assigned(SystemTypeInfo) then
  begin
    ValueTypeNameList := SystemTypeInfo.ValueTypeNameList;
    for i := 0 to ValuetypeNamelist.Count-1 do
      Proc(ValueTypeNameList[i].AsString);
  end;
end;

{---TBoldGUIOclEditor---}
procedure TBoldOclEditor.ExecuteVerb(Index: Integer);
begin
  case index of
    0: begin
      with TBoldOclPropEditForm.Create(nil) do
      try
        OclExpr := GetExpressionFromComponent;
        Variables := GetVariablesFromComponent;
        Context := GetContextType;
        if (ShowModal = mrOK) and
          (OclExpr <> GetExpressionFromComponent) then
        begin
          SetExpressionInComponent(OclExpr);
          // note: "Designer" is the designer of the Ocleditorform
          // while "self.designer" is the designer of the prop-editor
          self.Designer.Modified;
        end;
      finally
        Free;
      end;
    end;
    1: EditModel;
  end;
end;

function TBoldOclEditor.GetVariablesFromComponent: TBoldExternalVariableList;
begin
  result := nil;
end;

function TBoldOclEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:  Result := sEditOCL;
    1:  Result := GetEditModelMenuCaption;
  end;
end;

function TBoldOclEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{---TBoldOclExpressionProperty---}
function TBoldOclExpressionProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paMultiSelect];
end;

procedure TBoldOclExpressionProperty.Edit;
begin
  with TBoldOclPropEditForm.Create(nil) do
  try
    OclExpr := GetValue;
    Context := GetContextType(GetComponent(0));
    Variables := GetVariableList(GetComponent(0));

    if ShowModal = mrOK then
    begin
      SetValue(OclExpr);
    end;
  finally
    Free;
  end;
end;

{ TBoldOclComponentEditor }

function TBoldOclComponentEditor.GetContextType: TBoldElementTypeInfo;
begin
  result := GetOclComponent.ContextType;
end;

function TBoldOclComponentEditor.GetExpressionFromComponent: string;
begin
  result := GetOclComponent.Expression;
end;

function TBoldOclComponentEditor.GetOclComponent: IBoldOclComponent;
begin
  if not Component.GetInterface(IBoldOclComponent, result) then
    raise EBold.CreateFmt(sComponentNotIBoldOCLComponent, [ClassName, Component.Name, Component.ClassName]);
end;

function TBoldOclComponentEditor.GetVariablesFromComponent: TBoldExternalVariableList;
begin
  result := GetOclComponent.VariableList;
end;

procedure TBoldOclComponentEditor.SetExpressionInComponent(Expression: String);
begin
  GetOclComponent.Expression := Expression;
end;

{ TBoldOclExpressionForListHandle }

function TBoldOclExpressionForOclComponent.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
var OclComponent: IBoldOclComponent;
begin
  if Component.GetInterface(IBoldOclComponent, OclComponent) then
    result := OclComponent.ContextType
  else
    raise EBold.CreateFmt(sCouldNotGetIBoldOCLComponent, [ClassName, 'GetContextType']); // do not localize
end;

{ TBoldTypeNameSelectorProperty }

procedure TBoldTypeNameSelectorProperty.Edit;
var
  TypeValue: String;
  Context: TBoldSystemTypeINfo;
  form: TfrmBoldTypeNameSelector;
begin
  Context := GetContextType(GetComponent(0));
  if assigned(Context) then
  begin
    form := TfrmBoldTypeNameSelector.Create(nil);
    try
      TypeValue := Value;
      if form.Select(TypeValue, Context, GetApprovedTypes) = mrOK then
        Value := TypeValue;
    finally
      form.Free;
    end;
  end
  else
    ShowMessage(sNeedContextToEditType);
end;

function TBoldTypeNameSelectorProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paMultiSelect];
end;

{ TBoldTypeNameSelectorForHandles }

function TBoldTypeNameSelectorForHandles.GetContextType(
  Component: TPersistent): TBoldSystemTypeInfo;
begin
  if Component.InheritsFrom(TBoldNonSystemHandle) then
    result := (Component as TBoldNonSystemHandle).StaticSystemTypeInfo
  else
    result := nil;
end;

function TBoldTypeNameSelectorPropertyForAllTypes.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtAttr, bvtList, bvtClass, bvtSystem, bvtType];
end;

{ TBoldTypeNameSelectorPropertyForVariableHandle }

function TBoldTypeNameSelectorPropertyForVariableHandle.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtAttr, bvtList];
end;

procedure TBoldOclExpressionProperty.EnsureComponentType(
  Component: TPersistent; ComponentClass: TClass);
begin
  if not(Component is ComponentClass) then
    raise EBold.CreateFmt(sUnexpectedClass, [ClassName, Component.ClassName, Componentclass.ClassName]);
end;

{ TBoldTypeNameSelectorForSQLHandle }

function TBoldTypeNameSelectorForSQLHandle.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtClass];
end;

function TBoldOclExpressionProperty.GetVariableList(Component: TPersistent): TBoldExternalVariableList;
begin
  result := nil;
end;

function TBoldOclExpressionForOclComponent.GetVariableList(
  Component: TPersistent): TBoldExternalVariableList;
var
  OclComponent: IBoldOclComponent;
begin
  if Component.GetInterface(IBoldOclComponent, OclComponent) then
    result := OclComponent.VariableList
  else
    raise EBold.CreateFmt(sCouldNotGetIBoldOCLComponent, [ClassName, 'GetVariableList']); // do not localize
end;

end.
