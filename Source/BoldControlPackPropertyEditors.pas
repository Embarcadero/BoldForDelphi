
{ Global compiler directives }
{$include bold.inc}
unit BoldControlPackPropertyEditors;

interface

uses
  Classes,
  DesignEditors,
  DesignIntf,
  BoldElements,
  BoldSystemRt,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors;

type
  {---TBoldRepresentationProperty---}
  TBoldRepresentationProperty = class(TBoldIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(proc: TGetStrProc); override;
    function GetValue: string; override;
    procedure SetValue(const value: string); override;
  end;

  TBoldOCLExpressionForFollowerControllersProperty = class(TBoldOCLExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
    function GetVariableList(Component: TPersistent): TBoldExternalVariableList; override;
  end;

  TBoldOCLExpressionForGenericListPart = class(TBoldOCLExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
  end;

  TBoldTypeNameSelectorPropertyForTreeFollowerController = class(TBoldTypeNameSelectorProperty)
  protected
    function GetApprovedTypes: TBoldValueTypes; override;
    function GetContextType(Component: TPersistent): TBoldSystemTypeInfo; override;
  public
  end;

  {---TBoldSingleFollowerControllerEditor---}
  TBoldSingleFollowerControllerEditor = class(TBoldClassProperty)
  public
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  {---TBoldSingleFollowerControllerEditor---}
  TBoldTreeFollowerControllerEditor = class(TBoldClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  {---TBoldRendererComponentProperty---}
  TBoldRendererComponentProperty = class(TBoldComponentProperty)
  public
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  {---TBoldNodeDescriptionEditor---}
  TBoldNodeDescriptionEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  {---TBoldAsStringRendererEditor---}
  TBoldAsStringRendererEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

  {---TBoldAsIntegerRendererEditor---}
  TBoldAsIntegerRendererEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

  {---TBoldAsFloatRendererEditor---}
  TBoldAsFloatRendererEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

  {---TBoldAsCheckBoxStateRendererEditor---}
  TBoldAsCheckBoxStateRendererEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

  {---TBoldGetAsStringMethodProperty---}
  TBoldGetAsStringMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

  {---TBoldGetAsCheckBoxStateMethodProperty---}
  TBoldGetAsCheckBoxStateMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

  {---TBoldGetAsIntegerEventMethodProperty---}
  TBoldGetAsIntegerEventMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

  {---TBoldGetAsFloatEventMethodProperty---}
  TBoldGetAsFloatEventMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

  {---TBoldGetAsViewerMethodProperty---}
  TBoldGetAsViewerMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

  { TBoldHoldsChangedValueMethodProperty }
  TBoldHoldsChangedValueMethodProperty = class(TBoldOneLinerWithEvalMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
  end;

  { TBoldReleaseChangedValueMethodProperty }
  TBoldReleaseChangedValueMethodProperty = class(TBoldOneLinerWithEvalMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
  end;

  { TBoldMayModifyMethodProperty }
  TBoldMayModifyMethodProperty = class(TBoldOneLinerWithEvalMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
    function GetDeltaLines: integer; override;
  end;

  {---TBoldAsVariantRendererEditor---}
  TBoldAsVariantRendererEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

  {---TBoldGetAsVariantMethodProperty---}
  TBoldGetAsVariantMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function ImplementationTextToInsert: string; override;
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  TypInfo,
  Controls,
  BoldControlPack,
  BoldTreeView,
  BoldDefs,
  BoldDefsDT,
  BoldNodeDescriptionEditor,
  BoldNodeControlPack,
  BoldOCLPropEditor,
  BoldGenericListControlPack;

type
  TBoldFollowerControllerHack = class(TBoldFollowerController);

{---TBoldSingleFollowerContollerEditor---}
function TBoldSingleFollowerControllerEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paMultiSelect];
end;

function TBoldSingleFollowerControllerEditor.GetValue: string;
var
  P: TPersistent;
begin
  p := TPersistent(GetOrdValue);
  if Assigned(P) and (P is TBoldFollowerController) then
  begin
    Result := TBoldFollowerControllerHack(P).Expression;
    if Result = '' then
      Result := '('+TBoldFollowerControllerHack(P).EffectiveRenderer.Name+')';
  end
  else
    Result := inherited GetValue;
end;

procedure TBoldSingleFollowerControllerEditor.Edit;
var
  FollowerController: TBoldSingleFollowerController;
begin
  FollowerController := TObject(GetOrdValue) as TBoldSingleFollowerController;
  if paDialog in GetAttributes then
  begin
    with TBoldOclPropEditForm.Create(nil) do
      try
        OclExpr := FollowerController.Expression;
        Context := FollowerController.ContextType;
        Variables := FollowerController.VariableList;
        if (ShowModal = mrOK) and
        (FollowerController.Expression <> OCLExpr) then
        begin
          FollowerController.Expression := OCLExpr;
          Modified;
        end;
      finally
        Free;
      end;
  end;
end;

{---TBoldAsStringRendererEditor---}
function TBoldAsStringRendererEditor.GetDefaultMethodName: string;
begin
  Result := 'OnGetAsString';
end;

{---TBoldAsIntegerRendererEditor---}
function TBoldAsIntegerRendererEditor.GetDefaultMethodName: string;
begin
  Result := 'OnGetAsInteger';
end;

{---TBoldAsCheckBoxStateRendererEditor---}
function TBoldAsCheckBoxStateRendererEditor.GetDefaultMethodName: string;
begin
  Result := 'OnGetAsCheckBoxState';
end;

{---TBoldGetAsStringMethodProperty---}
function TBoldGetAsStringMethodProperty.ImplementationTextToInsert: string;
begin
  Result := '';
{$IFDEF BOLD_DELPHI}
  Result :=                 '  Result := '''';' + BOLDCRLF;
{$ENDIF}
  Result := Result + Format('  if %s(AFollower.Element) %s', [BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_END]);
end;

function TBoldGetAsStringMethodProperty.GetDeltaLines: integer;
begin
  Result := -1;
end;

function TBoldGetAsStringMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;

{---TBoldGetAsCheckBoxStateMethodProperty---}
function TBoldGetAsCheckBoxStateMethodProperty.ImplementationTextToInsert: string;
begin
  Result := '';
{$IFDEF BOLD_DELPHI}
  Result :=                 '  Result := cbGrayed;' + BOLDCRLF;
{$ENDIF}
  Result := Result + Format('  if %s(AFollower.Element) %s', [BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_END]);
end;

function TBoldGetAsCheckBoxStateMethodProperty.GetDeltaLines: integer;
begin
  Result := -1;
end;

function TBoldGetAsCheckBoxStateMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;

{---TBoldGetAsIntegerEventMethodProperty---}
function TBoldGetAsIntegerEventMethodProperty.ImplementationTextToInsert: string;
begin
  Result := '';
{$IFDEF BOLD_DELPHI}
  Result :=                 '  Result := 0;' + BOLDCRLF;
{$ENDIF}
  Result := Result + Format('  if %s(AFollower.Element) %s', [BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_END]);
end;

function TBoldGetAsIntegerEventMethodProperty.GetDeltaLines: integer;
begin
  Result := -1;
end;

function TBoldGetAsIntegerEventMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;

{--- TBoldHoldsChangedValueMethodProperty ---}

function TBoldHoldsChangedValueMethodProperty.ImplementationTextToInsert: string;
begin
  result := Format('  AFollower.Element%sEvaluateExpressionAsDirectElement(%s%s).RegisterModifiedValueHolder(AFollower.Subscriber);',
                   [BOLDSYM_POINTERDEREFERENCE, BOLDSYM_QUOTECHAR, BOLDSYM_QUOTECHAR]);
end;

{--- TBoldReleaseChangedValueMethodProperty ---}

function TBoldReleaseChangedValueMethodProperty.ImplementationTextToInsert: string;
begin
  result := Format('  AFollower.Element%sEvaluateExpressionAsDirectElement(%s%s).UnRegisterModifiedValueHolder(AFollower.Subscriber);',
                   [BOLDSYM_POINTERDEREFERENCE, BOLDSYM_QUOTECHAR, BOLDSYM_QUOTECHAR]);
end;

function TBoldMayModifyMethodProperty.ImplementationTextToInsert: string;
begin
  result := Format('  %sresult %s AFollower.Element%sEvaluateExpressionAsDirectElement(%s%s).ObserverMayModify(AFollower.subscriber);',
                   [BOLDSYM_TYPEINTEGER, BOLDSYM_ASSIGNMENT, BOLDSYM_POINTERDEREFERENCE, BOLDSYM_QUOTECHAR, BOLDSYM_QUOTECHAR]);
  Result := Result + BOLDSYM_RETURNRESULT;
end;

function TBoldMayModifyMethodProperty.GetDeltaLines: integer;
begin
  Result := -CharCount(BOLDLF, BOLDSYM_RETURNRESULT);
end;

{---TBoldGetAsViewerMethodProperty---}
function TBoldGetAsViewerMethodProperty.ImplementationTextToInsert: string;
begin
  Result := '';
{$IFDEF BOLD_DELPHI}
  Result :=                 '  Result := nil;' + BOLDCRLF;
{$ENDIF}
  Result := Result + Format('  if %s(AFollower.Element) %s', [BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_END]);
end;

function TBoldGetAsViewerMethodProperty.GetDeltaLines: integer;
begin
  Result := -1;
end;

function TBoldGetAsViewerMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;

{---TBoldRepresentationProperty---}
function TBoldRepresentationProperty.GetAttributes: TPropertyAttributes;
begin
  result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TBoldRepresentationProperty.GetValues(proc: TGetStrProc);
var
  I: Integer;
  P: TPersistent;
begin
  P := GetComponent(0);
  if Assigned(P) and (P is TBoldFollowerController) then
    with TBoldFollowerControllerHack(P).EffectiveRenderer do
      for I := 0 to Representations.Count-1 do
        Proc(Representations[I]);
end;

function TBoldRepresentationProperty.GetValue: string;
var
  I: Integer;
  S: string;
  P: TPersistent;
begin
  P := GetComponent(0);
  if Assigned(P) and (P is TBoldFollowerController) then
  begin
    I := GetOrdValue;
    S := IntToStr(I);
    with TBoldFollowerControllerHack(P).EffectiveRenderer do
      if Representations.Values[S]<>'' then
        Result := Format('%d=%s', [I, Representations.Values[S]])
      else
        Result := inherited GetValue;
  end
  else
    Result := inherited GetValue;
end;

procedure TBoldRepresentationProperty.SetValue(const value: string);
var
  I: Integer;
begin
  I := Pos('=', Value);
  if I=0 then
    inherited SetValue(Value)
  else
    inherited SetValue(Trim(Copy(Value, 1, I-1)));
end;

{---TBoldRendererComponentProperty---}
function TBoldRendererComponentProperty.GetValue: string;
begin
  Result := Designer.GetComponentName(TComponent(GetOrdValue));
  if (Result='') then
    Result := '(default)';
end;

procedure TBoldRendererComponentProperty.GetValues(Proc: TGetStrProc);
begin
  Proc('(default)');
  Designer.GetComponentNames(GetTypeData(GetPropType), Proc);
end;

procedure TBoldRendererComponentProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
  if (Value = '') or (Value[1]='(') then
    Component := nil
  else
  begin
    Component := Designer.GetComponent(Value);
    if not (Component is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.Create('Invalid property value');
  end;
  SetOrdValue(Longint(Component));
end;

{---TBoldNodeDescriptionEditor---}
type
  TExposedTreeView = class(TBoldCustomTreeView);

procedure TBoldNodeDescriptionEditor.ExecuteVerb(Index: Integer);
begin
  if Component is TBoldCustomTreeView then
    TFormNodeEditor.Edit(Component, TExposedTreeView(Component).BoldProperties, Designer);
  Designer.Modified;
end;

function TBoldNodeDescriptionEditor.GetVerb(Index: Integer): string;
begin
  Result := '&Edit Node Descriptions...';
end;

function TBoldNodeDescriptionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

{ TBoldOCLExpressionForFollowerControllersProperty }

function TBoldOCLExpressionForFollowerControllersProperty.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
begin
  EnsureComponentType(Component, TBoldFollowerController);
  Result := (Component as TBoldFollowerController).ContextType;
end;

function TBoldOCLExpressionForFollowerControllersProperty.GetVariableList(
  Component: TPersistent): TBoldExternalVariableList;
begin
  EnsureComponentType(Component, TBoldFollowerController);
  Result := (Component as TBoldFollowerController).VariableList;
end;

{ TBoldOCLExpressionForGenericListPart }

function TBoldOCLExpressionForGenericListPart.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
begin
  EnsureComponentType(Component, TBoldGenericListPart);
  Result := (Component as TBoldGenericListPart).ContextType;
end;


{ TBoldTypeNameSelectorPropertyForTreeFollowerController }

function TBoldTypeNameSelectorPropertyForTreeFollowerController.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtAttr, bvtList, bvtClass, bvtSystem, bvtType];
end;

function TBoldTypeNameSelectorPropertyForTreeFollowerController.GetContextType(
  Component: TPersistent): TBoldSystemTypeInfo;
begin
  if Component is TBoldNodeDescription then
    result := ((Component as TBoldNodeDescription).NodeFollowerController.SystemTypeInfo)
  else
    result := nil;
end;

{ TBoldAsFloatRendererEditor }

function TBoldAsFloatRendererEditor.GetDefaultMethodName: string;
begin
  Result := 'OnGetAsFloat';
end;

{ TBoldGetAsFloatEventMethodProperty }

function TBoldGetAsFloatEventMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;

function TBoldGetAsFloatEventMethodProperty.GetDeltaLines: integer;
begin
  Result := -1;
end;

function TBoldGetAsFloatEventMethodProperty.ImplementationTextToInsert: string;
begin
  Result := '';
{$IFDEF BOLD_DELPHI}
  Result :=                 '  Result := 0;' + BOLDCRLF;
{$ENDIF}
  Result := Result + Format('  if %s(AFollower.Element) %s', [BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF;
  Result := Result + Format('  %s', [BOLDSYM_END]);
end;

{ TBoldTreeFollowerControllerEditor }

procedure TBoldTreeFollowerControllerEditor.Edit;
var
  Properties: TBoldTreeFollowerController;
begin
  if (TObject(GetOrdValue) is TBoldTreeFollowerController) and
    (GetComponent(0) is TComponent) then
  begin
    Properties := TObject(GetOrdValue) as TBoldTreeFollowerController;
    TFormNodeEditor.Edit(GetComponent(0) as TComponent, Properties, Designer);
    Designer.Modified;
  end
  else
    raise EPropertyError.Create('Invalid property value');
end;

function TBoldTreeFollowerControllerEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog] - [paMultiSelect];
end;

{ TBoldAsVariantRendererEditor }

function TBoldAsVariantRendererEditor.GetDefaultMethodName: string;
begin
  Result := 'OnGetAsVariant'; // do not localize
end;

{ TBoldGetAsVariantMethodProperty }

function TBoldGetAsVariantMethodProperty.GetColPos: integer;
begin
  Result := 5;
end;
 
function TBoldGetAsVariantMethodProperty.GetDeltaLines: integer;
begin
  Result := -1;
end;
 
function TBoldGetAsVariantMethodProperty.ImplementationTextToInsert: string;
begin
  Result := '';
{$IFDEF BOLD_DELPHI}
  Result :=                 '  Result := Null;' + BOLDCRLF;  // do not localize
{$ENDIF}
  Result := Result + Format('  if %s(AFollower.Element) %s', [BOLDSYM_ASSIGNED, BOLDSYM_THEN]) + BOLDCRLF;  // do not localize
  Result := Result + Format('  %s', [BOLDSYM_BEGIN]) + BOLDCRLF + BOLDCRLF; // do not localize
  Result := Result + Format('  %s', [BOLDSYM_END]);  // do not localize
end;

end.
