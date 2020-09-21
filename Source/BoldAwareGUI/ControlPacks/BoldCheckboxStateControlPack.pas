unit BoldCheckboxStateControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  StdCtrls, // TCheckBoxState
  BoldDefs,
  BoldElements,
  BoldControlPack,
  BoldSubscription;

type
  {Forward declaration of classes}
  TBoldCheckBoxStateFollowerController = class;
  TBoldAsCheckBoxStateRenderer = class;
  TBoldCheckBoxRendererData = class;

  { TBoldAsCheckBoxStateRenderer }
  TBoldGetAsCheckBoxState = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TCheckBoxState of object;
  TBoldSetAsCheckBoxState = procedure (Element: TBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldValidateCheckBoxState = function (Element: TBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;
  TBoldCheckBoxIsChanged = function (RendererData: TBoldCheckBoxRendererData; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldCheckBoxRendererData }
  TBoldCheckBoxRendererData = class(TBoldRendererData)
  private
    fOldvalue: TCheckBoxState;
    fCurrentValue: TCheckBoxState;
  public
    property Oldvalue: TCheckBoxState read fOldvalue write fOldvalue;
    property CurrentValue: TCheckBoxState read fCurrentValue write fCurrentValue;
  end;

  { TBoldAsCheckBoxStateRenderer }
  TBoldAsCheckBoxStateRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsCheckBoxState: TBoldGetAsCheckBoxState;
    FOnSetAsCheckBoxState: TBoldSetAsCheckBoxState;
    FOnValidate: TBoldValidateCheckBoxState;
    fOnIsChanged: TBoldCheckBoxIsChanged;
  protected
    function GetRendererDataClass: TBoldRendererDataClass; override;
  public
    class function DefaultRenderer: TBoldAsCheckBoxStateRenderer;
    class function DefaultGetAsCheckBoxStateAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TCheckBoxState; virtual;
    class procedure DefaultSetAsCheckBoxState(Element: TBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    class function DefaultValidateCheckBoxState(Element: TBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;  VariableList: TBoldExternalVariableList): Boolean; virtual;
    procedure MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); override;
    function DefaultIsChanged(RendererData: TBoldCheckBoxRendererData; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;  VariableList: TBoldExternalVariableList): Boolean;
    function GetAsCheckBoxStateAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TCheckBoxState; virtual;
    procedure SetAsCheckBoxState(Element: TBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    function ValidateCheckBoxState(Element: TBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;VariableList: TBoldExternalVariableList): Boolean; virtual;
    function IsChanged(RendererData: TBoldCheckBoxRendererData; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
  published
    property OnGetAsCheckBoxState: TBoldGetAsCheckBoxState read FOnGetAsCheckBoxState write FOnGetAsCheckBoxState;
    property OnSetAsCheckBoxState: TBoldSetAsCheckBoxState read FOnSetAsCheckBoxState write FOnSetAsCheckBoxState;
    property OnValidateCheckBoxState: TBoldValidateCheckBoxState read FOnValidate write FOnValidate;
    property OnIsChanged: TBoldCheckBoxIsChanged read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldCheckBoxStateFollowerController }
  TBoldCheckBoxStateFollowerController = class(TBoldSingleFollowerController)
  private
    function GetEffectiveAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRenderer;
    function GetRenderer: TBoldAsCheckBoxStateRenderer;
    procedure SetRenderer(Value: TBoldAsCheckBoxStateRenderer);
  protected
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRenderer read GetEffectiveAsCheckBoxStateRenderer;
  public
    function GetCurrentAsCheckBoxState(Follower: TBoldFollower): TCheckBoxState;
    procedure MakeClean(Follower: TBoldFollower); override;
    procedure MayHaveChanged(NewValue: TCheckBoxState; Follower: TBoldFollower);
    procedure SetAsCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollower);
    function ValidateCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollower): Boolean;
  published
    property Renderer: TBoldAsCheckBoxStateRenderer read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldGuiResourceStrings,
  BoldControlPackDefs,
  BoldAttributes;

var
  DefaultAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRenderer;

{ TBoldCheckBoxStateFollowerController }

function TBoldCheckBoxStateFollowerController.GetRenderer: TBoldAsCheckBoxStateRenderer;
begin
  Result := UntypedRenderer as TBoldAsCheckBoxStateRenderer;
end;

procedure TBoldCheckBoxStateFollowerController.SetRenderer(Value: TBoldAsCheckBoxStateRenderer);
begin
  UntypedRenderer := Value;
end;

function TBoldCheckBoxStateFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsCheckBoxStateRenderer;
end;

function TBoldCheckBoxStateFollowerController.GetEffectiveAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsCheckBoxStateRenderer.DefaultRenderer;
end;

function TBoldCheckBoxStateFollowerController.GetCurrentAsCheckBoxState(Follower: TBoldFollower): TCheckBoxState;
begin
  Result := (Follower.RendererData as TBoldCheckBoxRendererData).CurrentValue;
end;

procedure TBoldCheckBoxStateFollowerController.SetAsCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollower);
begin
  EffectiveAsCheckBoxStateRenderer.SetAsCheckBoxState(Follower.Element, Value, Representation, Expression, VariableList);
end;

function TBoldCheckBoxStateFollowerController.ValidateCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsCheckBoxStateRenderer.ValidateCheckBoxState(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldCheckBoxStateFollowerController.MayHaveChanged(NewValue: TCheckBoxState; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldCheckBoxRendererData).CurrentValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsCheckBoxStateRenderer.IsChanged(Follower.RendererData as TBoldCheckBoxRendererData, NewValue, Representation, Expression, VariableList));
  end;
end;

procedure TBoldCheckBoxStateFollowerController.MakeClean(Follower: TBoldFollower);
begin
  ReleaseChangedValue(Follower); // note, must do first, since set can change element
  SetAsCheckBoxState(GetCurrentAsCheckBoxState(Follower), Follower);
end;

{ TBoldAsCheckBoxStateRenderer }
procedure TBoldAsCheckBoxStateRenderer.MakeUpToDateANdSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber);
var
  s: TCheckBoxState;
  Controller: TBoldCheckBoxStateFollowerController;
begin
  Controller := FollowerController as TBoldCheckBoxStateFollowerController;
  s := GetAsCheckBoxStateAndSubscribe(Element, Controller.Representation, Controller.Expression,  Controller.GetVariableListAndSubscribe(Subscriber), Subscriber);
  (RendererData as TBoldCheckBoxRendererData).OldValue := s;
  (RendererData as TBoldCheckBoxRendererData).CurrentValue := s;
end;

class function TBoldAsCheckBoxStateRenderer.DefaultGetAsCheckBoxStateAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TCheckBoxState;
var
  {$IFDEF BOLDCOMCLIENT} // DefaultGet
  e: IBoldElement;
  Attribute: IBoldAttribute;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
begin
  Result := cbGrayed;
  if Assigned(Element) then
  begin
    {$IFDEF BOLDCOMCLIENT} // defaultGet
    if assigned(Subscriber) then
      e := Element.EvaluateAndSubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, false, false)
    else
      e := Element.EvaluateExpression(Expression);
    if E.QueryInterface(IBoldAttribute, Attribute) = S_OK then
    begin
      if attribute.IsNull then
        Result := cbGrayed
      else if attribute.AsVariant then
        Result := cbChecked
      else
        Result := cbUnchecked;
    end;
    {$ELSE}
    E := TBoldIndirectElement.Create;
    try
      Element.EvaluateAndSubscribeToExpression(Expression, Subscriber, E, False, False, VariableList);
      if E.Value is TBABoolean then
        with E.Value as TBABoolean do
          if IsNull then
            Result := cbGrayed
          else if AsBoolean then
            Result := cbChecked
          else
            Result := cbUnchecked;
    finally
      E.Free;
    end;
    {$ENDIF}
  end;
end;

class procedure TBoldAsCheckBoxStateRenderer.DefaultSetAsCheckBoxState(Element: TBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  ValueElement: TBoldElement;
  {$IFDEF BOLDCOMCLIENT} // defaulSet
  Attribute: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  if valueElement.QueryInterface(IBoldAttribute, Attribute) = S_OK then
  begin
     if NewValue = cbGrayed then
       Attribute.SetToNull
     else if Newvalue = cbChecked then
       Attribute.AsVariant := true
     else
       Attribute.AsVariant := false;
  {$ELSE}
  if ValueElement is TBABoolean then
  begin
    with ValueElement as TBABoolean do
    begin
      if newValue = cbGrayed then
        SetToNull
      else if newValue = cbChecked then
        AsBoolean := True
      else
        AsBoolean := False;
    end;
  {$ENDIF}
  end
  else
    raise EBold.CreateFmt(sCannotSetValue, [ClassName]);
end;

function TBoldAsCheckBoxStateRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldCheckBoxRendererData;
end;

  class function TBoldAsCheckBoxStateRenderer.DefaultValidateCheckBoxState(Element: TBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := True;
end;

function TBoldAsCheckBoxStateRenderer.GetAsCheckBoxStateAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TCheckBoxState;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
       OnSubscribe(Element, Representation, Expression, Subscriber);
    Subscriber := nil;
  end;

  if Assigned(OnGetAsCheckBoxState) then
    Result := OnGetAsCheckBoxState(Element, Representation, Expression)
  else
    Result := DefaultGetAsCheckBoxStateAndSubscribe(Element, Representation, Expression, VariableList, Subscriber);
end;

procedure TBoldAsCheckBoxStateRenderer.SetAsCheckBoxState(Element: TBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;VariableList: TBoldExternalVariableList);
begin
  if Assigned(OnSetAsCheckBoxState) then
    OnSetAsCheckBoxState(Element, Value, Representation, Expression)
  else
    DefaultSetAsCheckBoxState(Element, Value, Representation, Expression, VariableList);
end;

function TBoldAsCheckBoxStateRenderer.ValidateCheckBoxState(Element: TBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(OnValidateCheckBoxState) then
    Result := OnValidateCheckBoxState(Element, Value, Representation, Expression)
  else
    Result := DefaultValidateCheckBoxState(Element, Value, Representation, Expression, VariableList);
end;

  class function TBoldAsCheckBoxStateRenderer.DefaultRenderer: TBoldAsCheckBoxStateRenderer;
begin
  Result := DefaultAsCheckBoxStateRenderer;
end;

function TBoldAsCheckBoxStateRenderer.DefaultIsChanged(RendererData: TBoldCheckBoxRendererData; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> TBoldCheckBoxRendererData(RendererData).OldValue;
end;

function TBoldAsCheckBoxStateRenderer.IsChanged(RendererData: TBoldCheckBoxRendererData; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
end;

initialization
  DefaultAsCheckBoxStateRenderer := TBoldAsCheckBoxStateRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsCheckBoxStateRenderer);

end.
