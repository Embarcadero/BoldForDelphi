{ Global compiler directives }
{$include bold.inc}

unit BoldCheckboxStateControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Classes,
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
  TBoldGetAsCheckBoxState = function (aFollower: TBoldFollower): TCheckBoxState of object;
  TBoldSetAsCheckBoxState = procedure (aFollower: TBoldFollower; newValue: TCheckBoxState) of object;
  TBoldValidateCheckBoxState = function (aFollower: TBoldFollower; Value: TCheckBoxState): Boolean of object;
  TBoldCheckBoxIsChanged = function (Follower: TBoldFollower; NewValue: TCheckBoxState): Boolean of object;

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
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
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
    class function DefaultGetAsCheckBoxStateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TCheckBoxState; virtual;
    class procedure DefaultSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState); virtual;
    class function DefaultValidateCheckBoxState(aFollower: TBoldFollower; Value: TCheckBoxState): Boolean; virtual;
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
    function DefaultIsChanged(Follower: TBoldFollower; NewValue: TCheckBoxState): Boolean;
    function GetAsCheckBoxStateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TCheckBoxState; virtual;
    procedure SetAsCheckBoxState(aFollower: TBoldFollower; Value: TCheckBoxState); virtual;
    function ValidateCheckBoxState(aFollower: TBoldFollower; Value: TCheckBoxState): Boolean; virtual;
    function IsChanged(Follower: TBoldFollower; NewValue: TCheckBoxState): Boolean;
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
  BoldAttributes,
  BoldGuard;

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
  EffectiveAsCheckBoxStateRenderer.SetAsCheckBoxState(Follower, Value);
end;

function TBoldCheckBoxStateFollowerController.ValidateCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsCheckBoxStateRenderer.ValidateCheckBoxState(Follower, Value);
end;

procedure TBoldCheckBoxStateFollowerController.MayHaveChanged(NewValue: TCheckBoxState; Follower: TBoldFollower);
var
  lIsChanged: boolean;
  lRendererData: TBoldCheckBoxRendererData;
begin
  if Follower.State in bfsDisplayable then
  begin
    lRendererData := Follower.RendererData as TBoldCheckBoxRendererData;
    lRendererData.CurrentValue := NewValue;
    lIsChanged := EffectiveAsCheckBoxStateRenderer.IsChanged(Follower, NewValue);
    if lIsChanged then
    begin
      Follower.ControlledValueChanged;
    end;
  end;
end;

procedure TBoldCheckBoxStateFollowerController.MakeClean(Follower: TBoldFollower);
begin
//  if (ApplyPolicy <> bapChange) or EffectiveRenderer.ChangedValueEventsAssigned then
  begin
    ReleaseChangedValue(Follower); // note, must do first, since set can change element
  end;
  SetAsCheckBoxState(GetCurrentAsCheckBoxState(Follower), Follower);
end;

{ TBoldAsCheckBoxStateRenderer }
procedure TBoldAsCheckBoxStateRenderer.MakeUpToDateANdSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  s: TCheckBoxState;
  lRendererData: TBoldCheckBoxRendererData;
begin
  s := GetAsCheckBoxStateAndSubscribe(aFollower, Subscriber);
  lRendererData := (aFollower.RendererData as TBoldCheckBoxRendererData);
  lRendererData.OldValue := s;
  lRendererData.CurrentValue := s;
end;

class function TBoldAsCheckBoxStateRenderer.DefaultGetAsCheckBoxStateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TCheckBoxState;
var
  {$IFDEF BOLDCOMCLIENT} // DefaultGet
  e: IBoldElement;
  Attribute: IBoldAttribute;
  {$ELSE}
  E: TBoldIndirectElement;
  lResultElement: TBoldElement;
  lGuard: IBoldGuard;
  {$ENDIF}
begin
  Result := cbGrayed;
  if Assigned(aFollower.Element) then
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
    lResultElement := aFollower.Value;
    if not Assigned(lResultElement) then
    begin
      lGuard:= TBoldGuard.Create(E);
      e := TBoldIndirectElement.Create;
      aFollower.Element.EvaluateAndSubscribeToExpression(aFollower.AssertedController.Expression, Subscriber, E, False, False, aFollower.Controller.GetVariableListAndSubscribe(Subscriber));
      lResultElement := e.Value;
    end;
    if lResultElement is TBABoolean then
      with lResultElement as TBABoolean do
        if IsNull then
          Result := cbGrayed
        else if AsBoolean then
          Result := cbChecked
        else
          Result := cbUnchecked;
    {$ENDIF}
  end;
end;

class procedure TBoldAsCheckBoxStateRenderer.DefaultSetAsCheckBoxState(aFollower: TBoldFollower; newValue: TCheckBoxState);
var
  ValueElement: TBoldElement;
  {$IFDEF BOLDCOMCLIENT} // defaulSet
  Attribute: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := aFollower.Value;
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
    raise EBold.CreateFmt('%s: Can''t set value', [ClassName]);
end;

function TBoldAsCheckBoxStateRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldCheckBoxRendererData;
end;

class function TBoldAsCheckBoxStateRenderer.DefaultValidateCheckBoxState(aFollower: TBoldFollower; Value: TCheckBoxState): Boolean;
begin
  Result := True;
end;

function TBoldAsCheckBoxStateRenderer.GetAsCheckBoxStateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TCheckBoxState;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
       OnSubscribe(aFollower, Subscriber);
    Subscriber := nil;
  end;

  if Assigned(OnGetAsCheckBoxState) then
    Result := OnGetAsCheckBoxState(aFollower)
  else
    Result := DefaultGetAsCheckBoxStateAndSubscribe(aFollower, Subscriber);
end;

procedure TBoldAsCheckBoxStateRenderer.SetAsCheckBoxState(aFollower: TBoldFollower; Value: TCheckBoxState);
begin
  if Assigned(OnSetAsCheckBoxState) then
    OnSetAsCheckBoxState(aFollower, Value)
  else
    DefaultSetAsCheckBoxState(aFollower, Value);
end;

function TBoldAsCheckBoxStateRenderer.ValidateCheckBoxState(aFollower: TBoldFollower; Value: TCheckBoxState): Boolean;
begin
  if Assigned(OnValidateCheckBoxState) then
    Result := OnValidateCheckBoxState(aFollower, Value)
  else
    Result := DefaultValidateCheckBoxState(aFollower, Value);
end;

  class function TBoldAsCheckBoxStateRenderer.DefaultRenderer: TBoldAsCheckBoxStateRenderer;
begin
  Result := DefaultAsCheckBoxStateRenderer;
end;

function TBoldAsCheckBoxStateRenderer.DefaultIsChanged(Follower: TBoldFollower; NewValue: TCheckBoxState): Boolean;
begin
  Result := NewValue <> TBoldCheckBoxRendererData(Follower.RendererData).OldValue;
end;

function TBoldAsCheckBoxStateRenderer.IsChanged(Follower: TBoldFollower; NewValue: TCheckBoxState): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(Follower, NewValue)
  else
    Result := DefaultIsChanged(Follower, NewValue);
end;

initialization
  DefaultAsCheckBoxStateRenderer := TBoldAsCheckBoxStateRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsCheckBoxStateRenderer);

end.

