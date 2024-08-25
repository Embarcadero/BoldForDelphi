
{ Global compiler directives }
{$include bold.inc}
unit BoldCheckboxStateControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  StdCtrls,
  BoldClientElementSupport,
  BoldComClient,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldDefs;

type

  {Forward declaration of classes}
  TBoldCheckBoxStateFollowerControllerCom = class;
  TBoldAsCheckBoxStateRendererCom = class;
  TBoldCheckBoxRendererDataCom = class;

  { TBoldAsCheckBoxStateRendererCom }
  TBoldGetAsCheckBoxStateCom = function (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TCheckBoxState of object;
  TBoldSetAsCheckBoxStateCom = procedure (Element: IBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldValidateCheckBoxStateCom = function (Element: IBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;
  TBoldCheckBoxIsChangedCom = function (RendererData: TBoldCheckBoxRendererDataCom; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldCheckBoxRendererDataCom }
  TBoldCheckBoxRendererDataCom = class(TBoldFollowerDataCom)
  private
    fOldvalue: TCheckBoxState;
    fCurrentValue: TCheckBoxState;
  public
    property Oldvalue: TCheckBoxState read fOldvalue write fOldvalue;
    property CurrentValue: TCheckBoxState read fCurrentValue write fCurrentValue;
  end;

  { TBoldAsCheckBoxStateRendererCom }
  TBoldAsCheckBoxStateRendererCom = class(TBoldSingleRendererCom)
  private
    FOnGetAsCheckBoxState: TBoldGetAsCheckBoxStateCom;
    FOnSetAsCheckBoxState: TBoldSetAsCheckBoxStateCom;
    FOnValidate: TBoldValidateCheckBoxStateCom;
    fOnIsChanged: TBoldCheckBoxIsChangedCom;
  protected
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
  public
    class function DefaultRenderer: TBoldAsCheckBoxStateRendererCom;
    class function DefaultGetAsCheckBoxStateAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TCheckBoxState; virtual;
    class procedure DefaultSetAsCheckBoxState(Element: IBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    class function DefaultValidateCheckBoxState(Element: IBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;  VariableList: IBoldExternalVariableList): Boolean; virtual;
    procedure MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber); override;
    function DefaultIsChanged(RendererData: TBoldCheckBoxRendererDataCom; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;  VariableList: IBoldExternalVariableList): Boolean;
    function GetAsCheckBoxStateAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TCheckBoxState; virtual;
    procedure SetAsCheckBoxState(Element: IBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    function ValidateCheckBoxState(Element: IBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;VariableList: IBoldExternalVariableList): Boolean; virtual;
    function IsChanged(RendererData: TBoldCheckBoxRendererDataCom; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
  published
    property OnGetAsCheckBoxState: TBoldGetAsCheckBoxStateCom read FOnGetAsCheckBoxState write FOnGetAsCheckBoxState;
    property OnSetAsCheckBoxState: TBoldSetAsCheckBoxStateCom read FOnSetAsCheckBoxState write FOnSetAsCheckBoxState;
    property OnValidateCheckBoxState: TBoldValidateCheckBoxStateCom read FOnValidate write FOnValidate;
    property OnIsChanged: TBoldCheckBoxIsChangedCom read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldCheckBoxStateFollowerControllerCom }
  TBoldCheckBoxStateFollowerControllerCom = class(TBoldSingleFollowerControllerCom)
  private
    function GetEffectiveAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRendererCom;
    function GetRenderer: TBoldAsCheckBoxStateRendererCom;
    procedure SetRenderer(Value: TBoldAsCheckBoxStateRendererCom);
  protected
    function GetEffectiveRenderer: TBoldRendererCom; override;
    property EffectiveAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRendererCom read GetEffectiveAsCheckBoxStateRenderer;
  public
    function GetCurrentAsCheckBoxState(Follower: TBoldFollowerCom): TCheckBoxState;
    procedure MakeClean(Follower: TBoldFollowerCom); override;
    procedure MayHaveChanged(NewValue: TCheckBoxState; Follower: TBoldFollowerCom);
    procedure SetAsCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollowerCom);
    function ValidateCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollowerCom): Boolean;
  published
    property Renderer: TBoldAsCheckBoxStateRendererCom read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldControlPackDefs;

var
  DefaultAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRendererCom;

{ TBoldCheckBoxStateFollowerControllerCom }

function TBoldCheckBoxStateFollowerControllerCom.GetRenderer: TBoldAsCheckBoxStateRendererCom;
begin
  Result := UntypedRenderer as TBoldAsCheckBoxStateRendererCom;
end;

procedure TBoldCheckBoxStateFollowerControllerCom.SetRenderer(Value: TBoldAsCheckBoxStateRendererCom);
begin
  UntypedRenderer := Value;
end;

function TBoldCheckBoxStateFollowerControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := EffectiveAsCheckBoxStateRenderer;
end;

function TBoldCheckBoxStateFollowerControllerCom.GetEffectiveAsCheckBoxStateRenderer: TBoldAsCheckBoxStateRendererCom;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsCheckBoxStateRendererCom.DefaultRenderer;
end;

function TBoldCheckBoxStateFollowerControllerCom.GetCurrentAsCheckBoxState(Follower: TBoldFollowerCom): TCheckBoxState;
begin
  Result := (Follower.RendererData as TBoldCheckBoxRendererDataCom).CurrentValue;
end;

procedure TBoldCheckBoxStateFollowerControllerCom.SetAsCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollowerCom);
begin
  EffectiveAsCheckBoxStateRenderer.SetAsCheckBoxState(Follower.Element, Value, Representation, Expression, VariableList);
end;

function TBoldCheckBoxStateFollowerControllerCom.ValidateCheckBoxState(Value: TCheckBoxState; Follower: TBoldFollowerCom): Boolean;
begin
  Result := EffectiveAsCheckBoxStateRenderer.ValidateCheckBoxState(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldCheckBoxStateFollowerControllerCom.MayHaveChanged(NewValue: TCheckBoxState; Follower: TBoldFollowerCom);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldCheckBoxRendererDataCom).CurrentValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsCheckBoxStateRenderer.IsChanged(Follower.RendererData as TBoldCheckBoxRendererDataCom, NewValue, Representation, Expression, VariableList));
  end;
end;

procedure TBoldCheckBoxStateFollowerControllerCom.MakeClean(Follower: TBoldFollowerCom);
begin
  ReleaseChangedValue(Follower);
  SetAsCheckBoxState(GetCurrentAsCheckBoxState(Follower), Follower);
end;

{ TBoldAsCheckBoxStateRendererCom }
procedure TBoldAsCheckBoxStateRendererCom.MakeUptoDateANdSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
var
  s: TCheckBoxState;
  Controller: TBoldCheckBoxStateFollowerControllerCom;
begin
  Controller := FollowerController as TBoldCheckBoxStateFollowerControllerCom;
  s := GetAsCheckBoxStateAndSubscribe(Element, Controller.Representation, Controller.Expression,  Controller.GetVariableListAndSubscribe(Subscriber), Subscriber);
  (RendererData as TBoldCheckBoxRendererDataCom).OldValue := s;
  (RendererData as TBoldCheckBoxRendererDataCom).CurrentValue := s;
end;

class function TBoldAsCheckBoxStateRendererCom.DefaultGetAsCheckBoxStateAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TCheckBoxState;
var
  {$IFDEF BOLDCOMCLIENT}
  e: IBoldElement;
  Attribute: IBoldAttribute;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
begin
  Result := cbGrayed;
  if Assigned(Element) then
  begin
    {$IFDEF BOLDCOMCLIENT}
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

class procedure TBoldAsCheckBoxStateRendererCom.DefaultSetAsCheckBoxState(Element: IBoldElement; newValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
var
  ValueElement: IBoldElement;
  {$IFDEF BOLDCOMCLIENT}
  Attribute: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  {$IFDEF BOLDCOMCLIENT}
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

function TBoldAsCheckBoxStateRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldCheckBoxRendererDataCom;
end;

  class function TBoldAsCheckBoxStateRendererCom.DefaultValidateCheckBoxState(Element: IBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  Result := True;
end;

function TBoldAsCheckBoxStateRendererCom.GetAsCheckBoxStateAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TCheckBoxState;
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

procedure TBoldAsCheckBoxStateRendererCom.SetAsCheckBoxState(Element: IBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression;VariableList: IBoldExternalVariableList);
begin
  if Assigned(OnSetAsCheckBoxState) then
    OnSetAsCheckBoxState(Element, Value, Representation, Expression)
  else
    DefaultSetAsCheckBoxState(Element, Value, Representation, Expression, VariableList);
end;

function TBoldAsCheckBoxStateRendererCom.ValidateCheckBoxState(Element: IBoldElement; Value: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(OnValidateCheckBoxState) then
    Result := OnValidateCheckBoxState(Element, Value, Representation, Expression)
  else
    Result := DefaultValidateCheckBoxState(Element, Value, Representation, Expression, VariableList);
end;

  class function TBoldAsCheckBoxStateRendererCom.DefaultRenderer: TBoldAsCheckBoxStateRendererCom;
begin
  Result := DefaultAsCheckBoxStateRenderer;
end;

function TBoldAsCheckBoxStateRendererCom.DefaultIsChanged(RendererData: TBoldCheckBoxRendererDataCom; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> TBoldCheckBoxRendererDataCom(RendererData).OldValue;
end;

function TBoldAsCheckBoxStateRendererCom.IsChanged(RendererData: TBoldCheckBoxRendererDataCom; NewValue: TCheckBoxState; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
end;

initialization
  DefaultAsCheckBoxStateRenderer := TBoldAsCheckBoxStateRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultAsCheckBoxStateRenderer);
  
end.
