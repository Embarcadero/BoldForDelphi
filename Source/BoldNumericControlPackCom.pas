
{ Global compiler directives }
{$include bold.inc}
unit BoldNumericControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 14:59:57}

interface

uses
  BoldClientElementSupport,
  BoldComClient,
  BoldComObjectSpace_TLB,
  BoldControlPackCom,
  BoldDefs;

type             
  {Forward declarations}
  TBoldIntegerRendererDataCom = class;
  TBoldAsIntegerRendererCom = class;
  TBoldIntegerFollowerControllerCom = class;

  {TBoldAsIntegerRendererCom}
  TBoldGetAsIntegerEventCom = function (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): Integer of object;
  TBoldSetAsIntegerEventCom = procedure (Element: IBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldIntegerIsChangedEventCom = function (RenderData: TBoldIntegerRendererDataCom; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldIntegerRendererDataCom }
  TBoldIntegerRendererDataCom = class(TBoldFollowerDataCom)
  private
    FOldIntegerValue: Integer;
    FCurrentIntegerValue: Integer;
  public
    property CurrentIntegerValue: Integer read FCurrentIntegerValue write FCurrentIntegerValue;
    property OldIntegerValue: Integer read FOldIntegerValue write FOldIntegerValue;
  end;

  { TBoldAsIntegerRendererCom }
  TBoldAsIntegerRendererCom = class(TBoldSingleRendererCom)
  private
    FOnGetAsInteger: TBoldGetAsIntegerEventCom;
    FOnSetAsInteger: TBoldSetAsIntegerEventCom;
    FOnIsChanged: TBoldIntegerIsChangedEventCom;
  protected
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
    function DefaultGetAsIntegerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Integer; virtual;
    procedure DefaultSetAsInteger(Element: IBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    function GetAsIntegerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Integer; virtual;
    procedure SetAsInteger(Element: IBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
  public
    class function DefaultRenderer: TBoldAsIntegerRendererCom;
    function DefaultIsChanged(RendererData: TBoldIntegerRendererDataCom; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
    function DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean; override;
    procedure MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber); override;
    function IsChanged(RendererData: TBoldIntegerRendererDataCom; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
  published
    property OnGetAsInteger: TBoldGetAsIntegerEventCom read FOnGetAsInteger write FOnGetAsInteger;
    property OnSetAsInteger: TBoldSetAsIntegerEventCom read FOnSetAsInteger write FOnSetAsInteger;
    property OnIsChanged: TBoldIntegerIsChangedEventCom read FOnIsChanged write FOnIsChanged;
  end;

  { TBoldIntegerFollowerControllerCom }
  TBoldIntegerFollowerControllerCom = class(TBoldSingleFollowerControllerCom)
  private
    function GetRenderer: TBoldAsIntegerRendererCom;
    procedure SetRenderer(Value: TBoldAsIntegerRendererCom);
    function GetEffectiveAsIntegerRenderer: TBoldAsIntegerRendererCom;
  protected
    function GetEffectiveRenderer: TBoldRendererCom; override;
    property EffectiveAsIntegerRenderer: TBoldAsIntegerRendererCom read GetEffectiveAsIntegerRenderer;
  public
    function GetCurrentAsInteger(Follower: TBoldFollowerCom): Integer;
    procedure MakeClean(Follower: TBoldFollowerCom); override;
    procedure MayHaveChanged(NewValue: Integer; Follower: TBoldFollowerCom);
    procedure SetAsInteger(Value: Integer; Follower: TBoldFollowerCom);
  published
    property Renderer: TBoldAsIntegerRendererCom read GetRenderer write SetRenderer;
  end;

implementation

uses
  SysUtils,
  BoldGuiResourceStringsCom,
  {!! DO NOT REMOVE !! BoldAttributes ,}
  BoldControlPackDefs;

var
  DefaultAsIntegerRenderer: TBoldAsIntegerRendererCom = nil;

{ TBoldAsIntegerRendererCom }
class function TBoldAsIntegerRendererCom.DefaultRenderer: TBoldAsIntegerRendererCom;
begin
  Result := DefaultAsIntegerRenderer;
end;

function TBoldAsIntegerRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldIntegerRendererDataCom;
end;

function TBoldAsIntegerRendererCom.DefaultMayModify(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Boolean;
{$IFNDEF BOLDCOMCLIENT}
var
  ValueElement: IBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  result := (ValueElement is TBANumeric) and ValueElement.ObserverMayModify(Subscriber)
end;
{$ELSE}
begin
  result := inherited DefaultMayModify(Element, Representation, Expression, VariableList, Subscriber);
end;
{$ENDIF}

function TBoldAsIntegerRendererCom.DefaultGetAsIntegerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Integer;
var
  {$IFDEF BOLDCOMCLIENT}
  el: IBoldElement;
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
  {$ENDIF}
begin
  Result := 0;
  if Assigned(Element) then
  begin
    {$IFDEF BOLDCOMCLIENT}
    if assigned(Subscriber) then
      el := Element.EvaluateAndSubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, false, false)
    else
      el := Element.EvaluateExpression(Expression);
    if assigned(el) and (el.QueryInterface(IBoldAttribute, attr) = S_OK) then
      result := round(attr.AsVariant)
    else
      raise EBold.CreateFmt(sCantGetIntegerValue, [el.AsString])
    {$ELSE}
    IndirectElement := TBoldIndirectElement.Create;
    try
      Element.EvaluateAndSubscribeToExpression(Expression, Subscriber, IndirectElement, False, False, VariableList);
      if Assigned(IndirectElement.Value) then
      begin
        if (IndirectElement.Value is TBAInteger) then
          Result := (IndirectElement.Value as TBAInteger).AsInteger
        else if (IndirectElement.Value is TBANumeric) then
          Result := Round((IndirectElement.Value as TBANumeric).AsFloat)
        else
          raise EBold.CreateFmt(sCantGetIntegerValue, [IndirectElement.Value.ClassName])
      end;
    finally
      IndirectElement.Free;
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsIntegerRendererCom.DefaultSetAsInteger(Element: IBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
var
  ValueElement: IBoldElement;
  {$IFDEF BOLDCOMCLIENT}
  Attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  {$IFDEF BOLDCOMCLIENT}
  if assigned(ValueElement) and (ValueElement.QueryInterface(IBoldAttribute, attr) = S_OK) then
    attr.AsVariant := Value
  else
    raise EBold.CreateFmt(sCantSetIntegerValue, [ValueElement.AsString]);
  {$ELSE}
  if ValueElement is TBANumeric then
    (ValueElement as TBANumeric).AsInteger := Value
  else
    raise EBold.CreateFmt(sCantSetIntegerValue, [ValueElement.ClassName]);
  {$ENDIF}
end;

function TBoldAsIntegerRendererCom.GetAsIntegerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): Integer;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, Representation, Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsInteger) then
    Result := OnGetAsInteger(Element, Representation, Expression)
  else
    Result := DefaultGetAsIntegerAndSubscribe(Element, Representation, Expression, VariableList, Subscriber);
end;

procedure TBoldAsIntegerRendererCom.SetAsInteger(Element: IBoldElement; const Value: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
begin
  if Assigned(FOnSetAsInteger) then
    OnSetAsInteger(Element, Value, Representation, Expression)
  else
    DefaultSetAsInteger(Element, Value, Representation, Expression, VariableList)
end;

procedure TBoldAsIntegerRendererCom.MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
var
  Value: Integer;
  Controller: TBoldIntegerFollowerControllerCom;
begin
  Controller := FollowerController as TBoldIntegerFollowerControllerCom;
  Value := GetAsIntegerAndSubscribe(Element, Controller.Representation, Controller.Expression, Controller.GetVariableListAndSubscribe(Subscriber) ,Subscriber);
  with (RendererData as TBoldIntegerRendererDataCom) do
  begin
    OldIntegerValue := Value;
    CurrentIntegerValue := Value;
  end;
end;

function TBoldAsIntegerRendererCom.DefaultIsChanged(RendererData: TBoldIntegerRendererDataCom; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldIntegerValue;
end;

function TBoldAsIntegerRendererCom.IsChanged(RendererData: TBoldIntegerRendererDataCom; const NewValue: Integer; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnIsChanged) then
    Result := FOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
end;

{ TBoldIntegerFollowerControllerCom }

function TBoldIntegerFollowerControllerCom.GetRenderer: TBoldAsIntegerRendererCom;
begin
  Result := UntypedRenderer as TBoldAsIntegerRendererCom;
end;

procedure TBoldIntegerFollowerControllerCom.SetRenderer(Value: TBoldAsIntegerRendererCom);
begin
  UntypedRenderer := Value;
end;

function TBoldIntegerFollowerControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := EffectiveAsIntegerRenderer;
end;

function TBoldIntegerFollowerControllerCom.GetEffectiveAsIntegerRenderer: TBoldAsIntegerRendererCom;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := DefaultAsIntegerRenderer;
end;

procedure TBoldIntegerFollowerControllerCom.MakeClean(Follower: TBoldFollowerCom);
begin
  ReleaseChangedValue(Follower);
  SetAsInteger(GetCurrentAsInteger(Follower), Follower);
end;

function TBoldIntegerFollowerControllerCom.GetCurrentAsInteger(Follower: TBoldFollowerCom): Integer;
begin
  Result := (Follower.RendererData as TBoldIntegerRendererDataCom).CurrentIntegerValue;
end;

procedure TBoldIntegerFollowerControllerCom.SetAsInteger(Value: Integer; Follower: TBoldFollowerCom);
begin
  EffectiveAsIntegerRenderer.SetAsInteger(Follower.Element, Value, Representation, Expression, GetVariableListAndSubscribe(follower.Subscriber));
end;

procedure TBoldIntegerFollowerControllerCom.MayHaveChanged(NewValue: Integer; Follower: TBoldFollowerCom);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldIntegerRendererDataCom).CurrentIntegerValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsIntegerRenderer.IsChanged(Follower.RendererData as TBoldIntegerRendererDataCom, NewValue, Representation, Expression, GetVariableListAndSubscribe(follower.Subscriber)));
  end;
end;

initialization
  DefaultAsIntegerRenderer := TBoldAsIntegerRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultAsIntegerRenderer);

end.
