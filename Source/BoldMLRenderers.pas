
{ Global compiler directives }
{$include bold.inc}
unit BoldMLRenderers;

interface

uses
  Classes,
  BoldDefs,
  BoldSubscription,
  BoldControlPack,
  BoldStringControlPack,
  BoldElements;

type
  { forward declarations }
  TBoldAsMLStringRenderer = class;

  TBoldLanguageGetAsString = function (aFollower: TBoldFollower;  const Language: String): string of object;
  TBoldLanguageSetAsString = procedure (aFollower: TBoldFollower; const Value: string; const Language: String) of object;
  TBoldLanguageSubscribe = procedure (aFollower: TBoldFollower; const Language: String; Subscriber: TBoldSubscriber) of object;

  { TBoldAsMLStringRenderer }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldAsMLStringRenderer = class (TBoldAsStringRenderer)
  private
    fLanguage: String;
    fOnLanguageGetAsString: TBoldLanguageGetAsString;
    fOnLanguageSetAsString: TBoldLanguageSetAsString;
    fOnLanguageSubscribe: TBoldLanguageSubscribe;
    procedure SetLanguage(const newValue: String);
  protected
    procedure SetAsString(aFollower: TBoldFollower; const Value: string); override;
    function GetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string; override;
  public
    function DefaultGetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string; override;
    procedure DefaultSetAsString(aFollower: TBoldFollower; const Value: string); override;
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
  published
    property Language: String read fLanguage write SetLanguage;
    property OnLanguageGetAsString: TBoldLanguageGetAsString read FOnLanguageGetAsString write FOnLanguageGetAsString;
    property OnLanguageSetAsString: TBoldLanguageSetAsString read FOnLanguageSetAsString write FOnLanguageSetAsString;
    property OnLanguageSubscribe: TBoldLanguageSubscribe read FOnLanguageSubscribe write FOnLanguageSubscribe;
  end;

implementation

uses
  SysUtils,
  BoldMLAttributes,
  BoldUtils,
  BoldGuard;

function TBoldAsMLStringRenderer.GetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string;
begin
  if Assigned(OnLanguageSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
      OnLanguageSubscribe(aFollower, Language, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnLanguageGetAsString) then
  begin
    Result := OnLanguageGetAsString(aFollower, Language);
    if not assigned(OnLanguageSubscribe) and assigned(Subscriber) then
      DefaultGetAsStringAndSubscribe(aFollower, Subscriber);
  end
  else
    Result := DefaultGetAsStringAndSubscribe(aFollower, Subscriber);
end;

function TBoldAsMLStringRenderer.DefaultGetAsStringAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): string;
var
  E: TBoldIndirectElement;
  lResultElement: TBoldElement;
  lGuard: IBoldGuard;
  lFollowerController: TBoldFollowerController;
begin
  Result := '';
  if Assigned(aFollower.Element) then
  begin
    lFollowerController := aFollower.AssertedController;
    if Assigned(aFollower.Value) then
    begin
      lResultElement := aFollower.Value
    end
    else
    begin
      lGuard:= TBoldGuard.Create(E);
      E := TBoldIndirectElement.Create;
      aFollower.Element.EvaluateAndSubscribeToExpression(lFollowerController.Expression, Subscriber, E, False, False, lFollowerController.GetVariableListAndSubscribe(Subscriber));
      lResultElement := e.Value;
    end;
    if Assigned(lResultElement) then
    begin
      if lResultElement is TBAMLString then
      begin
        result := (lResultElement as TBAMLString).AsStringByLanguage[Language].StringRepresentation[lFollowerController.Representation];
        if Assigned(Subscriber) then
        begin
          (lResultElement as TBAMLString).SubscribeToLanguage(Language, Subscriber, breReEvaluate);
          Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
        end;
      end
      else if lResultElement is TBAMLValueSet then
      begin
        result := (lResultElement as TBAMLValueSet).StringRepresentationByLanguage[lFollowerController.Representation, Language];
        if Assigned(Subscriber) then
        begin
          (lResultElement as TBAMLvalueSet).SubscribeToLanguage(lFollowerController.Representation, Language, Subscriber, breReEvaluate);
          Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
        end;
      end
      else
      begin
        Result := lResultElement.StringRepresentation[lFollowerController.Representation];
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lFollowerController.Representation, Subscriber, breReEvaluate);
      end;
    end
    else
      Result := '';
  end;
end;

procedure TBoldAsMLStringRenderer.SetAsString(aFollower: TBoldFollower; const Value: string);
begin
  if assigned(OnLanguageSetAsString) then
    OnLanguageSetAsString(aFollower, Value, Language)
  else
    DefaultSetAsString(aFollower, Value);
end;

procedure TBoldAsMLStringRenderer.DefaultSetAsString(aFollower: TBoldFollower; const Value: string);
var
  ValueElement: TBoldElement;
  MLValueSet: TBAMLValueSet;
  lRepresentation: integer;
begin
  ValueElement := aFollower.Value;
  lRepresentation := aFollower.AssertedController.Representation;
  if Assigned(ValueElement) then
  begin
    if (ValueElement is TBAMLValueSet) then
    begin
      MLValueSet := (ValueElement as TBAMLValueSet);
      MLValueSet.StringRepresentationByLanguage[lRepresentation, Language] := Value
    end
    else if ValueElement is TBAMLString then
      (ValueElement as TBAMLString).AsStringByLanguage[Language].StringRepresentation[lRepresentation] := Value
    else
      ValueElement.StringRepresentation[lRepresentation] := Value
  end else
    raise EBold.CreateFmt('%s.DefaultSetAsString: Can''t set string value', [ClassName]);
end;

procedure TBoldAsMLStringRenderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  E: TBoldIndirectElement;
  S: String;
  lRepresentation: integer;
  lRendererData: TBoldStringRendererData;
  lResultElement: TBoldElement;
  lFollowerController: TBoldFollowerController;
  lGuard: IBoldGuard;
begin
  S := '';
  lRendererData := aFollower.RendererData as TBoldStringRendererData;
  lFollowerController := aFollower.AssertedController;
  if Assigned(aFollower.Element) then
  begin
    lRepresentation := lFollowerController.Representation;
    lResultElement := aFollower.Value;
    if not Assigned(lResultElement) then
    begin
      lGuard:= TBoldGuard.Create(E);
      E := TBoldIndirectElement.Create;
      aFollower.Element.EvaluateAndSubscribeToExpression(lFollowerController.Expression, Subscriber, E, False, False, lFollowerController.GetVariableListAndSubscribe(Subscriber));
      lResultElement := e.Value;
    end;
    if Assigned(lResultElement) then
    begin
      if lResultElement is TBAMLString then
      begin
        S := (lResultElement as TBAMLString).AsStringByLanguage[Language].StringRepresentation[lRepresentation];
        if Assigned(Subscriber) then
        begin
          (lResultElement as TBAMLString).SubscribeToLanguage(Language, Subscriber, breReEvaluate);
          Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
        end
      end
      else if lResultElement is TBAMLValueSet then
      begin
        S := TBAMLValueSet(lResultElement).StringRepresentationByLanguage[lRepresentation, Language];
        if Assigned(Subscriber) then
        begin
          TBAMLValueSet(lResultElement).SubscribeToLanguage(lRepresentation, Language, Subscriber, breReEvaluate);
          Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
        end;
      end
      else
      begin
        S := lResultElement.StringRepresentation[lRepresentation];
        if Assigned(Subscriber) then
          lResultElement.SubscribeToStringRepresentation(lRepresentation, Subscriber, breReEvaluate);
      end;
//      lRendererData.MayModify := lResultElement.ObserverMayModify(Subscriber);
    end
    else
      S := '';
  end;
  lRendererData.OldStringValue := S;
  lRendererData.CurrentStringValue := S;
end;

procedure TBoldAsMLStringRenderer.SetLanguage(const newValue: String);
begin
  if NewValue <> fLanguage then
  begin
    fLanguage := NewValue;
    SendEvent(self, beValueChanged);
  end;
end;

end.
