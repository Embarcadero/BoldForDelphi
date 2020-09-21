unit BoldMLRenderers;

interface

uses
  BoldDefs,
  BoldSubscription,
  BoldControlPack,
  BoldStringControlPack,
  BoldElements;

type
  { forward declarations }
  TBoldAsMLStringRenderer = class;

  TBoldLanguageGetAsString = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Language: String): string of object;
  TBoldLanguageSetAsString = procedure (Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; Language: String) of object;
  TBoldLanguageSubscribe = procedure (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; Subscriber: TBoldSubscriber; Language: String) of object;

  { TBoldAsMLStringRenderer }
  TBoldAsMLStringRenderer = class (TBoldAsStringRenderer)
  private
    fLanguage: String;
    fOnLanguageGetAsString: TBoldLanguageGetAsString;
    fOnLanguageSetAsString: TBoldLanguageSetAsString;
    fOnLanguageSubscribe: TBoldLAnguageSubscribe;
    procedure SetLanguage(newValue: String);
  protected
    procedure SetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); override;
    function GetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string; override;
  public
    function DefaultGetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string; override;
    procedure DefaultSetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); override;
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber); override;
  published
    property Language: String read fLanguage write SetLanguage;
    property OnLanguageGetAsString: TBoldLanguageGetAsString read FOnLanguageGetAsString write FOnLanguageGetAsString;
    property OnLanguageSetAsString: TBoldLanguageSetAsString read FOnLanguageSetAsString write FOnLanguageSetAsString;
    property OnLanguageSubscribe: TBoldLanguageSubscribe read FOnLanguageSubscribe write FOnLanguageSubscribe;
  end;

implementation

uses
  SysUtils,
  BoldGuiResourceStrings,
  BoldMLAttributes,
  BoldUtils;

function TBoldAsMLStringRenderer.GetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string;
begin
  if Assigned(OnLanguageSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnLanguageSubscribe(Element, FollowerController.Representation, FollowerController.Expression, Subscriber, Language);
    Subscriber := nil;
  end;
  if Assigned(OnLanguageGetAsString) then
  begin
    Result := OnLanguageGetAsString(Element, FollowerController.Representation, FollowerController.Expression, Language);
    if not assigned(OnLanguageSubscribe) and assigned(Subscriber) then
      DefaultGetAsStringAndSubscribe(Element, FollowerController, Subscriber);
  end
  else
    Result := DefaultGetAsStringAndSubscribe(Element, FollowerController, Subscriber);
end;

function TBoldAsMLStringRenderer.DefaultGetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string;
var
  E: TBoldIndirectElement;
begin
  Result := '';
  if Assigned(Element) then
  begin
    E := TBoldIndirectElement.Create;
    try
      Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False, false, FollowerController.GetVariableListAndSubscribe(subscriber));
      if Assigned(E.Value) then
      begin
        if e.Value is TBAMLString then
        begin
          result := (e.Value as TBAMLString).AsStringByLanguage[Language].StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
          begin
            (e.Value as TBAMLString).SubscribeToLanguage(Language, Subscriber, breReEvaluate);
            Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
          end;
        end
        else if e.Value is TBAMLValueSet then
        begin
          result := (e.Value as TBAMLValueSet).StringRepresentationByLanguage[FollowerController.Representation, Language];
          if Assigned(Subscriber) then
          begin
            (e.Value as TBAMLvalueSet).SubscribeToLanguage(FollowerController.Representation, Language, Subscriber, breReEvaluate);
            Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
          end;
        end
        else
        begin
          Result := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end;
      end
      else
        Result := '';
    finally
      E.Free;
    end;
  end;
end;

procedure TBoldAsMLStringRenderer.SetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  if assigned(OnLanguageSetAsString) then
    OnLanguageSetAsString(Element, Value, Representation, Expression, Language)
  else
    DefaultSetAsString(Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldAsMLStringRenderer.DefaultSetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  ValueElement: TBoldElement;
  MLValueSet: TBAMLValueSet;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
  begin
    if (ValueElement is TBAMLValueSet) then
    begin
      MLValueSet := (ValueElement as TBAMLValueSet);
      MLValueSet.StringRepresentationByLanguage[Representation, Language] := Value
    end
    else if ValueElement is TBAMLString then
      (ValueElement as TBAMLString).AsStringByLanguage[Language].StringRepresentation[Representation] := Value
    else
      ValueElement.StringRepresentation[Representation] := Value
  end else
    raise EBold.CreateFmt(sCannotSetStringValue, [ClassName]);
end;

procedure TBoldAsMLStringRenderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber);
var
  E: TBoldIndirectElement;
  S: String;
begin
  S := '';

  if Assigned(Element) then
  begin
    E := TBoldIndirectElement.Create;
    try
      Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False);
      if Assigned(E.Value) then
      begin
        if e.Value is TBAMLString then
        begin
          S := (e.Value as TBAMLString).AsStringByLanguage[Language].StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
          begin
            (e.Value as TBAMLString).SubscribeToLanguage(Language, Subscriber, breReEvaluate);
            Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
          end
        end
        else if e.Value is TBAMLValueSet then
        begin
          S := (e.Value as TBAMLValueSet).StringRepresentationByLanguage[FollowerController.Representation, Language];
          if Assigned(Subscriber) then
          begin
            (e.Value as TBAMLvalueSet).SubscribeToLanguage(FollowerController.Representation, Language, Subscriber, breReEvaluate);
            Publisher.AddSubscription(subscriber, beValueChanged, breReevaluate);
          end;
        end
        else
        begin
          S := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end;
        RendererData.MayModify := E.Value.ObserverMayModify(Subscriber);
      end
      else
        S := '';
    finally
      E.Free;
    end;
  end;
  (RendererData as TBoldStringRendererData).OldStringValue := S;
  (RendererData as TBoldStringRendererData).CurrentStringValue := S;
end;

procedure TBoldAsMLStringRenderer.SetLanguage(newValue: String);
begin
  if NewValue <> fLanguage then
  begin
    fLanguage := NewValue;
    SendEvent(self, beValueChanged);
  end;
end;

end.
