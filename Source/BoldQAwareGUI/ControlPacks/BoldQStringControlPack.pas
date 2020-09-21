unit BoldQStringControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  QGraphics,
  Types,
  Classes,
  //QGUI Windows,
  Variants,
  BoldDefs,
  BoldControlPackDefs,
  BoldContainers,
  BoldElements,
  {$IFNDEF BOLDCOMCLIENT}
  BoldSystem,
  {$ENDIF}
  BoldQControlPack,
  BoldSubscription;

type
  {Forward declaration of classes}
  TBoldStringFollowerController = class;
  TBoldAsStringRenderer = class;
  TBoldStringRendererData = class;

  { TBoldAsStringRenderer prototypes }
  TBoldGetAsString = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): string of object;
  TBoldSetAsString = procedure (Element: TBoldElement; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldValidateString = function (Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;
  TBoldSetFont = procedure (Element: TBoldElement; AFont: TFont; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldSetColor = procedure (Element: TBoldElement; var AColor: TColor; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldStringIsChanged = function (RendererData: TBoldStringRendererData; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldStringRendererData }
  TBoldStringRendererData = class(TBoldRendererData)
  private
    fOldStringValue: string;
    fCurrentStringValue: string;
    fMaxStringLength: integer;
  public
    constructor Create(OwningFollower: TBoldFollower); override;
    property OldStringValue: string read fOldStringValue write fOldStringValue;
    property CurrentStringValue: string read fCurrentStringValue write fCurrentStringValue;
    property MaxStringLength: integer read fMaxStringLength write fMaxStringLength;
  end;

  { TBoldAsStringRenderer }
  TBoldAsStringRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsString: TBoldGetAsString;
    FOnSetAsString: TBoldSetAsString;
    FOnValidateCharacter: TBoldValidateString;
    FOnValidateString: TBoldValidateString;
    fOnSetFont: TBoldSetFont;
    fOnSetColor: TBoldSetColor;
    fOnIsChanged: TBoldStringIsChanged;
  protected
    function GetSupportsMulti: Boolean; override;
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function GetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string; virtual;
    procedure SetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    procedure DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint); override;
  public
    class function DefaultRenderer: TBoldAsStringRenderer;
    class procedure DrawStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: string);
    function DefaultGetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string; virtual;
    procedure DefaultSetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    function DefaultValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function DefaultValidateString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function DefaultIsChanged(RendererData: TBoldStringRendererData; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    function ValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function ValidateString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean; virtual;
    function IsChanged(RendererData: TBoldStringRendererData; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    procedure SetFont(Element: TBoldElement; EffectiveFont, Font: TFont; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
    procedure SetColor(Element: TBoldElement; var EffectiveColor: TColor; Color: TColor; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
    procedure MakeUpToDateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); override;
    procedure MultiMakeUpToDateAndSubscribe(Elements: TBoldClientableList; Subscribers: TBoldObjectArray; RendererData: TBoldObjectArray; FollowerController: TBoldFollowerController);
    procedure DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber); virtual;
  published
    property OnGetAsString: TBoldGetAsString read FOnGetAsString write FOnGetAsString;
    property OnSetAsString: TBoldSetAsString read FOnSetAsString write FOnSetAsString;
    property OnValidateCharacter: TBoldValidateString read FOnValidateCharacter write FOnValidateCharacter;
    property OnValidateString: TBoldValidateString read FOnValidateString write FOnValidateString;
    property OnSetFont: TBoldSetFont read fOnSetFont write fOnSetFont;
    property OnSetColor: TBoldSetColor read fOnSetColor write fOnSetColor;
    property OnIsChanged: TBoldStringIsChanged read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldStringFollowerController }
  TBoldStringFollowerController = class(TBoldSingleFollowerController)
  private
    FNilStringRepresentation: string;
    function GetRenderer: TBoldAsStringRenderer;
    procedure SetRenderer(Value: TBoldAsStringRenderer);
    function GetEffectiveAsStringRenderer: TBoldAsStringRenderer;
    procedure SetNilStringRepresentation(const Value: string);
  protected
    function GetSupportsMultiEnsure: Boolean; override;
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsStringRenderer: TBoldAsStringRenderer read GetEffectiveAsStringRenderer;
    procedure DoMultiMakeUptodateAndSubscribe(Followers: TBoldObjectArray); override;
  public
    procedure MakeClean(Follower: TBoldFollower); override;
    function GetCurrentAsString(Follower: TBoldFollower): string;
    procedure SetAsString(Value: string; Follower: TBoldFollower);
    function ValidateCharacter(C: AnsiChar; Follower: TBoldFollower): Boolean;
    function ValidateString(Value: string; Follower: TBoldFollower): Boolean;
    procedure SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollower);
    procedure SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollower);
    procedure MayHaveChanged(NewValue: string; Follower: TBoldFollower);
    procedure DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean); override;
    function GetAsString(Element: TBoldElement): string;
  published
    property Renderer: TBoldAsStringRenderer read GetRenderer write SetRenderer;
    property NilStringRepresentation: string read FNilStringRepresentation write SetNilStringRepresentation;
  end;

implementation

uses
  SysUtils,
  BoldRev,
  BoldUtils;

var
  DefaultAsStringRenderer: TBoldAsStringRenderer;

{---TBoldStringFollowerController---}

function TBoldStringFollowerController.GetRenderer: TBoldAsStringRenderer;
begin
  Result := UntypedRenderer as TBoldAsStringRenderer;
end;

procedure TBoldStringFollowerController.SetRenderer(Value: TBoldAsStringRenderer);
begin
  UntypedRenderer := Value;
end;

function TBoldStringFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsStringRenderer;
end;

function TBoldStringFollowerController.GetEffectiveAsStringRenderer: TBoldAsStringRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsStringRenderer.DefaultRenderer;
end;

function TBoldStringFollowerController.GetCurrentAsString(Follower: TBoldFollower): string;
begin
  Result := (Follower.RendererData as TBoldStringRendererData).CurrentStringValue;
end;

procedure TBoldStringFollowerController.SetAsString(Value: string; Follower: TBoldFollower);
begin
  EffectiveAsStringRenderer.SetAsString(Follower.Element, Value, Representation, Expression, VariableList);
end;

function TBoldStringFollowerController.ValidateCharacter(C: AnsiChar; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsStringRenderer.ValidateCharacter(Follower.Element, C, Representation, Expression, VariableList);
end;

function TBoldStringFollowerController.ValidateString(Value: string; Follower: TBoldFollower): Boolean;
begin
  Result := EffectiveAsStringRenderer.ValidateString(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldStringFollowerController.SetFont(EffectiveFont, Font: tFont; Follower: TBoldFollower);
begin
  EffectiveAsStringRenderer.SetFont(Follower.Element, EffectiveFont, Font, Representation, Expression, VariableList);
end;

procedure TBoldStringFollowerController.SetColor(var EffectiveColor: tColor; COLOR: tColor; Follower: TBoldFollower);
begin
  EffectiveAsStringRenderer.SetColor(Follower.Element, EffectiveColor, COLOR, Representation, Expression, VariableList);
end;

procedure TBoldStringFollowerController.MayHaveChanged(NewValue: string; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldStringRendererData).CurrentStringValue := NewValue;
    Follower.ControlledValueChanged(EffectiveAsStringRenderer.IsChanged(Follower.RendererData as TBoldStringRendererData, NewValue, Representation, Expression, VariableList));
  end;
end;

procedure TBoldStringFollowerController.MakeClean(Follower: TBoldFollower);
begin
  if ValidateString(GetCurrentAsString(Follower), Follower) then
  begin
    ReleaseChangedValue(Follower); // note, must do first, since set can change element
    SetAsString(GetCurrentAsString(Follower), Follower);
  end
  else
    raise EBold.Create('String validation failed')
end;

procedure TBoldStringFollowerController.DoMakeUptodateAndSubscribe(Follower: TBoldFollower; Subscribe: Boolean);
var
  renderer: TBoldAsStringRenderer;
  Subscriber: TBoldSubscriber;
begin
  If Subscribe then
    Subscriber := Follower.Subscriber
  else
    Subscriber := nil;
  Renderer := EffectiveRenderer as TBoldAsStringRenderer;
  if Assigned(Renderer.OnGetAsString) or Assigned(Renderer.OnSubscribe) or Assigned(Renderer.OnMayModify) then   begin
    Renderer.MakeUptodateAndSubscribe(Follower.Element, Follower.RendererData, Self, Subscriber);
    Follower.RendererData.MayModify := Renderer.MayModify(Follower.Element, Representation, Expression, GetVariableListAndSubscribe(follower.Subscriber), Follower.Subscriber);
  end else
    renderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Follower.Element, Follower.RendererData, Self, Subscriber);
end;

procedure TBoldStringFollowerController.SetNilStringRepresentation(const Value: string);
begin
  if (FNilStringRepresentation<>Value) then
  begin
    FNilStringRepresentation := Value;
    Changed;
  end;
end;

{---TBoldAsStringRenderer---}
  class procedure TBoldAsStringRenderer.DrawStringOnCanvas(Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint; S: string);
var
  Left: Integer;
begin
  // Adjust for alignment
  case Alignment of
    taLeftJustify: Left := Margins.X + Rect.Left;
    taRightJustify: Left := (Rect.Right - Rect.Left) - Canvas.TextWidth(S) + Rect.Left - 1 - Margins.X;
  else
    Left := Rect.Left + ((Rect.Right - Rect.Left) - Canvas.TextWidth(S)) div 2;
  end;
  Canvas.TextRect(Rect, Left, Rect.Top + Margins.Y, S);
end;

procedure TBoldAsStringRenderer.DrawOnCanvas(Follower: TBoldFollower; Canvas: TCanvas; Rect: TRect; Alignment: TAlignment; Margins: TPoint);
begin
  DrawStringOnCanvas(Canvas, Rect, Alignment, Margins, TBoldStringRendererData(Follower.RendererData).CurrentStringValue);
end;

procedure TBoldAsStringRenderer.MakeUpToDateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber);
var
  S: string;
begin
  S := GetAsStringAndSubscribe(Element, FollowerController as TBoldStringFollowerController, Subscriber);
  (RendererData as TBoldStringRendererData).OldStringValue := S;
  (RendererData as TBoldStringRendererData).CurrentStringValue := S;
end;

procedure TBoldAsStringRenderer.DefaultMakeUptodateAndSetMayModifyAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber);
var
  {$IFDEF BOLDCOMCLIENT} // defaultMakeUpToDate
  e: IBoldElement;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
  S: String;
begin
  S := '';
  if (csDesigning in ComponentState) and (Self<>DefaultRenderer) then
  begin
    if Name<>'' then
      S := '('+Name+')'
    else
      S := '('+ClassName+')';
    RendererData.MayModify := False;
  end
  else
  begin
    if Assigned(Element) then
    begin
      {$IFDEF BOLDCOMCLIENT} // defaultMakeUpToDate
      e := Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false);
      if Assigned(E) then
      begin
        S := E.StringRepresentation[FollowerController.Representation];
        if Assigned(Subscriber) then
          E.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber.ClientId, Subscriber.SubscriberId, breReEvaluate, false);
        RendererData.MayModify := true;
      end
      else
        S := FollowerController.NilStringRepresentation
      {$ELSE}
      E := TBoldIndirectElement.Create;
      try
        Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False, False, FollowerController.GetVariableListAndSubscribe(Subscriber));
        if (e.Value is TBoldObjectReference) and not assigned((e.Value as TBoldObjectReference).BoldObject) then
        begin
          s := FollowerController.NilStringRepresentation;
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
          RendererData.MayModify := E.Value.ObserverMayModifyAsString(FollowerController.Representation, Subscriber);
          (RendererData as TBoldStringRendererData).MaxStringLength := -1;
        end
        else if Assigned(E.Value) then
        begin
          S := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
          RendererData.MayModify := E.Value.ObserverMayModifyAsString(FollowerController.Representation, Subscriber);
          if (E.Value is TBoldAttribute) and assigned((E.Value as TBoldAttribute).BoldAttributeRTInfo) then
            (RendererData as TBoldStringRendererData).MaxStringLength := (E.Value as TBoldAttribute).BoldAttributeRTInfo.Length
          else
            (RendererData as TBoldStringRendererData).MaxStringLength := -1;
        end
        else
          S := FollowerController.NilStringRepresentation
      finally
        E.Free;
      end;
      {$ENDIF}
    end
    else
      S := FollowerController.NilStringRepresentation
  end;
  (RendererData as TBoldStringRendererData).OldStringValue := S;
  (RendererData as TBoldStringRendererData).CurrentStringValue := S;
end;

function TBoldAsStringRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldStringRendererData;
end;

function TBoldAsStringRenderer.DefaultGetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string;
var
  {$IFDEF BOLDCOMCLIENT} // DefaultGet
  e: IBoldElement;
  {$ELSE}
  E: TBoldIndirectElement;
  {$ENDIF}
begin
  Result := '';
  if (csDesigning in ComponentState) and (Self<>DefaultRenderer) then
  begin
    if Name<>'' then
      Result := '('+Name+')'
    else
      Result := '('+ClassName+')';
  end
  else
  begin
    if Assigned(Element) then
    begin
      {$IFDEF BOLDCOMCLIENT} // defaultGet
      e := Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber.ClientId, Subscriber.SubscriberId, False, false);
      if Assigned(E) then
      begin
        Result := E.StringRepresentation[FollowerController.Representation];
        if Assigned(Subscriber) then
          E.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber.ClientId, Subscriber.SubscriberId, breReEvaluate, false);
      end
      else
        Result := FollowerController.NILStringRepresentation;
      {$ELSE}
      E := TBoldIndirectElement.Create;
      try
        Element.EvaluateAndSubscribeToExpression(FollowerController.Expression, Subscriber, E, False, False, FollowerController.GetVariableListAndSubscribe(Subscriber));
        if (e.Value is TBoldObjectReference) and not assigned((e.Value as TBoldObjectReference).BoldObject) then
        begin
          result := FollowerController.NilStringRepresentation;
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end
        else if Assigned(E.Value) then
        begin
          Result := E.Value.StringRepresentation[FollowerController.Representation];
          if Assigned(Subscriber) then
            E.Value.SubscribeToStringRepresentation(FollowerController.Representation, Subscriber, breReEvaluate);
        end
        else
          Result := FollowerController.NILStringRepresentation;
      finally
        E.Free;
      end;
      {$ENDIF}
    end
    else
      Result := FollowerController.NILStringRepresentation;
  end;
end;

procedure TBoldAsStringRenderer.DefaultSetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    ValueElement.StringRepresentation[Representation] := Value
  else
    raise EBold.Create('TBoldAsStringRenderer.DefaultSetAsString: Can''t set string value');
end;

function TBoldAsStringRenderer.DefaultValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateCharacter(C, Representation)
  else
    Result := False;
end;

function TBoldAsStringRenderer.DefaultValidateString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
var
  ValueElement: TBoldElement;
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
    Result := ValueElement.ValidateString(Value, Representation)
  else
    Result := False;
end;

function TBoldAsStringRenderer.GetAsStringAndSubscribe(Element: TBoldElement; FollowerController: TBoldStringFollowerController; Subscriber: TBoldSubscriber): string;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, FollowerController.Representation, FollowerController.Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsString) then
    Result := OnGetAsString(Element, FollowerController.Representation, FollowerController.Expression)
  else
    Result := DefaultGetAsStringAndSubscribe(Element, FollowerController, Subscriber);
end;

procedure TBoldAsStringRenderer.SetAsString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  if Assigned(FOnSetAsString) then
    OnSetAsString(Element, Value, Representation, Expression)
  else
    DefaultSetAsString(Element, Value, Representation, Expression, VariableList)
end;

function TBoldAsStringRenderer.ValidateCharacter(Element: TBoldElement; C: AnsiChar; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnValidateCharacter) then
    Result := OnValidateCharacter(Element, C, Representation, Expression)
  else
    Result := DefaultValidateCharacter(Element, C, Representation, Expression, VariableList);
end;

function TBoldAsStringRenderer.ValidateString(Element: TBoldElement; Value: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(FOnValidateString) then
    Result := OnValidateString(Element, Value, Representation, Expression)
  else
    Result := DefaultValidateString(Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldAsStringRenderer.SetFont(Element: TBoldElement; EffectiveFont, Font: tFont; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  EffectiveFont.Assign(Font);
  if Assigned(fOnSetFont) then
    fOnSetFont(Element, EffectiveFont, Representation, Expression);
end;

procedure TBoldAsStringRenderer.SetColor(Element: TBoldElement; var EffectiveColor: tColor; COLOR: tColor; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  EffectiveColor := COLOR;
  if Assigned(fOnSetColor) then
    fOnSetColor(Element, EffectiveColor, Representation, Expression);
end;

  class function TBoldAsStringRenderer.DefaultRenderer: TBoldAsStringRenderer;
begin
  Result := DefaultAsStringRenderer;
end;

function TBoldAsStringRenderer.DefaultIsChanged(RendererData: TBoldStringRendererData; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := NewValue <> RendererData.OldStringValue;
end;

function TBoldAsStringRenderer.IsChanged(RendererData: TBoldStringRendererData; NewValue: string; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, Variablelist);
end;

{ TBoldStringRendererData }

constructor TBoldStringRendererData.Create(OwningFollower: TBoldFollower);
begin
  inherited;
  fMaxStringLength := -1;
end;

function TBoldAsStringRenderer.GetSupportsMulti: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := not  (Assigned(fOnGetAsString) or Assigned(OnSubscribe) or Assigned(OnMayModify));
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

function TBoldStringFollowerController.GetSupportsMultiEnsure: Boolean;
begin
  {$IFDEF BOLDCOMCLIENT}
  Result := False;
  {$ELSE}
  Result := (EffectiveRenderer = DefaultAsStringRenderer) and
    (pos('+', expression) = 0);
  {$ENDIF}
end;

procedure TBoldStringFollowerController.DoMultiMakeUptodateAndSubscribe(
  Followers: TBoldObjectArray);
var
  Renderer: TBoldAsStringRenderer;
  Elements: TBoldClientableList;
  Subscribers: TBoldObjectArray;
  RendererData: TBoldObjectArray;
  I: integer;
  MaxIndex: integer;
begin
  Assert(SupportsMulti);
  MaxIndex := Followers.Count - 1;
  Renderer := EffectiveRenderer as TBoldAsStringRenderer;
  Elements := TBoldClientableList.Create(MaxIndex,[]);
  Subscribers := TBoldObjectArray.Create(MaxIndex,[]);
  RendererData := TBoldObjectArray.Create(MaxIndex,[]);
  try
    for I := 0 to MaxIndex do
    begin
      Elements.Add(TBoldFollower(Followers[I]).Element);
      if TBoldFollower(Followers[I]).State in bfdNeedResubscribe then
        Subscribers.Add(Followers[i])
      else
        Subscribers.Add(nil);
      RendererData.Add(TBoldFollower(Followers[I]).RendererData);
    end;
    Renderer.MultiMakeUpToDateAndSubscribe(Elements, Subscribers, RendererData, Self);
  finally
    Elements.Free;
    Subscribers.Free;
    RendererData.Free;
  end;
end;

procedure TBoldAsStringRenderer.MultiMakeUpToDateAndSubscribe(
  Elements: TBoldClientableList; Subscribers,
  RendererData: TBoldObjectArray;
  FollowerController: TBoldFollowerController);
  {$IFDEF BOLDCOMCLIENT}
var
  I: integer;
  ResultStrings: variant;
  SubscriberIds: variant;
  ElementsInt: variant;
  ClientId: string;
  TheElement: IBoldElement;
  unk: IUnknown;
begin
  SubscriberIds := VarArrayCreate([0, Elements.Count - 1], varInteger);
  ElementsInt := VarArrayCreate([0, Elements.Count - 1], varUnknown);
  ClientId := '';
  for I := 0 to  Elements.Count - 1 do
  begin
    ElementsInt[I] := Elements[I] as IUnknown;
    unk := Elements[0];
    TheElement := unk as IBoldElement;
    if Assigned(Subscribers[i]) then
    begin
      SubscriberIds[I] := TBoldComClientSubscriber(Subscribers[i]).SubscriberId;
      ClientId := TBoldComClientSubscriber(Subscribers[i]).ClientId;
    end
    else
      SubscriberIds[I] := 0;
  end;
  ResultStrings := TheElement.MultiEvaluateExpressionAsStringList(ElementsInt,
      TBoldSingleFollowerControllerCom(FollowerController).Expression, ClientId, SubscriberIds,
      TBoldSingleFollowerControllerCom(FollowerController).Representation);
  for I := 0 to  Elements.Count - 1 do
    with RendererData[i] as TBoldStringRendererDataCom do
    begin
       OldStringValue := ResultStrings[I];
       CurrentStringValue := ResultStrings[I];
    end;
  {$ELSE}
  begin
  {$ENDIF}
end;

function TBoldStringFollowerController.GetAsString(Element: TBoldElement): string;
begin
  result := EffectiveAsStringRenderer.GetAsStringAndSubscribe(element, self, nil);
end;

initialization
  DefaultAsStringRenderer := TBoldAsStringRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsStringRenderer);

end.

