{ Global compiler directives }
{$include bold.inc}
unit BoldNewObjectInterceptor;

interface

uses
  Classes,
  BoldElements,
  BoldSubscription,
  BoldSystem,
  BoldAttributes,
  BoldPlaceableSubscriber;

type
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldNewObjectInterceptor = class(TBoldPlaceableSubscriber)
  private
    fActive: boolean;
    fFileName: string;
    fStringList: TStringList;
    fOverrideInitialValue: Boolean;
    function EnsureStringList: boolean;
    function GetWordList: TStringList;
    procedure SetFileName(const Value: string);
  protected
    function GetRandomWord: string; virtual;
    procedure SubscribeToElement(element: TBoldElement; Subscriber: TBoldSubscriber); override;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const); override;
    procedure InitializeAttribute(Attribute: TBoldAttribute); virtual;
    procedure SetRandomAttributes(BoldObject: TBoldObject);
  public
    destructor Destroy; override;
    property WordList: TStringList read GetWordList;
  published
    property Active: boolean read fActive write fActive;
    property Filename: string read fFileName write SetFileName;
    property OverrideInitialValue: Boolean read fOverrideInitialValue write fOverrideInitialValue;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldNewObjectInterceptor }

destructor TBoldNewObjectInterceptor.Destroy;
begin
  inherited;
  FreeAndNil(fStringList);
end;

function TBoldNewObjectInterceptor.EnsureStringList: boolean;
begin
  if not Assigned(fStringList) then
  begin
    fStringList := TStringList.Create;
    if FileExists(Filename) then
    begin
      try
        fStringList.LoadFromFile(FileName);
      except
        fStringList.Clear;
      end;
    end;
  end;
  Result := Assigned(fStringList);
end;

function TBoldNewObjectInterceptor.GetRandomWord: string;
begin
  if EnsureStringList and (fStringList.Count > 0) then
    Result := fStringList[Random(fStringList.Count)]
  else
    result := '';
end;

function TBoldNewObjectInterceptor.GetWordList: TStringList;
begin
  EnsureStringList;
  Result := fStringList;
end;

procedure TBoldNewObjectInterceptor.InitializeAttribute(Attribute: TBoldAttribute);
var
  i: integer;
begin
  if Attribute is TBAInteger then
  begin
    repeat
      i := Random(20000) - 10000;
    until (Attribute as TBAInteger).CheckRange(i);
    (Attribute as TBAInteger).AsInteger := i;
  end

  else if Attribute is TBAFloat then
    (Attribute as TBAFloat).AsFloat := Random(10000) + random

  else if Attribute is TBACurrency then
    (Attribute as TBACurrency).AsCurrency := Random(10000) + random

  else if Attribute is TBAString then
    Attribute.AsString := Copy(GetRandomWord, 1, Attribute.BoldAttributeRTInfo.Length)

  else if Attribute is TBADate then
    (Attribute as TBAdate).AsDate := round(now) - Random(365*30) + 365*10

  else if Attribute is TBADateTime then
    (Attribute as TBADateTime).AsDateTime := round(now) - Random(365*30) + 365*10 + random

  else if Attribute is TBATime then
    (Attribute as TBATime).AsTime := random

  else if Attribute is TBAValueSet then
    (Attribute as TBAValueSet).AsInteger := random((Attribute as TBAValueSet).Values.Count)
end;

procedure TBoldNewObjectInterceptor.Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
var
  Locator: TBoldObjectLocator;
begin
  inherited;

  if Active and (OriginalEvent = beItemAdded) then
  begin
    if (originator is TBoldObjectList) then
    begin
      if Args[0].VObject is TBoldObjectLocator then
      begin
        Locator := Args[0].VObject as TBoldObjectLocator;
        if not Locator.BoldObjectID.IsStorable then
          SetRandomAttributes(Locator.BoldObject);
      end;
    end;
  end;
end;

procedure TBoldNewObjectInterceptor.SetFileName(const Value: string);
begin
  FreeAndNil(fStringList);
  fFileName := Value;
end;

procedure TBoldNewObjectInterceptor.SetRandomAttributes(BoldObject: TBoldObject);
var
  i: integer;
  BoldAttribute: TBoldAttribute;
begin
  for i := 0 to BoldObject.BoldMembercount - 1 do
  begin
    if not BoldObject.BoldMembers[i].Derived and (BoldObject.BoldMembers[i] is TBoldAttribute) then
    begin
      BoldAttribute := BoldObject.BoldMembers[i] as TBoldAttribute;
      if OverrideInitialValue or not BoldAttribute.BoldAttributeRTInfo.HasInitialValue then
        InitializeAttribute(BoldAttribute);
    end;
  end;
end;

procedure TBoldNewObjectInterceptor.SubscribeToElement(
  element: TBoldElement; Subscriber: TBoldSubscriber);
begin
  inherited;
  if Element is TBoldSystem then
    (Element as TBoldSystem).Classes[0].DefaultSubscribe(Subscriber, breReEvaluate);
end;

end.
