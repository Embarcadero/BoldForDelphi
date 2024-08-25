{ Global compiler directives }
{$include bold.inc}
unit BoldViewerControlPack;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Windows,
  Classes,
  Graphics,
  Clipbrd,
  BoldDefs,
  BoldControlPackDefs,
  BoldBase,
  BoldElements,
  {$IFDEF BOLDCOMCLIENT}
  BoldComUtils,
  {$ENDIF}
  BoldControlPack,
  BoldSubscription,
  BoldAttributes;

type
  { Forward declaration of classes }
  TBoldViewerFollowerController = class;
  TBoldAsViewerRenderer = class;
  TBoldViewerRendererData = class;
  TBoldAbstractViewAdapter = class;

  { TBoldAsViewerRenderer }
  {$IFDEF BOLD_BCB}
  TBoldGetAsViewer = procedure (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; var AbstractViewAdapter: TBoldAbstractViewAdapter) of object;
  {$ENDIF}
  {$IFDEF BOLD_DELPHI}
  TBoldGetAsViewer = function (aFollower: TBoldFollower): TBoldAbstractViewAdapter of object;
  {$ENDIF}
  TBoldSetAsViewer = procedure (aFollower: TBoldFollower; Value: TBoldAbstractViewAdapter) of object;
  TBoldViewerIsChanged = function (aFollower: TBoldFollower; NewValue: TBoldAbstractViewAdapter): Boolean of object;

  { TBoldViewerRendererData }
  TBoldViewerRendererData = class(TBoldRendererData)
  private
    fViewAdapter: TBoldAbstractViewAdapter;
    fHasChanged: Boolean;
    procedure SetViewAdapter(Value: TBoldAbstractViewAdapter);
  public
    property ViewAdapter: TBoldAbstractViewAdapter read FViewAdapter write SetViewAdapter;
    property HasChanged: Boolean read fHasChanged write fHasChanged;
    destructor Destroy; override;
  end;

  { TBoldAsViewerRenderer }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldAsViewerRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsViewer: TBoldGetAsViewer;
    FOnSetAsViewer: TBoldSetAsViewer;
    fOnIsChanged: TBoldViewerIsChanged;
  protected
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function GetAsViewerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter; virtual;
    procedure SetAsViewer(aFollower: TBoldFollower; Value: TBoldAbstractViewAdapter); virtual;
    function HasSetValueEventOverrides: boolean; override;
  public
    class function DefaultRenderer: TBoldAsViewerRenderer;
    function DefaultGetAsViewerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter; virtual;
    procedure DefaultSetAsViewer(aFollower: TBoldFollower; Value: TBoldAbstractViewAdapter); virtual;
    function DefaultIsChanged(aFollower: TBoldFollower; NewValue: TBoldAbstractViewAdapter): Boolean;
    function IsChanged(aFollower: TBoldFollower; NewValue: TBoldAbstractViewAdapter): Boolean;
    procedure MakeUptodateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber); override;
  published
    property OnGetAsViewer: TBoldGetAsViewer read FOnGetAsViewer write FOnGetAsViewer;
    property OnSetAsViewer: TBoldSetAsViewer read FOnSetAsViewer write FOnSetAsViewer;
    property OnIsChanged: TBoldViewerIsChanged read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldViewerFollowerController }
  TBoldViewerFollowerController = class(TBoldSingleFollowerController)
  private
    function GetRenderer: TBoldAsViewerRenderer;
    procedure SetRenderer(Value: TBoldAsViewerRenderer);
    function GetEffectiveAsViewerRenderer: TBoldAsViewerRenderer;
  protected
    function GetEffectiveRenderer: TBoldRenderer; override;
    property EffectiveAsViewerRenderer: TBoldAsViewerRenderer read GetEffectiveAsViewerRenderer;
  public
    procedure MakeClean(Follower: TBoldFollower); override;
    function GetCurrentViewer(Follower: TBoldFollower): TBoldAbstractViewAdapter;
    procedure SetAsViewer(Value: TBoldAbstractViewAdapter; Follower: TBoldFollower);
    procedure MayHaveChanged(NewValue: TBoldAbstractViewAdapter; Follower: TBoldFollower);
  published
    property Renderer: TBoldAsViewerRenderer read GetRenderer write SetRenderer;
  end;

  {-- TBoldAbstractViewAdapter --}
  TBoldViewAdapterClass = class of TBoldAbstractViewAdapter;
  TBoldAbstractViewAdapter = class(TBoldMemoryManagedObject)
  public
    constructor Create; virtual;
    class procedure RegisterViewAdapter(ViewAdapterClass: TBoldViewAdapterClass);
    class function ViewAdapterClassCount: integer;
    class function GetViewAdapterClass(index: integer): TBoldViewAdapterClass;
    {Content}
    function Empty: Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function HasChanged: Boolean; virtual; abstract;
    class function CanReadContent(const ContentType: string): Boolean; virtual; 
    function ContentType: string; virtual; abstract;
    class function Description: string; virtual;
    {Clipboard}
    procedure CopyToClipboard; virtual; abstract;
    class function CanPasteFromClipboard(const AcceptedContentType: string): Boolean; virtual; 
    procedure PasteFromClipboard; virtual; abstract;
    {Streams}
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    {Files}
    class function DefaultExtension: string; virtual; 
    class function FileFilter: string; virtual;
    class function CanLoadFromFile(const Filename: string): Boolean; virtual;
    procedure LoadFromFile(const Filename: string); virtual; abstract;
    procedure SaveToFile(const Filename: string); virtual; abstract;
    {Canvas}
    procedure Paint(Canvas: TCanvas; Rect: TRect); virtual; abstract;
    function GetPalette: HPALETTE; virtual; abstract;
    function Width: Integer; virtual; abstract;
    function Height: Integer; virtual; abstract;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldImageBitmap,
  BoldGuard;

var
  DefaultAsViewerRenderer: TBoldAsViewerRenderer;
  ViewAdapterList: TList;

{ TBoldViewerRendererData }

destructor TBoldViewerRendererData.Destroy;
begin
  FreeAndNil(fViewAdapter);
  inherited Destroy;
end;

procedure TBoldViewerRendererData.SetViewAdapter(Value: TBoldAbstractViewAdapter);
begin
  if Value <> fViewAdapter then
  begin
    FreeAndNil(fViewAdapter);
    fViewAdapter := Value;
    fHasChanged := True;
  end;
end;

{ TBoldAsViewerRenderer }

procedure TBoldAsViewerRenderer.MakeUpToDateAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
var
  lRendererData: TBoldViewerRendererData;
begin
  lRendererData := aFollower.RendererData as TBoldViewerRendererData;
  lRendererData.ViewAdapter := GetAsViewerAndSubscribe(aFollower, Subscriber);
  lRendererData.HasChanged := False;
end;

class function TBoldAsViewerRenderer.DefaultRenderer: TBoldAsViewerRenderer;
begin
  Result := DefaultAsViewerRenderer;
end;

function TBoldAsViewerRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldViewerRendererData;
end;

function TBoldAsViewerRenderer.HasSetValueEventOverrides: boolean;
begin
  result := Assigned(FOnSetAsViewer);
end;

function TBoldAsViewerRenderer.DefaultGetAsViewerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter;
var
  e: TBoldElement;
  {$IFDEF BOLDCOMCLIENT}
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
  blob: TBABlob;
  lResultElement: TBoldElement;
  lGuard: IBoldGuard;
  {$ENDIF}

  function GetViewer: TBoldAbstractViewAdapter;
  var
    I: Integer;
  begin
    Result := nil;
    for I:=0 to TBoldAbstractViewAdapter.ViewAdapterClassCount - 1 do
      if TBoldAbstractViewAdapter.GetViewAdapterClass(I).CanReadContent(e.StringRepresentation[brShort]) then
      begin
        Result := TBoldAbstractViewAdapter.GetViewAdapterClass(I).Create;
        break;
      end;
  end;

begin
  Result := nil;
  if Assigned(aFollower.Element) then
  begin
    {$IFDEF BOLDCOMCLIENT}
    if assigned(Subscriber) then
      e := Element.EvaluateAndSubscribeToExpression(Expression, Subscriber.ClientId, Subscriber.SubscriberId, false, false)
    else
      e := Element.EvaluateExpression(Expression);
    if Assigned(e) and
      (e.QueryInterface(IBoldAttribute, Attr) = S_OK) and
      not attr.IsNull then
    begin
      Result := GetViewer;
      if Assigned(Result) then
      begin
        Stream := TMemoryStream.Create;
        BoldVariantToStream(attr.AsVariant, Stream);
        try
          Result.LoadFromStream(Stream);
        finally
          Stream.Free;
        end;
      end
      else
        raise EBold.CreateFmt('Viewer not available (%s)', [attr.StringRepresentation[brShort]])
    end
    else
      Result := nil;
    {$ELSE}
    lResultElement := aFollower.Value;
    if not Assigned(lResultElement) then
    begin
      lGuard:= TBoldGuard.Create(IndirectElement);
      IndirectElement := TBoldIndirectElement.Create;
      aFollower.Element.EvaluateAndSubscribeToExpression(aFollower.AssertedController.Expression, Subscriber, IndirectElement, False, False, aFollower.Controller.GetVariableListAndSubscribe(Subscriber));
      lResultElement := IndirectElement.Value;
    end;
    if lResultElement is TBABlob then
    begin
      blob := TBABlob(lResultElement);
      if not Blob.IsNull then
      begin
        e := Blob;
        Result := GetViewer;
        if Assigned(Result) then
        begin
          Result.LoadFromStream(blob.AsStream);
        end
        else
          raise EBold.CreateFmt('Viewer not available (%s)', [Blob.ContentType])
      end
      else
        Result := nil;
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsViewerRenderer.DefaultSetAsViewer(aFollower: TBoldFollower; Value: TBoldAbstractViewAdapter);
var
  ValueElement: TBoldElement;
  {$IFDEF BOLDCOMCLIENT}
  Attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := aFollower.Value;
  if Assigned(ValueElement) then
  begin
    {$IFDEF BOLDCOMCLIENT}
    ValueElement.QueryInterface(IBoldAttribute, Attr);
    if Assigned(Value) then
    begin
      Stream := TMemoryStream.Create;
      try
        Value.SaveToStream(Stream);
        Attr.AsVariant := BoldStreamToVariant(Stream);
        Attr.StringRepresentation[brShort] := Value.ContentType;
      finally
        Stream.Free;
      end;
    end
    else
      Attr.SetToNull;
    {$ELSE}
    if Assigned(Value) then
    begin
      Value.SaveToStream((ValueElement as TBABlob).AsStream);
      (ValueElement as TBABlob).ContentType := Value.ContentType;
    end
    else
      (ValueElement as TBABlob).SetToNull;
    {$ENDIF}
  end
  else
    raise EBold.CreateFmt('%s.DefaultSetAsViewer: Can''t set value', [ClassName]);
end;

function TBoldAsViewerRenderer.GetAsViewerAndSubscribe(aFollower: TBoldFollower; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(aFollower.Element) then
      OnSubscribe(aFollower, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsViewer) then
  begin
    {$IFDEF BOLD_BCB}
    Result := nil;
    OnGetAsViewer(Element, Representation, Expression, Result);
    {$ENDIF}
    {$IFDEF BOLD_DELPHI}
    Result := OnGetAsViewer(aFollower);
    {$ENDIF}
  end
  else
    Result := DefaultGetAsViewerAndSubscribe(aFollower, Subscriber);
end;

procedure TBoldAsViewerRenderer.SetAsViewer(aFollower: TBoldFollower; Value: TBoldAbstractViewAdapter);
begin
  if Assigned(FOnSetAsViewer) then
    OnSetAsViewer(aFollower, Value)
  else
    DefaultSetAsViewer(aFollower, Value);
end;

function TBoldAsViewerRenderer.DefaultIsChanged(aFollower: TBoldFollower; NewValue: TBoldAbstractViewAdapter): Boolean;
begin
  Result := (TBoldViewerRendererData(aFollower.RendererData).HasChanged) or (Assigned(NewValue) and NewValue.HasChanged);
end;

function TBoldAsViewerRenderer.IsChanged(aFollower: TBoldFollower; NewValue: TBoldAbstractViewAdapter): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(aFollower, NewValue)
  else
    Result := DefaultIsChanged(aFollower, NewValue);
end;

{ TBoldViewerFollowerController }

function TBoldViewerFollowerController.GetRenderer: TBoldAsViewerRenderer;
begin
  Result := UntypedRenderer as TBoldAsViewerRenderer;
end;

procedure TBoldViewerFollowerController.SetRenderer(Value: TBoldAsViewerRenderer);
begin
  UntypedRenderer := Value;
end;

function TBoldViewerFollowerController.GetEffectiveRenderer: TBoldRenderer;
begin
  Result := EffectiveAsViewerRenderer;
end;

function TBoldViewerFollowerController.GetEffectiveAsViewerRenderer: TBoldAsViewerRenderer;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsViewerRenderer.DefaultRenderer;
end;

function TBoldViewerFollowerController.GetCurrentViewer(Follower: TBoldFollower): TBoldAbstractViewAdapter;
begin
  Result := (Follower.RendererData as TBoldViewerRendererData).ViewAdapter;
end;

procedure TBoldViewerFollowerController.SetAsViewer(Value: TBoldAbstractViewAdapter; Follower: TBoldFollower);
begin
  EffectiveAsViewerRenderer.SetAsViewer(Follower, Value);
end;

procedure TBoldViewerFollowerController.MayHaveChanged(NewValue: TBoldAbstractViewAdapter; Follower: TBoldFollower);
var
  lIsChanged: boolean;
  lRendererData: TBoldViewerRendererData;
begin
  if Follower.State in bfsDisplayable then
  begin
    lRendererData := Follower.RendererData as TBoldViewerRendererData;
    lRendererData.ViewAdapter := NewValue;
    lIsChanged := EffectiveAsViewerRenderer.IsChanged(Follower, NewValue);
    if lIsChanged then
    begin
      Follower.ControlledValueChanged;
    end;
  end;
end;

procedure TBoldViewerFollowerController.MakeClean(Follower: TBoldFollower);
begin
  ReleaseChangedValue(Follower);
  SetAsViewer(GetCurrentViewer(Follower), Follower);
end;

{-- TBoldAbstractViewAdapter --}

constructor TBoldAbstractViewAdapter.Create;
begin
end;

class procedure TBoldAbstractViewAdapter.RegisterViewAdapter(ViewAdapterClass: TBoldViewAdapterClass);
begin
  if not Assigned(ViewAdapterList) then
    ViewAdapterList := TList.Create;
  ViewAdapterList.Add(ViewAdapterClass);
end;

class function TBoldAbstractViewAdapter.CanReadContent(const ContentType: string): Boolean;
begin
  Result := False;
end;

class function TBoldAbstractViewAdapter.CanPasteFromClipboard(const AcceptedContentType: string): Boolean;
begin
  Result := False;
end;

class function TBoldAbstractViewAdapter.CanLoadFromFile(const Filename: string): Boolean;
begin
  Result := False;
end;

class function TBoldAbstractViewAdapter.DefaultExtension: string;
begin
  Result := '';
end;

class function TBoldAbstractViewAdapter.FileFilter: string;
begin
  Result := '';
end;

class function TBoldAbstractViewAdapter.Description: string;
begin
  Result := '';
end;

class function TBoldAbstractViewAdapter.ViewAdapterClassCount: integer;
begin
  if Assigned(ViewAdapterList) then
    Result := ViewAdapterList.Count
  else
    Result := 0;
end;

class function TBoldAbstractViewAdapter.GetViewAdapterClass(index: integer): TBoldViewAdapterClass;
begin
  Result := nil;
  if Assigned(ViewAdapterList) then
    Result := ViewAdapterList[index];
end;

initialization
  DefaultAsViewerRenderer := TBoldAsViewerRenderer.Create(nil);

finalization
  FreeAndNil(DefaultAsViewerRenderer);
  FreeAndNil(ViewAdapterList);
  
end.
