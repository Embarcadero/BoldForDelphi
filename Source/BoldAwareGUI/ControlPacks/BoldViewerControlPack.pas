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
  TBoldGetAsViewer = function (Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TBoldAbstractViewAdapter of object;
  {$ENDIF}
  TBoldSetAsViewer = procedure (Element: TBoldElement; Value: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldViewerIsChanged = function (RendererData: TBoldViewerRendererData; NewValue: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

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
  TBoldAsViewerRenderer = class(TBoldSingleRenderer)
  private
    FOnGetAsViewer: TBoldGetAsViewer;
    FOnSetAsViewer: TBoldSetAsViewer;
    fOnIsChanged: TBoldViewerIsChanged;
  protected
    function GetRendererDataClass: TBoldRendererDataClass; override;
    function GetAsViewerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter; virtual;
    procedure SetAsViewer(Element: TBoldElement; Value: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
  public
    class function DefaultRenderer: TBoldAsViewerRenderer;
    function DefaultGetAsViewerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter; virtual;
    procedure DefaultSetAsViewer(Element: TBoldElement; Value: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList); virtual;
    function DefaultIsChanged(RendererData: TBoldViewerRendererData; NewValue: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    function IsChanged(RendererData: TBoldViewerRendererData; NewValue: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
    procedure MakeUptodateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber); override;
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

  // BCB does not support abstract class methods
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
    class function Description: string; virtual;  // How to handle Localizastion?
    {Clipboard}
    procedure CopyToClipboard; virtual; abstract;
    class function CanPasteFromClipboard(const AcceptedContentType: string): Boolean; virtual;
    procedure PasteFromClipboard; virtual; abstract;
    {Streams}
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;
    {Files}
    class function DefaultExtension: string; virtual;
    class function FileFilter: string; virtual;  // How to handle Localizastion?
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
  BoldGuiResourceStrings,
  BoldUtils,
  BoldImageBitmap; //FIXME Temp!

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

procedure TBoldAsViewerRenderer.MakeUpToDateAndSubscribe(Element: TBoldElement; RendererData: TBoldRendererData; FollowerController: TBoldFollowerController; Subscriber: TBoldSubscriber);
var
  Controller: TBoldViewerFollowerController;
begin
  Controller := FollowerController as TBoldViewerFollowerController;
  (RendererData as TBoldViewerRendererData).ViewAdapter := GetAsViewerAndSubscribe(Element, Controller.Representation, Controller.Expression, Controller.GetVariableListAndSubscribe(Subscriber), Subscriber);
  (RendererData as TBoldViewerRendererData).HasChanged := False;
end;

class function TBoldAsViewerRenderer.DefaultRenderer: TBoldAsViewerRenderer;
begin
  Result := DefaultAsViewerRenderer;
end;

function TBoldAsViewerRenderer.GetRendererDataClass: TBoldRendererDataClass;
begin
  Result := TBoldViewerRendererData;
end;

function TBoldAsViewerRenderer.DefaultGetAsViewerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter;
var
  e: TBoldElement;
  Stream: TStream;
  {$IFDEF BOLDCOMCLIENT} // defaultGet
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
  blob: TBABlob;
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
  if Assigned(Element) then
  begin
    {$IFDEF BOLDCOMCLIENT} // defaultGet
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
        raise EBold.CreateFmt(sViewerNotAvailable, [attr.StringRepresentation[brShort]])
    end
    else
      Result := nil;
    {$ELSE}
    IndirectElement := TBoldIndirectElement.Create;
    try
      Element.EvaluateAndSubscribeToExpression(Expression, Subscriber, IndirectElement, False, false, VariableList);
      e := IndirectElement.Value;
      if e is TBABlob then
      begin
        blob := TBABlob(e);
        if not Blob.IsNull then
        begin
          Result := GetViewer;
          if Assigned(Result) then
          begin
            Stream := blob.CreateBlobStream(bmRead);
            try
              Result.LoadFromStream(Stream);
            finally
              Stream.Free;
            end;
          end
          else
            raise EBold.CreateFmt(sViewerNotAvailable, [Blob.ContentType])
        end
        else
          Result := nil;
      end;
    finally
      IndirectElement.Free;
    end;
    {$ENDIF}
  end;
end;

procedure TBoldAsViewerRenderer.DefaultSetAsViewer(Element: TBoldElement; Value: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
var
  ValueElement: TBoldElement;
  Stream: TStream;
  {$IFDEF BOLDCOMCLIENT} // defaultSet
  Attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
  if Assigned(ValueElement) then
  begin
    {$IFDEF BOLDCOMCLIENT} // DefaultSet
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
      Stream := (ValueElement as TBABlob).CreateBlobStream(bmWrite);
      try
        Value.SaveToStream(Stream);
        (ValueElement as TBABlob).ContentType := Value.ContentType;
      finally
        Stream.Free;
      end;
    end
    else
      (ValueElement as TBABlob).SetToNull;
    {$ENDIF}
  end
  else
    raise EBold.CreateFmt(sCannotSetValue, [ClassName]);
end;

function TBoldAsViewerRenderer.GetAsViewerAndSubscribe(Element: TBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList; Subscriber: TBoldSubscriber): TBoldAbstractViewAdapter;
begin
  if Assigned(OnSubscribe) and Assigned(Subscriber) then
  begin
    if Assigned(Element) then
      OnSubscribe(Element, Representation, Expression, Subscriber);
    Subscriber := nil;
  end;
  if Assigned(OnGetAsViewer) then
  begin
    {$IFDEF BOLD_BCB}
    Result := nil;
    OnGetAsViewer(Element, Representation, Expression, Result);
    {$ENDIF}
    {$IFDEF BOLD_DELPHI}
    Result := OnGetAsViewer(Element, Representation, Expression);
    {$ENDIF}
  end
  else
    Result := DefaultGetAsViewerAndSubscribe(Element, Representation, Expression, VariableList, Subscriber);
end;

procedure TBoldAsViewerRenderer.SetAsViewer(Element: TBoldElement; Value: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList);
begin
  if Assigned(FOnSetAsViewer) then
    OnSetAsViewer(Element, Value, Representation, Expression)
  else
    DefaultSetAsViewer(Element, Value, Representation, Expression, VariableList);
end;

function TBoldAsViewerRenderer.DefaultIsChanged(RendererData: TBoldViewerRendererData; NewValue: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  Result := (RendererData.HasChanged) or (Assigned(NewValue) and NewValue.HasChanged);
end;

function TBoldAsViewerRenderer.IsChanged(RendererData: TBoldViewerRendererData; NewValue: TBoldAbstractViewAdapter; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: TBoldExternalVariableList): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
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
  EffectiveAsViewerRenderer.SetAsViewer(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldViewerFollowerController.MayHaveChanged(NewValue: TBoldAbstractViewAdapter; Follower: TBoldFollower);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldViewerRendererData).ViewAdapter := NewValue;
    Follower.ControlledValueChanged(EffectiveAsViewerRenderer.IsChanged(Follower.RendererData as TBoldViewerRendererData, NewValue, Representation, Expression, VariableList));
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
  // Left for subclasses to implement
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

