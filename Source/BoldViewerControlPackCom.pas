
{ Global compiler directives }
{$include bold.inc}
unit BoldViewerControlPackCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 13:13:02}

interface

uses
  Windows,
  Classes,
  Graphics,
  Clipbrd,
  BoldDefs,
  BoldControlPackDefs,
  BoldBase,
  BoldComObjectSpace_TLB, BoldClientElementSupport, BoldComClient,
  {$IFDEF BOLDCOMCLIENT}
  BoldComUtils,
  {$ENDIF}
  BoldControlPackCom,
  BoldSubscription,
  BoldAttributes;

type
  { Forward declaration of classes }
  TBoldViewerFollowerControllerCom = class;
  TBoldAsViewerRendererCom = class;
  TBoldViewerRendererDataCom = class;
  TBoldAbstractViewAdapterCom = class;

  { TBoldAsViewerRendererCom }
  {$IFDEF BOLD_BCB}
  TBoldGetAsViewerCom = procedure (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; var AbstractViewAdapter: TBoldAbstractViewAdapterCom) of object;
  {$ENDIF}
  {$IFDEF BOLD_DELPHI}
  TBoldGetAsViewerCom = function (Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression): TBoldAbstractViewAdapterCom of object;
  {$ENDIF}
  TBoldSetAsViewerCom = procedure (Element: IBoldElement; Value: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression) of object;
  TBoldViewerIsChangedCom = function (RendererData: TBoldViewerRendererDataCom; NewValue: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression): Boolean of object;

  { TBoldViewerRendererDataCom }
  TBoldViewerRendererDataCom = class(TBoldFollowerDataCom)
  private
    fViewAdapter: TBoldAbstractViewAdapterCom;
    fHasChanged: Boolean;
    procedure SetViewAdapter(Value: TBoldAbstractViewAdapterCom);
  public
    property ViewAdapter: TBoldAbstractViewAdapterCom read FViewAdapter write SetViewAdapter;
    property HasChanged: Boolean read fHasChanged write fHasChanged;
    destructor Destroy; override;
  end;

  { TBoldAsViewerRendererCom }
  TBoldAsViewerRendererCom = class(TBoldSingleRendererCom)
  private
    FOnGetAsViewer: TBoldGetAsViewerCom;
    FOnSetAsViewer: TBoldSetAsViewerCom;
    fOnIsChanged: TBoldViewerIsChangedCom;
  protected
    function GetRendererDataClass: TBoldRendererDataClassCom; override;
    function GetAsViewerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TBoldAbstractViewAdapterCom; virtual;
    procedure SetAsViewer(Element: IBoldElement; Value: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
  public
    class function DefaultRenderer: TBoldAsViewerRendererCom;
    function DefaultGetAsViewerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TBoldAbstractViewAdapterCom; virtual;
    procedure DefaultSetAsViewer(Element: IBoldElement; Value: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList); virtual;
    function DefaultIsChanged(RendererData: TBoldViewerRendererDataCom; NewValue: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
    function IsChanged(RendererData: TBoldViewerRendererDataCom; NewValue: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
    procedure MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber); override;
  published
    property OnGetAsViewer: TBoldGetAsViewerCom read FOnGetAsViewer write FOnGetAsViewer;
    property OnSetAsViewer: TBoldSetAsViewerCom read FOnSetAsViewer write FOnSetAsViewer;
    property OnIsChanged: TBoldViewerIsChangedCom read fOnIsChanged write fOnIsChanged;
  end;

  { TBoldViewerFollowerControllerCom }
  TBoldViewerFollowerControllerCom = class(TBoldSingleFollowerControllerCom)
  private
    function GetRenderer: TBoldAsViewerRendererCom;
    procedure SetRenderer(Value: TBoldAsViewerRendererCom);
    function GetEffectiveAsViewerRenderer: TBoldAsViewerRendererCom;
  protected
    function GetEffectiveRenderer: TBoldRendererCom; override;
    property EffectiveAsViewerRenderer: TBoldAsViewerRendererCom read GetEffectiveAsViewerRenderer;
  public
    procedure MakeClean(Follower: TBoldFollowerCom); override;
    function GetCurrentViewer(Follower: TBoldFollowerCom): TBoldAbstractViewAdapterCom;
    procedure SetAsViewer(Value: TBoldAbstractViewAdapterCom; Follower: TBoldFollowerCom);
    procedure MayHaveChanged(NewValue: TBoldAbstractViewAdapterCom; Follower: TBoldFollowerCom);
  published
    property Renderer: TBoldAsViewerRendererCom read GetRenderer write SetRenderer;
  end;

  {-- TBoldAbstractViewAdapterCom --}
  TBoldViewAdapterClassCom = class of TBoldAbstractViewAdapterCom;
  TBoldAbstractViewAdapterCom = class(TBoldMemoryManagedObject)
  public
    constructor Create; virtual;
    class procedure RegisterViewAdapter(ViewAdapterClass: TBoldViewAdapterClassCom);
    class function ViewAdapterClassCount: integer;
    class function GetViewAdapterClass(index: integer): TBoldViewAdapterClassCom;
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
  BoldImageBitmapCom;

var
  DefaultAsViewerRenderer: TBoldAsViewerRendererCom;
  ViewAdapterList: TList;

{ TBoldViewerRendererDataCom }

destructor TBoldViewerRendererDataCom.Destroy;
begin
  FreeAndNil(fViewAdapter);
  inherited Destroy;
end;

procedure TBoldViewerRendererDataCom.SetViewAdapter(Value: TBoldAbstractViewAdapterCom);
begin
  if Value <> fViewAdapter then
  begin
    FreeAndNil(fViewAdapter);
    fViewAdapter := Value;
    fHasChanged := True;
  end;
end;

{ TBoldAsViewerRendererCom }

procedure TBoldAsViewerRendererCom.MakeUptoDateAndSubscribe(Element: IBoldElement; RendererData: TBoldFollowerDataCom; FollowerController: TBoldFollowerControllerCom; Subscriber: TBoldComClientSubscriber);
var
  Controller: TBoldViewerFollowerControllerCom;
begin
  Controller := FollowerController as TBoldViewerFollowerControllerCom;
  (RendererData as TBoldViewerRendererDataCom).ViewAdapter := GetAsViewerAndSubscribe(Element, Controller.Representation, Controller.Expression, Controller.GetVariableListAndSubscribe(Subscriber), Subscriber);
  (RendererData as TBoldViewerRendererDataCom).HasChanged := False;
end;

class function TBoldAsViewerRendererCom.DefaultRenderer: TBoldAsViewerRendererCom;
begin
  Result := DefaultAsViewerRenderer;
end;

function TBoldAsViewerRendererCom.GetRendererDataClass: TBoldRendererDataClassCom;
begin
  Result := TBoldViewerRendererDataCom;
end;

function TBoldAsViewerRendererCom.DefaultGetAsViewerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TBoldAbstractViewAdapterCom;
var
  e: IBoldElement;
  Stream: TStream;
  {$IFDEF BOLDCOMCLIENT}
  attr: IBoldAttribute;
  {$ELSE}
  IndirectElement: TBoldIndirectElement;
  blob: TBABlob;
  {$ENDIF}

  function GetViewer: TBoldAbstractViewAdapterCom;
  var
    I: Integer;
  begin
    Result := nil;
    for I:=0 to TBoldAbstractViewAdapterCom.ViewAdapterClassCount - 1 do
      if TBoldAbstractViewAdapterCom.GetViewAdapterClass(I).CanReadContent(e.StringRepresentation[brShort]) then
      begin
        Result := TBoldAbstractViewAdapterCom.GetViewAdapterClass(I).Create;
        break;
      end;
  end;

begin
  Result := nil;
  if Assigned(Element) then
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
            raise EBold.CreateFmt('Viewer not available (%s)', [Blob.ContentType])
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

procedure TBoldAsViewerRendererCom.DefaultSetAsViewer(Element: IBoldElement; Value: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
var
  ValueElement: IBoldElement;
  Stream: TStream;
  {$IFDEF BOLDCOMCLIENT}
  Attr: IBoldAttribute;
  {$ENDIF}
begin
  ValueElement := GetExpressionAsDirectElement(Element, Expression, VariableList);
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
    raise EBold.CreateFmt('%s.DefaultSetAsViewer: Can''t set value', [ClassName]);
end;

function TBoldAsViewerRendererCom.GetAsViewerAndSubscribe(Element: IBoldElement; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList; Subscriber: TBoldComClientSubscriber): TBoldAbstractViewAdapterCom;
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

procedure TBoldAsViewerRendererCom.SetAsViewer(Element: IBoldElement; Value: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList);
begin
  if Assigned(FOnSetAsViewer) then
    OnSetAsViewer(Element, Value, Representation, Expression)
  else
    DefaultSetAsViewer(Element, Value, Representation, Expression, VariableList);
end;

function TBoldAsViewerRendererCom.DefaultIsChanged(RendererData: TBoldViewerRendererDataCom; NewValue: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  Result := (RendererData.HasChanged) or (Assigned(NewValue) and NewValue.HasChanged);
end;

function TBoldAsViewerRendererCom.IsChanged(RendererData: TBoldViewerRendererDataCom; NewValue: TBoldAbstractViewAdapterCom; Representation: TBoldRepresentation; Expression: TBoldExpression; VariableList: IBoldExternalVariableList): Boolean;
begin
  if Assigned(fOnIsChanged) then
    Result := fOnIsChanged(RendererData, NewValue, Representation, Expression)
  else
    Result := DefaultIsChanged(RendererData, NewValue, Representation, Expression, VariableList);
end;

{ TBoldViewerFollowerControllerCom }

function TBoldViewerFollowerControllerCom.GetRenderer: TBoldAsViewerRendererCom;
begin
  Result := UntypedRenderer as TBoldAsViewerRendererCom;
end;

procedure TBoldViewerFollowerControllerCom.SetRenderer(Value: TBoldAsViewerRendererCom);
begin
  UntypedRenderer := Value;
end;

function TBoldViewerFollowerControllerCom.GetEffectiveRenderer: TBoldRendererCom;
begin
  Result := EffectiveAsViewerRenderer;
end;

function TBoldViewerFollowerControllerCom.GetEffectiveAsViewerRenderer: TBoldAsViewerRendererCom;
begin
  if Assigned(Renderer) then
    Result := Renderer
  else
    Result := TBoldAsViewerRendererCom.DefaultRenderer;
end;

function TBoldViewerFollowerControllerCom.GetCurrentViewer(Follower: TBoldFollowerCom): TBoldAbstractViewAdapterCom;
begin
  Result := (Follower.RendererData as TBoldViewerRendererDataCom).ViewAdapter;
end;

procedure TBoldViewerFollowerControllerCom.SetAsViewer(Value: TBoldAbstractViewAdapterCom; Follower: TBoldFollowerCom);
begin
  EffectiveAsViewerRenderer.SetAsViewer(Follower.Element, Value, Representation, Expression, VariableList);
end;

procedure TBoldViewerFollowerControllerCom.MayHaveChanged(NewValue: TBoldAbstractViewAdapterCom; Follower: TBoldFollowerCom);
begin
  if Follower.State in bfsDisplayable then
  begin
    (Follower.RendererData as TBoldViewerRendererDataCom).ViewAdapter := NewValue;
    Follower.ControlledValueChanged(EffectiveAsViewerRenderer.IsChanged(Follower.RendererData as TBoldViewerRendererDataCom, NewValue, Representation, Expression, VariableList));
  end;
end;

procedure TBoldViewerFollowerControllerCom.MakeClean(Follower: TBoldFollowerCom);
begin
  ReleaseChangedValue(Follower);
  SetAsViewer(GetCurrentViewer(Follower), Follower);
end;

{-- TBoldAbstractViewAdapterCom --}

constructor TBoldAbstractViewAdapterCom.Create;
begin
end;

class procedure TBoldAbstractViewAdapterCom.RegisterViewAdapter(ViewAdapterClass: TBoldViewAdapterClassCom);
begin
  if not Assigned(ViewAdapterList) then
    ViewAdapterList := TList.Create;
  ViewAdapterList.Add(ViewAdapterClass);
end;

class function TBoldAbstractViewAdapterCom.CanReadContent(const ContentType: string): Boolean;
begin
  Result := False;
end;

class function TBoldAbstractViewAdapterCom.CanPasteFromClipboard(const AcceptedContentType: string): Boolean;
begin
  Result := False;
end;

class function TBoldAbstractViewAdapterCom.CanLoadFromFile(const Filename: string): Boolean;
begin
  Result := False;
end;

class function TBoldAbstractViewAdapterCom.DefaultExtension: string;
begin
  Result := '';
end;

class function TBoldAbstractViewAdapterCom.FileFilter: string;
begin
  Result := '';
end;

class function TBoldAbstractViewAdapterCom.Description: string;
begin
  Result := '';
end;

class function TBoldAbstractViewAdapterCom.ViewAdapterClassCount: integer;
begin
  if Assigned(ViewAdapterList) then
    Result := ViewAdapterList.Count
  else
    Result := 0;
end;

class function TBoldAbstractViewAdapterCom.GetViewAdapterClass(index: integer): TBoldViewAdapterClassCom;
begin
  Result := nil;
  if Assigned(ViewAdapterList) then
    Result := ViewAdapterList[index];
end;

initialization
  DefaultAsViewerRenderer := TBoldAsViewerRendererCom.Create(nil);

finalization
  FreeAndNil(DefaultAsViewerRenderer);
  FreeAndNil(ViewAdapterList);
  
end.
