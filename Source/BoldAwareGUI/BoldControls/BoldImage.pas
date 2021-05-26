
{ Global compiler directives }
{$include bold.inc}
unit BoldImage;

{$UNDEF BOLDCOMCLIENT}

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  BoldEnvironmentVCL,
  BoldControlsDefs,
  BoldHandles,
  BoldElementHandleFollower,
  BoldControlPack,
  BoldViewerControlPack,
  BoldElements,
  BoldDefs;

type
  {forward declarations}
  TBoldImage = class;

  {-- TBoldImage --}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldImage = class(TCustomControl, IBoldOCLComponent)
  private
    fBoldProperties: TBoldViewerFollowerController;
    fHandleFollower: TBoldElementHandleFollower;
    fReadOnly: Boolean;
    fBorderStyle: TBorderStyle;
    fAutoSize: Boolean;
    fDrawFocus: Boolean;
    fContentTypeOnPaste: string;
    fStretchMode: TBoldStretchMode;
    fCenter: Boolean;
    fQuickDraw: Boolean;
    fScale: Double;
    fDisplayRect: TRect;
    FOnResize: TNotifyEvent;   
    function GetContextType: TBoldElementTypeInfo;
    procedure SetExpression(const Value: TBoldExpression);
    function GetExpression: TBoldExpression;
    function GetVariableList: TBoldExternalVariableList;

    function GetBoldHandle: TBoldElementHandle;
    function GetFollower: TBoldFOllower;
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetAutoSize(Value: Boolean); reintroduce;
    procedure SetDrawFocus(Value: Boolean);
    procedure SetBoldProperties(Value: TBoldViewerFollowerController);
    procedure SetBoldHandle(value: TBoldElementHandle);
    function GetViewer: TBoldAbstractViewAdapter;
    procedure SetViewer(Value: TBoldAbstractViewAdapter);
    procedure SetStretchMode(Value: TBoldStretchMode);
    function GetScale: Integer;
    procedure SetScale(Value: Integer);
    {Messages}
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
    procedure WMSize(var Message: TMessage); message WM_SIZE;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    function GetPalette: HPALETTE; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    procedure AfterMakeUptoDate(Follower: TBoldFollower);
    function EffectiveReadOnly: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CopyToClipboard;
    procedure CutToClipboard;
    procedure PasteFromClipboard;
    procedure LoadFromFile(fileName: String);
    procedure CalcDisplayRect;
    property Viewer: TBoldAbstractViewAdapter read GetViewer write SetViewer;
    property Follower: TBoldFollower read GetFollower;
  published
    property BoldHandle: TBoldElementHandle read GetBoldHandle write SetBoldHandle;
    property BoldProperties: TBoldViewerFollowerController read FBoldProperties write SetBoldProperties;
    property ReadOnly: Boolean read fReadOnly write fReadOnly default False;
    property AutoSize: Boolean read fAutoSize write SetAutoSize default False;
    property DrawFocus: Boolean read fDrawFocus write SetDrawFocus default True;
    property ContentTypeOnPaste: string read fContentTypeOnPaste write fContentTypeOnPaste;
    property StretchMode: TBoldStretchMode read fStretchMode write SetStretchMode;
    property Scale: Integer read GetScale write SetScale default 100;
    property Center: Boolean read fCenter write fCenter;
    property QuickDraw: Boolean read fQuickDraw write fQuickDraw;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property OnResize: TNotifyEvent read FOnResize write FOnResize;
    {Standard properties}
    {$IFNDEF T2H}
    property Align;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  BoldControlPackDefs;

{-- TBoldImage --}

constructor TBoldImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csReplicatable];
  if not NewStyleControls then ControlStyle := ControlStyle + [csFramed];
  Width := 105;
  Height := 105;
  TabStop := True;
  ParentColor := False;

  fBoldProperties := TBoldViewerFollowerController.Create(Self);
  fBoldProperties.AfterMakeUptoDate := AfterMakeUptoDate;
  fBoldProperties.OnGetContextType := GetContextType;
  fHandleFollower := TBoldElementHandleFollower.Create(Owner, FBoldProperties);

  fBorderStyle := bsSingle;
  fReadOnly := False;
  fAutoSize := False;
  fDrawFocus := True;
  fScale := 1.0;
end;

destructor TBoldImage.Destroy;
begin
  FreeAndNil(fHandleFollower);
  FreeAndNil(fBoldProperties);
  inherited Destroy;
end;

procedure TBoldImage.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    if FBorderStyle = bsSingle then
      if NewStyleControls and Ctl3D then
        ExStyle := ExStyle or WS_EX_CLIENTEDGE
      else
        Style := Style or WS_BORDER;
    WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
end;

procedure TBoldImage.SetBoldProperties(Value: TBoldViewerFollowerController);
begin
  FBoldProperties.Assign(Value);
end;

procedure TBoldImage.SetBoldHandle(Value: TBoldElementHandle);
begin
    fHandleFollower.BoldHandle := value;
end;

function TBoldImage.GetViewer: TBoldAbstractViewAdapter;
begin
  Result := fBoldProperties.GetCurrentViewer(Follower);
end;

procedure TBoldImage.SetViewer(Value: TBoldAbstractViewAdapter);
begin
  fBoldProperties.MayHaveChanged(Value, Follower);
end;

procedure TBoldImage.SetBorderStyle(Value: TBorderStyle);
begin
  if fBorderStyle <> Value then
  begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TBoldImage.SetAutoSize(Value: Boolean);
begin
  if fAutoSize <> Value then
  begin
    fAutoSize := Value;
    Invalidate;
  end;
end;

procedure TBoldImage.SetDrawFocus(Value: Boolean);
begin
  if fDrawFocus <> Value then
  begin
    fDrawFocus := Value;
    Invalidate;
  end;
end;

function TBoldImage.GetScale: Integer;
begin
  Result := Round(fScale*100);
end;

procedure TBoldImage.SetScale(Value: Integer);
begin
  if (Value<>fScale) and (Value>0) then
  begin
    fScale := Value/100;
    StretchMode := bsmStretchToScale;
    Invalidate;
  end;
end;

procedure TBoldImage.SetStretchMode(Value: TBoldStretchMode);
begin
  if (Value<>fStretchMode) then
  begin
    fStretchMode := Value;
    Invalidate;
  end;
end;

{Message handling}

procedure TBoldImage.CMEnter(var Message: TCMEnter);
begin
  if fDrawFocus then
    Invalidate; { Draw the focus marker }
  inherited;
end;

procedure TBoldImage.CMExit(var Message: TCMExit);
begin
  try
    if (Follower.controller.ApplyPolicy = bapExit) then
      Follower.Apply;
  except
    SetFocus;
    raise;
  end;
  if fDrawFocus then
    Invalidate; { Erase the focus marker }
  inherited;
end;

procedure TBoldImage.WMLButtonDown(var Message: TWMLButtonDown);
begin
  if TabStop and CanFocus then SetFocus;
  inherited;
end;

procedure TBoldImage.WMCut(var Message: TMessage);
begin
  CutToClipboard;
end;

procedure TBoldImage.WMCopy(var Message: TMessage);
begin
  CopyToClipboard;
end;

procedure TBoldImage.WMPaste(var Message: TMessage);
begin
  PasteFromClipboard;
end;

procedure TBoldImage.WMSize(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) and Assigned(FOnResize) then
    FOnResize(Self);
  Invalidate;
end;

procedure TBoldImage.CMTextChanged(var Message: TMessage);
begin
  inherited;
end;

procedure TBoldImage.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
end;

{}
function TBoldImage.GetPalette: HPALETTE;
begin
  if Assigned(Viewer) then
    Result := Viewer.GetPalette
  else
  Result := 0;
end;

procedure TBoldImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_INSERT:
      if ssShift in Shift then
        PasteFromClipBoard
      else if ssCtrl in Shift then
        CopyToClipBoard;
    VK_DELETE:
      if ssShift in Shift then CutToClipBoard;
  end;
end;

procedure TBoldImage.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  case Key of
    ^X: CutToClipBoard;
    ^C: CopyToClipBoard;
    ^V: PasteFromClipBoard;
    BOLDESC: Follower.DiscardChange;
  end;
end;

procedure TBoldImage.CalcDisplayRect;
var
  ScaleX,
  ScaleY: Double;
  CR: TRect;
begin
  CR := ClientRect;
  case fStretchMode of
    bsmNoStretch: begin
      fScale := 1.0;
      SetRect(fDisplayRect, 0, 0, Viewer.Width, Viewer.Height);
    end;
    bsmStretchProportional: begin
      if ((Viewer.Width>0) and (Viewer.Height>0)) then
      begin
        ScaleX := (CR.Right-CR.Left)/Viewer.Width;
        ScaleY := (CR.Bottom-CR.Top)/Viewer.Height;
        if ScaleX<ScaleY then
          fScale := ScaleX
        else
          fScale := ScaleY;
      end
      else
        fScale := 1.0;
      SetRect(fDisplayRect, 0, 0, Round(Viewer.Width*fScale), Round(Viewer.Height*fScale));
    end;
    bsmStretchToFit: begin
      fScale := 1.0;
      SetRect(fDisplayRect, 0, 0, (CR.Right-CR.Left), (CR.Bottom-CR.Top));
    end;
    bsmStretchToScale: begin
      SetRect(fDisplayRect, 0, 0, Round(Viewer.Width*fScale), Round(Viewer.Height*fScale));
    end;
  end;
  if AutoSize and Assigned(Viewer) and (Viewer.Width > 0) and (Viewer.Height > 0) then
    SetBounds(Left, Top, Round(Viewer.Width*fScale), Round(Viewer.Height*fScale));
  if Center then
    OffsetRect(fDisplayRect, (CR.Right - CR.Left - fDisplayRect.Right) div 2, (CR.Bottom - CR.Top - fDisplayRect.Bottom) div 2);
end;

procedure TBoldImage.Paint;
var
  Size: TSize;
  R: TRect;
  S: string;
  Form: TCustomForm;
  Pal: HPalette;
begin
  with Canvas do
  begin
    Brush.Style := bsSolid;
    Brush.Color := Color;
    if (Assigned(Viewer) and not (Viewer.Empty)) or (csPaintCopy in ControlState) then
    begin
      Pal := 0;
      try
        if (csPaintCopy in ControlState) and (Assigned(Viewer) and not Viewer.Empty) then
        begin
{         FIXME Paint picture in highest resulution without pallette realization
            Bitmap.IgnorePalette := QuickDraw;}
        end
        else
        begin
          if Focused and Assigned(Viewer) and (Viewer.GetPalette <> 0) then
          begin { Control has focus, so realize the bitmap palette in foreground }
            Pal := SelectPalette(Handle, Viewer.GetPalette, False);
            RealizePalette(Handle);
          end;
        end;

        if Assigned(Viewer) then
        begin
          CalcDisplayRect;
          Viewer.Paint(Canvas, fDisplayRect);
          ExcludeClipRect(Handle, fDisplayRect.Left, fDisplayRect.Top, fDisplayRect.Right, fDisplayRect.Bottom);
          FillRect(ClientRect);
          SelectClipRgn(Handle, 0);
        end
        else
          FillRect(ClientRect);

      finally
        if Pal <> 0 then SelectPalette(Handle, Pal, True);
      end;
    end
    {Draw Text if nothing to view}
    else begin
      Font := Self.Font;
      if (csDesigning in ComponentState) then
        S := '(' + Name + ')'
      else
        S := '';
      Size := TextExtent(S);
      R := ClientRect;
      TextRect(R, (R.Right - Size.cx) div 2, (R.Bottom - Size.cy) div 2, S);
    end;
    {Draw Focus}
    if fDrawFocus then
    begin
      Form := GetParentForm(Self);
      if Assigned(Form) and (Form.ActiveControl = Self) and
        not (csDesigning in ComponentState) and
        not (csPaintCopy in ControlState) then
      begin
        Brush.Color := clWindowFrame;
        FrameRect(ClientRect);
      end;
    end;
  end;
end;

procedure TBoldImage.AfterMakeUptoDate(Follower: TBoldFollower);
begin
  Invalidate;
end;

function TBoldImage.EffectiveReadOnly: Boolean;
begin
  Result := fReadOnly or not fBoldProperties.MayModify(Follower);
end;

procedure TBoldImage.CopyToClipboard;
begin
  if Assigned(Viewer) then
    Viewer.CopyToClipboard;
end;

procedure TBoldImage.CutToClipboard;
begin
  if Assigned(Viewer) then
  begin
    Viewer.CopyToClipboard;
    if not EffectiveReadOnly then
    begin
      Viewer := nil;
      Invalidate;
      if not Focused and (BoldProperties.ApplyPolicy = bapExit) then
        Follower.Apply;
    end;
  end;
end;

procedure TBoldImage.PasteFromClipboard;
var
  aViewer: TBoldAbstractViewAdapter;

  function GetViewer: TBoldAbstractViewAdapter;
  var
    I: Integer;
  begin
    i := 0;
    while (i < TBoldAbstractViewAdapter.ViewAdapterClassCount) and
      not TBoldAbstractViewAdapter.GetViewAdapterClass(i).CanPasteFromClipboard(fContentTypeOnPaste) do
        inc(i);
    if i < TBoldAbstractViewAdapter.ViewAdapterClassCount then
      result := TBoldAbstractViewAdapter.GetViewAdapterClass(i).Create
    else
      result := nil;
  end;

begin
  if not EffectiveReadOnly then
  begin
    aViewer := GetViewer;
    if Assigned(aViewer) then
    begin
      aViewer.PasteFromClipboard;
      Viewer := aViewer;
      Invalidate;
      if not Focused and (BoldProperties.ApplyPolicy = bapExit) then
        Follower.Apply;
    end;
  end;
end;

Procedure TBoldImage.LoadFromFile(FileName: String);
var
  aViewer: TBoldAbstractViewAdapter;

  function GetViewer: TBoldAbstractViewAdapter;

  var
    I: Integer;
  begin
    i := 0;
    While (i < TBoldAbstractViewAdapter.ViewAdapterClassCount) and
      not TBoldAbstractViewAdapter.GetViewAdapterClass(i).CanLoadFromFile(FileName) do
        inc(i);
    if i < TBoldAbstractViewAdapter.ViewAdapterClassCount then
      result := TBoldAbstractViewAdapter.GetViewAdapterClass(i).Create
    else
      result := nil;
  end;

begin
  if not EffectiveReadOnly then
  begin
    aViewer := GetViewer;
    if Assigned(aViewer) then
    begin
      aViewer.LoadFromFile(FileName);
      Viewer := aViewer;
      Invalidate;
      if not Focused and (BoldProperties.ApplyPolicy = bapExit) then
        Follower.Apply;
    end
    else
      raise EBold.CreateFmt('%s.LoadFromFile: File format unknown: File: ''%s''', [ClassName, FileName]);
  end;
end;

function TBoldImage.GetBoldHandle: TBoldElementHandle;
begin
  Result := fHandleFollower.BoldHandle;
end;

function TBoldImage.GetFollower: TBoldFOllower;
begin
    Result := fHandleFollower.Follower;
end;

function TBoldImage.GetContextType: TBoldElementTypeInfo;
begin
  if Assigned(BoldHandle) then
    Result := BoldHandle.StaticBoldType
  else
    Result := nil;
end;

function TBoldImage.GetExpression: TBoldExpression;
begin
  Result := BoldProperties.Expression;
end;

procedure TBoldImage.SetExpression(const Value: TBoldExpression);
begin
  BoldProperties.Expression := Value;
end;

function TBoldImage.GetVariableList: TBoldExternalVariableList;
begin
  result := BoldProperties.VariableList;
end;

end.
