
{ Global compiler directives }
{$include bold.inc}
unit BoldNavigatorCom;

{$DEFINE BOLDCOMCLIENT} {Clientified 2002-08-05 14:59:57}

interface

uses
  // VCL
  Windows,
  Messages,
  Classes,
  Controls,

  // Bold
  {$IFNDEF BOLDCOMCLIENT}
  BoldComObjectSpace_TLB,
  {$ENDIF}
  BoldComObjectSpace_TLB,
  BoldNavigatorDefs,
  BoldAbstractListHandleCom,
  BoldListHandleFollowerCom,
  BoldStringControlPackCom,
  BoldControlPackCom,
  BoldListListControlPackCom;

type
  { forward declarations }
  TBoldCustomNavigatorCom = class;
  TBoldNavigatorCom = class;

  { TBoldCustomNavigatorCom }
  TBoldCustomNavigatorCom = class(TBoldNavigateBtnImageIndexOwner)
  private
    ButtonWidth: Integer;
    fBoldDeleteMode: TBoldDeleteMode;
    fBoldProperties: TBoldListAsFollowerListControllerCom;
    fDummyController: TBoldStringFollowerControllerCom;
    fHandleFollower: TBoldListHandleFollowerCom;
    fConfirmDelete: Boolean;
    fFlat: Boolean;
    fHints: TStrings;
    fOnNavClick: TBoldNavClick;
    fVisibleButtons: TBoldButtonSet;
    MinBtnSize: TPoint;
    fImageIndices: TBoldNavigateBtnImageIndex;
    fImages: TImageList;
    fDeleteQuestion: String;
    {$IFNDEF BOLDCOMCLIENT}
    fRemoveQuestion: String;
    fUnlinkQuestion: string;
    {$ENDIF}
    procedure AdjustSizeXY(var W: Integer; var H: Integer);
    procedure _AfterMakeUptoDate(Follower: TBoldFollowerCom);
    procedure BtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ClickHandler(Sender: TObject);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure InitButtons;
    procedure InitHints;
    function MapMinus(CanDeleteObject: Boolean): Boolean;
    procedure SetBoldListHandle(Value: TBoldAbstractListHandleCom);
    function GetBoldListHandle: TBoldAbstractListHandleCom;
    procedure SetFlat(Value: Boolean);
    procedure SetHints(Value: TStrings);
    procedure SetVisible(Value: TBoldButtonSet);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetImages(const Value: TImageList);
  protected
    Buttons: array[TBoldNavigateBtn] of TBoldNavButton;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure SetActiveButtons;
    procedure FixButtonGlyphs; override;
    property BoldDeleteMode: TBoldDeleteMode read fBoldDeleteMode write fBoldDeleteMode default dmDefault;
    property BoldHandle: TBoldAbstractListHandleCom read GetBoldListHandle write SetBoldListHandle;
    property ConfirmDelete: Boolean read fConfirmDelete write fConfirmDelete default True;
    property Flat: Boolean read fFlat write SetFlat default False;
    property Hints: TStrings read fHints write SetHints;
    property VisibleButtons: TBoldButtonSet read fVisibleButtons write SetVisible
      default [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete];
    property OnClick: TBoldNavClick read fOnNavClick write fOnNavClick;
    property ImageIndices: TBoldNavigateBtnImageIndex read fImageIndices write fImageIndices;
    property Images: TImageList read FImages write SetImages;
    property DeleteQuestion: string read fDeleteQuestion write fDeleteQuestion;
    {$IFNDEF BOLDCOMCLIENT}
    property UnlinkQuestion: string read fUnlinkQuestion write fUnlinkQuestion;
    property RemoveQuestion: String read fRemoveQuestion write fRemoveQuestion;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure BtnClick(index: TBoldNavigateBtn);
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  { TBoldNavigatorCom }
  TBoldNavigatorCom = class(TBoldCustomNavigatorCom)
  published
    {$IFNDEF T2H}
    property Align;
    property Anchors;
    property BoldDeleteMode;
    property BoldHandle;
    property ConfirmDelete;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Flat;
    property Hints;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property VisibleButtons;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseMove;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property ImageIndices;
    property Images;
    property DeleteQuestion;
    {$IFNDEF BOLDCOMCLIENT}
    property UnlinkQuestion;
    property RemoveQuestion;
    {$ENDIF}
    {$ENDIF}
  end;

implementation

uses
  // VCL
  Buttons,
  Dialogs,
  ExtCtrls,
  SysUtils,

  // Bold
  BoldDefs,
  BoldCommonBitmaps,
  {!! DO NOT REMOVE !! BoldSystemRT ,}
  BoldGuiResourceStringsCom;

var
  BtnHintId: array[TBoldNavigateBtn] of Pointer = (@SNavHintFirst, @SNavHintPrior, @SNavHintNext, @SNavHintLast, @SNavHintNew, @SNavHintDelete, @SNavHintMoveUp, @SNavHintMoveDown);

procedure TBoldCustomNavigatorCom.InitHints;
var
  I: Integer;
  ButtonIdx: TBoldNavigateBtn;
begin
  for ButtonIdx := Low(Buttons) to High(Buttons) do
    Buttons[ButtonIdx].Hint := LoadResString(BtnHintId[ButtonIdx]);
  ButtonIdx := Low(Buttons);
  for I := 0 to (fHints.Count - 1) do
  begin
    if fHints.Strings[I] <> '' then
      Buttons[ButtonIdx].Hint := fHints.Strings[I];
    if ButtonIdx = High(Buttons) then
      Exit;
    Inc(ButtonIdx);
  end;
end;

procedure TBoldCustomNavigatorCom.SetFlat(Value: Boolean);
var
  ButtonIdx: TBoldNavigateBtn;
begin
  if fFlat <> Value then
  begin
    fFlat := Value;
    for ButtonIdx := Low(Buttons) to High(Buttons) do
      Buttons[ButtonIdx].Flat := Value;
  end;
end;

procedure TBoldCustomNavigatorCom.SetHints(Value: TStrings);
begin
  fHints.Assign(Value);
  InitHints;
end;

procedure TBoldCustomNavigatorCom.GetChildren(Proc: TGetChildProc; ROOT: TComponent);
begin

end;

procedure TBoldCustomNavigatorCom.SetVisible(Value: TBoldButtonSet);
var
  I: TBoldNavigateBtn;
  W, H: Integer;
begin
  W := Width;
  H := Height;
  fVisibleButtons := Value;
  for I := Low(Buttons) to High(Buttons) do
    Buttons[I].Visible := I in fVisibleButtons;
  AdjustSizeXY(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Invalidate;
end;

procedure TBoldCustomNavigatorCom.AdjustSizeXY(var W: Integer; var H: Integer);
var
  Count: Integer;
  MinW: Integer;
  Space: Integer;
  Temp: Integer;
  Remain: Integer;
  X: Integer;
  I: TBoldNavigateBtn;
begin
  if (csLoading in ComponentState) or
    not Assigned(Buttons[nbFirst]) then
    Exit;

  Count := 0;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I].Visible then
      Inc(Count);

  if Count = 0 then
    Inc(Count);

  MinW := Count * MinBtnSize.X;
  if W < MinW then
    W := MinW;
  if H < MinBtnSize.Y then
    H := MinBtnSize.Y;

  ButtonWidth := W div Count;
  Temp := Count * ButtonWidth;
  if Align = alNone then
    W := Temp;

  X := 0;
  Remain := W - Temp;
  Temp := Count div 2;
  for I := Low(Buttons) to High(Buttons) do
  begin
    if Buttons[I].Visible then
    begin
      Space := 0;
      if Remain <> 0 then
      begin
        Dec(Temp, Remain);
        if Temp < 0 then
        begin
          Inc(Temp, Count);
          Space := 1;
        end;
      end;
      Buttons[I].SetBounds(X, 0, ButtonWidth + Space, Height);
      Inc(X, ButtonWidth + Space);
    end
    else
      Buttons[I].SetBounds(Width + 1, 0, ButtonWidth, Height);
  end;
end;

procedure TBoldCustomNavigatorCom.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  if not HandleAllocated then
    AdjustSizeXY(W, H);
  inherited SetBounds(ALeft, ATop, W, H);
end;

procedure TBoldCustomNavigatorCom.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustSizeXY(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

procedure TBoldCustomNavigatorCom.ClickHandler(Sender: TObject);
begin
  BtnClick(TBoldNavButton(Sender).index);
end;

procedure TBoldCustomNavigatorCom.BtnMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
var
  OldFocus: TBoldNavigateBtn;
begin
  OldFocus := FocusedButton;
  FocusedButton := TBoldNavButton(Sender).index;
  if TabStop and (GetFocus <> Handle) and CanFocus then
  begin
    SetFocus;
    if (GetFocus <> Handle) then
      Exit;
  end
  else if TabStop and (GetFocus = Handle) and (OldFocus <> FocusedButton) then
  begin
    Buttons[OldFocus].Invalidate;
    Buttons[FocusedButton].Invalidate;
  end;
end;

procedure TBoldCustomNavigatorCom.WMSetFocus(var Message: TWMSetFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TBoldCustomNavigatorCom.WMKillFocus(var Message: TWMKillFocus);
begin
  Buttons[FocusedButton].Invalidate;
end;

procedure TBoldCustomNavigatorCom.KeyDown(var KEY: Word; Shift: TShiftState);
var
  NewFocus: TBoldNavigateBtn;
  OldFocus: TBoldNavigateBtn;
begin
  OldFocus := FocusedButton;
  case KEY of
    VK_RIGHT:
    begin
      NewFocus := FocusedButton;
      repeat
        if NewFocus < High(Buttons) then
          NewFocus := Succ(NewFocus);
      until (NewFocus = High(Buttons)) or (Buttons[NewFocus].Visible);

      if NewFocus <> FocusedButton then
      begin
        FocusedButton := NewFocus;
        Buttons[OldFocus].Invalidate;
        Buttons[FocusedButton].Invalidate;
      end;
    end;

    VK_LEFT:
    begin
      NewFocus := FocusedButton;
      repeat
        if NewFocus > Low(Buttons) then
          NewFocus := Pred(NewFocus);
      until (NewFocus = Low(Buttons)) or (Buttons[NewFocus].Visible);
      if NewFocus <> FocusedButton then
      begin
        FocusedButton := NewFocus;
        Buttons[OldFocus].Invalidate;
        Buttons[FocusedButton].Invalidate;
      end;
    end;

    VK_SPACE:
      if Buttons[FocusedButton].Enabled then
        Buttons[FocusedButton].Click;
  end;
end;

procedure TBoldCustomNavigatorCom.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;

procedure TBoldCustomNavigatorCom.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if not (csLoading in ComponentState) then
    SetActiveButtons;
end;

procedure TBoldCustomNavigatorCom.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustSizeXY(W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  InitHints;
  FixButtonGlyphs;
  SetActiveButtons;
end;

constructor TBoldCustomNavigatorCom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fImageIndices := TBoldNavigateBtnImageIndex.Create(self);
  fImageIndices.nbFirst := -1;
  fImageIndices.nbPrior := -1;
  fImageIndices.nbNext := -1;
  fImageIndices.nbLast := -1;
  fImageIndices.nbInsert := -1;
  fImageIndices.nbDelete := -1;
  fImageIndices.nbMoveUp := -1;
  fImageIndices.nbMoveDown := -1;

  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  if not NewStyleControls then
    ControlStyle := ControlStyle + [csFramed];
  fVisibleButtons := [nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete];
  fHints := TStringList.Create;
  InitButtons;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  Width := (Integer(High(TBoldNavigateBtn)) - Integer(Low(TBoldNavigateBtn)) + 1) * 24 + 1;
  Height := 25;
  ButtonWidth := 0;
  FocusedButton := nbFirst;
  fConfirmDelete := True;
  FullRepaint := False;
  fDummyController := TBoldStringFollowerControllerCom.Create(self);
  fBoldProperties := TBoldListAsFollowerListControllerCom.Create(self, fDummyController);
  fHandleFollower := TBoldListHandleFollowerCom.Create(Owner, fBoldProperties);
  fBoldProperties.AfterMakeUptoDate := _AfterMakeUptoDate;

  fDeleteQuestion := 'Delete "%1:s"?';
  {$IFNDEF BOLDCOMCLIENT}
  fUnlinkQuestion := 'Unlink "%1:s" from "%2:s"?';
  fRemoveQuestion := 'Remove "%1:s" from the list?';
  {$ENDIF}
end;

destructor TBoldCustomNavigatorCom.Destroy;
begin
  FreeAndNil(fImageIndices);
  FreeAndNil(fHints);
  FreeAndNil(fHandleFollower);
  FreeAndNil(fDummyController);
  FreeAndNil(fBoldProperties);
  inherited Destroy;
end;

procedure TBoldCustomNavigatorCom.BtnClick(index: TBoldNavigateBtn);

  {$IFDEF BOLDCOMCLIENT}
  procedure Delete(AskFirst: Boolean);
  begin
    if (not AskFirst or
          (MessageDlg(Format(fDeleteQuestion, [BoldHandle.Value.BoldType.ModelName]), mtConfirmation, mbOKCancel, 0) <> IDCANCEL)) then
      case BoldDeleteMode of
        dmDefault, dmRemoveFromList:
          BoldHandle.RemoveCurrentElement;
        dmDelete:
          BoldHandle.CurrentBoldObject.Delete;
        dmUnlinkAllAndDelete:
          with BoldHandle.CurrentBoldObject do
          begin
            UnLinkAll;
            Delete;
          end;
      end;
  end;
  {$ELSE}
  procedure Delete(AskFirst: Boolean);
  var
    EffectiveDeleteMode: TBoldDeleteMode;
    RoleRTInfo: TBoldRoleRTInfo;
    Element: IBoldElement;
    obj: IBoldObject;
    Unlink: Boolean;
    Query: string;
    PerformOp: Boolean;
  begin
    EffectiveDeleteMode := BoldDeleteMode;
    if assigned(BoldHandle.ObjectList) and (BoldHandle.ObjectList.BoldMemberRTInfo is TBoldRoleRTInfo) then
      RoleRTInfo := BoldHandle.ObjectList.BoldMemberRTInfo as TBoldRoleRTInfo
    else
      RoleRTInfo := nil;

    Element := BoldHandle.Value;

    if BoldDeleteMode = dmDefault then
    begin
      if assigned(BoldHandle.ObjectList) and (BoldHandle.ObjectList.OwningElement is IBoldSystem) then
        EffectiveDeleteMode := dmDelete
      else
        EffectiveDeleteMode := dmRemoveFromList;
    end;

    unlink := false;

    if EffectiveDeleteMode = dmRemoveFromList then
    begin
      if assigned(RoleRTInfo) then
      begin
        if RoleRTInfo.RoleType = rtLinkRole then
          EffectiveDeleteMode := dmDelete
        else
          Unlink := true;
      end;
    end;

    query := fDeleteQuestion;
    if EffectiveDeleteMode = dmRemoveFromList then
    begin
      if Unlink then
        query := fUnlinkQuestion
      else
        query := fRemoveQuestion;
    end;

    PerformOp := Not AskFirst or
      (MessageDlg(Format(query, [Element.BoldType.ModelName,
                                Element.AsString,
                                RoleRTInfo.ModelName]), mtConfirmation, mbOKCancel, 0) <> mrCancel);

    if PerformOp then
      case EffectiveDeleteMode of
        dmRemoveFromList: BoldHandle.RemoveCurrentElement;
        dmDelete, dmUnlinkAllAndDelete:
        begin
          Obj := BoldHandle.CurrentBoldObject;
          if assigned(Obj) then
          begin
            obj.BoldComObjectSpace_TLB.StartTransaction;
            try
              if EffectiveDeleteMode = dmUnlinkAllAndDelete then
                Obj.UnlinkAll;
              Obj.Delete;
              obj.BoldComObjectSpace_TLB.CommitTransaction;
            except
              Obj.BoldComObjectSpace_TLB.RollBackTransaction;
              raise;
            end;
          end;
        end;
      end;
  end;
  {$ENDIF}

begin
  if (owner is TControl) and
     (Owner as TControl).Visible and
     (Owner as TControl).enabled and CanFocus then
    SetFocus;

  with BoldHandle do
  begin
    case index of
      nbPrior:
        Prior;
      nbNext:
        Next;
      nbFirst:
        First;
      nbLast:
        Last;
      nbInsert:
        CurrentIndex := List.IndexOf(MutableList.AddNew);
      nbDelete:
          Delete(fConfirmDelete);
      nbMoveUp:
        List.Move(CurrentIndex, CurrentIndex - 1);
      nbMoveDown:
        List.Move(CurrentIndex, CurrentIndex + 1);
    end;
  end;
  if not (csDesigning in ComponentState) and Assigned(fOnNavClick) then
    fOnNavClick(Self, index);
end;

procedure TBoldCustomNavigatorCom.InitButtons;
var
  I: TBoldNavigateBtn;
  Btn: TBoldNavButton;
  X: Integer;
begin
  MinBtnSize := Point(20, 18);
  X := 0;
  for I := Low(Buttons) to High(Buttons) do
  begin
    Btn := TBoldNavButton.Create(Self);
    Btn.Flat := Flat;
    Btn.index := I;
    Btn.Visible := I in fVisibleButtons;
    Btn.Enabled := True;
    Btn.SetBounds(X, 0, MinBtnSize.X, MinBtnSize.Y);
    Btn.Enabled := False; {!DT?: Force creation of speedbutton images}
    Btn.Enabled := True;
    Btn.OnClick := ClickHandler;
    Btn.OnMouseDown := BtnMouseDown;
    Btn.Parent := Self;
    Buttons[I] := Btn;
    X := X + MinBtnSize.X;
  end;
  FixButtonGlyphs;
  InitHints;
//  Buttons[nbPrior].NavStyle := Buttons[nbPrior].NavStyle + [nsAllowTimer];
//  Buttons[nbNext].NavStyle := Buttons[nbNext].NavStyle + [nsAllowTimer];
end;

procedure TBoldCustomNavigatorCom.SetActiveButtons;
var
  EnabledAndHandle: Boolean;
begin
  EnabledAndHandle := Enabled and assigned(BoldHandle);

  Buttons[nbFirst].Enabled := EnabledAndHandle and BoldHandle.HasPrior;
  Buttons[nbPrior].Enabled := Buttons[nbFirst].Enabled;
  Buttons[nbNext].Enabled := EnabledAndHandle and BoldHandle.HasNext;
  Buttons[nbLast].Enabled := Buttons[nbNext].Enabled;
  Buttons[nbInsert].Enabled := EnabledAndHandle and
                               assigned(BoldHandle.MutableList) and
                               BoldHandle.MutableList.CanCreateNew;

  Buttons[nbDelete].Enabled := EnabledAndHandle and
                               assigned(BoldHandle.CurrentBoldObject) and
                               MapMinus(BoldHandle.CurrentBoldObject.CanDelete);
  Buttons[nbMoveUp].Enabled := EnabledAndHandle and
                               assigned(BoldHandle.CurrentBoldObject) and
                               BoldHandle.List.CanMove(BoldHandle.CurrentIndex,
                               BoldHandle.CurrentIndex -1);
  Buttons[nbMoveDown].Enabled := EnabledAndHandle and
                               assigned(BoldHandle.CurrentBoldObject) and
                               BoldHandle.List.CanMove(BoldHandle.CurrentIndex, BoldHandle.CurrentIndex + 1);
end;

procedure TBoldCustomNavigatorCom.SetBoldListHandle(Value: TBoldAbstractListHandleCom);
begin
   fHandleFollower.BoldHandle := value;
end;

function TBoldCustomNavigatorCom.GetBoldListHandle: TBoldAbstractListHandleCom;
begin
  Assert(Assigned(fHandleFollower));
  Result := fHandleFollower.BoldHandle;
end;

procedure TBoldCustomNavigatorCom._AfterMakeUptoDate(Follower: TBoldFollowerCom);
begin
  SetActiveButtons;
end;

function TBoldCustomNavigatorCom.MapMinus(CanDeleteObject: Boolean): Boolean;
begin
  case BoldDeleteMode of
    dmDefault, dmRemoveFromList:
      Result := True;
    dmDelete:
      Result := CanDeleteObject;
    dmUnlinkAllAndDelete:
      Result := True;
    else
      raise EBold.CreateFmt('%s.MapMinus: Unknown delete mode', [ClassName]);
  end;
end;

procedure TBoldCustomNavigatorCom.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TBoldCustomNavigatorCom then
    fImageIndices.assign((Source as TBoldCustomNavigatorCom).ImageIndices);
end;

procedure TBoldCustomNavigatorCom.FixButtonGlyphs;
var
  i: TBoldNavigateBtn;
  Btn: TBoldNavButton;
  Index: integer;
begin
  for I := Low(Buttons) to High(Buttons) do
  begin
    Btn := Buttons[i];
    if assigned(btn) then
    begin
      case i of
        nbFirst: index := ImageIndices.nbFirst;
        nbPrior: index := ImageIndices.nbPrior;
        nbNext: index := ImageIndices.nbNext;
        nbLast: index := ImageIndices.nbLast;
        nbInsert: index := ImageIndices.nbInsert;
        nbDelete: index := ImageIndices.nbDelete;
        nbMoveUp: index := ImageIndices.nbMoveUp;
        nbMoveDown: index := ImageIndices.nbMoveDown
        else index := -1;
      end;
      if assigned(Images) and
        (Index >= 0) and
        (Index < Images.Count) then
        Images.GetBitMap(Index, btn.glyph)
      else
        case i of
          nbFirst: Btn.Glyph := bmpBoldNavigatorFirst;
          nbPrior: Btn.Glyph := bmpBoldNavigatorPrior;
          nbNext: Btn.Glyph := bmpBoldNavigatorNext;
          nbLast: Btn.Glyph := bmpBoldNavigatorLast;
          nbInsert: Btn.Glyph := bmpBoldNavigatorInsert;
          nbDelete: Btn.Glyph := bmpBoldNavigatorDelete;
          nbMoveUp: Btn.Glyph := bmpBoldNavigatorMoveUp;
          nbMoveDown: Btn.Glyph := bmpBoldNavigatorMoveDown
        end;

      if btn.glyph.Width > btn.glyph.Height then
        btn.NumGlyphs := 2
      else
        btn.NumGlyphs := 1;
    end;
  end;
end;

procedure TBoldCustomNavigatorCom.SetImages(const Value: TImageList);
begin
  FImages := Value;
  FixButtonGlyphs;
end;

end.
