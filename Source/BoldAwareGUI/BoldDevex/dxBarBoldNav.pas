unit dxBarBoldNav;

interface

{$I bold.inc}
{$I cxVer.inc}


//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

uses
  Classes, Messages, DB, cxClasses, dxBar,
  BoldNavigatorDefs,
  BoldListListControlPack,
  BoldVariantControlPack,
  BoldListHandleFollower,
  BoldAbstractListHandle,
  BoldControlPack;


resourcestring
  dxSBAR_DBNAVERROR1 = 'You already have an existing BoldNavigator button with the same defined style';

  dxSBAR_CATEGORYNAME = 'Bold Navigator';
  dxSBAR_DELETERECORD = 'Do you want to delete the current record?';

  dxSBAR_BTNCAPTION_FIRST = 'First';
  dxSBAR_BTNCAPTION_PRIOR = 'Prior';
  dxSBAR_BTNCAPTION_NEXT = 'Next';
  dxSBAR_BTNCAPTION_LAST = 'Last';
  dxSBAR_BTNCAPTION_INSERT = 'Insert';
  dxSBAR_BTNCAPTION_DELETE = 'Delete';
  dxSBAR_BTNCAPTION_EDIT = 'Edit';
  dxSBAR_BTNCAPTION_POST = 'Post';
  dxSBAR_BTNCAPTION_CANCEL = 'Cancel';
  dxSBAR_BTNCAPTION_REFRESH = 'Refresh';

type
  TdxBarBoldNavigator = class;
  TdxBarBoldNavButton = class;

  TdxBarDBEnableType = (dxdbtCanModify, dxdbtNotEOF, dxdbtNotBOF,
    dxdbtHasRecords, dxdbtIsModified, dxdbtIsNotModified);
  TdxBarDBEnableTypes = set of TdxBarDBEnableType;

//  TdxBarBoldNavButtonType = (dxbnFirst, dxbnPrior, dxbnNext, dxbnLast, dxbnInsert, dxbnDelete, dxbnEdit, dxbnPost, dxbnCancel, dxbnRefresh);
//  TdxBarBoldNavButtonTypes = set of TdxBarBoldNavButtonType;

  TBoldNavigateBtn = (nbFirst, nbPrior, nbNext, nbLast, nbInsert, nbDelete, nbMoveUp, nbMoveDown);
  TBoldButtonSet = set of TBoldNavigateBtn;

  TdxBarBoldNavButton = class(TdxBarButton)
  private
    FBarBoldNavigator: TdxBarBoldNavigator;
    FNavButton: TBoldNavigateBtn;
    procedure SetNavButton(Value: TBoldNavigateBtn);
  protected
    procedure Loaded; override;
    procedure UpdateButtons;
  public
    destructor Destroy; override;
    procedure DoClick; override;
  published
    property BarBoldNavigator: TdxBarBoldNavigator read FBarBoldNavigator write FBarBoldNavigator;
    property NavButton: TBoldNavigateBtn read FNavButton write SetNavButton;
  end;

  TBoldNavigatorDeleteEvent = TNotifyEvent;
  TBoldNavigatorInsertEvent = TNotifyEvent;

  TdxBarBoldNavigator = class(TComponent)
  private
    FBarManager: TdxBarManager;
    FCategoryName: string;
    FConfirmDelete: Boolean;
    FSetVisFlag: Boolean;
    FVisibleButtons: TBoldButtonSet;

    fBoldDeleteMode: TBoldDeleteMode;
    fBoldProperties: TBoldListAsFollowerListController;
    fFollowerController: TBoldVariantFollowerController;
    fHandleFollower: TBoldListHandleFollower;
    fOnBoldNavigatorDelete: TBoldNavigatorDeleteEvent;
    fOnBoldNavigatorInsert: TBoldNavigatorInsertEvent;

    procedure SetBoldListHandle(Value: TBoldAbstractListHandle);
    function GetBoldListHandle: TBoldAbstractListHandle;

    procedure SetBarManager(Value: TdxBarManager);
    procedure SetCategoryName(const Value: String);
    procedure SetVisibleButtons(Value: TBoldButtonSet);

    procedure AddButton(AButton: TdxBarBoldNavButton);
    procedure RemoveButton(AButton: TdxBarBoldNavButton);

    function MapMinus(CanDeleteObject: Boolean): Boolean;

  protected
    Buttons: array[TBoldNavigateBtn] of TdxBarBoldNavButton;
    procedure ActiveChanged;
    procedure DataChanged;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure _BeforeMakeUptoDate(Follower: TBoldFollower);
    procedure _AfterMakeUptoDate(Follower: TBoldFollower);
    procedure SetActiveButtons;        
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BarManager: TdxBarManager read FBarManager write SetBarManager;
    property CategoryName: string read FCategoryName write SetCategoryName;
    property VisibleButtons: TBoldButtonSet read FVisibleButtons
      write SetVisibleButtons default [];
    property BoldDeleteMode: TBoldDeleteMode read fBoldDeleteMode write fBoldDeleteMode default dmDefault;
    property BoldHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
    property ConfirmDelete: Boolean read fConfirmDelete write fConfirmDelete default True;
    property OnBoldNavigatorDelete: TBoldNavigatorDeleteEvent read fOnBoldNavigatorDelete write fOnBoldNavigatorDelete;
    property OnBoldNavigatorInsert: TBoldNavigatorInsertEvent read fOnBoldNavigatorInsert write fOnBoldNavigatorInsert;
  end;

implementation

{$R dxBarBoldNav.res}

uses
  Windows,
  SysUtils,
  Forms,
//  dxBardbNavStrs,
  dxCore,
  BoldElements,
  BoldSystem,
  BoldGuiResourceStrings,
  BoldDefs;

const
  BtnResStr = 'DXBARBOLDNAVBTN_';
  dxBarBoldNavBtnName = 'dxBarBoldNav';

{ dxBarBoldNavButton }

destructor TdxBarBoldNavButton.Destroy;
begin
  if BarBoldNavigator <> nil then
    BarBoldNavigator.RemoveButton(Self);
  inherited Destroy;
end;

procedure TdxBarBoldNavButton.SetNavButton(Value: TBoldNavigateBtn);
const
  dxBarNames: array[TBoldNavigateBtn] of string =
    ('First', 'Prev', 'Next', 'Last', 'Insert', 'Delete', 'MoveUp', 'MoveDown');
{
  PResStrs: array[TBoldNavigateBtn] of String = (SNavHintFirst,
    SNavHintPrior, SNavHintNext, SNavHintLast,
    SNavHintNew, SNavHintDelete, SNavHintMoveUp, SNavHintMoveDown);
}
  PResStrs: array[TBoldNavigateBtn] of Pointer = (@dxSBAR_BTNCAPTION_FIRST,
    @dxSBAR_BTNCAPTION_PRIOR, @dxSBAR_BTNCAPTION_NEXT, @dxSBAR_BTNCAPTION_LAST,
    @dxSBAR_BTNCAPTION_INSERT, @dxSBAR_BTNCAPTION_DELETE, @dxSBAR_BTNCAPTION_EDIT,
    @dxSBAR_BTNCAPTION_POST{, @dxSBAR_BTNCAPTION_CANCEL, @dxSBAR_BTNCAPTION_REFRESH});

begin
  if (FNavButton <> Value) or (Name = '') then
    if csLoading in ComponentState then
      FNavButton := Value
    else
    begin
      if (BarBoldNavigator <> nil) and not BarBoldNavigator.FSetVisFlag and
        (BarBoldNavigator.Buttons[Value] <> nil) then
        raise Exception.Create(cxGetResourceString(@dxSBAR_DBNavERROR1));
      if (BarBoldNavigator <> nil) and not BarBoldNavigator.FSetVisFlag then
        BarBoldNavigator.RemoveButton(Self);
      FNavButton := Value;
      if BarBoldNavigator <> nil then
      begin
        BarBoldNavigator.AddButton(Self);
{$IFDEF BOLD_DELPHI25_OR_LATER}
        Glyph.LoadFromResource(HInstance, PChar(BtnResStr + IntToStr(Integer(FNavButton) + 1)), RT_BITMAP);
{$ELSE}
        Glyph.LoadFromResourceName(HInstance, PChar(BtnResStr + IntToStr(Integer(FNavButton) + 1)));
{$ENDIF}
      end;
      try
        if BarManager.Designing then
          Name := (BarManager as IdxBarDesigner).UniqueName(dxBarBoldNavBtnName + dxBarNames[FNavButton]);
        Caption := cxGetResourceString(PResStrs[FNavButton]);
        Hint := Caption;
      except
        raise;
      end;
    end;
end;

procedure TdxBarBoldNavButton.Loaded;
begin
  inherited Loaded;
  if BarBoldNavigator <> nil then
  begin
    BarBoldNavigator.AddButton(Self);
    BarBoldNavigator.ActiveChanged;
  end;
end;

procedure TdxBarBoldNavButton.DoClick;
var
  lSelection: TBoldObjectList;

procedure GetSelection;
{var
  i: integer;
  lFollower: TBoldFollower;
}
begin
{  lFollower := BarBoldNavigator.fHandleFollower.Follower;
  lSelection := TBoldObjectList.Create;
  lSelection.SubscribeToObjectsInList := false;
  lSelection.SubscribeToLocatorsInList := false;
  for i := 0 to lFollower.SubFollowerCount - 1 do
  begin
    if BarBoldNavigator.fBoldProperties.GetSelected(lFollower, i) then
      lSelection.Add( lFollower.SubFollowers[i].Element as TBoldObject );
  end;
}
  if (lSelection.Count = 0) then
    lSelection.Add(BarBoldNavigator.BoldHandle.CurrentBoldObject);
end;

var
  i: integer;
  lList: TBoldObjectList;
  lBoldObject: TBoldObject;
begin
  inherited;
  if Assigned(OnClick) then Exit;
  if BarBoldNavigator <> nil then
    with BarBoldNavigator.BoldHandle do
      case FNavButton of
        nbFirst: First;
        nbPrior: Prior;
        nbNext: Next;
        nbLast: Last;
        nbInsert:
        begin
          if Assigned(BarBoldNavigator.fOnBoldNavigatorInsert) then
            BarBoldNavigator.fOnBoldNavigatorInsert(BarBoldNavigator)
          else
            List.AddNew; //Insert;
        end;
        nbDelete:
          if not BarBoldNavigator.ConfirmDelete or
            (Application.MessageBox(PChar(cxGetResourceString(@dxSBAR_DELETERECORD)),
               PChar(Application.Title), MB_ICONQUESTION or MB_YESNO) = ID_YES) then
               begin
                 if Assigned(BarBoldNavigator.fOnBoldNavigatorDelete) then
                   BarBoldNavigator.fOnBoldNavigatorDelete(BarBoldNavigator)
                 else
                 begin
                   lList := BarBoldNavigator.BoldHandle.ObjectList;
                   GetSelection;
                   try
                     for i := lSelection.count - 1 downto 0 do
                     begin
                       lBoldObject := lSelection[i];
                       case BarBoldNavigator.BoldDeleteMode of
                         dmDefault, dmRemoveFromList:
                           lList.Remove(lBoldObject);
                         dmDelete:
                           lBoldObject.delete;
                         dmUnlinkAllAndDelete:
                           begin
                             lBoldObject.UnLinkAll;
                             lBoldObject.Delete
                           end;
                       end;
                     end;
                   finally
                     lSelection.free;
                   end;
                 end;
               end;
        nbMoveUp: ; //Edit;
        nbMoveDown: ; //Post;
      end;
end;

{ TdxBarBoldNavigator }

constructor TdxBarBoldNavigator.Create(AOwner: TComponent);
var
  ABarManager: TdxBarManager;
begin
  ABarManager := GetBarManagerForComponent(AOwner);
  {if (ABarManager = nil) and (dxBarManagerList.Count <> 0) then
    ABarManager := dxBarManagerList[0];}
  inherited Create(AOwner);
  FBarManager := ABarManager;
  FCategoryName := 'Bold Navigator';
  ConfirmDelete := true;
  fFollowerController := TBoldVariantFollowerController.Create(self);
  fBoldProperties := TBoldListAsFollowerListController.Create(self, fFollowerController);
  fHandleFollower := TBoldListHandleFollower.Create(Owner, fBoldProperties);
  if not (csDesigning in ComponentState) then
  begin
    fBoldProperties.AfterMakeUptoDate := _AfterMakeUptoDate;
    fBoldProperties.BeforeMakeUptoDate := _BeforeMakeUptoDate;
  end;
end;

destructor TdxBarBoldNavigator.Destroy;
begin
  VisibleButtons := [];
  inherited Destroy;
end;

procedure TdxBarBoldNavigator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = BarManager then BarManager := nil;
//    if AComponent = DataSource then DataSource := nil;
  end;
end;

procedure TdxBarBoldNavigator.DataChanged;
begin
  SetActiveButtons;
end;

procedure TdxBarBoldNavigator.ActiveChanged;
begin
  DataChanged;
end;

procedure TdxBarBoldNavigator.SetBarManager(Value: TdxBarManager);
var
  SaveVisButtons: TBoldButtonSet;
begin
  if FBarManager <> Value then
  begin
    SaveVisButtons := FVisibleButtons;
    VisibleButtons := [];
    FBarManager := Value;
    VisibleButtons := SaveVisButtons;
  end;
end;

procedure TdxBarBoldNavigator.SetCategoryName(const Value: String);
var
  Index1, Index2: Integer;
  I: TBoldNavigateBtn;
begin
  if FCategoryName <> Value then
    if csLoading in ComponentState then
      FCategoryName := Value
    else
      if BarManager <> nil then
      begin
        Index1 := BarManager.Categories.IndexOf(FCategoryName);
        FCategoryName := Value;
        Index2 := BarManager.Categories.IndexOf(FCategoryName);
        if Index2 = -1 then
        begin
          BarManager.Categories.Add(FCategoryName);
          Index2 := BarManager.Categories.IndexOf(FCategoryName);
        end;
        for I := Low(Buttons) to High(Buttons) do
          if Buttons[I] <> nil then Buttons[I].Category := Index2;
        if (Index1 > -1) and (BarManager.GetCountByCategory(Index1) = 0) then
          BarManager.Categories.Delete(Index1);
      end;
end;

procedure TdxBarBoldNavigator.SetVisibleButtons(Value: TBoldButtonSet);
var
  I: TBoldNavigateBtn;
  AIndex: Integer;
begin
  if FVisibleButtons <> Value then
  begin
    FVisibleButtons := Value;
    FSetVisFlag := True;
    if not (csLoading in ComponentState) and (BarManager <> nil) then
      for I := Low(Buttons) to High(Buttons) do
      begin
        if (Buttons[I] <> nil) and //not (csDestroying in Buttons[I].ComponentState) and
          not (I in Value) then
        begin
          Buttons[I].Free;
          Buttons[I] := nil;
        end;
        if (Buttons[I] = nil) and (I in Value) then
        begin
          Buttons[I] := TdxBarBoldNavButton.Create(Owner);
          with Buttons[I] do
          begin
            BarBoldNavigator := Self;
            Buttons[I].Tag := Ord(i);
            AIndex := BarManager.Categories.IndexOf(FCategoryName);
            if AIndex = -1 then
            begin
              BarManager.Categories.Add(FCategoryName);
              AIndex := BarManager.Categories.IndexOf(FCategoryName);
            end;
            Category := AIndex;
            NavButton := I;
          end;
        end;
      end;
    ActiveChanged;
    FSetVisFlag := False;
  end;
  if (FVisibleButtons = []) and (BarManager <> nil) and
    not (csDestroying in BarManager.ComponentState) then
  begin
    AIndex := BarManager.Categories.IndexOf(FCategoryName);
    if (AIndex > -1) and (BarManager.GetCountByCategory(AIndex) = 0) then
      BarManager.Categories.Delete(AIndex);
  end;
end;

procedure TdxBarBoldNavigator.AddButton(AButton: TdxBarBoldNavButton);
begin
  Buttons[AButton.NavButton] := AButton;
  FVisibleButtons := FVisibleButtons + [AButton.NavButton];
end;

procedure TdxBarBoldNavigator.RemoveButton(AButton: TdxBarBoldNavButton);
begin
  Buttons[AButton.NavButton] := nil;
  FVisibleButtons := FVisibleButtons - [AButton.NavButton];
end;

function TdxBarBoldNavigator.GetBoldListHandle: TBoldAbstractListHandle;
begin
  if Assigned(fHandleFollower) then
    Result := fHandleFollower.BoldHandle
  else
    Result := nil;
end;

procedure TdxBarBoldNavigator.SetBoldListHandle(
  Value: TBoldAbstractListHandle);
begin
   fHandleFollower.BoldHandle := value;
end;

procedure TdxBarBoldNavigator._BeforeMakeUptoDate(Follower: TBoldFollower);
begin
  if Assigned(BoldHandle) and Assigned(Follower) then
    fBoldProperties.SetActiveRange(Follower, BoldHandle.CurrentIndex, BoldHandle.CurrentIndex);
end;

procedure TdxBarBoldNavigator._AfterMakeUptoDate(Follower: TBoldFollower);
begin
  SetActiveButtons;
end;

procedure TdxBarBoldNavigator.SetActiveButtons;
var
  i: TBoldNavigateBtn;
begin
  if (csDesigning in ComponentState) then exit;
  for I := Low(Buttons) to High(Buttons) do
    if Buttons[I] <> nil then Buttons[I].UpdateButtons;
end;

function TdxBarBoldNavigator.MapMinus(CanDeleteObject: Boolean): Boolean;
begin
  case BoldDeleteMode of
    dmDefault, dmRemoveFromList:
      Result := True;
    dmDelete:
      Result := CanDeleteObject;
    dmUnlinkAllAndDelete:
      Result := True;
    else
      raise EBold.CreateFmt(sUnknownDeleteMode, [ClassName]);
  end;
end;

procedure TdxBarBoldNavButton.UpdateButtons;
var
  EnabledAndHandle: boolean;
  lBoldHandle: TBoldAbstractListHandle;
begin
  lBoldHandle := BarBoldNavigator.BoldHandle;
  EnabledAndHandle := assigned(lBoldHandle) and Assigned(lBoldHandle.Value);
  case TBoldNavigateBtn(Tag) of
    nbFirst: Enabled := EnabledAndHandle and lBoldHandle.HasPrior;
    nbPrior: Enabled := EnabledAndHandle and lBoldHandle.HasPrior;
    nbNext:  Enabled := EnabledAndHandle and lBoldHandle.HasNext;
    nbLast: Enabled := EnabledAndHandle and lBoldHandle.HasNext;
    nbInsert: Enabled := assigned(lBoldHandle) {and (nbInsert in BarBoldNavigator.VisibleButtons)} and
                               assigned(lBoldHandle.MutableList) and
                               lBoldHandle.MutableList.CanCreateNew;
    nbDelete: Enabled := EnabledAndHandle {and (nbDelete in VisibleButtons)} and
                               assigned(lBoldHandle.CurrentBoldObject) and
                               BarBoldNavigator.MapMinus(lBoldHandle.CurrentBoldObject.CanDelete);
(*    nbMoveUp: Enabled := EnabledAndHandle {and (nbMoveUp in VisibleButtons)} and
                               assigned(lBoldHandle.CurrentBoldObject) and
                               lBoldHandle.List.CanMove(lBoldHandle.CurrentIndex,
                               lBoldHandle.CurrentIndex -1);
    nbMoveDown: Enabled := EnabledAndHandle {and (nbMoveDown in VisibleButtons)} and
                               assigned(lBoldHandle.CurrentBoldObject) and
                               lBoldHandle.List.CanMove(lBoldHandle.CurrentIndex, lBoldHandle.CurrentIndex + 1);
*)
  end;
end;

initialization
  dxBarRegisterItem(TdxBarBoldNavButton, TdxBarButtonControl, False);

end.
