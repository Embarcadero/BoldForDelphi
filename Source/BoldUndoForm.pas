unit BoldUndoForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxListView,
  cxBoldEditors, BoldSystemHandle, BoldSubscription, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxNavigator, dxDateRanges,
  dxScrollbarAnnotations, cxCheckBox, cxMemo, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridBoldSupportUnit, cxClasses,
  cxGridCustomView, cxGrid,
  BoldUndoHandler, Vcl.ExtCtrls, cxTimeEdit, Vcl.Menus, System.Actions,
  Vcl.ActnList, BoldUndoActions, BoldHandleAction, BoldActions;

type
  TfrmBoldUndo = class(TForm)
    GridUndoList: TcxGrid;
    LevelUndoList: TcxGridLevel;
    tvUndoList: TcxGridTableView;
    colCaption: TcxGridColumn;
    UpdateTimer: TTimer;
    colTime: TcxGridColumn;
    ColObjectCount: TcxGridColumn;
    popUndo: TPopupMenu;
    ActionList1: TActionList;
    actRedo: TBoldRedoAction;
    actUndo: TBoldUndoAction;
    miUndo: TMenuItem;
    miRedo: TMenuItem;
    actSetCheckPoint: TBoldSetCheckPointAction;
    miSetcheckpoint: TMenuItem;
    actShowChanges: TAction;
    actShowChanges1: TMenuItem;
    colName: TcxGridColumn;
    actClear: TBoldClearUndoAction;
    Clear1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    procedure UpdateTimerTimer(Sender: TObject);
    procedure tvUndoListCellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
    procedure tvUndoListCustomDrawCell(Sender: TcxCustomGridTableView;
      ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
      var ADone: Boolean);
    procedure actShowChangesUpdate(Sender: TObject);
    procedure actShowChangesExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FBoldSystemHandle: TBoldSystemHandle;
    fSubscriber: TBoldExtendedPassthroughSubscriber;
    FHideEmptyBlocks: boolean;
    procedure SetBoldSystemHandle(const Value: TBoldSystemHandle);
    procedure Subscribe;
    function Subscriber: TBoldSubscriber;
    procedure Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent; const Args: array of const);
    procedure UpdateGrid;
    function GetUndoHandler: TBoldUndoHandler;
    procedure SetHideEmptyBlocks(const Value: boolean);
    function GetSelectedBlock: TBoldUndoBlock;
  protected
    property UndoHandler: TBoldUndoHandler read GetUndoHandler;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
    property BoldSystemHandle: TBoldSystemHandle read FBoldSystemHandle write SetBoldSystemHandle;
    property HideEmptyBlocks: boolean read FHideEmptyBlocks write SetHideEmptyBlocks;
  end;

implementation

uses
  BoldUndoInterfaces, BoldHandles, cxGridCommon;

{$R *.dfm}

{ TUndoForm }

procedure TfrmBoldUndo.actShowChangesExecute(Sender: TObject);
begin
  with tvUndoList.DataController do
    ShowMessage(Copy(GetSelectedBlock.Content ,1,65535));
end;

procedure TfrmBoldUndo.actShowChangesUpdate(Sender: TObject);
begin
  with tvUndoList.DataController do
    (Sender as TAction).Enabled :=  (GetFocusedRecordIndex <> -1) and (GetSelectedBlock.Content <> '');
end;

procedure TfrmBoldUndo.AfterConstruction;
begin
  inherited;
  HideEmptyBlocks := true;
end;

destructor TfrmBoldUndo.Destroy;
begin
  fSubscriber.free;
  inherited;
end;

procedure TfrmBoldUndo.FormShow(Sender: TObject);
begin
  if not Assigned(Parent) then
  begin
    BorderStyle := bsSizeToolWin;
    Align := alNone;
  end
  else
  begin
    BorderStyle := bsNone;
    if Align = alNone then
      Align := alClient;
  end;
end;

function TfrmBoldUndo.GetSelectedBlock: TBoldUndoBlock;
var
  BlockName: string;
begin
  BlockName := tvUndoList.DataController.Values[tvUndoList.DataController.FocusedRecordIndex, 0];
  result := UndoHandler.RedoBlocks.BlockByName[BlockName];
  if not Assigned(result) then
    result := UndoHandler.UndoBlocks.BlockByName[BlockName];
end;

function TfrmBoldUndo.GetUndoHandler: TBoldUndoHandler;
begin
  if Assigned(BoldSystemHandle) and BoldSystemHandle.Active then
    result :=  BoldSystemHandle.System.UndoHandler as TBoldUndoHandler
  else
    result := nil;
end;

procedure TfrmBoldUndo.Receive(Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent; const Args: array of const);
begin
  if (Requestedevent in beUndoEvents) then
    UpdateTimer.Enabled := true
  else
    Subscribe;
end;

procedure TfrmBoldUndo.SetBoldSystemHandle(const Value: TBoldSystemHandle);
begin
  FBoldSystemHandle := Value;
  actSetCheckPoint.BoldSystemHandle := Value;
  actUndo.BoldSystemHandle := Value;
  actRedo.BoldSystemHandle := Value;
  Subscribe;
end;

procedure TfrmBoldUndo.SetHideEmptyBlocks(const Value: boolean);
begin
  if FHideEmptyBlocks <> Value then
  begin
    FHideEmptyBlocks := Value;
    UpdateTimer.Enabled := true;
  end;
end;

procedure TfrmBoldUndo.Subscribe;
begin
  Subscriber.CancelAllSubscriptions;
  actUndo.BoldSystemHandle := BoldSystemHandle;
  actRedo.BoldSystemHandle := BoldSystemHandle;
  actSetCheckPoint.BoldSystemHandle := BoldSystemHandle;
  actClear.BoldSystemHandle := BoldSystemHandle;
  if Assigned(BoldSystemHandle) then
  begin
    BoldSystemHandle.AddSmallSubscription(Subscriber, [beDestroying], beDestroying);
    BoldSystemHandle.AddSmallSubscription(Subscriber, [beValueIDentityChanged], beValueIDentityChanged);
    if BoldSystemHandle.Active then
    begin
      BoldSystemHandle.System.UndoHandler.AddSubscription(Subscriber, beUndoChanged, beUndoChanged);
      BoldSystemHandle.System.UndoHandler.AddSubscription(Subscriber, beUndoBlock, beUndoBlock);
      BoldSystemHandle.System.UndoHandler.AddSubscription(Subscriber, beRedoBlock, beRedoBlock);
      BoldSystemHandle.System.UndoHandler.AddSubscription(Subscriber, beUndoSetCheckpoint, beUndoSetCheckpoint);
    end;
  end;
end;

function TfrmBoldUndo.Subscriber: TBoldSubscriber;
begin
  if not Assigned(fSubscriber) then
    fSubscriber := TBoldExtendedPassthroughSubscriber.CreateWithExtendedReceive(Receive);
  result := fSubscriber;
end;

procedure TfrmBoldUndo.tvUndoListCellDblClick(Sender: TcxCustomGridTableView;
  ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
  AShift: TShiftState; var AHandled: Boolean);
var
  Block: TBoldUndoBlock;
begin
  Block := UndoHandler.RedoBlocks.BlockByName[ACellViewInfo.RecordViewInfo.GridRecord.Values[0]];
  if not Assigned(Block) then
    Block := UndoHandler.UndoBlocks.BlockByName[ACellViewInfo.RecordViewInfo.GridRecord.Values[0]];
  if Assigned(Block) then
    ShowMessage(Copy(Block.Content,1,1024*64));
end;

procedure TfrmBoldUndo.tvUndoListCustomDrawCell(Sender: TcxCustomGridTableView;
  ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo;
  var ADone: Boolean);
var
  Block: TBoldUndoBlock;
begin
  Block := UndoHandler.RedoBlocks.BlockByName[AViewInfo.GridRecord.Values[0]];
  if Assigned(Block) then
    ACanvas.SetBrushColor(clSilver);
end;

procedure TfrmBoldUndo.UpdateTimerTimer(Sender: TObject);
begin
  UpdateTimer.Enabled := false;
  UpdateGrid;
end;

procedure TfrmBoldUndo.UpdateGrid;
var
  i,j: integer;
  Block: TBoldUndoBlock;

  procedure AddBlockToGrid(ABlock: TBoldUndoBlock);
  begin
    if HideEmptyBlocks and not ABlock.ContainsChanges then
      exit;
    tvUndoList.DataController.Values[j,0] := ABlock.BlockName;
    tvUndoList.DataController.Values[j,1] := ABlock.Caption;
    tvUndoList.DataController.Values[j,2] := ABlock.ObjectCount;
    tvUndoList.DataController.Values[j,3] := ABlock.Created;
    inc(j);
  end;
begin
  if not Assigned(UndoHandler) then
  begin
    tvUndoList.DataController.RecordCount := 0;
    exit;
  end;
  tvUndoList.DataController.BeginFullUpdate;
  try
    j := UndoHandler.UndoBlocks.Count + UndoHandler.RedoBlocks.Count;
    tvUndoList.DataController.RecordCount := j;
    if j = 0 then
      exit;
    j := 0;
    tvUndoList.DataController.FocusedRecordIndex := j-1;
    for I := 0 to UndoHandler.RedoBlocks.Count-1  do
    begin
      Block := UndoHandler.RedoBlocks[i];
      AddBlockToGrid(Block);
    end;
    for I := UndoHandler.UndoBlocks.Count-1 downto 0 do
    begin
      Block := UndoHandler.UndoBlocks[i];
      AddBlockToGrid(Block);
    end;
    tvUndoList.DataController.RecordCount := j;
  finally
    tvUndoList.DataController.EndFullUpdate;
  end;
  Visible := tvUndoList.DataController.RecordCount > 0;
end;

end.
