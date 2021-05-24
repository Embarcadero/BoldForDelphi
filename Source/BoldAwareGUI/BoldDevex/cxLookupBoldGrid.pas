unit cxLookupBoldGrid;

{$I cxVer.inc}

//  v2.03 - 25 Jan 2011  2007-2011 Daniel Mauric

interface

uses
  Windows,
  SysUtils, Classes, Controls, Graphics, Forms, StdCtrls, //DB,
  cxClasses, cxControls, cxGraphics, cxLookAndFeelPainters,
  cxEdit,
//  cxDBEdit,
  cxCustomData,
//  cxDB, cxDBData,
  cxEditRepositoryItems,
  cxLookupGrid,

  cxGridBoldSupportUnit,
  cxBoldEditors,
  BoldVariantControlPack, //BoldStringControlPack,
  BoldAbstractListHandle,
  BoldListListControlPack,
  BoldControlPack;

const
  DefaultSyncMode = False;

type
  TcxCustomLookupBoldGrid = class;

  { TcxLookupGridBoldDataController }

  TcxLookupGridBoldDataController = class(TcxBoldDataController)
  private
    function GetGrid: TcxCustomLookupBoldGrid;
  protected
    function GetItemID(AItem: TObject): Integer; override;
    function GetItemData(AItem: TObject): Integer; override;
//    procedure UpdateScrollBars; override;
    procedure _AfterMakeCellUptoDate(Follower: TBoldFollower); override;
    procedure _AfterMakeListUptoDate(Follower: TBoldFollower); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetItem(Index: Integer): TObject; override;
    property Grid: TcxCustomLookupBoldGrid read GetGrid;
  published
    property OnCompare;
  end;

  { TcxLookupBoldGridColumn }

  TcxLookupBoldGridDefaultValuesProvider = class(TcxCustomBoldEditDefaultValuesProvider)
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
  end;

  TcxLookupBoldGridColumn = class(TcxLookupGridColumn, IBoldAwareViewItem)
  private
    fBoldProperties: TBoldVariantFollowerController;
    function GetDataController: TcxLookupGridBoldDataController;
    procedure SetBoldProperties(
      const Value: TBoldVariantFollowerController);
    function GetBoldProperties: TBoldVariantFollowerController;
//    function GetField: TField;
//    function GetFieldName: string;
//    procedure SetFieldName(const Value: string);
  protected
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    procedure InitDefaultValuesProvider;
    property DataController: TcxLookupGridBoldDataController read GetDataController;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultCaption: string; override;
    function DefaultRepositoryItem: TcxEditRepositoryItem; override;
    function DefaultWidth: Integer; override;
//    property Field: TField read GetField;
  published
    property BoldProperties: TBoldVariantFollowerController read GetBoldProperties write SetBoldProperties;
  end;

  { TcxLookupBoldGridColumns }

  TcxLookupBoldGridColumns = class(TcxLookupGridColumns)
  private
    function GetColumn(Index: Integer): TcxLookupBoldGridColumn;
    procedure SetColumn(Index: Integer; Value: TcxLookupBoldGridColumn);
  public
    function Add: TcxLookupBoldGridColumn;
//    function ColumnByFieldName(const AFieldName: string): TcxLookupBoldGridColumn;
    property Items[Index: Integer]: TcxLookupBoldGridColumn read GetColumn write SetColumn; default;
  end;

  { TcxLookupBoldGridOptions }

  TcxLookupBoldGridOptions = class(TcxLookupGridOptions)
  private
    function GetGrid: TcxCustomLookupBoldGrid;
    function GetSyncMode: Boolean;
    procedure SetSyncMode(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    property Grid: TcxCustomLookupBoldGrid read GetGrid;
  published
    property SyncMode: Boolean read GetSyncMode write SetSyncMode
      default DefaultSyncMode;
  end;

  { TcxCustomLookupBoldGrid }

  TcxCustomLookupBoldGrid = class(TcxCustomLookupGrid)
  private
    function GetColumns: TcxLookupBoldGridColumns;
    function GetDataController: TcxLookupGridBoldDataController;
//    function GetDataSource: TDataSource;
//    function GetKeyFieldNames: string;
    function GetOptions: TcxLookupBoldGridOptions;
    procedure SetColumns(Value: TcxLookupBoldGridColumns);
    procedure SetDataController(Value: TcxLookupGridBoldDataController);
//    procedure SetDataSource(Value: TDataSource);
//    procedure SetKeyFieldNames(const Value: string);
    procedure SetOptions(Value: TcxLookupBoldGridOptions);
    function GetBoldListHandle: TBoldAbstractListHandle;
    procedure SetBoldListHandle(const Value: TBoldAbstractListHandle);
    function GetBoldProperties: TBoldListAsFollowerListController;
    procedure SetBoldProperties(
      const Value: TBoldListAsFollowerListController);
  protected
//    procedure CreateColumnsByFields(AFieldNames: TStrings); virtual;
    procedure DataChanged; override;
    function GetColumnClass: TcxLookupGridColumnClass; override;
    function GetColumnsClass: TcxLookupGridColumnsClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetOptionsClass: TcxLookupGridOptionsClass; override;
    procedure InitScrollBarsParameters; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure UpdateScrollBars; override; // for Delphi .NET
  public
    procedure CreateAllColumns;
//    procedure CreateColumnsByFieldNames(const AFieldNames: string);
    property Align;
    property Anchors;
    property Color;
    property Columns: TcxLookupBoldGridColumns read GetColumns write SetColumns;
    property DataController: TcxLookupGridBoldDataController read GetDataController write SetDataController;
    property Font;
    property LookAndFeel;
    property Options: TcxLookupBoldGridOptions read GetOptions write SetOptions;
    property ParentFont;
    property Visible;
  published
//    property DataSource: TDataSource read GetDataSource write SetDataSource;
//    property KeyFieldNames: string read GetKeyFieldNames write SetKeyFieldNames;
    property BoldLookupListHandle: TBoldAbstractListHandle read GetBoldListHandle write SetBoldListHandle;
    property BoldProperties: TBoldListAsFollowerListController read GetBoldProperties write SetBoldProperties;
  end;

  TcxCustomLookupBoldGridClass = class of TcxCustomLookupBoldGrid;

implementation

uses
  BoldElements,
  BoldSystem,
  BoldOcl,
  BoldSystemRT,
  BoldControlPackDefs;
//  cxEditBoldRegisteredRepositoryItems; //  cxEditDBRegisteredRepositoryItems;

function TcxLookupBoldGridDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  with TcxLookupBoldGridColumn(Owner) do
    Result := DataController.GetItemTextStored(Index);
end;

{ TcxLookupBoldGridColumn }

procedure TcxLookupBoldGridColumn.Assign(Source: TPersistent);
begin
  if Source is TcxLookupBoldGridColumn then
    BoldProperties.Assign(TcxLookupBoldGridColumn(Source).BoldProperties);
  inherited Assign(Source);
end;

function TcxLookupBoldGridColumn.DefaultCaption: string;
var
  lContextType: TBoldElementTypeInfo;
  lResultType: TBoldElementTypeInfo;
  lBoldMemberRTInfo: TBoldMemberRTInfo;
  lEvaluator: TBoldOcl;
begin
  result := '';
  lContextType := DataController.GetHandleStaticType;
  if Assigned(lContextType) then
  begin
    lEvaluator := lContextType.Evaluator as TBoldOcl;
    lBoldMemberRTInfo := lEvaluator.RTInfo(BoldProperties.Expression, lContextType, false);
    if Assigned(lBoldMemberRTInfo) then
      result := lBoldMemberRTInfo.ModelName
    else
    begin
      lResultType := lEvaluator.ExpressionType(BoldProperties.Expression, lContextType, false, BoldProperties.VariableList);
      if Assigned(lResultType) then
      begin
        result := lResultType.ModelName;
      end;
    end;
  end;
end;

function TcxLookupBoldGridColumn.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
// TODO: fix
  result := nil;
//  Result := GetDefaultEditDBRepositoryItems.GetItemByField(Field);
end;

function TcxLookupBoldGridColumn.DefaultWidth: Integer;
var
  lContextType: TBoldElementTypeInfo;
  lBoldMemberRTInfo: TBoldMemberRTInfo;
  lEvaluator: TBoldOcl;
  ACanvas: TcxCanvas;
  W: Integer;
  lLength: Integer;
begin
  Result := inherited DefaultWidth;
  lContextType := DataController.GetHandleStaticType;
  if Assigned(lContextType) then
  begin
    lEvaluator := lContextType.Evaluator as TBoldOcl;
    lBoldMemberRTInfo := lEvaluator.RTInfo(BoldProperties.Expression, lContextType, false);
    if (lBoldMemberRTInfo is TBoldAttributeRTInfo) then
    begin
      lLength := TBoldAttributeRTInfo(lBoldMemberRTInfo).Length;
      // we could perhaps provide sensible values for types with more or less known length,
      // such as Date, Time, Boolean, maybe even integer...
      if lLength > 0 then
      begin
        ACanvas := Grid.ViewInfo.Canvas;
        ACanvas.Font := GetContentFont;
        Result := lLength * ACanvas.TextWidth('0') + 4;
        if Grid.Options.ShowHeader then
        begin
          W := Grid.Painter.LFPainterClass.HeaderWidth(ACanvas, cxBordersAll, Caption,
            Grid.ViewInfo.GetHeaderFont);
          if W > Result then Result := W;
        end;
      end;
    end;
  end;
  CheckWidthValue(Result);
end;

function TcxLookupBoldGridColumn.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxLookupBoldGridDefaultValuesProvider;
end;

procedure TcxLookupBoldGridColumn.InitDefaultValuesProvider;
begin
//  TcxCustomDBEditDefaultValuesProvider(DefaultValuesProvider.GetInstance).Field := Field;
  TcxCustomBoldEditDefaultValuesProvider(DefaultValuesProvider.GetInstance).BoldHandleFollower := DataController.BoldHandleFollower;
//  TcxCustomBoldEditDefaultValuesProvider(DefaultValuesProvider.GetInstance).BoldHandle := DataController.BoldHandle;
  TcxCustomBoldEditDefaultValuesProvider(DefaultValuesProvider.GetInstance).BoldProperties := BoldProperties;
  // TODO: check this

end;

function TcxLookupBoldGridColumn.GetDataController: TcxLookupGridBoldDataController;
begin
  Result := TcxLookupGridBoldDataController(inherited DataController);
end;
{
function TcxLookupBoldGridColumn.GetField: TField;
begin
  Result := DataController.GetItemField(Index);
end;

function TcxLookupBoldGridColumn.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(Index);
end;

procedure TcxLookupBoldGridColumn.SetFieldName(const Value: string);
begin
  DataController.ChangeFieldName(Index, Value);
end;
}
constructor TcxLookupBoldGridColumn.Create(Collection: TCollection);
begin
  inherited;
  fBoldProperties := TBoldVariantFollowerController.Create(nil); // no useful owner can be provided
  fBoldProperties.ApplyPolicy := bapExit;
  fBoldProperties.OnGetContextType := DataController.GetHandleStaticType;
  DataController.BoldColumnsProperties.Add(fBoldProperties);

  // TODO: check this
  (DefaultValuesProvider as TcxLookupBoldGridDefaultValuesProvider).BoldHandleFollower := DataController.BoldHandleFollower;
  (DefaultValuesProvider as TcxLookupBoldGridDefaultValuesProvider).BoldProperties := BoldProperties;
//  fBoldProperties.AddSmallSubscription(fSubscriber, [beValueChanged], beValueChanged);
end;

destructor TcxLookupBoldGridColumn.Destroy;
begin
// FBoldProperties get freeded by DataController.BoldColumnsProperties
// so we only need to free FBoldProperties if DataController is assigned
  if Assigned(DataController) then
  begin
    DataController.BoldColumnsProperties.Remove(fBoldProperties);
    FreeAndNil(FBoldProperties);
  end;
  inherited;
end;

procedure TcxLookupBoldGridColumn.SetBoldProperties(
  const Value: TBoldVariantFollowerController);
begin
  if Assigned(Value) then
    fBoldProperties.Assign(Value);
end;

function TcxLookupBoldGridColumn.GetBoldProperties: TBoldVariantFollowerController;
begin
  result := fBoldProperties;
end;

{ TcxLookupBoldGridColumns }

function TcxLookupBoldGridColumns.Add: TcxLookupBoldGridColumn;
begin
  Result := inherited Add as TcxLookupBoldGridColumn;
end;

{function TcxLookupBoldGridColumns.ColumnByFieldName(const AFieldName: string): TcxLookupBoldGridColumn;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if AnsiCompareText(Result.FieldName, AFieldName) = 0 then
      Exit;
  end;
  Result := nil;
end;
}
function TcxLookupBoldGridColumns.GetColumn(Index: Integer): TcxLookupBoldGridColumn;
begin
  Result := inherited Items[Index] as TcxLookupBoldGridColumn;
end;

procedure TcxLookupBoldGridColumns.SetColumn(Index: Integer; Value: TcxLookupBoldGridColumn);
begin
  inherited Items[Index] := Value;
end;

{ TcxLookupGridBoldDataController }

constructor TcxLookupGridBoldDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  DataModeController.SyncMode := DefaultSyncMode;
//  DataModeController.SyncInsert := False;
end;

function TcxLookupGridBoldDataController.GetItem(Index: Integer): TObject;
begin
  Result := Grid.Columns[Index];
end;

{procedure TcxLookupGridBoldDataController.UpdateScrollBars;
begin
  Grid.UpdateScrollBars;
end;}

function TcxLookupGridBoldDataController.GetGrid: TcxCustomLookupBoldGrid;
begin
  Result := GetOwner as TcxCustomLookupBoldGrid;
end;

{procedure TcxLookupGridBoldDataController.UpdateScrollBars;
begin
  Grid.UpdateScrollBars;
end;}

procedure TcxLookupGridBoldDataController._AfterMakeCellUptoDate(
  Follower: TBoldFollower);
{var
  lRowIndex: integer;
  lRecordIndex: integer;
  lcxLookupGridRowViewInfo: TcxLookupGridRowViewInfo;
  lcxCustomEditViewInfo: TcxCustomEditViewInfo;
}
begin
//  inherited;
  if DataHasChanged or (SkipMakeCellUptoDate > 0) then
    exit;
  Grid.Change([lgcData]);
//  Grid.CheckChanges
  exit;
{  lRecordIndex := Follower.OwningFollower.index;
  lRowIndex := GetRowIndexByRecordIndex(lRecordIndex, false);
  lcxLookupGridRowViewInfo := Grid.ViewInfo.Rows.FindByRowIndex(lRowIndex);
  lcxCustomEditViewInfo := lcxLookupGridRowViewInfo[Follower.index].EditViewInfo;
  if Assigned(lcxCustomEditViewInfo.Edit) then
  begin
    lcxCustomEditViewInfo.Edit.Refresh;
    lcxCustomEditViewInfo.Edit.Invalidate;
  end;
}
end;

procedure TcxLookupGridBoldDataController._AfterMakeListUptoDate(
  Follower: TBoldFollower);
begin
// would be nice if we could force a recalc of dropdown height somehow in order for it to adjust to potential new record count
  inherited;
end;

function TcxLookupGridBoldDataController.GetItemID(
  AItem: TObject): Integer;
begin
  if AItem is TcxLookupBoldGridColumn then
    Result := TcxLookupBoldGridColumn(AItem).ID
  else
    Result := -1;
end;

function TcxLookupGridBoldDataController.GetItemData(
  AItem: TObject): Integer;
begin
  if AItem is TcxLookupBoldGridColumn then
    Result := Integer(TcxLookupBoldGridColumn(AItem).Index)
  else
    Result := -1;
end;

{ TcxLookupBoldGridOptions }

procedure TcxLookupBoldGridOptions.Assign(Source: TPersistent);
begin
  if Source is TcxLookupBoldGridOptions then
  begin
    if Assigned(Grid) then
      Grid.BeginUpdate;
    try
      inherited Assign(Source);
      SyncMode := TcxLookupBoldGridOptions(Source).SyncMode;
    finally
      if Assigned(Grid) then
        Grid.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxLookupBoldGridOptions.GetGrid: TcxCustomLookupBoldGrid;
begin
  Result := TcxCustomLookupBoldGrid(FGrid);
end;

function TcxLookupBoldGridOptions.GetSyncMode: Boolean;
begin
  Result := DefaultSyncMode;
{  if Assigned(Grid) then
    Result := Grid.DataController.DataModeController.SyncMode
  else
    Result := DefaultSyncMode;
}
end;

procedure TcxLookupBoldGridOptions.SetSyncMode(Value: Boolean);
begin
//  if Assigned(Grid) then
//    Grid.DataController.DataModeController.SyncMode := Value;
end;

{ TcxCustomLookupBoldGrid }

procedure TcxCustomLookupBoldGrid.CreateAllColumns;
begin
  Assert(false, 'TcxCustomLookupBoldGrid.CreateAllColumns not implemented yet.');
end;

{procedure TcxCustomLookupBoldGrid.CreateColumnsByFieldNames(const AFieldNames: string);
begin
  Assert(false);
end;}

{procedure TcxCustomLookupBoldGrid.CreateColumnsByFields(AFieldNames: TStrings);
begin
  Assert(false);
end;}

procedure TcxCustomLookupBoldGrid.DataChanged;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    Columns[I].InitDefaultValuesProvider;
  inherited DataChanged;
end;

function TcxCustomLookupBoldGrid.GetColumnClass: TcxLookupGridColumnClass;
begin
  Result := TcxLookupBoldGridColumn;
end;

function TcxCustomLookupBoldGrid.GetColumnsClass: TcxLookupGridColumnsClass;
begin
  Result := TcxLookupBoldGridColumns;
end;

function TcxCustomLookupBoldGrid.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxLookupGridBoldDataController;
end;

function TcxCustomLookupBoldGrid.GetOptionsClass: TcxLookupGridOptionsClass;
begin
  Result := TcxLookupBoldGridOptions;
end;

procedure TcxCustomLookupBoldGrid.InitScrollBarsParameters;
begin
{
  if DataController.IsGridMode and DataController.IsSequenced then
  begin
    SetScrollBarInfo(sbVertical, 0,
      (DataController.DataSetRecordCount - 1) + (ViewInfo.VisibleRowCount - 1),
      1, ViewInfo.VisibleRowCount, DataController.RecNo - 1, True, True);
  end
  else
}
    inherited InitScrollBarsParameters;
end;

procedure TcxCustomLookupBoldGrid.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
{  if DataController.IsGridMode and DataController.IsSequenced then
  begin
    if AScrollBarKind = sbVertical then
    begin
      case AScrollCode of
        scLineUp:
          FocusNextRow(False);
        scLineDown:
          FocusNextRow(True);
        scPageUp:
          FocusPriorPage;
        scPageDown:
          FocusNextPage;
        scTrack: ;
        scPosition:
          DataController.RecNo := AScrollPos + 1;
      end;
    end
    else
      inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
    AScrollPos := DataController.RecNo - 1;
  end
  else
}
    inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxCustomLookupBoldGrid.UpdateScrollBars;
begin
  inherited UpdateScrollBars;
end;

function TcxCustomLookupBoldGrid.GetColumns: TcxLookupBoldGridColumns;
begin
  Result := inherited Columns as TcxLookupBoldGridColumns;
end;

function TcxCustomLookupBoldGrid.GetDataController: TcxLookupGridBoldDataController;
begin
  Result := TcxLookupGridBoldDataController(FDataController);
end;

function TcxCustomLookupBoldGrid.GetOptions: TcxLookupBoldGridOptions;
begin
  Result := TcxLookupBoldGridOptions(FOptions);
end;

procedure TcxCustomLookupBoldGrid.SetColumns(Value: TcxLookupBoldGridColumns);
begin
  inherited Columns := Value;
end;

procedure TcxCustomLookupBoldGrid.SetDataController(Value: TcxLookupGridBoldDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxCustomLookupBoldGrid.SetOptions(Value: TcxLookupBoldGridOptions);
begin
  FOptions.Assign(Value);
end;

function TcxCustomLookupBoldGrid.GetBoldListHandle: TBoldAbstractListHandle;
begin
  result := DataController.BoldHandle;
end;

procedure TcxCustomLookupBoldGrid.SetBoldListHandle(
  const Value: TBoldAbstractListHandle);
begin
  DataController.BoldHandle.Assign(Value);
end;

function TcxCustomLookupBoldGrid.GetBoldProperties: TBoldListAsFollowerListController;
begin
  result := DataController.BoldProperties;
end;

procedure TcxCustomLookupBoldGrid.SetBoldProperties(
  const Value: TBoldListAsFollowerListController);
begin
  DataController.BoldProperties.Assign(Value);
end;

end.
