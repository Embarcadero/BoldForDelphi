unit BoldOCLExplorer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, BoldListBox, BoldSubscription,
  BoldHandles, BoldRootedHandles, BoldAbstractListHandle, BoldCursorHandle,
  BoldListHandle, BoldEdit, ExtCtrls, BoldExpressionHandle,
  BoldVariableDefinition, BoldOclVariables, ActnList, Menus, BoldSystem,
  BoldHandleAction, BoldActions, BoldSystemHandle, cxStyles, cxCustomData,
  cxGraphics, cxFilter, cxData, cxDataStorage, cxEdit,
  cxGridCustomTableView, cxGridTableView, cxGridBoldSupportUnit,
  cxGridLevel, cxClasses, cxControls, cxGridCustomView, cxGrid,
  BoldPlaceableListSubscriber, BoldControlPack, BoldElements,
  BoldStringControlPack, cxSpinEdit, cxCheckBox, BoldDerivedHandle,
  cxLookAndFeels, cxLookAndFeelPainters, BoldVariantControlPack,
  cxGridCustomPopupMenu, cxGridPopupMenu, cxNavigator, BoldRawSQLHandle,
  dxDateRanges, dxScrollbarAnnotations, dxBarBuiltInMenu, System.Actions;

type
  TOclExplorerForm = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    Panel3: TPanel;
    List2EditOCLButton: TButton;
    List2OCLMemo: TMemo;
    Panel4: TPanel;
    Panel5: TPanel;
    Label2: TLabel;
    List1Handle: TBoldListHandle;
    Panel8: TPanel;
    Splitter3: TSplitter;
    Panel9: TPanel;
    Panel10: TPanel;
    List1EditOCLButton: TButton;
    List1OCLMemo: TMemo;
    Panel11: TPanel;
    Panel12: TPanel;
    Label1: TLabel;
    List2Handle: TBoldListHandle;
    Splitter5: TSplitter;
    BoldVariableDefinition: TBoldOclVariables;
    MainMenu1: TMainMenu;
    FileMenu: TMenuItem;
    ActionList1: TActionList;
    CloseApplicationAction: TAction;
    AboutAction: TAction;
    Close1: TMenuItem;
    ToolsMenu: TMenuItem;
    EditOCL1: TMenuItem;
    HelpMenu: TMenuItem;
    About1: TMenuItem;
    ShowDebuggerAction: TAction;
    EditList1OCL1: TMenuItem;
    EditList2OCL1: TMenuItem;
    EditList1Action: TAction;
    EditList2Action: TAction;
    ShowOCLSyntaxSummary: TAction;
    ShowOCLSummary1: TMenuItem;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    N1: TMenuItem;
    UpdateDB1: TMenuItem;
    cbEvaluateInPS2: TCheckBox;
    cbEvaluateInPS1: TCheckBox;
    cxGrid1: TcxGrid;
    LeftView: TcxGridBoldTableView;
    cxGrid1Level1: TcxGridLevel;
    LeftViewClass: TcxGridBoldColumn;
    cxGrid2: TcxGrid;
    RightView: TcxGridBoldTableView;
    cxGridLevel1: TcxGridLevel;
    LeftViewObjectCount: TcxGridBoldColumn;
    BoldPlaceableListSubscriber1: TBoldPlaceableListSubscriber;
    BoldPlaceableListSubscriber2: TBoldPlaceableListSubscriber;
    LeftViewTopSortedIndex: TcxGridBoldColumn;
    LeftViewPersistent: TcxGridBoldColumn;
    LeftViewClassState: TcxGridBoldColumn;
    brObjects: TBoldAsVariantRenderer;
    brIndex: TBoldAsVariantRenderer;
    bfIsPersistent: TBoldAsVariantRenderer;
    bfClassState: TBoldAsVariantRenderer;
    cxGridPopupMenu1: TcxGridPopupMenu;
    cxGridPopupMenu2: TcxGridPopupMenu;
    LeftViewIdCount: TcxGridBoldColumn;
    brIds: TBoldAsVariantRenderer;
    LeftViewColumn1: TcxGridBoldColumn;
    brIsAbstract: TBoldAsVariantRenderer;
    LeftViewColumn2: TcxGridBoldColumn;
    brIsLinkClass: TBoldAsVariantRenderer;
    RightViewColumn1: TcxGridBoldColumn;
    RightViewColumn2: TcxGridBoldColumn;
    RightViewColumn3: TcxGridBoldColumn;
    procedure List2EditOCL(Sender: TObject);
    procedure List1EditOCL(Sender: TObject);
    procedure CloseApplicationActionExecute(Sender: TObject);
    procedure ShowDebuggerActionExecute(Sender: TObject);
    procedure BoldEditListActionPostExecute(Sender: TObject);
    procedure ShowOCLSyntaxSummaryExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbEvaluateInPS2Click(Sender: TObject);
    procedure cbEvaluateInPS1Click(Sender: TObject);
    procedure BoldPlaceableListSubscriber1AfterMakeUptoDate(
      Follower: TBoldFollower);
    procedure BoldPlaceableListSubscriber2AfterMakeUptoDate(
      Follower: TBoldFollower);
    function brObjectsGetAsVariant(
      aFollower: TBoldFollower): Variant;
    function brIndexGetAsVariant(aFollower: TBoldFollower): Variant;
    procedure brClassListSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    function bfIsPersistentGetAsVariant(aFollower: TBoldFollower): Variant;
    function bfClassStateGetAsVariant(aFollower: TBoldFollower): Variant;
    procedure bfClassStateSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
    function brIdsGetAsVariant(AFollower: TBoldFollower): Variant;
    function brIsAbstractGetAsVariant(AFollower: TBoldFollower): Variant;
    function brIsLinkClassGetAsVariant(AFollower: TBoldFollower): Variant;
  private
    FBoldSystem: TBoldSystem;
    fSystemHandle: TBoldAbstractSystemHandle;
    { Private declarations }
    Procedure UpdateStatus;
    procedure EditOCL(ListHandle: TBoldListHandle);
//    procedure ExportBoldListToStringList(BoldList: TBoldListHandle; StringList: TStrings);
    procedure SetBoldSystem(const Value: TBoldSystem);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    constructor CreateWithSystem(anOwner: TComponent; SystemHandle: TBoldSystemHandle);
    property BoldSystem: TBoldSystem read FBoldSystem write SetBoldSystem;
  end;

implementation

uses
//  BoldOclSymbolLister,
  BoldOclPropEditor,
  BoldSystemDebuggerForm,
  BoldObjectListControllers,
  cxVariants, BoldDefs, BoldSystemRT, Variants, BoldDBInterfaces;

{$R *.DFM}

procedure TOclExplorerForm.EditOCL(ListHandle: TBoldListHandle);
begin
  with TBoldOclPropEditForm.Create(Self) do
  try
    Context := ListHandle.RootHandle.BoldType;
    OclExpr := ListHandle.Expression;

    // Make sure that any variables we have added are visible to the OCL editor
    if assigned(ListHandle.Variables) then
      Variables := ListHandle.Variables.VariableList;

    if ShowModal = mrOK then
      ListHandle.Expression := OCLExpr;
  finally
    Free;
  end;
end;

procedure TOclExplorerForm.List2EditOCL(Sender: TObject);
begin
  EditOCL(List2Handle);
  UpdateStatus;
end;

procedure TOclExplorerForm.UpdateStatus;
var
  lTypeName: string;
begin
  if Assigned(List1Handle.BoldType) then
    lTypeName := List1Handle.BoldType.AsString
  else
    lTypeName := '';
  Label1.Caption := Format('%s (%d)', [lTypeName, List1Handle.count]);

  if Assigned(List2Handle.BoldType) then
    lTypeName := List2Handle.BoldType.AsString
  else
    lTypeName := '';
  if Assigned(List2Handle.BoldType) then  
    Label2.Caption := Format('%s (%d)', [List2Handle.BoldType.AsString, List2Handle.count])
  else
    Label2.Caption := '';
  List1OCLMemo.Text := AdjustLineBreaks(List1Handle.Expression);
//  ExportBoldListToStringList(List1Handle, List1ResultMemo.Lines);

  List2OCLMemo.Text := AdjustLineBreaks(List2Handle.Expression);
//  ExportBoldListToStringList(List2Handle, List2ResultMemo.Lines);
end;

procedure TOclExplorerForm.List1EditOCL(Sender: TObject);
begin
  EditOCL(List1Handle);
  UpdateStatus;
end;

(*procedure TOclExplorerForm.ExportBoldListToStringList(BoldList: TBoldListHandle;
    StringList: TStrings);
var Counter: Integer;
begin
try
  StringList.BeginUpdate;
  try
    StringList.Clear;
    StringList.Add('Type Information');
    StringList.Add('----------------');
    StringList.Add('List Type (AsString)    = ' + BoldList.ListType.AsString);
    StringList.Add('List Type (DelphiClass) = ' + BoldList.ListType.ClassName);
    if Assigned(BoldList.BoldType) then
    begin
      StringList.Add('Bold Type (AsString)    = ' + BoldList.BoldType.AsString);
      StringList.Add('Bold Type (DelphiClass) = ' + BoldList.BoldType.ClassName);
    end
    else
    begin
      StringList.Add('Bold Type (AsString)    = nil');
      StringList.Add('Bold Type (DelphiClass) = nil');
    end;

    if Assigned(BoldList.List) then
      StringList.Add('List Type (DelphiClass) = ' + BoldList.List.ClassName)
    else
      StringList.Add('List Type (DelphiClass) = nil');

    StringList.Add('');
    StringList.Add('Content Information');
    StringList.Add('-------------------');
    StringList.Add('Result Count = ' + IntToStr(BoldList.Count));
    if Assigned(BoldList.Value) then
    begin
      StringList.Add('Value (AsString)    = ' + BoldList.Value.AsString);
      StringList.Add('Value (DelphiClass) = ' + BoldList.Value.ClassName);
    end
    else
      StringList.Add('Value = nil');
{
    if assigned(BoldList.List) then
    begin
      StringList.Add('');
      StringList.Add('List Contents');
      StringList.Add('-------------------');
      for counter := 0 to  BoldList.List.Count -1 do
        StringList.Add(BoldList.List[counter].AsString);
    end;
}
  finally
    StringList.EndUpdate;
  end;
except
end;
end;
*)

procedure TOclExplorerForm.CloseApplicationActionExecute(Sender: TObject);
begin
  close;
end;

procedure TOclExplorerForm.ShowDebuggerActionExecute(Sender: TObject);
begin
  Assert(assigned(FBoldSystem));
  with TBoldSystemDebuggerFrm.CreateWithSystem(application,fSystemHandle.System) do
    show;
end;

procedure TOclExplorerForm.BoldEditListActionPostExecute(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TOclExplorerForm.ShowOCLSyntaxSummaryExecute(Sender: TObject);
begin
//  TOCLSyntaxForm.Create(self).Show;
end;

procedure TOclExplorerForm.SetBoldSystem(const Value: TBoldSystem);
begin
  FBoldSystem := Value;
end;

procedure TOclExplorerForm.FormCreate(Sender: TObject);
begin
  if BoldSystem = nil then
    BoldSystem:= TBoldSystem.DefaultSystem;
  if FSystemHandle = nil then
    fSystemHandle:= TBoldAbstractSystemHandle.DefaultBoldSystemHandle;

  List1Handle.Expression  := BoldSystem.BoldSystemTypeInfo.RootClassTypeInfo.ExpressionName + '.allSubClasses';

  List1Handle.RootHandle:= fSystemHandle;
  RightView.DataController.ClearColumnsOnTypeChange := false;
  BoldUpdateDBAction1.BoldSystemHandle := TBoldSystemHandle(fSystemHandle);
  UpdateStatus;
end;

procedure TOclExplorerForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TOclExplorerForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

constructor TOclExplorerForm.CreateWithSystem(anOwner: TComponent;
  SystemHandle: TBoldSystemHandle);
begin
  fSystemHandle:= SystemHandle;
  BoldSystem := SystemHandle.System;
  if anOwner = nil then
    inherited Create(Application)
  else
    inherited Create(anOwner);
end;

procedure TOclExplorerForm.cbEvaluateInPS1Click(Sender: TObject);
begin
  List1Handle.EvaluateInPS := cbEvaluateInPS1.Checked;
end;

procedure TOclExplorerForm.cbEvaluateInPS2Click(Sender: TObject);
begin
  List2Handle.EvaluateInPS := cbEvaluateInPS2.Checked;
end;

procedure TOclExplorerForm.BoldPlaceableListSubscriber1AfterMakeUptoDate(
  Follower: TBoldFollower);
begin
  Label1.Caption := Format('%s (%d)', [List1Handle.BoldType.AsString, List1Handle.count]);
  if Assigned(List1Handle.CurrentElement) then
  begin
    List2Handle.RootHandle := fSystemHandle;
    List2Handle.Expression := List1Handle.CurrentElement.AsString + '.allLoadedObjects';
    List2Handle.MutableListExpression := BoldSystem.BoldSystemTypeInfo.RootClassTypeInfo.ExpressionName + '.allInstances'
  end
  else
  begin
    List2Handle.Expression := '';
    List2Handle.RootHandle := nil;    
  end;
  Label2.Caption := Format('%s (%d)', [List2Handle.ListElementType.AsString, List2Handle.count]);
end;

procedure TOclExplorerForm.BoldPlaceableListSubscriber2AfterMakeUptoDate(
  Follower: TBoldFollower);
begin
//  Label2.Caption := Format('%s (%d)', [List2Handle.BoldType.AsString, List2Handle.count]);
end;

type TBoldObjectListAccess = Class(TBoldObjectList);

function TOclExplorerForm.brObjectsGetAsVariant(
  aFollower: TBoldFollower): Variant;
var
  lClassBoldObjectList: TBoldObjectList;
  lBoldClassListController: TBoldClassListController;
begin
  Result := Null;
  if Assigned(aFollower.Element) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass((aFollower.Element as TBoldClassTypeInfo).ObjectClass)];
    lBoldClassListController := TBoldObjectListAccess(lClassBoldObjectList).ObjectListController as TBoldClassListController;
    result := lBoldClassListController.LoadedObjectCount;
  end;
end;

procedure TOclExplorerForm.brClassListSubscribe(aFollower: TBoldFollower;
  Subscriber: TBoldSubscriber);
var
  lClassBoldObjectList: TBoldObjectList;
begin
  if Assigned(aFollower.Element) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass((aFollower.Element as TBoldClassTypeInfo).ObjectClass)];
    lClassBoldObjectList.AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted, beObjectFetched, beObjectUnloaded], breReEvaluate);
  end;
end;

function TOclExplorerForm.brIdsGetAsVariant(AFollower: TBoldFollower): Variant;
var
  Query: IBoldQuery;
  sql: string;
  lClassBoldObjectList: TBoldObjectList;
  ClassTypeInfo: TBoldClassTypeInfo;
  dbType: integer;
  TableName: string;
const
  cAllInstancesIdCount = 'select count(bold_id) from %s where bold_type = %d';
begin
  Result := Null;
  if Assigned(AFollower.Element) then
  begin
    ClassTypeInfo := (aFollower.Element as TBoldClassTypeInfo);
    if ClassTypeInfo.Persistent then
    begin
      with TBoldSystemHandle(fSystemHandle).PersistenceHandleDB.PersistenceControllerDefault.PersistenceMapper do
      begin
        TableName := RootClassObjectPersistenceMapper.MainTable.SQLName;
        dbType := BoldDbTypeForTopSortedIndex(ClassTypeInfo.TopSortedIndex);
      end;
      if dbType = 0 then
        exit;
      Query := TBoldSystemHandle(fSystemHandle).PersistenceHandleDB.DatabaseInterface.GetQuery;
      try
        Sql := Format(cAllInstancesIdCount, [TableName, dbType]);
        Query.AssignSQLText(SQL);
        Query.Open;
        result := Query.Fields[0].AsInteger;
      finally
        Query.Close;
        TBoldSystemHandle(fSystemHandle).PersistenceHandleDB.DatabaseInterface.ReleaseQuery(Query);
      end;
    end
    else
    begin
      lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass(ClassTypeInfo.ObjectClass)];
      result := lClassBoldObjectList.Count;
    end;
  end;
end;

function TOclExplorerForm.brIndexGetAsVariant(
  aFollower: TBoldFollower): Variant;
begin
  Result := Null;
  if Assigned(aFollower.Element) then
  begin
    result := (aFollower.Element as TBoldClassTypeInfo).TopSortedIndex;
  end;
end;

function TOclExplorerForm.brIsAbstractGetAsVariant(
  AFollower: TBoldFollower): Variant;
begin
  Result := Null;
  if Assigned(aFollower.Element) then
  begin
    result := (aFollower.Element as TBoldClassTypeInfo).IsAbstract;
  end;
end;

function TOclExplorerForm.brIsLinkClassGetAsVariant(
  AFollower: TBoldFollower): Variant;
begin
  Result := Null;
  if Assigned(AFollower.Element) then
  begin
    result := (aFollower.Element as TBoldClassTypeInfo).IsLinkClass;
  end;
end;

function TOclExplorerForm.bfIsPersistentGetAsVariant(
  aFollower: TBoldFollower): Variant;
begin
  Result := Null;
  if Assigned(aFollower.Element) then
  begin
    result := (aFollower.Element as TBoldClassTypeInfo).Persistent;
  end;
end;

function TOclExplorerForm.bfClassStateGetAsVariant(
  aFollower: TBoldFollower): Variant;
var
  lClassBoldObjectList: TBoldObjectList;
begin
  Result := Null;
  if Assigned(aFollower.Element) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass((aFollower.Element as TBoldClassTypeInfo).ObjectClass)];
    result := lClassBoldObjectList.BoldPersistenceState = bvpsCurrent;
{    case lClassBoldObjectList.BoldPersistenceState of
      bvpsCurrent : result := 'Current';
      bvpsModified : result := 'Modified';
      bvpsInvalid : result := '';
      bvpsTransient : result := 'Transient';
    end;
}
  end;
end;

procedure TOclExplorerForm.bfClassStateSubscribe(aFollower: TBoldFollower;
  Subscriber: TBoldSubscriber);
var
  lClassBoldObjectList: TBoldObjectList;
begin
  if Assigned(aFollower.Element) then
  begin
    lClassBoldObjectList := BoldSystem.ClassByObjectClass[TBoldObjectClass((aFollower.Element as TBoldClassTypeInfo).ObjectClass)];
    lClassBoldObjectList.AddSmallSubscription(Subscriber, [beItemAdded, beItemDeleted, beObjectFetched, beClassListStateChanged, beObjectUnloaded], breReEvaluate);
  end;
end;

end.
