unit BoldAppMain;

{$INCLUDE Bold.inc}

interface

uses
  {$IFDEF BOLD_DELPHI6_OR_LATER}
  HTTPProd,
  {$ENDIF}
  SysUtils,
  Classes,
  Controls,
  Forms,
  HTTPApp,
  ActnList,
  BoldHandleAction,
  BoldActions,
  BoldDBActions,
  StdCtrls,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  Grids,
  BoldGrid,
  Menus,
  BoldAFPPluggable,
  BoldExpressionHandle,
  BoldEdit,
  BoldLabel,
  BoldElements,
  BoldSortedHandle,
  BoldAppDataModUnit,
  BudgetClasses,
  BoldHandle,
  BoldReferenceHandle, BoldIBDatabaseAction
  ;

type
  TMainForm = class(TForm)
    ActionList1: TActionList;
    blhAllBudget: TBoldListHandle;
    blhAllRow: TBoldListHandle;
    blhAllColumn: TBoldListHandle;
    blhAllCell: TBoldListHandle;
    bgrBudget: TBoldGrid;
    bgrRow: TBoldGrid;
    bgrCol: TBoldGrid;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    PopupMenu1: TPopupMenu;
    New1: TMenuItem;
    Delete1: TMenuItem;
    Label5: TLabel;
    BoldGrid1: TBoldGrid;
    blhBudgetCells: TBoldListHandle;
    btnCreateCells: TButton;
    BoldLabel1: TBoldLabel;
    BoldLabel2: TBoldLabel;
    BoldLabel3: TBoldLabel;
    bcCells: TBoldComparer;
    bcColumn: TBoldComparer;
    bcRow: TBoldComparer;
    btnClearCells: TButton;
    Button1: TButton;
    Edit1: TEdit;
    Label4: TLabel;
    Memo1: TMemo;
    BoldReferenceHandle1: TBoldReferenceHandle;
    cmdUpdate: TButton;
    cmdCreateDB: TButton;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    cmdOpen: TButton;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure HelloWorldHTMLTag(Sender: TObject; Tag: TTag;
      const TagString: String; TagParams: TStrings;
      var ReplaceText: String);
    procedure New1Click(Sender: TObject);
    procedure btnUpdateDBClick(Sender: TObject);
    procedure Delete1Click(Sender: TObject);
    procedure btnCreateCellsClick(Sender: TObject);
    function bcCellsCompare(Item1, Item2: TBoldElement): Integer;
    function bcRowCompare(Item1, Item2: TBoldElement): Integer;
    function bcColumnCompare(Item1, Item2: TBoldElement): Integer;
    procedure btnClearCellsClick(Sender: TObject);
    procedure BoldActivateSystemAction1SystemClosed(Sender: TObject);
    procedure BoldActivateSystemAction1SystemOpened(Sender: TObject);
    procedure BoldIBAliasAction1AliasCreated(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FWho: string;
    FBudget: string;
    procedure CreateCells;
    procedure EnableControls;
  end;

var
  MainForm: TMainForm;


implementation

uses
  dmXMLProducerForBudget
  ;

{$R *.DFM}

procedure TMainForm.HelloWorldHTMLTag(Sender: TObject; Tag: TTag;
  const TagString: String; TagParams: TStrings; var ReplaceText: String);
begin
  ReplaceText := FWho;
  Caption := FWho + DateTimeToStr(now);
end;

procedure TMainForm.New1Click(Sender: TObject);
begin
  if bgrBudget.Focused then
    TBudget.Create(dmMain.BoldSystemHandle1.System, true)
  else if bgrCol.Focused then
    TCol.Create(dmMain.BoldSystemHandle1.System, true)
  else if bgrRow.Focused then
    TRow.Create(dmMain.BoldSystemHandle1.System, true);
end;

procedure TMainForm.btnUpdateDBClick(Sender: TObject);
begin
  if dmMain.BoldSystemHandle1.Active then
    dmMain.BoldSystemHandle1.UpdateDatabase;
end;

procedure TMainForm.Delete1Click(Sender: TObject);
begin
  if bgrBudget.Focused then
    blhAllBudget.RemoveCurrentElement
  else if bgrCol.Focused then
    blhAllColumn.RemoveCurrentElement
  else if bgrRow.Focused then
    blhAllRow.RemoveCurrentElement;
end;

procedure TMainForm.CreateCells;
var
  i, j: integer;
  budget: TBudget;
  NewCell, temp: TACell;
  r: TRow;
  c: TCol;
begin
  if (blhAllBudget.Count = 0) then
    Exit;
  budget:= (blhAllBudget.CurrentBoldObject) as TBudget;
  for i := 0 to blhAllRow.Count -1 do
    for j := 0 to blhAllColumn.Count - 1 do
    begin
      r := (blhAllRow.List[i] as TRow);
      c := (blhAllColumn.List[j] as TCol);
      temp := budget.ACell[c.aNumber, r.aNumber];
      if not Assigned(temp) then
      begin
        budget.Row.Add(r);
        budget.Col.Add(c);
        NewCell := TACell.Create(dmMain.BoldSystemHandle1.System, true);
        NewCell.Row := r;
        NewCell.Col := c;
        budget.M_ACell.Add(NewCell);
      end;
    end;
end;

procedure TMainForm.btnCreateCellsClick(Sender: TObject);
begin
  if dmMain.BoldSystemHandle1.Active then
    CreateCells;
end;

function TMainForm.bcCellsCompare(Item1, Item2: TBoldElement): Integer;
var
  acell1, acell2: TACell;
begin
  Result := 0;
  if Assigned(item1) and Assigned(item2) then
  begin
    aCell1 := Item1 as TACell;
    aCell2 := Item2 as TACell;
    if (aCell1.Row.aNumber < aCell2.Row.aNumber) then
      Result := -1
    else if (aCell1.Row.aNumber > aCell2.Row.aNumber) then
      Result := 1
    else if (aCell1.Col.aNumber < aCell2.Col.aNumber) then
      Result := -1
    else if (aCell1.Col.aNumber > aCell2.Col.aNumber) then
      Result := 1;
  end;
end;

function TMainForm.bcRowCompare(Item1, Item2: TBoldElement): Integer;
var
  aRow1, aRow2: TRow;
begin
  Result := 0;
  if Assigned(item1) and Assigned(item2) then
  begin
    aRow1 := Item1 as TRow;
    aRow2 := Item2 as TRow;
    if (ARow1.aNumber = aRow2.aNumber) then
      Result := 0
    else if (ARow1.aNumber > aRow2.aNumber) then
      Result := 1
    else
      Result := -1;
  end;
end;

function TMainForm.bcColumnCompare(Item1, Item2: TBoldElement): Integer;
var
  aCol1, aCol2: TCol;
begin
  Result := 0;
  if Assigned(item1) and Assigned(item2) then
  begin
    aCol1 := Item1 as TCol;
    aCol2 := Item2 as TCol;
    if (ACol1.aNumber = aCol2.aNumber) then
      Result := 0
    else if (ACol1.aNumber > aCol2.aNumber) then
      Result := 1
    else
      Result := -1;
  end;
end;

procedure TMainForm.btnClearCellsClick(Sender: TObject);
begin
  if dmMain.BoldSystemHandle1.Active then
   ((blhAllBudget.CurrentBoldObject) as TBudget).M_ACell.Clear;
end;

procedure TMainForm.EnableControls;
begin
  btnCreateCells.Enabled := dmMain.BoldSystemHandle1.Active;
  btnClearCells.Enabled := dmMain.BoldSystemHandle1.Active;
end;

procedure TMainForm.BoldActivateSystemAction1SystemClosed(Sender: TObject);
begin
  EnableControls;
end;

procedure TMainForm.BoldActivateSystemAction1SystemOpened(Sender: TObject);
begin
  EnableControls;
end;

procedure TMainForm.BoldIBAliasAction1AliasCreated(Sender: TObject);
begin
  EnableControls;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
//  Memo1.Lines.Text := dmXML.GetXMLForBudget(Edit1.Text);
  Memo1.Lines.Text := dmXMLProducer.GetXMLForBudget(Edit1.Text);
end;

end.
