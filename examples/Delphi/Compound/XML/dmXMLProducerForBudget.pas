unit dmXMLProducerForBudget;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldSubscription, BoldElements, BoldManipulators, BoldXMLProducers,
  BoldStringList, MSXML_TLB, BudgetClasses, BoldHandle,
  BoldServerHandles, BoldComServerHandles, BoldXMLDispatcher, BoldXMLRequests,
  BoldControlPack, BoldStringControlPack;

type
  TdmXmlProducer = class(TDataModule)
    BudgetProducer: TBoldXMLProducer;
    BoldManipulator1: TBoldManipulator;
    BoldXMLDispatcher1: TBoldXMLDispatcher;
    BoldComServerHandle1: TBoldComServerHandle;
    procedure BudgetProducerProduce(const paramList: TBoldStringList;
      const DomDoc: IXMLDOMDocument);
    procedure MainDispatcherActions1Action(const request: TBoldXMLRequest;
      out response: String);
    function BoldManipulator1Mappers0Get(Element: TBoldElement): String;
    procedure BoldManipulator1Mappers0Set(Element: TBoldElement;
      const NewValue: String);
    procedure BoldXMLDispatcher1GetXMLRequest(const XML: String;
      out Request: TBoldXMLRequest);
  private
    { Private declarations }
  public
    { Public declarations }
    function GetXMLForBudget(const BudgetName: string): string;
    procedure AddRow(Budget: TBudget; Row: TRow; parentElement: IXMLDomElement);
  end;

var
  dmXmlProducer: TdmXmlProducer;

implementation

uses
  BoldAppDataModUnit,
  BoldUtils,
  BoldDefs
  ;

{$R *.DFM}

{ TDataModule1 }

function TdmXmlProducer.GetXMLForBudget(const BudgetName: string): string;
var
  ParamList: TBoldStringList;
  Data: TBoldXMLRequest;
begin
  if Trim(BudgetName) = '' then
    Result := ''
  else
  begin
    try
      Data := TBoldXMLRequest.CreateInitialized;
      ParamList:= TBoldStringList.Create;
      ParamList.Add(Format('%s=%s', ['BudgetName', BudgetName]));
      Data.SetAction('GetBudget');
      Data.SetParams(ParamList);
      BoldXMLDispatcher1.DispatchAction(TboldXMLRequest.CreateFromXML(Data.DomDocument.XML), Result);
    finally
      FreeAndNil(ParamList);
    end;
  end;
end;

procedure TdmXmlProducer.BudgetProducerProduce(
  const paramList: TBoldStringList; const DomDoc: IXMLDOMDocument);
var
  Budget: TBudget;
  BudgetName: string;
  RootElement, CurrentElement, cur: IXMLDOMElement;
  Boldelement: TBoldIndirectElement;
  c, r: integer;
  Col: TCol;
  row: TRow;
begin
  BudgetName := paramList.Values['BudgetName'];
  if (BudgetName <> '') then
  begin
    if not dmMain.BoldSystemHandle1.Active then
      dmMain.BoldSystemHandle1.Active := true;
    Budget := dmMain.getBudgetByName(BudgetName);
    if not Assigned(Budget) then
      raise EBold.Create('Budget not found');
    RootElement := DomDoc.createElement('BUDGET');
    RootElement.setAttribute('aName', Budget.aName);
    RootElement.setAttribute('aNumber', Budget.aNumber);
    DomDoc.DocumentElement := RootElement;
    BoldElement := TBoldIndirectElement.Create;
    try
      {columns}
      Budget.EvaluateExpression('col->OrderBy(aNumber)', BoldElement);
      CurrentElement := BudgetProducer.AddDomElement(RootElement, 'COLUMNS');
      for c := 0 to (BoldElement.Value as TColList).Count-1 do
      begin
        Col := ( BoldElement.Value as TColList)[c];
        cur := BudgetProducer.AddDomElementForBoldElement(CurrentElement, Col, '', 'COL', []);
        cur.setAttribute('aName', col.aName);
        cur.setAttribute('aNumber', col.aNumber);
      end;
      {rows}
      CurrentElement := BudgetProducer.AddDomElement(RootElement, 'ROWS');
      Budget.EvaluateExpression('row->OrderBy(aNumber)', BoldElement);
      for r := 0 to (BoldElement.Value as TRowList).Count-1 do
      begin
        Row := (BoldElement.Value as TRowList)[r];
        cur := BudgetProducer.AddDomElementForBoldElement(CurrentElement, Row, '', 'ROW', []);
        cur.setAttribute('aName', row.aName);
        cur.setAttribute('aNumber', row.aNumber);
        AddRow(Budget, Row, cur);
      end;
    finally
      FreeAndNil(BoldElement);
    end;
  end;
end;

procedure TdmXmlProducer.AddRow(Budget: TBudget; Row: TRow; parentElement: IXMLDomElement);
var
  BoldElement: TBoldIndirectElement;
  c: integer;
  Cell: TACell;
  Col: TCol;
begin
  BoldElement := TBoldIndirectElement.Create;
  try
    Budget.EvaluateExpression('col->OrderBy(aNumber)', BoldElement);
    for c := 0 to (BoldElement.Value as TColList).Count-1 do
    begin
      Col := ( BoldElement.Value as TColList)[c];
      Cell := Budget.ACell[Col.aNumber, Row.aNumber];
      if Assigned(Cell) then
      begin
        BudgetProducer.AddDomElementForBoldElement(ParentElement, Cell, 'CELL', 'CELL');
      end;
    end;
  finally
    FreeAndNil(BoldElement);
  end;
end;

procedure TdmXmlProducer.MainDispatcherActions1Action(
  const request: TBoldXMLRequest; out response: String);
var
  i: integer;
begin
  for i:= 0 to request.IdentifiedValues.Count - 1 do
  begin
    try
      if Assigned(BoldManipulator1.ElementForIdString(request.IdentifiedValues.Names[i])) then
        BoldManipulator1.SetValue(request.IdentifiedValues.Names[i], request.IdentifiedValues.Values[request.IdentifiedValues.Names[i]]);
    except
    end;          
  end;
  dmMain.BoldSystemHandle1.UpdateDatabase;
  response := BudgetProducer.getDocumentAsString(request.Params);
end;

function TdmXmlProducer.BoldManipulator1Mappers0Get(
  Element: TBoldElement): String;
begin
  Result := Format('%s', [(Element as TACell).aValue]);
end;

procedure TdmXmlProducer.BoldManipulator1Mappers0Set(Element: TBoldElement;
  const NewValue: String);
begin
  (Element as TACell).aValue := NewValue;
end;

procedure TdmXmlProducer.BoldXMLDispatcher1GetXMLRequest(const XML: String;
  out Request: TBoldXMLRequest);
begin
  Request := TBoldXMLRequest.CreateFromXML(XML);
end;

end.
