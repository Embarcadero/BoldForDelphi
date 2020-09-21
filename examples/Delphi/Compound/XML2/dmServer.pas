unit dmServer;

interface

uses
  SysUtils,
  Classes,
  BoldElements,
  BoldManipulators,
  BoldComServerHandles,
  BoldHandle,
  BoldServerHandles,
  BoldXMLDispatcher,
  BoldSubscription,
  BoldXMLProducers,
  BoldStringList,
  BoldXMLExporter,
  BoldXMLRequests,
  MSXML_TLB, HTTPApp, HTTPProd;

type
  TServerDataModule = class(TDataModule)
    XMLDispatcher: TBoldXMLDispatcher;
    BoldComServerHandle1: TBoldComServerHandle;
    BoldManipulator1: TBoldManipulator;
    bxpFetch: TBoldXMLProducer;
    ppLastPage: TPageProducer;
    procedure DataModuleDestroy(Sender: TObject);
    procedure BoldXMLDispatcher1ActionsUpdateAction(
      const request: TBoldXMLRequest; out response: String);
    procedure BoldXMLDispatcher1ActionsEvaluateOCLAction(
      const request: TBoldXMLRequest; out response: String);
    procedure BoldXMLDispatcher1ActionsFetchAction(
      const request: TBoldXMLRequest; out response: String);
  private
    { Private declarations }
    fXMLExporter: TBoldXMLExporter;
  public
    { Public declarations }
    function XMLExporter: TBoldXMLExporter;
  end;

var
  ServerDataModule: TServerDataModule;

implementation

uses
  dialogs,
  Forms,
  MainDataModule,
  BoldSystem,
  BoldUtils,
  OrgChartClasses;

{$R *.dfm}

function XSLTransform(XMLDoc: IXMLDOMDocument; XSLFileName: string): string;
var
  XSLDoc :IXMLDOMDocument;
begin
  XSLDoc := CoDOMDocument.Create;
  Result := XMLDoc.xml;
  if FileExists(XSLFileName) then
  begin
    XSLDoc.load(XSLFileName);
    result := String(XMLDoc.transformNode(XSLDoc));
  end;
end;

function TServerDataModule.XMLExporter: TBoldXMLExporter;
begin
  if not Assigned(fXMLExporter) then
  begin
    fXMLExporter := TBoldXMLExporter.Create;
    fXMLExporter.BoldManipulator := BoldManipulator1;
    fXMLExporter.ExportLInks := True;
  end;
  Result := fXMLExporter;
end;

procedure TServerDataModule.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(fXMLExporter);
end;

procedure TServerDataModule.BoldXMLDispatcher1ActionsUpdateAction(
  const request: TBoldXMLRequest; out response: String);
var
  i: integer;
  ElementId, NewValue: string;
begin
  for i:= 0 to request.IdentifiedValues.Count - 1 do
  begin
    try
      ElementId := request.IdentifiedValues.Names[i];
      NewValue := request.IdentifiedValues.Values[ElementId];
      if Assigned(BoldManipulator1.ElementForIdString(ElementId)) then
        BoldManipulator1.SetValue(ElementId, NewValue);
    except
    end;
  end;
  try
    dmMain.OrgChartSystem.UpdateDatabase;
    response := ppLastPage.Content;
  except on E: Exception do
    response := 'An error has occured was saving to database: ' + E.Message;
  end;
end;

procedure TServerDataModule.BoldXMLDispatcher1ActionsEvaluateOCLAction(
  const request: TBoldXMLRequest; out response: String);
var
  XMLDoc: IXMLDomDocument;
  RootElement: IXMLDomElement;
  Elem: TBoldIndirectElement;
  oclExpr: string;
  p: string;
  ObjList: TBoldObjectList;
begin
  XMLDoc := CoDOMDocument.Create;
  RootElement := XMLDoc.createElement(dmMain.OrgChartSystem.System.BoldSystemTypeInfo.ModelName);
  XMLDoc.documentElement := RootElement;
  oclExpr := Request.Params.Values['OCL'];
  Elem := TBoldIndirectElement.Create;
  try
    dmMain.OrgChartSystem.System.EvaluateExpression(oclExpr, Elem, False);
    ObjList := Elem.Value as TBoldObjectList;
    if (ObjList is TPersonList) then
      XMLExporter.ExportObjects(Elem.Value as TBoldObjectList, RootElement, ['Company', 'Office', 'Address', 'Employment', 'Department', 'Description'])
    else if (Objlist is TCompanyList) then
      XMLExporter.ExportObjects(Elem.Value as TBoldObjectList, RootElement, ['Person', 'Office', 'Address', 'Employment', 'Department', 'Description']);
    //XSL transformation
    p := ExtractFilePath(Application.ExeName);
    if (ObjList.Count > 0) then
      response := XSLTransform(XMLDoc, Format('%s\XSL\%s.xslt', [p, ObjList.BoldObjects[0].BoldClassTypeInfo.ExpressionName]))
    else
      response := 'Search did not return any results';
  finally
    FreeAndNil(Elem);
  end;
end;

procedure TServerDataModule.BoldXMLDispatcher1ActionsFetchAction(
  const request: TBoldXMLRequest; out response: String);
var
  XMLDoc : IXMLDomDocument;
  RootElement: IXMLDomElement;
  Elem: TBoldElement;
  BoldId: string;
  Obj : TBoldObject;
  p: string;
begin
  XMLDoc := CoDOMDocument.Create;
  RootElement := XMLDoc.createElement(dmMain.OrgChartSystem.System.BoldSystemTypeInfo.ModelName);
  XMLDoc.documentElement := RootElement;
  BoldId := Request.Params.Values['BOLDID'];
  Elem := XMLExporter.BoldManipulator.ElementForIdString(BoldId);
  try
    Assert(Elem is TBoldObject);
    Obj := Elem as TBoldObject;
    if (Obj is TPerson) then
      XMLExporter.ExportObject(Obj, RootElement, ['Employment', 'Company', 'Office', 'Address', 'Department'])
    else if (Obj is TCompany) then
      XMLExporter.ExportObject(Obj, RootElement, ['Employment', 'Person', 'Office', 'Address', 'Department'])
    else if (Obj is TDepartment) then
      XMLExporter.ExportObject(Obj, RootElement, ['Employment', 'Person', 'Company', 'Address', 'Office'])  ;
    //XSL transformation
    p := ExtractFilePath(Application.ExeName);
    response := XSLTransform(XMLDoc, Format('%s\XSL\Edit%s.xslt', [p, obj.BoldClassTypeInfo.ExpressionName]));
  finally
  end;
end;

end.
