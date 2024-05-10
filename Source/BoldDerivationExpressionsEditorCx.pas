unit BoldDerivationExpressionsEditorCx;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData,
  cxDataStorage, cxEdit, cxNavigator, dxDateRanges, dxScrollbarAnnotations,
  BoldSubscription, BoldHandles, BoldReferenceHandle, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridBoldSupportUnit, cxClasses,
  cxGridLevel, cxGrid, Vcl.StdCtrls, Vcl.ExtCtrls, BoldAbstractListHandle,
  BoldCursorHandle, BoldRootedHandles, BoldDerivedHandle,
  BoldElements,
  BoldUMLModel, cxButtonEdit, BoldControlPack, BoldVariantControlPack,
  System.ImageList, Vcl.ImgList, cxImageList, System.Actions, Vcl.ActnList,
  Vcl.ComCtrls, Vcl.ToolWin, cxBoldLookupComboBox;

type
  TfrmBoldDerivationExpressionsEditorCx = class(TForm)
    Panel1: TPanel;
    CancelBtn: TButton;
    OKBtn: TButton;
    lvlDerivationExpressions: TcxGridLevel;
    cxGridDerivationExpressions: TcxGrid;
    tvDerivationExpressions: TcxGridBoldTableView;
    brhTaggedValue: TBoldReferenceHandle;
    bdhTaggedValue: TBoldDerivedHandle;
    bchTaggedValue: TBoldCursorHandle;
    rElement: TBoldAsVariantRenderer;
    rExpression: TBoldAsVariantRenderer;
    colElement: TcxGridBoldColumn;
    colExpression: TcxGridBoldColumn;
    cxImageList1: TcxImageList;
    ActionList1: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    ToolBar1: TToolBar;
    btAdd: TToolButton;
    btRemove: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    bchElements: TBoldCursorHandle;
    bdhElements: TBoldDerivedHandle;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bdhTaggedValueDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    function rElementGetAsVariant(AFollower: TBoldFollower): Variant;
    function rExpressionGetAsVariant(AFollower: TBoldFollower): Variant;
    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
    procedure actMoveUpUpdate(Sender: TObject);
    procedure actMoveDownUpdate(Sender: TObject);
    procedure tvDerivationExpressionsColumn2PropertiesButtonClick(
      Sender: TObject; AButtonIndex: Integer);
    procedure bdhElementsDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure rExpressionSubscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
  private
    fTagStringList: TStringList;
    function GetUMLTaggedValue: TUMLTaggedValue;
    function GetTagStringList: TStringList;
    procedure StringListChanged(Sender: TObject);
  protected
    property TaggedValue: TUMLTaggedValue read GetUMLTaggedValue;
    property TagStringList: TStringList read GetTagStringList;
  public
    { Public declarations }
  end;

procedure ShowDerivationExpressionsEditor(DerivExprTV: TUMLTaggedValue);

implementation

uses
  BoldUMLModelDataModule,
  BoldSystem,
  BoldDerivationExpressionsSelectorCx,
  BoldUMLModelEditorHandlesDataModule,
  BoldUMLOCLEditor,
  BoldDefs,
  BoldQueue;

{$R *.dfm}

var
  frmBoldDerivationExpressionsEditor: TfrmBoldDerivationExpressionsEditorCx;

procedure ShowDerivationExpressionsEditor(DerivExprTV: TUMLTaggedValue);
begin
  frmBoldDerivationExpressionsEditor := TfrmBoldDerivationExpressionsEditorCx.Create(nil);
  with frmBoldDerivationExpressionsEditor do
  begin
    brhTaggedValue.Value := DerivExprTV as TBoldElement;
    TabSheet1.Caption := TaggedValue.modelElement.AsString;
    ShowModal;
  end;
end;

procedure TfrmBoldDerivationExpressionsEditorCx.actAddExecute(Sender: TObject);
var
  Selector: TfrmBoldDerivationExpressionsSelectorCx;
  Member: TUMLFeature;
  Ocl: string;
begin
  Selector := TfrmBoldDerivationExpressionsSelectorCx.Create(nil);
  Selector.brhMembers.value := bdhElements.Value;
  if Selector.ShowModal = mrOk then
  begin
    Member := (Selector.bchMembers.CurrentBoldObject as TUMLFeature);
    OCL := BoldUMLOclEditor_.EditOcl(
      dmBoldUMLModelEditorHandles.ModelHandle,
      TaggedValue.modelElement,
      '');
    TaggedValue.value := TaggedValue.value + BoldCRLF + Member.name + '=' + OCL;
  end;
end;

procedure TfrmBoldDerivationExpressionsEditorCx.actDeleteExecute(
  Sender: TObject);
begin
  fTagStringList.Delete(tvDerivationExpressions.CurrentIndex);
end;

procedure TfrmBoldDerivationExpressionsEditorCx.actDeleteUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := tvDerivationExpressions.CurrentIndex <> -1;
end;

procedure TfrmBoldDerivationExpressionsEditorCx.actMoveDownExecute(
  Sender: TObject);
begin
  fTagStringList.Move(tvDerivationExpressions.CurrentIndex, tvDerivationExpressions.CurrentIndex+1);
end;

procedure TfrmBoldDerivationExpressionsEditorCx.actMoveDownUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := tvDerivationExpressions.CurrentIndex < tvDerivationExpressions.DataController.RecordCount-1;
end;

procedure TfrmBoldDerivationExpressionsEditorCx.actMoveUpExecute(
  Sender: TObject);
begin
  fTagStringList.Move(tvDerivationExpressions.CurrentIndex, tvDerivationExpressions.CurrentIndex-1);
end;

procedure TfrmBoldDerivationExpressionsEditorCx.actMoveUpUpdate(
  Sender: TObject);
begin
  (Sender as TAction).Enabled := tvDerivationExpressions.CurrentIndex > 0;
end;

procedure TfrmBoldDerivationExpressionsEditorCx.bdhElementsDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
var
  vClass: TUMLClassifier;
const
  cOcl = 'allFeature->filterOnType(UMLAttribute)->union(superclasses.associationEnd)->select(derived and (taggedValue[''Bold.DerivationOCL''].value <> ''''))';
begin
  vClass := TaggedValue.modelElement as TUMLClassifier;
  ResultElement.SetOwnedValue(vClass.EvaluateExpressionAsNewElement(cOcl));
end;

procedure TfrmBoldDerivationExpressionsEditorCx.bdhTaggedValueDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
var
  BoldMemberList: TBoldMemberList;
  i: integer;
begin
  with TaggedValue.BoldSystem.BoldSystemTypeInfo do
    BoldMemberList := TBoldMemberList.CreateWithTypeInfo(ListTypeInfoByElement[AttributeTypeInfoByExpressionName['String']]);
  ResultElement.SetOwnedValue(BoldMemberList);
  for i := 0 to TagStringList.Count-1 do
    BoldMemberList.AddNew.AsString := fTagStringList[i];
  TaggedValue.M_value.DefaultSubscribe(Subscriber);
end;

procedure TfrmBoldDerivationExpressionsEditorCx.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

function TfrmBoldDerivationExpressionsEditorCx.GetTagStringList: TStringList;
begin
  if not Assigned(fTagStringList) then
    fTagStringList := TStringList.Create;
  result := fTagStringList;
  fTagStringList.OnChange := nil;
  result.Text := TaggedValue.value;
  fTagStringList.OnChange := StringListChanged;
end;

function TfrmBoldDerivationExpressionsEditorCx.GetUMLTaggedValue: TUMLTaggedValue;
begin
  result := brhTaggedValue.Value as TUMLTaggedValue;
end;

function TfrmBoldDerivationExpressionsEditorCx.rElementGetAsVariant(
  AFollower: TBoldFollower): Variant;
var
  i: integer;
  s: string;
begin
  s := AFollower.Value.AsString;
  i := Pos('=', s);
  result := Copy(s, 1, i-1);
end;

function TfrmBoldDerivationExpressionsEditorCx.rExpressionGetAsVariant(
  AFollower: TBoldFollower): Variant;
var
  i: integer;
  s: string;
begin
  s := AFollower.Value.AsString;
  i := Pos('=', s);
  result := Copy(s, i+1, maxInt);
end;

procedure TfrmBoldDerivationExpressionsEditorCx.rExpressionSubscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  brhTaggedValue.Value.SubscribeToExpression('value', Subscriber);
end;

procedure TfrmBoldDerivationExpressionsEditorCx.StringListChanged(
  Sender: TObject);
begin
  TaggedValue.value := fTagStringList.Text;
end;

procedure TfrmBoldDerivationExpressionsEditorCx.tvDerivationExpressionsColumn2PropertiesButtonClick(
  Sender: TObject; AButtonIndex: Integer);
var
  vClass: TUMLClass;
  Ocl: string;
  s: string;
begin
  vClass := TaggedValue.modelElement as TUMLClass;
  ocl := tvDerivationExpressions.DataController.Values[tvDerivationExpressions.CurrentIndex, colExpression.Index];
  s := BoldUMLOclEditor_.EditOcl(
    BoldUMLModelEditorHandlesDataModule.dmBoldUMLModelEditorHandles.ModelHandle,
    TaggedValue.modelElement,
    Ocl
    );
  if Trim(ocl) <> trim(s) then
    fTagStringList[tvDerivationExpressions.CurrentIndex] := s;
end;

end.
