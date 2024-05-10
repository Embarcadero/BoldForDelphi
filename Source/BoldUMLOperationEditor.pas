unit BoldUMLOperationEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, BoldCheckBox,
  BoldComboBox, BoldEdit, BoldLabel, Vcl.ExtCtrls, BoldSubscription,
  BoldHandles, BoldReferenceHandle, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxStyles, cxCustomData, cxFilter, cxData,
  cxDataStorage, cxEdit, cxNavigator, dxDateRanges, dxScrollbarAnnotations,
  Data.DB, cxDBData, cxTextEdit, cxDropDownEdit, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridBoldSupportUnit, cxClasses,
  cxGridCustomView, cxGridDBTableView, cxGrid, Vcl.ComCtrls,
  BoldPropertiesController, cxCheckBox, cxBoldLookupComboBox, cxContainer,
  cxBoldEditors, cxMaskEdit, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle, BoldListHandle, Vcl.ToolWin, System.Actions, Vcl.ActnList,
  System.ImageList, Vcl.ImgList, cxImageList;

type
  TBoldUMLOperationEditForm = class(TForm)
    lblOperationName: TLabel;
    lblOperationOwnerScope: TLabel;
    lblOperationDelphiFunctionType: TLabel;
    lblOperationStereotype: TLabel;
    lblOperationVisibility: TLabel;
    CancelBtn: TButton;
    OKBtn: TButton;
    brhOperation: TBoldReferenceHandle;
    pcOperationTabs: TPageControl;
    TabSheet5: TTabSheet;
    cxBoldGrid5: TcxGrid;
    cxBoldGrid5DBTableView1: TcxGridDBTableView;
    cxBoldGrid5BoldTableView: TcxGridBoldTableView;
    cxBoldGrid5BoldTableViewName: TcxGridBoldColumn;
    cxBoldGrid5BoldTableViewKind: TcxGridBoldColumn;
    cxBoldGrid5BoldTableViewType: TcxGridBoldColumn;
    cxBoldGrid5BoldTableViewExpressionname: TcxGridBoldColumn;
    cxBoldGrid5BoldTableViewIsconst: TcxGridBoldColumn;
    cxBoldGrid5Level1: TcxGridLevel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    BoldPropertiesController1: TBoldPropertiesController;
    cxBoldGrid5BoldTableViewConstraints: TcxGridBoldColumn;
    tbxOperationStereotype: TcxBoldTextEdit;
    tbxOperationName: TcxBoldTextEdit;
    cmbOwnerScope: TcxBoldComboBox;
    cmbOperationVisibility: TcxBoldComboBox;
    cmbDelphiFunctionType: TcxBoldComboBox;
    cbOperationOverrideInAllSubclasses: TcxBoldCheckBox;
    blhParameters: TBoldListHandle;
    ToolBar1: TToolBar;
    btAdd: TToolButton;
    btRemove: TToolButton;
    cxImageList1: TcxImageList;
    ActionList1: TActionList;
    actAdd: TAction;
    actDelete: TAction;
    actMoveUp: TAction;
    actMoveDown: TAction;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actDeleteUpdate(Sender: TObject);
    procedure actAddExecute(Sender: TObject);
    procedure actDeleteExecute(Sender: TObject);
    procedure cxBoldGrid5BoldTableViewDblClick(Sender: TObject);
    procedure actMoveUpExecute(Sender: TObject);
    procedure actMoveDownExecute(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  BoldUMLModelDataModule,
  BoldUMLOperationParamEditor,
  BoldUMLModel,
  BoldUtils,
  BoldUMLModelSupport;

{$R *.dfm}

procedure TBoldUMLOperationEditForm.actAddExecute(Sender: TObject);
var
  vParamEditForm: TBoldUMLOperationParamEditForm;
  vUMLParameter: TUMLParameter;
begin
  vParamEditForm := TBoldUMLOperationParamEditForm.Create(nil);
  vUMLParameter := blhParameters.MutableList.AddNew as TUMLParameter;
  TBoldUMLSupport.EnsureBoldTaggedValues(vUMLParameter);
  vUMLParameter.name := TBoldUMLSupport.UniqueName(vUMLParameter, 'NewParameter');
  vUMLParameter.typeName := 'String';
  blhParameters.CurrentBoldObject := vUMLParameter;
  vParamEditForm.brhParam.Value := vUMLParameter;
  if vParamEditForm.ShowModal = mrCancel then
    blhParameters.CurrentBoldObject.Delete;
end;

procedure TBoldUMLOperationEditForm.actDeleteExecute(Sender: TObject);
begin
  blhParameters.CurrentBoldObject.Delete;
end;

procedure TBoldUMLOperationEditForm.actDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := blhParameters.CurrentIndex <> -1;
end;

procedure TBoldUMLOperationEditForm.actMoveDownExecute(Sender: TObject);
begin
  blhParameters.MutableList.Move(blhParameters.CurrentIndex, blhParameters.CurrentIndex+1);
end;

procedure TBoldUMLOperationEditForm.actMoveUpExecute(Sender: TObject);
begin
  blhParameters.MutableList.Move(blhParameters.CurrentIndex, blhParameters.CurrentIndex-1);
end;

procedure TBoldUMLOperationEditForm.cxBoldGrid5BoldTableViewDblClick(
  Sender: TObject);
var
  vParamEditForm: TBoldUMLOperationParamEditForm;
  vUMLParameter: TUMLParameter;
  BlockName: string;
begin
  vParamEditForm := TBoldUMLOperationParamEditForm.Create(nil);
  vUMLParameter := blhParameters.CurrentBoldObject as TUMLParameter;
  vParamEditForm.brhParam.Value := vUMLParameter;
  BlockName := vUMLParameter.BoldSystem.UndoHandlerInterface.SetCheckPoint('Edit ' + vUMLParameter.AsString);
  if vParamEditForm.ShowModal = mrCancel then
  begin
    vUMLParameter.BoldSystem.UndoHandlerInterface.UndoBlock(BlockName);
    if vUMLParameter.BoldSystem.UndoHandlerInterface.RedoList.IndexOf(BlockName) <> -1 then
      vUMLParameter.BoldSystem.UndoHandlerInterface.RedoList.RemoveBlock(BlockName);
  end;
end;

procedure TBoldUMLOperationEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
