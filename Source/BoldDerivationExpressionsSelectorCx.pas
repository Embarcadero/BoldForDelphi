unit BoldDerivationExpressionsSelectorCx;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxCustomListBox, cxListBox,
  cxBoldEditors, Vcl.StdCtrls, Vcl.ExtCtrls, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxNavigator, dxDateRanges, dxScrollbarAnnotations,
  cxTextEdit, cxGridCustomTableView, cxGridTableView, cxGridBoldSupportUnit,
  cxGridCustomView, cxClasses, cxGridLevel, cxGrid, BoldReferenceHandle,
  BoldSubscription, BoldHandles, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle;

type
  TfrmBoldDerivationExpressionsSelectorCx = class(TForm)
    Panel1: TPanel;
    CancelBtn: TButton;
    OKBtn: TButton;
    bchMembers: TBoldCursorHandle;
    brhMembers: TBoldReferenceHandle;
    cxGrid1Level1: TcxGridLevel;
    cxGrid1: TcxGrid;
    cxGrid1BoldTableView1: TcxGridBoldTableView;
    cxGrid1BoldTableView1Column1: TcxGridBoldColumn;
    cxGrid1BoldTableView1Column2: TcxGridBoldColumn;
    cxGrid1BoldTableView1Column3: TcxGridBoldColumn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cxGrid1BoldTableView1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  BoldUMLModelDataModule;

{$R *.dfm}

procedure TfrmBoldDerivationExpressionsSelectorCx.cxGrid1BoldTableView1DblClick(
  Sender: TObject);
begin
  if cxGrid1BoldTableView1.CurrentIndex <> -1 then
    ModalResult := mrOk;
end;

procedure TfrmBoldDerivationExpressionsSelectorCx.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
