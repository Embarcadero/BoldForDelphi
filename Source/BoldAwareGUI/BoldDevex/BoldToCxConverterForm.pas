unit BoldToCxConverterForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, cxDropDownEdit, Menus, cxContainer, cxCheckBox,
  StdCtrls, cxButtons, cxGridCustomTableView, cxGridTableView, cxGridCustomView,
  cxClasses, cxGridLevel, cxGrid, cxNavigator, cxStyles, ExtCtrls,
  cxTextEdit, cxMemo, dxDateRanges, dxScrollbarAnnotations;

type
  TfrmBoldToCxConverter = class(TForm)
    DetectedComponentsGrid: TcxGrid;
    DetectedComponentsGridLevel1: TcxGridLevel;
    tv: TcxGridTableView;
    tvColumn1: TcxGridColumn;
    tvColumn2: TcxGridColumn;
    tvColumn3: TcxGridColumn;
    tvColumn4: TcxGridColumn;
    cxConvert: TcxButton;
    cxCancelButton: TcxButton;
    cxRemoveAfterConvertionCheckbox: TcxCheckBox;
    Panel1: TPanel;
    cxMemoLog: TcxMemo;
    procedure cxCancelButtonClick(Sender: TObject);
    procedure tvSelectionChanged(Sender: TcxCustomGridTableView);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBoldToCxConverter: TfrmBoldToCxConverter;

implementation

{$R *.dfm}

procedure TfrmBoldToCxConverter.cxCancelButtonClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmBoldToCxConverter.tvSelectionChanged(
  Sender: TcxCustomGridTableView);
begin
  cxConvert.Enabled := tv.Controller.SelectedRecordCount > 0;
  cxConvert.Caption := Format('Convert %d Selected', [tv.Controller.SelectedRecordCount]);
end;

end.
