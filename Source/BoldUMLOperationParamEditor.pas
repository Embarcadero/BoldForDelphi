unit BoldUMLOperationParamEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, BoldSubscription, BoldHandles,
  BoldReferenceHandle, cxCheckBox, cxBoldEditors, cxMaskEdit, cxDropDownEdit,
  cxTextEdit, Vcl.StdCtrls, Vcl.ComCtrls, BoldRootedHandles,
  BoldAbstractListHandle, BoldCursorHandle, BoldListHandle, BoldElements,
  BoldControlPack, BoldVariantControlPack, BoldCheckBox,
  BoldCheckboxStateControlPack;

type
  TBoldUMLOperationParamEditForm = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    lblOperationName: TLabel;
    lblOperationOwnerScope: TLabel;
    lblOperationVisibility: TLabel;
    tbxOperationName: TcxBoldTextEdit;
    cmbTypeName: TcxBoldComboBox;
    cmbOperationKind: TcxBoldComboBox;
    brhParam: TBoldReferenceHandle;
    OKBtn: TButton;
    CancelBtn: TButton;
    blhParameterDirectionKind: TBoldListHandle;
    cbIsConst: TBoldCheckBox;
    risConst: TBoldAsCheckBoxStateRenderer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    function risConstGetAsCheckBoxState(
      aFollower: TBoldFollower): TCheckBoxState;
    procedure risConstSetAsCheckBoxState(
      aFollower: TBoldFollower; newValue: TCheckBoxState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  BoldUMLModelDataModule,
  BoldUMLModel,
  BoldDefaultTaggedValues,
  BoldUtils;

{$R *.dfm}

function TBoldUMLOperationParamEditForm.risConstGetAsCheckBoxState(
  aFollower: TBoldFollower): TCheckBoxState;
begin
  Result := cbGrayed;
  if Assigned(AFollower.Element) then
  begin
    if Assigned(aFollower.Value) then
    begin
      if TVIsTrue(aFollower.Value.AsString) then
        Result := cbChecked
      else
        Result := cbUnchecked;
    end;
  end;
end;

procedure TBoldUMLOperationParamEditForm.risConstSetAsCheckBoxState(
  aFollower: TBoldFollower; newValue: TCheckBoxState);
begin
  aFollower.Value.AsString := BooleanToString(NewValue = cbChecked);
end;

procedure TBoldUMLOperationParamEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
