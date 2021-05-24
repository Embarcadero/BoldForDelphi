
{ Global compiler directives }
{$include bold.inc}
unit BoldDerivationExpressionsEditor;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  BoldMemo,
  BoldHandles,
  BoldReferenceHandle,
  BoldUMLModel,
  BoldSubscription;

type
  TfrmBoldDerivationExpressionsEditor = class(TForm)
    pnMain: TPanel;
    pnButtons: TPanel;
    btClose: TButton;
    BoldMemo1: TBoldMemo;
    brhTaggedValue: TBoldReferenceHandle;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

procedure ShowDerivationExpressionsEditor(DerivExprTV: TUMLTaggedValue);

implementation

uses
  BoldElements;

{$R *.dfm}

var
  frmBoldDerivationExpressionsEditor: TfrmBoldDerivationExpressionsEditor;

procedure ShowDerivationExpressionsEditor(DerivExprTV: TUMLTaggedValue);
begin
  frmBoldDerivationExpressionsEditor := TfrmBoldDerivationExpressionsEditor.Create(nil);
  frmBoldDerivationExpressionsEditor.brhTaggedValue.Value := DerivExprTV as TBoldElement;
  frmBoldDerivationExpressionsEditor.ShowModal;
end;

procedure TfrmBoldDerivationExpressionsEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

end.
