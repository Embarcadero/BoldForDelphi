unit BoldUMLQualifierEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxCustomListBox, cxCheckListBox,
  cxBoldEditors, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, BoldSubscription,
  BoldHandles, BoldReferenceHandle, BoldRootedHandles, BoldAbstractListHandle,
  BoldCursorHandle, BoldListHandle;

type
  TBoldUMLQualifierEditForm = class(TForm)
    Panel1: TPanel;
    CancelBtn: TButton;
    OKBtn: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    cxBoldSelectionCheckListBox1: TcxBoldSelectionCheckListBox;
    blhOtherEndAttributes: TBoldListHandle;
    brhAssociationEnd: TBoldReferenceHandle;
    blhQualifiers: TBoldListHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  BoldUMLModelDataModule;

{$R *.dfm}

end.
