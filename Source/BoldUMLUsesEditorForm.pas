
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLUsesEditorForm;

interface

uses
  Windows,
  Messages,
  Classes,
  Controls,
  Forms,
  Menus,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  BoldAttributes,
  BoldMemo,
  BoldHandles,
  BoldElements,
  BoldSubscription,
  BoldReferenceHandle, ImgList, System.ImageList;

type
  TfrmUsesEditor = class(TForm)
    pnButtons1: TPanel;
    StatusBar1: TStatusBar;
    pnText: TPanel;
    pnButtons2: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    behFormRoot: TBoldReferenceHandle;
    BoldMemo1: TBoldMemo;
    PopupMenu1: TPopupMenu;
    mnuCut: TMenuItem;
    mnuCopy: TMenuItem;
    mnuPaste: TMenuItem;
    ilCutCopyPaste: TImageList;
    procedure btnCancelClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnOKClick(Sender: TObject);
    procedure mnuCutClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuPasteClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    constructor CreateEditorWithParams(Title: String; Root: TBoldElement; Expression: String);
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldQueue;

{$R *.dfm}

procedure TfrmUsesEditor.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmUsesEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmUsesEditor.btnOKClick(Sender: TObject);
begin
  TBoldQueueable.ApplyAllMatching(Self);
  Close;
end;

constructor TfrmUsesEditor.CreateEditorWithParams(Title: String;
  Root: TBoldElement; Expression: String);
begin
  Create(nil);
  Caption := Title;
  behFormRoot.Value := Root;
  BoldMemo1.BoldProperties.Expression := Expression;
  Show;
end;

procedure TfrmUsesEditor.mnuCutClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    SendMessage(ActiveControl.handle, WM_CUT, 0, 0);
end;

procedure TfrmUsesEditor.mnuCopyClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    SendMessage(ActiveControl.handle, WM_COPY, 0, 0);
end;

procedure TfrmUsesEditor.mnuPasteClick(Sender: TObject);
begin
  if Assigned(ActiveControl) then
    SendMessage(ActiveControl.handle, WM_PASTE, 0, 0);
end;

end.
