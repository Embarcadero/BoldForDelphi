
{ Global compiler directives }
{$include bold.inc}
unit BoldLogForm;

interface

uses
  Messages,
  Windows,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  Menus,
  BoldLogHandler,
  BoldMath,
  ImgList,
  System.ImageList;

type
  TBoldLogForm = class(TForm)
    Log: TRichEdit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    SaveLog1: TMenuItem;
    N1: TMenuItem;
    Edit1: TMenuItem;
    mnuCopy: TMenuItem;
    mnuCloseLogForm: TMenuItem;
    Panel1: TPanel;
    ProgressBar1: TProgressBar;
    ImageList1: TImageList;
    mnuClearLog: TMenuItem;
    mnuAddSeparator: TMenuItem;
    procedure cmdSaveClick(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuAddSeparatorClick(Sender: TObject);
    procedure mnuClearLogClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    fLogStrings: TStrings;
    function GetLogLines: TStrings;
  public
    { Public declarations }
    procedure AddLog(const s: string);
    procedure Clear;
    procedure UpdateView;
    procedure SaveLog;
    property LogLines: TStrings read GetLogLines;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.dfm}

procedure TBoldLogForm.AddLog(const s: string);
const
  cMaxLines = 20000;
var
  Strings: TStrings;
begin
  if visible then
    Strings := LogLines
  else
    Strings := fLogStrings;
  while Strings.Count > cMaxLines do
    Strings.Delete(0);
  Strings.Add(s);
    UpdateView;
end;

procedure TBoldLogForm.Clear;
begin
  UpdateView;
end;

procedure TBoldLogForm.cmdSaveClick(Sender: TObject);
begin
  SaveLog;
end;

procedure TBoldLogForm.FormCreate(Sender: TObject);
begin
  fLogStrings := TStringList.Create;
end;

procedure TBoldLogForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fLogStrings);
end;

procedure TBoldLogForm.FormShow(Sender: TObject);
begin
  LogLines.AddStrings(fLogStrings);
  fLogStrings.clear;
end;

procedure TBoldLogForm.cmdCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TBoldLogForm.mnuCopyClick(Sender: TObject);
begin
  SendMessage(ActiveControl.Handle, WM_COPY, 0, 0);
end;

procedure TBoldLogForm.UpdateView;
begin
  if not Visible then
    exit;
  Log.SetFocus;
  Log.SelStart := Log.GetTextLen;
  Log.Perform(EM_SCROLLCARET, 0, 0);
end;

function TBoldLogForm.GetLogLines: TStrings;
begin
  result := Log.Lines;
end;

procedure TBoldLogForm.SaveLog;
begin
  with TSaveDialog.Create(nil) do
  try
    DefaultExt := 'log';
    Filter := 'Log files (*.log)|*.log|Text files (*.txt)|*.txt|All files (*.*)|*.*';
    Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn];
    Title := 'Save Log As';
    if Execute then
      LogLines.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TBoldLogForm.mnuAddSeparatorClick(Sender: TObject);
begin
  BoldLog.Separator;
  UpdateView;
end;

procedure TBoldLogForm.mnuClearLogClick(Sender: TObject);
begin
  fLogStrings.Clear;
  LogLines.Clear;
  UpdateView;
end;

end.