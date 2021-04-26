
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
  ImgList;

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
    mnuShowAll: TMenuItem;
    ImageList1: TImageList;
    mnuClearLog: TMenuItem;
    mnuAddSeparator: TMenuItem;
    procedure cmdSaveClick(Sender: TObject);
    procedure cmdCloseClick(Sender: TObject);
    procedure mnuCopyClick(Sender: TObject);
    procedure mnuShowAllClick(Sender: TObject);
    procedure mnuAddSeparatorClick(Sender: TObject);
    procedure mnuClearLogClick(Sender: TObject);
  private
    { Private declarations }
    function ShowAllLines: boolean;
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
  cMaxLines = 10000;
begin
  while LogLines.Count > cMaxLines do
    LogLines.Delete(LogLines.Count-1);

  LogLines.Add(s);
  try
    UpdateView;
  except on E:Exception do
    if MessageDlg(Format('Error: "%s". Clear log and continue?', [E.Message]),
                  mtWarning, [mbYes, mbNo], 0) = mrYes then
      Log.Lines.Clear
    else
      raise;
  end;
end;

procedure TBoldLogForm.Clear;
begin
  UpdateView;
end;

procedure TBoldLogForm.cmdSaveClick(Sender: TObject);
begin
  SaveLog;
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
var
  i: integer;
begin
  if not Visible then
    exit;
  Log.SetFocus;
  Log.SelStart := Log.GetTextLen;
  Log.Perform(EM_SCROLLCARET, 0, 0);
end;

procedure TBoldLogForm.mnuShowAllClick(Sender: TObject);
begin
  mnuShowAll.Checked := not mnuShowAll.Checked;
  UpdateView;
end;

function TBoldLogForm.ShowAllLines: boolean;
begin
  Result := mnuShowAll.Checked;
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
  LogLines.Clear;
  UpdateView;
end;

initialization

end.
