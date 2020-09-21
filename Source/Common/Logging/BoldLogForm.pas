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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuAddSeparatorClick(Sender: TObject);
    procedure mnuClearLogClick(Sender: TObject);
  private
    { Private declarations }
    fLogLines: TStringList;
    function ShowAllLines: boolean;
  public
    { Public declarations }
    procedure AddLog(const s: string);
    procedure Clear;
    procedure UpdateView;
    procedure SaveLog;
    property LogLines: TStringList read fLogLines;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldCommonConst;

{$R *.dfm}

procedure TBoldLogForm.AddLog(const s: string);
begin
  fLogLines.Add(s);
  try
    UpdateView;
  except on E:Exception do
    if MessageDlg(Format(sClearAndContinue, [E.Message]),
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
  if visible then
  begin
    Log.Lines.BeginUpdate;
    try
      Log.Lines.Clear;
      if ShowAllLines then
        Log.Lines.Assign(fLogLines)
      else
        for i := MaxIntValue([0, fLogLines.Count - 50]) to fLogLines.Count - 1 do
          Log.Lines.Add(fLogLines[i]);
    finally
      Log.Lines.EndUpdate;
    end;
  end;
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

procedure TBoldLogForm.FormCreate(Sender: TObject);
begin
  fLogLines := TStringList.Create;
end;

procedure TBoldLogForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fLogLines);
end;

procedure TBoldLogForm.SaveLog;
begin
  with TSaveDialog.Create(nil) do
  try
    DefaultExt := 'log'; // do not localize
    Filter := Format('%s (*.log)|*.log|%s (*.txt)|*.txt|%s (*.*)|*.*', [sLogFiles, sTextFiles, sAllFiles]); // do not localize
    Options := [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofNoReadOnlyReturn];
    Title := sSaveLogAs;
    if Execute then
      LogLines.SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure TBoldLogForm.mnuAddSeparatorClick(Sender: TObject);
begin
  BoldLog.Separator;
end;

procedure TBoldLogForm.mnuClearLogClick(Sender: TObject);
begin
  fLogLines.Clear;
end;

end.
