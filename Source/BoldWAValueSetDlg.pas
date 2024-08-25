
{ Global compiler directives }
{$include bold.inc}
unit BoldWAValueSetDlg;

interface

uses
  Windows,
  Classes,
  Graphics,
  Forms,
  Controls,
  StdCtrls,
  Buttons,
  ExtCtrls,
  comctrls;

type
  TValueSetDlg = class(TForm)
    pnRight: TPanel;
    lbValueName: TLabel;
    lbRepresentations: TLabel;
    edValueName: TEdit;
    Memo1: TMemo;
    Panel1: TPanel;
    OkBtn: TButton;
    CancelBtn: TButton;
    procedure CancelBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure edValueNameChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    fOk: Boolean;
    function IsValidInput: Boolean;
    procedure EnableControls(const Enable: Boolean);
    procedure ClearEditFields;
  public
    { Public declarations }
    function Edit(const Old_Name, Old_Reps: string;var New_Name, New_Reps: string): Boolean;
  end;

var
  ValueSetDlg: TValueSetDlg;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldVclUtils,
  dialogs;

{$R *.dfm}

function TValueSetDlg.IsValidInput: Boolean;
begin
  Result := not((IsEmptyStr(Trim(edValueName.Text)) or IsEmptyStr(Trim(Memo1.Lines.Text))))
end;

procedure TValueSetDlg.EnableControls(const Enable: Boolean);
begin
  if Enable then
  begin
    edValueName.Enabled := true;
    lbValueName.Enabled := true;
    lbRepresentations.Enabled := true;
    Memo1.Enabled := true;
  end
  else
  begin
    edValueName.Enabled := false;
    lbValueName.Enabled := false;
    lbRepresentations.Enabled := false;
    Memo1.Enabled := false;
    OkBtn.Enabled := false;
  end;
end;

procedure TValueSetDlg.ClearEditFields;
var
  func: TNotifyEvent;
begin
  func := edValueName.OnChange;
  edValueName.OnChange := nil;
  edValueName.Clear;
  edValueName.OnChange := func;
  func := memo1.OnChange;
  memo1.OnChange := nil;
  memo1.Lines.clear;
  memo1.OnChange := func;
end;

procedure TValueSetDlg.CancelBtnClick(Sender: TObject);
begin
  fOk := false;
  Close;
end;

procedure TValueSetDlg.OkBtnClick(Sender: TObject);
begin
  fOk := true;
  Close;
end;

function TValueSetDlg.Edit(const Old_Name, Old_Reps: string;var New_Name, New_Reps: string): Boolean;
var
  func: TNotifyEvent;
  i: integer;
  tempList: TStringList;
begin
  EnableControls(true);
  ClearEditFields;
  func := edValueName.OnChange;
  edValueName.OnChange := nil;
  edValueName.Text := Old_Name;
  edValuename.OnChange := func;
  func := memo1.OnChange;
  Memo1.OnChange := nil;
  Memo1.Lines.CommaText := StringReplace(Old_Reps,'''','',[rfReplaceAll, rfIgnoreCase]);
  Memo1.OnChange := func;
  ShowModal;
  Result := (fOk);
  New_Name := Trim(edValueName.Text);
  tempList := TStringList.Create();
  for i:= 0 to Memo1.Lines.Count - 1 do
    if (length(Memo1.Lines[i]) <> 0) then
      tempList.Add(QuotedStr(Memo1.Lines[i]));
  New_Reps := tempList.CommaText ;
  FreeAndNil(tempList);
end;


procedure TValueSetDlg.edValueNameChange(Sender: TObject);
begin
  OkBtn.Enabled := IsValidInput;
end;

procedure TValueSetDlg.FormShow(Sender: TObject);
begin
  edValueName.SetFocus;
end;

procedure TValueSetDlg.FormCreate(Sender: TObject);
begin
  fOk := false;
end;

end.
