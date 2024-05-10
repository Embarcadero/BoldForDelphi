unit BoldQueryUserDlg;

interface

uses
  Windows,
  Messages,
  Controls,
  Forms,
  Dialogs,
  Graphics,
  StdCtrls,
  Classes;

type
  TBoldQueryResult = (qrYesAll, qrYes, qrNo, qrNoAll);
  TfrmBoldQueryUser = class(TForm)
    cmdYesAll: TButton;
    cmdYes: TButton;
    cmdNo: TButton;
    cmdNoAll: TButton;
    lblQuestion: TLabel;
    procedure cmdYesAllClick(Sender: TObject);
    procedure cmdYesClick(Sender: TObject);
    procedure cmdNoClick(Sender: TObject);
    procedure cmdNoAllClick(Sender: TObject);
  private
    { Private declarations }
    fQueryResult: TBoldQueryResult;
  public
    { Public declarations }
    property QueryResult: TBoldQueryResult read fQueryResult;
  end;

function QueryUser(const Title, Query: string): TBoldQueryResult;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils;

{$R *.dfm}

function QueryUser(const Title, Query: string): TBoldQueryResult;
begin
  with TfrmBoldQueryUser.Create(nil) do
  try
    if Title <> '' then
      Caption := Title;
    if Query <> '' then
      lblQuestion.Caption := Query
    else
      lblQuestion.Caption := sDefaultQuery;
    ShowModal;
    Result := QueryResult;
  finally
    Free;
  end;
end;

procedure TfrmBoldQueryUser.cmdYesAllClick(Sender: TObject);
begin
  fQueryResult := qrYesAll;
end;

procedure TfrmBoldQueryUser.cmdYesClick(Sender: TObject);
begin
  fQueryResult := qrYes;
end;

procedure TfrmBoldQueryUser.cmdNoClick(Sender: TObject);
begin
  fQueryResult := qrNo;
end;

procedure TfrmBoldQueryUser.cmdNoAllClick(Sender: TObject);
begin
  fQueryResult := qrNoAll;
end;

end.