
{ Global compiler directives }
{$include bold.inc}
unit BoldGettingStartedForm;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Graphics;

type
  TBoldSetBooleanValueProc = procedure(const vBool: Boolean) of object;

  TFGettingStarted = class(TForm)
    cbHideGettingStarted: TCheckBox;
    btnShow: TButton;
    btnClose: TButton;
    Image1: TImage;
    procedure btnShowClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
    FDisplayGettingStarted: TNotifyEvent;
    FSetGettingStartedFlag: TBoldSetBooleanValueProc;
  public
    { Public declarations }
    property DisplayGettingStarted: TNotifyEvent read FDisplayGettingStarted write FDisplayGettingStarted;
    property SetGettingStartedFlag: TBoldSetBooleanValueProc read fSetGettingStartedFlag write fSetGettingStartedFlag;
  end;

implementation


{$R *.DFM}

procedure TFGettingStarted.btnShowClick(Sender: TObject);
begin
  if Assigned(DisplayGettingStarted) then
    DisplayGettingStarted(Sender);
  Close;
end;

procedure TFGettingStarted.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 if Assigned(SetGettingStartedFlag) then
  SetGettingStartedFlag(not cbHideGettingStarted.Checked);
end;

procedure TFGettingStarted.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
