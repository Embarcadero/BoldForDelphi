unit FServerConsole;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  ActnList,
  BoldSubscription,
  BoldHandles,
  BoldRootedHandles,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldListHandle,
  BoldAFPPluggable,
  BoldListBox,
  BoldHandleAction,
  BoldActions,
  BoldDBActions, BoldIBDatabaseAction;

type
  TfrmServerConsole = class(TForm)
    btnShutDown: TButton;
    BoldListHandle1: TBoldListHandle;
    BoldListBox1: TBoldListBox;
    BoldPlaceableAFP1: TBoldPlaceableAFP;
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
    procedure btnShutDownClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmServerConsole: TfrmServerConsole;

implementation

uses
  DMCore;

{$R *.DFM}

procedure TfrmServerConsole.btnShutDownClick(Sender: TObject);
begin
  Close;
end;

end.
