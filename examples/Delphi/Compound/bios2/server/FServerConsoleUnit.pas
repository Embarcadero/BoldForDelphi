unit FServerConsoleUnit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  dmProduct,
  StdCtrls,
  BoldLabel,
  ExtCtrls,
  BoldNavigator,
  Grids,
  BoldGrid,
  BoldActions,
  BoldDBActions,
  ActnList,
  BoldHandleAction,
  BoldNavigatorDefs, BoldIBDatabaseAction;

type
  TForm1 = class(TForm)
    BoldGrid1: TBoldGrid;
    BoldNavigator1: TBoldNavigator;
    Label1: TLabel;
    BoldLabel1: TBoldLabel;
    ActionList1: TActionList;
    BoldActivateSystemAction1: TBoldActivateSystemAction;
    BoldUpdateDBAction1: TBoldUpdateDBAction;
    cmdSave: TButton;
    cmdCreateDB: TButton;
    cmdOpenSystem: TButton;
    BoldIBDatabaseAction1: TBoldIBDatabaseAction;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

end.
