unit fStart;

interface

uses
  Classes,
  Controls,
  Forms,
  StdCtrls;

type
  TfrmStart = class(TForm)
    btnCancel: TButton;
    cmdCreate: TButton;
    cmdOpen: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStart: TfrmStart;

implementation

uses
  dMain;

{$R *.DFM}

end.
