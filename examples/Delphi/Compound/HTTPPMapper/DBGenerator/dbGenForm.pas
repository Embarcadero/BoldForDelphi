unit dbGenForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmMainForm = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMainForm: TfrmMainForm;

implementation

uses dmCoreUnit, dmPersistenceUnit;

{$R *.DFM}

procedure TfrmMainForm.Button1Click(Sender: TObject);
begin
  dmPersistence.BoldDatabaseAdapterIB1.EnsureInterbaseDatabase;
  dmPersistence.BoldPersistenceHandleDB1.CreateDataBaseSchema;
  showmessage('OK');
end;

end.
