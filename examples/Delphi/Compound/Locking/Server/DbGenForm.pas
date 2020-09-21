unit DbGenForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ModelDM,
  StdCtrls, BoldHandle, BoldPersistenceHandle,
  BoldPersistenceHandleDB, BoldSubscription,
  BoldHandles, BoldSystemHandle, DB, IBDatabase,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldAbstractPersistenceHandleDB;

type
  TfrmDBGen = class(TForm)
    Button1: TButton;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    IBDatabase1: TIBDatabase;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDBGen: TfrmDBGen;

implementation


{$R *.DFM}

procedure TfrmDBGen.Button1Click(Sender: TObject);
begin
  BoldDatabaseAdapterIB1.CreateInterbaseDatabase;
  BoldPersistenceHandleDB1.CreateDataBaseSchema;
  showmessage('OK');
end;

end.
