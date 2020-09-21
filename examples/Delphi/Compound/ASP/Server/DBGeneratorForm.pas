unit DBGeneratorForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, IBDatabase, BoldHandle, BoldPersistenceHandle,
  BoldAbstractPersistenceHandleDB, BoldPersistenceHandleDB,
  BoldSubscription, BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  StdCtrls;

type
  TfrmDBGen = class(TForm)
    Button1: TButton;
    BoldDatabaseAdapterIB1: TBoldDatabaseAdapterIB;
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
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

uses ModelDM;

{$R *.dfm}

procedure TfrmDBGen.Button1Click(Sender: TObject);
begin
  BoldDatabaseAdapterIB1.CreateInterbaseDatabase;
  BoldPersistenceHandleDB1.CreateDataBaseSchema;
  showmessage('Database generation successfull!');
end;

end.
