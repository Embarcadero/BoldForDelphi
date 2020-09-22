unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, IBDatabase, BoldAFPPluggable, BoldUMLModelLink,
  BoldUMLRose98Link, BoldHandles, BoldAbstractModel, BoldModel,
  BoldSystemHandle, BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB,
  BoldSubscription, BoldHandle, BoldPersistenceHandle,
  BoldAbstractPersistenceHandleDB, BoldPersistenceHandleDB;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses DataMod;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  DataModule2.BoldDatabaseAdapterIB1.EnsureInterbaseDatabase;
  showmessage('OK');
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  DataModule2.BoldSystemHandle1.Active := true;
end;

end.
