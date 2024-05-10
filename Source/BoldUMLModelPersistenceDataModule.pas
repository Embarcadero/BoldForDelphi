unit BoldUMLModelPersistenceDataModule;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait,
  FireDAC.VCLUI.Login, FireDAC.Comp.UI, Data.DB, FireDAC.Comp.Client,
  BoldAbstractDatabaseAdapter, BoldDatabaseAdapterFireDAC,
  BoldPersistenceHandleFile, BoldPersistenceHandleFileXML, BoldSubscription,
  BoldHandle, BoldPersistenceHandle, BoldAbstractPersistenceHandleDB,
  BoldPersistenceHandleDB;

type
  TdmBoldUMLModelPersistence = class(TDataModule)
    BoldPersistenceHandleDB1: TBoldPersistenceHandleDB;
    BoldPersistenceHandleFileXML1: TBoldPersistenceHandleFileXML;
    BoldDatabaseAdapterFireDAC1: TBoldDatabaseAdapterFireDAC;
    FDConnection1: TFDConnection;
    FDManager1: TFDManager;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmBoldUMLModelPersistence: TdmBoldUMLModelPersistence;

implementation

uses
  FireDAC.VCLUI.ConnEdit,
  FireDAC.Phys.MSSQL,
  FireDAC.Phys.MSSQLCli,
  FireDAC.Phys.MSSQLDef,
  FireDAC.Phys.MySQL,
  FireDAC.Phys.PG,
  FireDAC.Phys.SQLite,
  FireDAC.Phys.FB,
  FireDAC.Phys.IB,
  FireDAC.Phys.MongoDB,
  FireDAC.Phys.DB2;

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
