unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, HTTPApp, BoldSubscription,
  BoldHandle, BoldServerHandles, BoldComServerHandles,
  BoldSOAPServerPersistenceHandles,
  BoldHTTPServerPersistenceHandlePassthrough, BoldHandles,
  BoldSystemHandle, BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldAbstractModel, BoldModel,
  BoldAbstractPersistenceHandleDB, BoldPersistenceHandleDB_deprecated, DB,
  IBDatabase, BoldAbstractDatabaseAdapter, BoldDatabaseAdapterIB;

type
  TWebModule1 = class(TWebModule)
    httpPMapper: TBoldHTTPServerPersistenceHandlePassthrough;
    procedure WebModule1PMapperAction(Sender: TObject;
      Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
    procedure WebModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  WebModule1: TWebModule1;

implementation

uses
  dmCoreUnit,
  Forms
  , dmPersistenceUnit;

{$R *.DFM}

procedure TWebModule1.WebModule1PMapperAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
var
  reply: WideString;
begin
  httpPMapper.Get(Request.content, reply);
  Response.Content := Reply;
  Handled := true;
end;

procedure TWebModule1.WebModuleCreate(Sender: TObject);
begin
  if not Assigned(dmCore) then
    Application.CreateForm(TdmCore, dmCore);
  if not Assigned(dmPersistence) then
    Application.CreateForm(TdmPersistence, dmPersistence);
  dmPersistence.BoldPersistenceHandleDB1.Active := True;
end;

end.
