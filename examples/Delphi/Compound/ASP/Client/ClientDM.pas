unit ClientDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldWebConnection, BoldHandle, BoldPersistenceHandle,
  BoldHTTPClientPersistenceHandle, BoldHandles, BoldSystemHandle,
  BoldSubscription, ModelDM;

type
  TdmClient = class(TDataModule)
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldSystemHandle1: TBoldSystemHandle;
    BoldHTTPClientPersistenceHandle1: TBoldHTTPClientPersistenceHandle;
    BoldWebConnection1: TBoldWebConnection;
    BoldWebConnection2: TBoldWebConnection;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmClient: TdmClient;

implementation

{$R *.DFM}

end.
