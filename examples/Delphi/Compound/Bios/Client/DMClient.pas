unit DMClient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldClientHandles, BoldComClientHandles, BoldHandlesCom,
  BoldSystemHandleCom, BoldSubscription, BoldHandle;

type
  TDMClientSystem = class(TDataModule)
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    SystemHandle: TBoldSystemHandleCom;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMClientSystem: TDMClientSystem;

implementation

{$R *.DFM}

end.
