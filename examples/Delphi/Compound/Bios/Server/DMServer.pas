unit DMServer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldServerHandles, BoldComServerHandles, BoldComServerElementHandles,
  BoldSubscription, BoldHandle;

type
  TDataModule3 = class(TDataModule)
    BoldComServerHandle1: TBoldComServerHandle;
    SystemHandleExporter: TBoldComServerElementHandle;
    SystemExporter: TBoldComServerElementHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule3: TDataModule3;

implementation

uses DMCore;

{$R '..\core\BuildingsAndOwners.TLB'}

{$R *.DFM}

end.
