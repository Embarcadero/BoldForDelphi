unit dmSystem;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldSubscription, BoldHandle, BoldClientHandles, BoldComClientHandles,
  BoldHandlesCom, BoldSystemHandleCom;

type
  TDM = class(TDataModule)
    BoldSystemHandleCom1: TBoldSystemHandleCom;
    BoldComConnectionHandle1: TBoldComConnectionHandle;
    ClientObjectHandle: TBoldComClientObjectHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DM: TDM;

implementation

{$R *.DFM}

end.
