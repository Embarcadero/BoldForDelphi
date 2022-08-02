
{ Global compiler directives }
{$include bold.inc}
unit BoldOLLEDmmain;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  Boldhandles, BoldSystemHandle,
  BoldSubscription, BoldModel, BoldTypeNameHandle, BoldHandle,
  BoldPersistenceHandle, BoldPersistenceHandleDB,
  BoldAbstractModel, BoldRootedHandles, BoldExpressionHandle;

type
  TdmOLL = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldObjectInfoSystem: TBoldSystemHandle;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmOLL: TdmOLL;

implementation

uses
  SysUtils,
  BoldUtils;

{$R *.DFM}

end.
