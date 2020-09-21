unit dmCoreUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldSubscription, BoldAbstractModel, BoldModel;

type
  TdmCore = class(TDataModule)
    BoldModel1: TBoldModel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmCore: TdmCore;

implementation

{$R *.DFM}

end.
