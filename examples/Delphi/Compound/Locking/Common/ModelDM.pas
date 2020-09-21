unit ModelDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldSubscription, BoldAbstractModel, BoldModel;

type
  TdmModel = class(TDataModule)
    BoldModel1: TBoldModel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmModel: TdmModel;

implementation

{$R *.DFM}

end.
