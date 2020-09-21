unit MainDM;

interface

uses
  SysUtils, Classes, BoldHandles, BoldSubscription, BoldAbstractModel, forms,
  BoldModel;

type
  TDataModule1 = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DataModule1: TDataModule1;

implementation

{$R *.dfm}

end.
