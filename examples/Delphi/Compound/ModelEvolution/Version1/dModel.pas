unit dModel;

interface

uses
  Classes, 
  Controls, 
  Forms, 
  BoldSubscription, 
  BoldModel, 
  BoldHandle, 
  BoldUMLModelLink,
  BoldUMLRose98Link, BoldAbstractModel, BoldUMLMMLink;

type
  TdmModel = class(TDataModule)
    Model: TBoldModel;
    BoldUMLMMLink1: TBoldUMLMMLink;
    BoldUMLRoseLink1: TBoldUMLRoseLink;
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
