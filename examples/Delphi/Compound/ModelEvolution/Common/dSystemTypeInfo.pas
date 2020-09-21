unit dSystemTypeInfo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  BoldSubscription, BoldHandles;

type
  TdmSystemTypeInfo = class(TDataModule)
    SystemTypeInfo: TBoldSystemTypeInfoHandle;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dmSystemTypeInfo: TdmSystemTypeInfo;

implementation

uses dModel, ModelEvClasses;

{$R *.DFM}

end.
