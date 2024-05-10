unit dmModel1;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  BoldHandles,
  BoldSubscription,
  BoldModel,
  BoldHandle,
  BoldUMLModelLink,
  BoldUMLRose98Link, BoldAbstractModel;

type
  Tdm_Model1 = class(TDataModule)
    BoldModel1: TBoldModel;
    BoldSystemTypeInfoHandle1: TBoldSystemTypeInfoHandle;
    BoldUMLRose98Link1: TBoldUMLRoseLink;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dm_Model1: Tdm_Model1 = nil;

procedure Ensuredm_Model;
procedure Freedm_Model;

implementation
{$R *.DFM}

procedure Ensuredm_Model;
begin
  // Note: This may be a memory leak - who frees the dm_Model?
  // It was not quite OK to free it in finalization
  if not Assigned(dm_Model1) then
    dm_Model1 := tdm_Model1.Create(Application);
end;

procedure Freedm_Model;
begin
  dm_model1.Free;
end;


end.

