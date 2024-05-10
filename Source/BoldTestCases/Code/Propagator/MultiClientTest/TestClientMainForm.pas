unit TestClientMainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, comserv, Grids, BoldGrid, BoldEdit, BoldSubscription,
  BoldHandles, BoldRootedHandles, BoldExpressionHandle;

type
  TForm1 = class(TForm)
    bgClassA: TBoldGrid;
    bgThing: TBoldGrid;
    bgSong: TBoldGrid;
    bghitList: TBoldGrid;
    behHitList: TBoldExpressionHandle;
    cbTesting: TCheckBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;
var
  Form1: TForm1;

implementation
uses
  dmClient,
  maanDataGen;
{$R *.DFM}


procedure TForm1.Button1Click(Sender: TObject);
begin
  TDm.Ensuredm('PropagatorTest');
  bgClassA.BoldHandle := dm.blhClassA;
  bgThing.BoldHandle := dm.blhThing;
  bgSong.BoldHandle := dm.blhSong;
  bghitList.BoldHandle := dm.blhhitList;
  dm.blhSong.Enabled := true;
  dm.blhhitList.Enabled := true;

end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TDm.Ensuredm('PropagatorTest');
  bgClassA.BoldHandle := dm.blhClassA;
  bgThing.BoldHandle := dm.blhThing;
  bgSong.BoldHandle := dm.blhSong;
  bghitList.BoldHandle := dm.blhhitList;
  dm.blhSong.Enabled := true;
  dm.blhhitList.Enabled := true;

  dm.GenerateRandomEvent(100);
end;

end.
