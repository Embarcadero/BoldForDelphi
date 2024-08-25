
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldBootIDE;

interface


implementation

uses
  Windows,
  Forms,
  Graphics,
  ExtCtrls,
  Classes;

{$R *.res}

procedure DrawOnImage(Image: TImage; ResourceName: String; Pos: TPoint; ReleaseTextPos: TPoint);
var
  aBitMap: TBitMap;
begin
  aBitMap := TBitmap.Create;
  aBitmap.LoadFromResourceName(HInstance, ResourceName);
  Image.Canvas.Draw(Pos.x, pos.y, aBitMap);
  with Image.Canvas do
  begin
    Brush.Style := bsClear;
    Font.Style := [fsBold];
    Font.Color := 153;
    TextOut(ReleaseTextPos.x, ReleaseTextPos.y, 'Release 3');
  end;
  aBitMap.Free;
end;

function GetSplashImage(FormName: String): TImage;
var
  j, i: integer;
  f: TForm;
begin
  result := nil;
  for i := 0 to Application.ComponentCount -1 do
  begin
    if (Application.Components[i] is TForm) and (Application.Components[i].Name = FormName) then
    begin
      f := Application.Components[i] as TForm;
      for j := 0 to f.ComponentCount-1 do
        if f.Components[j] is TImage then
          result := f.Components[j] as TImage;
    end;
  end;
end;


{$IFDEF BOLD_DELPHI}

{$IFDEF BOLD_DELPHI6}
procedure DrawOnD6StartupScreen;
var
  image: TImage;
begin
  Image := GetSplashImage('SplashScreen');
  if assigned(image) then
  begin
    DrawOnImage(Image, 'BOLDD6LOGO', Point(22, 27), Point(140, 47));
    Image.Canvas.Brush.Style := bsClear;
    Image.Canvas.Font.Style := [fsBold];
    Image.Canvas.Font.Color := 153;
    Image.Canvas.Font.Size := 36;
    Image.Canvas.TextOut(140, 70, 'for');
  end;
end;
{$ENDIF}
{$ENDIF}

initialization
  {$IFDEF BOLD_DELPHI}
  {$IFDEF BOLD_DELPHI6}
  DrawOnD6StartupScreen;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF BCB}
  {$ENDIF}
end.