
{ Global compiler directives }
{$include bold.inc}
unit BoldWinUtils;

interface

uses
  Windows;

function GetTaskBarHeigth: Integer;

implementation

uses
  SysUtils,
  BoldUtils;

function GetTaskBarHeigth: Integer;
var Parent, Child: HWND;
    ChildRect, ParentRect: TRect;
begin
  Parent := FindWindow('Progman', 'Program Manager');
  Child := FindWindowEx(Parent, 0, nil, nil);
  GetWindowRect(Parent, ParentRect);
  GetWindowRect(Child, ChildRect);
  Result := ParentRect.Bottom - ChildRect.Bottom;
end;

end.
