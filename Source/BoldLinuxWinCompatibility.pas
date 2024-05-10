
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldLinuxWinCompatibility;

interface
{$IFDEF LINUX}
uses
  QControls;

type
  TWinControl = TWidgetControl;

{$ENDIF}
implementation
{$IFDEF LINUX}
{$ENDIF}
end.