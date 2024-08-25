
/////////////////////////////////////////////////////////
//                                                     //
//              Bold for Delphi                        //
//    Copyright (c) 2002 BoldSoft AB, Sweden           //
//                                                     //
/////////////////////////////////////////////////////////

{ Global compiler directives }
{$include bold.inc}
unit BoldVersionInfo;

interface

function BoldBuildVersionNumber: string;
function BoldBuildVersionNumberMajor: integer;
function BoldBuildVersionNumberMinor: integer;
function BoldBuildVersionNumberRelease: integer;
function BoldBuildVersionNumberBuild: integer;

function BoldBuildDate: TDateTime;
function BoldBuildTime: TDateTime;

function BoldBuildTarget: string;
function BoldBuildComment: string;
function BoldBuildCopyright: string;

implementation

uses
  BoldIsoDateTime,
  SysUtils;

const
  BOLD_BUILD_VERSION_NUMBER='4.0.0.19';
  BOLD_BUILD_VERSION_NUMBER_MAJOR='4';
  BOLD_BUILD_VERSION_NUMBER_MINOR='0';
  BOLD_BUILD_VERSION_NUMBER_RELEASE='0';
  BOLD_BUILD_VERSION_NUMBER_BUILD='19';

  BOLD_BUILD_DATE='2002-10-03';
  BOLD_BUILD_TIME='14:14:07';
  BOLD_BUILD_TARGET='D6';

  BOLD_BUILD_COMMENT='Release build.';

  BOLD_BUILD_COPYRIGHT='Copyright (c) 1996-2002 BoldSoft MDE AB, Sweden';

function BoldBuildVersionNumber: string;
begin
  if pos('$', BOLD_BUILD_VERSION_NUMBER_BUILD) = 0 then
    result := BOLD_BUILD_VERSION_NUMBER_BUILD
  else
    result := '0.0.0.0';
end;

function BoldBuildVersionNumberMajor: integer;
begin
  result := StrToIntDef(BOLD_BUILD_VERSION_NUMBER_MAJOR, 0)
end;

function BoldBuildVersionNumberMinor: integer;
begin
  result := StrToIntDef(BOLD_BUILD_VERSION_NUMBER_MINOR, 0)
end;

function BoldBuildVersionNumberRelease: integer;
begin
  result := StrToIntDef(BOLD_BUILD_VERSION_NUMBER_RELEASE, 0)
end;

function BoldBuildVersionNumberBuild: integer;
begin
  result := StrToIntDef(BOLD_BUILD_VERSION_NUMBER_BUILD, 0)
end;

function BoldBuildDate: TDateTime;
begin
  if pos('$', BOLD_BUILD_DATE) = 0 then
    result := ParseISODate(BOLD_BUILD_DATE)
  else
    result := 0;
end;

function BoldBuildTime: TDateTime;
begin
  if pos('$', BOLD_BUILD_TIME) = 0 then
    result := ParseISOTime(BOLD_BUILD_TIME)
  else
    result := 0;
end;

function BoldBuildTarget: string;
begin
  if pos('$', BOLD_BUILD_TARGET) = 0 then
    result := BOLD_BUILD_TARGET
  else
  begin
    {$IFDEF BOLD_DELPHI6}
    result := 'D6';
    {$ENDIF}
    {$IFDEF BOLD_DELPHI7}
    result := 'D7';
    {$ENDIF}
    {$IFDEF BOLD_DELPHI8}
    result := 'D8';
    {$ENDIF}
  end;
end;

function BoldBuildComment: string;
begin
  result := BOLD_BUILD_COMMENT;
end;

function BoldBuildCopyright: string;
begin
  result := BOLD_BUILD_COPYRIGHT;
end;

end.
