
{ Global compiler directives }
{$include bold.inc}
unit BoldWideStringInterface;

interface

uses
  { RTL/VCL }
  Windows,

  { Bold }
  BoldDefs,
  BoldStreams,
  BoldFreeStandingValues,
  BoldFreeStandingValueFactories,
  BoldValueInterfaces;

type
  { The following interface is used to retrieve and store values within the }
  { object space }
  IBoldWideStringContent = interface(IBoldNullableValue)
  ['{C5A9055A-A81A-4C10-BF02-9CF56500F123}']
    procedure SetContentAsWideString(NewValue: WideString);
    function GetContentAsWideString: WideString;
    property asWideString: WideString read GetContentAsWideString write SetContentAsWideString;
  end;

  { This class is the Bold freestanding valuespace implementation of the }
  { IBoldWideStringContent interace }
  TBFSWideString = class(TBoldFreeStandingNullableValue, IBoldWideStringContent)
  private
    fDataValue: WideString;
    procedure SetContentAsString(NewValue: String);
    function GetContentAsWideString: WideString;
    procedure SetContentAsWideString(NewValue: WideString);
  protected
    function GetStreamName: string; override;
    function GetContentAsString: String; override;
    procedure AssignContentValue(const Source: IBoldValue); override;
  public
    property asString: String read GetContentAsString write SetContentAsString;
    property asWideString: WideString read GetContentAsWideString write SetContentAsWideString;
  end;

const
  BoldContentName_WideString = 'WideString';

implementation

uses
  { RTL / VCL }
  SysUtils;

{******************************************************************************}
{* TBFSUnicodeString                                                          *}
{******************************************************************************}

{* Streaming routines *********************************************************}
function TBFSWideString.GetStreamName: String;
begin
  result := BoldContentName_WideString;
end;

{* Content management *********************************************************}

procedure TBFSWideString.AssignContentValue(const Source: IBoldValue);
var
  U: IBoldWideStringContent;
  S: IBoldStringContent;
begin
  if Source.QueryInterface(IBoldWideStringContent, U) = S_OK then
    if U.IsNull then
      SetContentToNull
    else
      asWideString := U.asWideString
  else if Source.QueryInterface(IBoldStringContent, S) = S_OK then
    if S.IsNull then
      SetContentToNull
    else
      asString := S.asString
  else
    raise EBold.CreateFmt('%s.AssignContentValue: unknown type of source', [classname]);
end;

function TBFSWideString.GetContentAsString: String;
begin
  Result := fDataValue;
end;

function TBFSWideString.GetContentAsWideString: WideString;
begin
  Result := fDataValue;
end;

procedure TBFSWideString.SetContentAsString(NewValue: String);
begin
  fDataValue := NewValue;
end;

procedure TBFSWideString.SetContentAsWideString(NewValue: WideString);
begin
  fDataValue := NewValue;
end;

initialization
  with FreeStandingValueFactory do
    RegisterFreeStandingClass(BoldContentName_WideString, TBFSWideString);
    
end.