
{ Global compiler directives }
{$include bold.inc}
unit BoldOLLEHandlesComponentEditor;

interface

uses
  DesignIntf,
  DesignEditors,
  BoldOLLEHandles;

type
  TBoldOLLEHandleComponentEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(index: Integer); override;
    function GetVerb(index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

implementation

uses
  SysUtils,
  BoldUtils;

{ TBoldOLLEHandleComponentEditor }

procedure TBoldOLLEHandleComponentEditor.ExecuteVerb(index: Integer);
begin
  case index of
    0: begin
    (Component as TBoldOLLEHandle).OLLEController.GenerateDatabase;
    end;
  end;
end;

function TBoldOLLEHandleComponentEditor.GetVerb(index: Integer): string;
begin
  case index of
  0: result := 'Generate OLLE database';
  else
    result := 'Nonexisting action';
  end;
end;

function TBoldOLLEHandleComponentEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

initialization
end.
