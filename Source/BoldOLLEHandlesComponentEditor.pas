
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
  BoldUtils,
  BoldCoreConsts;

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
  0: result := sGenerateOLLEDB;
  else
    result := sNonexistingAction;
  end;
end;

function TBoldOLLEHandleComponentEditor.GetVerbCount: Integer;
begin
  result := 1;
end;

end.
