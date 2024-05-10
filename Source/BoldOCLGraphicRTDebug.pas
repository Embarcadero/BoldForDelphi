
{ Global compiler directives }
{$include bold.inc}
unit BoldOCLGraphicRTDebug;

interface

uses
  Classes,

  BoldCoreConsts,
  BoldElements,
  BoldOclRTDebug,
  BoldDefs;

type
  { forward declarations }
  TBoldOclGraphicRTDebugger = class;

  { TBoldOclGraphicRTDebugger }
  TBoldOclGraphicRTDebugger = class (TBoldOclRTDebugger)
  private
    FOrgExprs: TStrings;
    FFixedExprs: TStrings;
    function MatchString(Context: TBoldElementTypeInfo; const Ocl: String): String;
  protected
    property OrgExprs: TStrings read FOrgExprs;
    property FixedExprs: TStrings read FFixedExprs;
  public
    constructor Create;
    destructor Destroy; override;
    function HasFixFor(const Ocl: String; Context: TBoldElementTypeInfo): Boolean; override;
    function GetFixFor(const Ocl: String; Context: TBoldElementTypeInfo): String; override;
    function AddFixFor(const Ocl: String; Context: TBoldElementTypeInfo; const ComponentPath: String; Message: String): Boolean; override;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  Controls,
  Dialogs,
  BoldOcl,
  BoldOclPropEditor;


function CaseSensitiveIndexOf(Strings: TStrings; Str: String): integer;
begin
  for Result := 0 to Strings.Count - 1 do
    if Strings[Result] = Str then
      Exit;
  result := -1;
end;

{ TBoldOclRTDebugger }

function TBoldOclGraphicRTDebugger.AddFixFor(const Ocl: String;
  Context: TBoldElementTypeInfo; const ComponentPath: String; Message: String): Boolean;
var
  FixForm: TBoldOclPropEditForm;
  OldPos: Integer;
begin
  BoldOCLRTDebugger := nil;
  Fixform := TBoldOclPropEditForm.Create(nil);
  fixform.Context := Context;
  FixForm.OclExpr := ocl;
  FixForm.Caption := sIllegalExpressionEncountered;
  FixForm.ExpressionPage.Caption := Format(sInComponent, [Componentpath]);
  ShowMessage(format(sIncorrectMessage, [ocl, BOLDCRLF, Componentpath, BOLDCRLF, Message, BOLDCRLF, Context.AsString]));
  if (FixForm.ShowModal = mrOK) and
    (ocl <> FixForm.OclExpr) then
  begin
    OldPos := CaseSensitiveIndexOf(OrgExprs, MatchString(Context, ocl));
    if OldPos <> -1 then
      FixedExprs[OldPos] := fixForm.OclExpr
    else
    begin
      FixedExprs.Add(fixForm.OclExpr);
      OrgExprs.Add(MatchString(Context, ocl));
    end;
    result := true;
  end
  else
    result := false;
  BoldOCLRTDebugger := self;
end;

function TBoldOclGraphicRTDebugger.GetFixFor(const Ocl: String;
  Context: TBoldElementTypeInfo): String;
var
  pos: integer;
begin
  result := ocl;
  pos := CaseSensitiveIndexOf(OrgExprs, MatchString(Context, ocl));
  while pos <> -1 do
  begin
    result := FixedExprs[Pos];
    POs := CaseSensitiveIndexOf(OrgExprs, MatchString(Context, ocl));
  end;
end;

function TBoldOclGraphicRTDebugger.HasFixFor(const Ocl: String;
  Context: TBoldElementTypeInfo): Boolean;
begin
  result := CaseSensitiveIndexOf(OrgExprs, MatchString(Context, Ocl)) <> -1;
end;

constructor TBoldOclGraphicRTDebugger.Create;
begin
  inherited;
  FOrgExprs:= TStringlist.Create;
  FFixedExprs:= TStringlist.Create;
end;

destructor TBoldOclGraphicRTDebugger.Destroy;
begin
  FreeAndNil(FOrgExprs);
  FreeAndNil(FFixedExprs);
  inherited;
end;

function TBoldOclGraphicRTDebugger.MatchString(Context: TBoldElementTypeInfo;
  const Ocl: String): String;
begin
  if assigned(Context) then
    result := Context.AsString + '.' + Ocl
  else
    result := '<no context>.' + Ocl;
end;

end.
