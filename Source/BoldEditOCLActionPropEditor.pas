
{ Global compiler directives }
{$include bold.inc}
unit BoldEditOCLActionPropEditor;

interface

uses
  DesignEditors,
  Classes;

type
  { forward declarations }
  TBoldOCLComponentEditor = class;

  { TBoldOCLComponentEditor }
  TBoldOCLComponentEditor = class(TComponentProperty)
  private
    fProc: TGetStrProc;
    procedure FilterOnInterface(const s: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

implementation

uses
  TypInfo,
  SysUtils,
  BoldElements;

{ TBoldOCLComponentEditor }

procedure TBoldOCLComponentEditor.FilterOnInterface(const s: string);
begin
  if Supports(Designer.GetComponent(s), IBoldOCLComponent) then
    fProc(s);
end;

procedure TBoldOCLComponentEditor.GetValues(Proc: TGetStrProc);
begin
  fProc := Proc;
  Designer.GetComponentNames(GetTypeData(GetPropType), FilterOnInterface);
end;

end.
