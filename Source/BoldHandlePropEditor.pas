
{ Global compiler directives }
{$include bold.inc}
unit BoldHandlePropEditor;

interface

uses
  Classes,
  DesignEditors,
  BoldHandles,
  BoldElements,
  BoldSystemRT,
  BoldAbstractPropertyEditors,
  BoldPropertyEditors,
  BoldOclRepository,
  BoldRootedHandles;

type
  { forward declarations }
  TBoldOclVariablesEditor = class;
  TBoldOCLRepositoryEditor = class;
  TBoldRootedHandleRootHandlePropertyEditor = class;
  TBoldOclExpressionForOclDefinition = class;

  { TBoldOclVariablesEditor }
  TBoldOclVariablesEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

  { TBoldOCLRepositoryEditor }
  TBoldOCLRepositoryEditor = class(TBoldComponentDblClickEditor)
  protected
    function GetDefaultMethodName: string; override;
  end;

  {Note, some things are classed component even though they are actually
   TPersistent. This convention has been retained from the superclasses}
  { TBoldRootedHandleRootHandlePropertyEditor }
  TBoldRootedHandleRootHandlePropertyEditor = class(TBoldComponentPropertyIndicateMissing)
  private
    fOrgProc: TGetStrProc;
    procedure MyGetProc(const s: string);
  protected
    function AllMayBeSetTo(NewValue: TPersistent): boolean;
    function ComponentMayBeSetTo(Component: TPersistent; NewValue: TPersistent):boolean; virtual;
    property OrgGetProc: TGetStrProc read fOrgProc write fOrgProc;
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TBoldOclExpressionForOclDefinition }
  TBoldOclExpressionForOclDefinition = class(TBoldOclExpressionProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldElementTypeInfo; override;
  end;

  { TBoldTypeNameSelectorForOclDefinition }
  TBoldTypeNameSelectorForOclDefinition = class(TBoldTypeNameSelectorProperty)
  protected
    function GetContextType(Component: TPersistent): TBoldSystemTypeInfo; override;
    function GetApprovedTypes: TBoldValueTypes; override;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils;
  
{ TBoldHandlePropertyEditor }
procedure TBoldRootedHandleRootHandlePropertyEditor.GetValues(Proc: TGetStrProc);
begin
  OrgGetProc := Proc;
  inherited GetValues(MyGetProc);
end;

procedure TBoldRootedHandleRootHandlePropertyEditor.MyGetProc(const s: string);
begin
  if AllMayBeSetTo(Designer.GetComponent(s)) then
    OrgGetProc(s);
end;

function TBoldRootedHandleRootHandlePropertyEditor.AllMayBeSetTo(NewValue: TPersistent): boolean;
var
  i: integer;
begin
  Result := True;
  for i := 0 to PropCount - 1 do  { PropCount is # or properties tested, i.e. # of selected components }
    Result := Result and ComponentMayBeSetTo(GetComponent(i), NewValue);
end;

function TBoldRootedHandleRootHandlePropertyEditor.ComponentMayBeSetTo(
  Component: TPersistent; NewValue: TPersistent): boolean;
begin
  if (NewValue = Component) or
    ((NewValue is TBoldRootedHandle) and (Component is TBoldRootedHandle) and
    TBoldRootedHandle(NewValue).IsRootLinkedTo(Component as TBoldRootedhandle)) then
    Result := False
  else
    Result := True;
end;        

{ TBoldOclExpressionForOclDefinition }

function TBoldOclExpressionForOclDefinition.GetContextType(
  Component: TPersistent): TBoldElementTypeInfo;
begin
  if component is TBoldOclDefinition then
    result := (Component as TBoldOclDefinition).GetContextType
  else
    raise Exception.CreateFmt(sComnponentNotOCLDefinition, [ClassName]);
end;

{ TBoldTypeNameSelectorForOclDefinition }

function TBoldTypeNameSelectorForOclDefinition.GetApprovedTypes: TBoldValueTypes;
begin
  Result := [bvtAttr, bvtList, bvtClass, bvtSystem, bvtType];
end;

function TBoldTypeNameSelectorForOclDefinition.GetContextType(
  Component: TPersistent): TBoldSystemTypeInfo;
begin
  if component is TBoldOclDefinition then
    result := (Component as TBoldOclDefinition).SystemTypeInfo
  else
    raise Exception.CreateFmt(sComnponentNotOCLDefinition, [ClassName]);
end;

{ TBoldOclVariablesEditor }

function TBoldOclVariablesEditor.GetDefaultMethodName: string;
begin
  Result := 'Variables';
end;

{ TBoldOCLRepositoryEditor }

function TBoldOCLRepositoryEditor.GetDefaultMethodName: string;
begin
  Result := 'OCLDefinitions';
end;

end.
