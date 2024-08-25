
{ Global compiler directives }
{$include bold.inc}
unit BoldGeneratorTemplates;

interface

uses
  Classes,
  BoldMeta,
  BoldTemplateExpander;

const
  publicTag = 0;
  protectedTag = 1;
  PrivateTag = 2;
  ImplementationTag = 3;

type
  TBoldGeneratorTemplateManager = class;
  TBoldGeneratorTemplateManagerClass = class of TBoldGeneratorTemplateManager;


  { TBoldGeneratorTemplateManager }
  TBoldGeneratorTemplateManager = class
  private
    fTemplateList: TBoldTemplateList;
    fComTemplateList: TBoldTemplateList;
  protected
    function GetDerivedMethodTemplate: TBoldTemplateHolder; virtual; abstract;
    function GetReverseDeriveMethodTemplate: TBoldTemplateHolder; virtual; abstract;
    function GetMethodTemplate: TBoldTemplateHolder; virtual; abstract;
    function GetIncFileHeaderTemplate: TBoldTemplateHolder; virtual; abstract;
    procedure InitializeTemplateList(TemplateList: TBoldTemplateList); virtual;
    procedure InitializeCOMTemplateList(TemplateList: TBoldTemplateList); virtual;
    function GetDefaultIncFileExtension: string; virtual; abstract;
    function GetPersistenceInterfaceTemplate: TBoldTemplateHolder; virtual; 
    function GetTemplateList: TBoldTemplateList;
    function GetCOMTemplateList: TBoldTemplateList;
  public
    destructor Destroy; override;
    function MethodToCOMHeader(OwningClass: TMoldClass; Method: TMoldMethod; InterfaceCode: Boolean; ParametersToCoerce: TStringList; ParametersToInterfaceCoerce: TStringList; MoldModel: TMoldModel; GenerateMIDLCode: Boolean): String; virtual; abstract;
    function MethodToCOMCall(OwningClass: TMoldClass; Method: TMoldMethod; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList; MoldModel: TMoldModel): String; virtual; abstract;
    function ReadMethodSignature(ClassName, AttributeName, AttributeType: string): string; virtual; abstract;
    function WriteMethodSignature(ClassName, AttributeName, AttributeType: string): string; virtual; abstract;
    function MethodToCodeHeader(OwningClass: TMoldClass; Method: TMoldMethod; TagValue: Integer; AddSignature: Boolean; AutoOverride: Boolean): String; virtual; abstract;
    function StringContainsMethodHeader(s: String): Boolean; virtual; abstract;
    procedure AddQualifierPropertyParam(var Params: String; ParamName, ParamType: String); virtual; abstract;
    procedure AddQualifierFunctionParam(var Params: String; ParamName, ParamType: String); virtual; abstract;
    function GetSearchStringfromMethodHeader(header: String; SearchParameterList: Boolean): String; virtual; abstract;
    function GenerateInheritedCall(MoldClass: TMoldClass; MoldMethod: TMoldMethod): String; virtual; abstract;

    property FileTemplates: TBoldTemplateList read GetTemplateList;
    property ComFileTemplates: TBoldTemplateList read GetCOMTemplateList;
    property PersistenceInterfaceTemplate: TBoldTemplateHolder read GetPersistenceInterfaceTemplate;
    property MethodTemplate: TBoldTemplateHolder read GetMethodTemplate;
    property DerivedMethodTemplate: TBoldTemplateHolder read GetDerivedMethodTemplate;
    property ReverseDeriveMethodTemplate: TBoldTemplateHolder read GetReverseDeriveMethodTemplate;
    property IncFileHeaderTemplate: TBoldTemplateHolder read GetIncFileHeaderTemplate;
    property DefaultIncFileExtension: string read GetDefaultIncFileExtension;
  end;


function BoldGeneratorTemplatesManager: TBoldGeneratorTemplateManager;

var
  BoldGeneratorTemplateManagerClass: TBoldGeneratorTemplateManagerClass = nil;

implementation

uses
  SysUtils,
  BoldDefs;

var
  G_BoldGeneratorTemplateManager: TBoldGeneratorTemplateManager;

function BoldGeneratorTemplatesManager: TBoldGeneratorTemplateManager;
begin
  if not assigned(G_BoldGeneratorTemplateManager) then
  begin
    if not assigned(BoldGeneratorTemplateManagerClass) then
      raise EBoldInternal.Create('No GeneratorTemplateManagerClass assigned, unable to create template manager');
    G_BoldGeneratorTemplateManager := BoldGeneratorTemplateManagerClass.Create;
  end;
  result := G_BoldGeneratorTemplateManager;
end;

{ TBoldGeneratorTemplateManager }


destructor TBoldGeneratorTemplateManager.Destroy;
begin
  FreeAndNil(fTemplateList);
  FreeAndNil(fComTemplateList);
  inherited;
end;

function TBoldGeneratorTemplateManager.GetCOMTemplateList: TBoldTemplateList;
begin
  if not assigned(fCOMTemplateList) then
  begin
    fCOMTemplateList := TBoldTemplateList.Create;
    InitializeCOMTemplateList(fCOMTemplateList);
  end;
  result := fCOMTemplateList;
end;



function TBoldGeneratorTemplateManager.GetPersistenceInterfaceTemplate: TBoldTemplateHolder;
begin
  result := nil;
end;

function TBoldGeneratorTemplateManager.GetTemplateList: TBoldTemplateList;
begin
  if not assigned(fTemplateList) then
  begin
    fTemplateList := TBoldTemplateList.Create;
    InitializeTemplateList(fTemplateList);
  end;
  result := fTemplateList;
end;

procedure TBoldGeneratorTemplateManager.InitializeCOMTemplateList(TemplateList: TBoldTemplateList);
begin
end;

procedure TBoldGeneratorTemplateManager.InitializeTemplateList(TemplateList: TBoldTemplateList);
begin
end;

initialization

finalization
  FreeAndNil(G_BoldGeneratorTemplateManager);  


end.
