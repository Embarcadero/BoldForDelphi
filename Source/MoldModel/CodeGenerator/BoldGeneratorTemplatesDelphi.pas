unit BoldGeneratorTemplatesDelphi;

interface

uses
  Classes,
  BoldMeta,
  BoldGeneratorTemplates,
  BoldTemplateExpander;


type
  { forward declarations }
  TBoldDelphiTemplateManagerNameBound = class;
  TBoldDelphiTemplateManager = class;

  { TBoldGeneratorTemplateDM }
  TBoldGeneratorTemplatesDelphiDM = class(TDataModule)
    UnitTemplate: TBoldTemplateHolder;
    InterfaceTemplate: TBoldTemplateHolder;
    MethodTemplate: TBoldTemplateHolder;
    DeriveAndSubscribeTemplate: TBoldTemplateHolder;
    IncFileHeaderTemplate: TBoldTemplateHolder;
    ReverseDeriveTemplate: TBoldTemplateHolder;
    ComAdaptersTemplate: TBoldTemplateHolder;
    IDLTemplate: TBoldTemplateHolder;
    PersistenceInterfaceTemplate: TBoldTemplateHolder;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  { TBoldDelphiTemplateManager }
  TBoldDelphiTemplateManager = class(TBoldGeneratorTemplateManager)
  private
    fDataModule: TBoldGeneratorTemplatesDelphiDM;
    function GetDataModule: TBoldGeneratorTemplatesDelphiDM;
    function GetDelphiSignature(MoldMethod: TMoldMethod): String;
    function EnsureSafeIDLType(const ParamType: String; MoldClass: TMoldClass): String;
  protected
    function GetDerivedMethodTemplate: TBoldTemplateHolder; override;
    function GetReverseDeriveMethodTemplate: TBoldTemplateHolder; override;
    function GetMethodTemplate: TBoldTemplateHolder; override;
    function GetIncFileHeaderTemplate: TBoldTemplateHolder; override;
    function GetPersistenceInterfaceTemplate: TBoldTemplateHolder; override;
    procedure InitializeTemplateList(TemplateList: TBoldTemplateList); override;
    procedure InitializeCOMTemplateList(TemplateList: TBoldTemplateList); override;

    function GetDefaultIncFileExtension: string; override;
    property DataModule: TBoldGeneratorTemplatesDelphiDM read GetDataModule;
  public
    destructor Destroy; override;
    function MethodToCOMHeader(OwningClass: TMoldClass; Method: TMoldMethod; InterfaceCode: Boolean; ParametersToCoerce: TStringList; ParametersToInterfaceCoerce: TStringList; MoldModel: TMoldModel; GenerateMIDLCode: Boolean): String; override;
    function MethodToCOMCall(OwningClass: TMoldClass; Method: TMoldMethod; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList; MoldModel: TMoldModel): String; override;
    function ReadMethodSignature(ClassName, AttributeName, AttributeType: string): string; override;
    function WriteMethodSignature(ClassName, AttributeName, AttributeType: string): string; override;
    class procedure InstallTemplates;
    function MethodToCodeHeader(OwningClass: TMoldClass; Method: TMoldMethod; TagValue: Integer; AddSignature: Boolean; AutoOverride: Boolean): String; override;
    function StringContainsMethodHeader(s: String): Boolean; override;
    function GetSearchStringfromMethodHeader(header: String; SearchParameterList: Boolean): String; override;
    procedure AddQualifierParam(var Params: String; ParamName, ParamType: String);
    procedure AddQualifierPropertyParam(var Params: String; ParamName, ParamType: String); override;
    procedure AddQualifierFunctionParam(var Params: String; ParamName, ParamType: String); override;
    function GenerateInheritedCall(MoldClass: TMoldClass; MoldMethod: TMoldMethod): String; override;
  end;

  { TBoldDelphiTemplateManagerNameBound }
  TBoldDelphiTemplateManagerNameBound = class(TBoldDelphiTemplateManager)
  protected
    procedure InitializeTemplateList(TemplateList: TBoldTemplateList); override;
  public
    class procedure InstallTemplates;
  end;

implementation

uses
  SysUtils,
  BoldRev,
  BoldUMLTypes,
  BoldTaggedValueSupport,
  BoldMetaSupport;

{$R *.dfm}

{ TBoldDelphiTemplateManager }

destructor TBoldDelphiTemplateManager.Destroy;
begin
  FreeAndNil(fDataModule);
  inherited;
end;

function TBoldDelphiTemplateManager.GetDataModule: TBoldGeneratorTemplatesDelphiDM;
begin
  if not assigned(fDataModule) then
    fDataModule := TBoldGeneratorTemplatesDelphiDM.Create(nil);
  result := fDataModule;
end;

function TBoldDelphiTemplateManager.GetDerivedMethodTemplate: TBoldTemplateHolder;
begin
  result := DataModule.DeriveAndSubscribeTemplate;
end;

function TBoldDelphiTemplateManager.GetIncFileHeaderTemplate: TBoldTemplateHolder;
begin
  result := DataModule.IncFileHeaderTemplate;
end;

function TBoldDelphiTemplateManager.GetMethodTemplate: TBoldTemplateHolder;
begin
  result := DataModule.MethodTemplate;
end;

function TBoldDelphiTemplateManager.GetReverseDeriveMethodTemplate: TBoldTemplateHolder;
begin
  Result := DataModule.ReverseDeriveTemplate;
end;

procedure TBoldDelphiTemplateManager.InitializeTemplateList(TemplateList: TBoldTemplateList);
begin
  if TemplateList.Count = 0 then
  begin
    TemplateList.Add(DataModule.UnitTemplate);
    TemplateList.Add(DataModule.InterfaceTemplate);
  end;
end;

procedure TBoldDelphiTemplateManager.InitializeCOMTemplateList(TemplateList: TBoldTemplateList);
begin
  if TemplateList.Count = 0 then
  begin
    TemplateList.Add(DataModule.IDLTemplate);
    TemplateList.Add(DataModule.ComAdaptersTemplate);
  end;
end;

function TBoldDelphiTemplateManager.EnsureSafeIDLType(const ParamType: String;
  MoldClass: TMoldClass): String;
var
  i: integer;
begin
  for i := 0 to MoldClass.Model.Classes.Count - 1 do
  begin
    if CompareText(ParamType, MoldClass.Model.Classes[i].ExpandedInterfaceName) = 0 then
    begin
      result := MoldClass.LowestCommonSuperClass(MoldClass.Model.Classes[i]).ExpandedInterfaceName;
      exit;
    end;
  end;
  result := ParamType;
end;

function TBoldDelphiTemplateManager.MethodToCOMHeader(OwningClass: TMoldClass; Method: TMoldMethod; InterfaceCode: Boolean; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList; MoldModel: TMoldModel; GenerateMIDLCode: Boolean): String;
var
  i: integer;
  IsConst: Boolean;
  s,
  params,
  CheckInterface,
  ParamType: String;
  Param: TMoldParameter;
begin
  s := '';
  if Method.HasReturnValue then
    s := s + 'function ' // do not localize
  else
    s := s + 'procedure '; // do not localize
  if not InterfaceCode then
    s := s + OwningClass.ExpandedDelphiName + 'Adapter.'; // do not localize

  s := s + Method.ExpandedDelphiName;

  params := '';
  for i := 0 to Method.Parameters.Count - 1 do
  begin
    if Params <> '' then
      params := params + '; ';

    Param := tMoldParameter(Method.Parameters[i]);
    ParamType := TBoldMetaSupport.ParameterTypeToComType(Param.ParameterType, MoldModel, IsConst);

    if not GenerateMIDLCode then
      ParamType := EnsureSafeIDLType(ParamType, Method.MoldClass);

    if param.ParameterKind in [pdOut, pdInout] then
    begin
      case param.ParameterKind of
        pdOut: params := params + 'out '; // do not localize
        pdInOut: params := params + 'var '; // do not localize
      end;
      if assigned(ParametersToCoerce) and
        ((ParamType = BoldWideStringTypeName) or
         (ParamType = BoldWordBoolTypeName)) then
        ParametersToCoerce.Add(Param.ParameterName + '=' + Param.DelphiParameterType);

      if assigned(ParametersToInterfaceCoerce) then
      begin
        CheckInterface := TBoldMetaSupport.ParameterTypeToInterfaceType(Param.ParameterType, MoldModel);

        if (CheckInterface <> '') then
          ParametersToInterfaceCoerce.Add(Param.ParameterName + '=' + Param.DelphiParameterType + '=' + CheckInterface);
      end;

    end
    else if IsConst then
      Params := Params + 'const '; // do not localize
    Params := Params + Param.ParameterName + ': ' + ParamType;
  end;
  if Params <> '' then
    s := s+ '(' + Params + ')';

  if Method.HasReturnValue then
  begin
    ParamType := TBoldMetaSupport.ParameterTypeToComType(Method.returnType, MoldModel, IsConst);
    if not GenerateMIDLCode then
      ParamType := EnsureSafeIDLType(ParamType, Method.MoldClass);

    s := s + ': '+ ParamType;
  end;

  s := s + ';';
  if InterfaceCode then
    s := s + ' safecall;'; // do not localize

  result := s;
end;

function TBoldDelphiTemplateManager.MethodToCOMCall(OwningClass: TMoldClass; Method: TMoldMethod; ParametersToCoerce, ParametersToInterfaceCoerce: TStringList; MoldModel: TMoldModel): String;
var
  i: integer;
  s: string;
  params: string;
  Interfacename: String;
  Param: TMoldParameter;

  function ParameterNeedsMarshalling(const ParameterType: String; var InterfaceName: string): Boolean;
  begin
    InterfaceName := TBoldMetaSupport.ParameterTypeToInterfaceType(ParameterType, MoldModel);
    Result := InterfaceName <> '';
  end;

begin
  s := 'As' + OwningClass.ExpandedExpressionName + '.' + Method.ExpandedDelphiName; // do not localize

  params := '';
  for i := 0 to Method.Parameters.Count - 1 do
  begin
    if Params <> '' then
      params := params + ', ';
    Param := tMoldParameter(Method.Parameters[i]);
    if ParametersToInterfaceCoerce.IndexOfName(Param.ParameterName)  <> -1 then
      params := params + param.ParameterName + '_temp' // do not localize
    else if ParameterNeedsMarshalling(Param.ParameterType, InterfaceName) then
      Params := Params + 'BoldComInterfaceToObject(' + Param.ParameterName + ') as ' + Param.DelphiParameterType // do not localize
    else if parametersToCoerce.IndexOfName(param.ParameterName) <> -1 then
      params := params + param.ParameterName + '_temp' // do not localize
    else
      Params := params + Param.ParameterName;
  end;

  if Params <> '' then
    s := s + '(' + Params + ')';

  if Method.HasReturnValue then
  begin
    if ParameterNeedsMarshalling(Trim(method.ReturnType), InterfaceName) then
      s :='BoldComCreateAdapter(' + s + ', False, ' + Interfacename + ', Result)' // do not localize
    else
      s := 'result := ' + s; // do not localize
  end;
  result := s + ';';
end;

function TBoldDelphiTemplateManager.MethodToCodeHeader(
  OwningClass: TMoldClass; Method: TMoldMethod; TagValue: Integer;
  AddSignature, AutoOverride: Boolean): String;
var
  s: string;
  Signature: String;
  functype: TDelphiFunctionType;
const
  directiveNames: array[TDelphiFunctionType] of string = ('', 'virtual;', 'override;', 'dynamic;', 'virtual; abstract;');
begin
  s := '';
  if Method.IsClassMethod then
    s := s + 'class '; // do not localize

  if AnsiCompareText(Method.Name, 'Destroy') = 0 then // do not localize
    s := s + 'destructor ' // do not localize
  else if Method.HasReturnValue then
    s := s + 'function ' // do not localize
  else
    s := s + 'procedure '; // do not localize

  if TagValue = ImplementationTag then
    s := s + OwningClass.ExpandedDelphiName + '.';
  s := s + Method.ExpandedDelphiName;

  if addSignature then
  begin
    Signature := GetDelphiSignature(Method);
    if Signature <> '' then
      s := s + '(' + Signature + ')';

    if Method.HasReturnValue then
      s := s + ': '+ Method.DelphiReturnType;

    s := s + ';';
  end;

  if TagValue in [publicTag..PrivateTag] then
  begin
    if Autooverride then
      FuncType := dfOverride
    else
    begin
      FuncType := Method.FuncType;
      if method.OverrideInAllSubclasses and (funcType = dfNormal) then
        funcType := dfVirtual;
    end;
    s := s + ' ' + DirectiveNames[FuncType];
  end;

  result := s;
end;

class procedure TBoldDelphiTemplateManager.InstallTemplates;
begin
  BoldGeneratorTemplateManagerClass := TBoldDelphiTemplateManager;
end;

function TBoldDelphiTemplateManager.GetDelphiSignature(
  MoldMethod: TMoldMethod): String;
var
  i: integer;
  Parameter: TMoldParameter;
begin
  result := '';
  for i := 0 to MoldMethod.parameters.count-1 do
  begin
    parameter := MoldMethod.Parameters[i] as TMoldParameter;

    if result <> '' then
      result := result + '; ';

    if Parameter.IsConst then
      Result := Result + 'const ' // do not localize
    else if Parameter.ParameterKind = pdInOut then
      Result := Result + 'var ' // do not localize
    else if Parameter.ParameterKind = pdOut then
      Result := Result + 'out ';   // do not localize

    result := result + format('%s: %s', [Parameter.ParameterName, // do not localize
                                         Parameter.DelphiParameterType]);
  end;
end;

function TBoldDelphiTemplateManager.GetDefaultIncFileExtension: string;
begin
  result := '.inc'; // do not localize
end;

function TBoldDelphiTemplateManager.StringContainsMethodHeader(s: String): Boolean;
var
  temp: String;
begin
  temp := UpperCase(s);
  result := (pos('PROCEDURE', temp) <> 0) or // do not localize
            (pos('FUNCTION', temp) <> 0) or // do not localize
            (pos('DESTRUCTOR', temp) <> 0); // do not localize
end;

procedure TBoldDelphiTemplateManager.AddQualifierParam(var Params: String; ParamName, ParamType: String);
begin
  if Params <> '' then
    Params := Params + '; ';
  Params := Params + ParamName+': '+ParamType; // do not localize
end;

procedure TBoldDelphiTemplateManager.AddQualifierFunctionParam(var Params: String; ParamName, ParamType: String);
begin
  AddQualifierParam(Params, ParamName, ParamType);
end;

procedure TBoldDelphiTemplateManager.AddQualifierPropertyParam(var Params: String; ParamName, ParamType: String);
begin
  AddQualifierParam(Params, ParamName, ParamType);
end;

function TBoldDelphiTemplateManager.GetSearchStringfromMethodHeader(header: String; SearchParameterList: Boolean): String;
begin
  result := header;
  if not SearchParameterList and (pos('(', result) <> 0) then
    result := copy(result, 1, pos('(', result) - 1);

  if not SearchParameterList and (pos(':', result ) <> 0) then
    result := copy(result, 1, pos(':', result) - 1);

end;

function TBoldDelphiTemplateManager.GenerateInheritedCall(MoldClass: TMoldClass; MoldMethod: TMoldMethod): String;
begin
  if not MoldMethod.HasReturnValue then
    result := 'inherited' // do not localize
  else
    result := format('result := inherited %s(%s)', [Moldmethod.Name, MoldMethod.CallSignature]); // do not localize
end;

function TBoldDelphiTemplateManager.ReadMethodSignature(ClassName, AttributeName, AttributeType: string): string;
begin
  result := format('function %s.Get%s: %s;', [ClassName, AttributeName, AttributeType]); // do not localize
end;

function TBoldDelphiTemplateManager.WriteMethodSignature(ClassName, AttributeName, AttributeType: string): string;
begin
  result := format('procedure %s.Set%s(NewValue: %s);', [ClassName, AttributeName, AttributeType]); // do not localize
end;

function TBoldDelphiTemplateManager.GetPersistenceInterfaceTemplate: TBoldTemplateHolder;
begin
  result := DataModule.PersistenceInterfacetemplate;
end;

{ TBoldDelphiTemplateManagerNameBound }

procedure TBoldDelphiTemplateManagerNameBound.InitializeTemplateList(TemplateList: TBoldTemplateList);
var
  template: String;
begin
  if TemplateList.Count = 0 then
  begin
    template := DataModule.UnitTemplate.Template.Text;
    Template := StringReplace(template, 'BoldMembers[$(MEMBERINDEX)]', 'BoldMemberByExpressionName[''$(MEMBERNAME)'']', [rfReplaceAll]); // do not localize
    Template := StringReplace(template, '$(MEMBERINDEX)', '-1', [rfReplaceAll]); // do not localize
    DataModule.UnitTemplate.Template.Text := Template;
  end;
  inherited;
end;

class procedure TBoldDelphiTemplateManagerNameBound.InstallTemplates;
begin
  BoldGeneratorTemplateManagerClass := TBoldDelphiTemplateManagerNameBound;
end;

initialization
  {$IFDEF BOLD_DELPHI}
  TBoldDelphiTemplateManager.InstallTemplates;
  {$ENDIF}
end.