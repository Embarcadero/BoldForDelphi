
{ Global compiler directives }
{$include bold.inc}
unit BoldTemplateExpander;

interface

uses
  Classes,
  BoldBase,
  BoldIndexableList,
  BoldHashIndexes,
  BoldIndex,
  BoldDefs;

const
  bvfReadOnly = 0;
  bvfPersistent = 1;
  BoldBooleanToTemplateVar: array[false..true] of char = ('0', '1');

type
  { forward declarations }
  TBoldTemplatevariable = class;
  TBoldTemplateVariables = class;
  TBoldTemplateHolder = class;

  TBoldVariableFlags = set of 0..31;

  { TBoldTemplateVariable }
  TBoldTemplatevariable = class(TBoldMemoryManagedObject)
  private
    fName: String;
    fValue: String;
    fFlags: TBoldvariableFlags;
  public
    constructor create(const name, value: string; Flags: TBoldVariableFlags);
    property Name: string read fName;
    property Value: String read fValue write fValue;
    property Flags: TBoldVariableFlags read fFlags;
  end;

  { TBoldTemplateVariables }
  TBoldTemplateVariables = class(TBoldUnOrderedIndexableList)
  private
    function GetValue(const Name: string): string;
    procedure InitializeDateTimeMacros;
    function GetVariableByName(const name: String): TBoldTemplateVariable;
  protected
    property VariableByName[const name: String]: TBoldTemplateVariable read GetVariableByName;
  public
    constructor Create;
    procedure Add(const Name, Value: string; Flags: TBoldVariableFlags);
    procedure ForceAdd(const Name, Value: string; Flags: TBoldVariableFlags);
    procedure Change(const Name, Value: string);
    procedure SetVariable(const Name, Value: string);
    function Exists(const Name: string): Boolean;
    function ExpandString(const Source: string): string;
    procedure Remove(const Name: string);
    property Values[const Name: string]: string read GetValue;
  end;

  { TBoldTemplateHolder }
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldTemplateHolder = class(TComponent)
  private
    fTemplate: TStringList;
    fExpandedTemplate: TStringList;
    fVariables: TBoldTemplateVariables;
    FDescription: String;
    FFileName: String;
    procedure SetTemplate(const Value: TStringList);
    function GetExpandedTemplate: TStringList;
    function GetExpandedFileName: String;
    procedure SetDescription(const Value: String);
    procedure SetFileName(const Value: String);
    procedure TemplateChanged(sender: TObject);
  public
    constructor Create(aOwner: Tcomponent); override;
    procedure ExpandTemplate;
    destructor Destroy; override;
    property Variables: TBoldTemplateVariables read fvariables;
    property ExpandedTemplate: TStringList read GetExpandedTemplate;
    property LastExpansion: TStringList read fExpandedTemplate;
    property ExpandedFileName: String read GetExpandedFileName;
  published
    property Template: TStringList read fTemplate write SetTemplate;
    property FileName: String read FFileName write SetFileName;
    property Description: String read FDescription write SetDescription;
  end;

  TBoldTemplateList = class(TList)
  private
    function GetItems(index: integer): TBoldTemplateHolder;
  public
    property items[index: integer]: TBoldTemplateHolder read GetItems; default;
  end;


function BoldExpandTemplate(const Source: string; Variables: TBoldtemplateVariables; MacroNamePad: String = ''): string;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils;

var
  IX_TemplateVariables: integer = -1;

type
  TBoldTemplatevariableIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

{ TBoldTemplateHolder }

constructor TBoldTemplateVariables.Create;
begin
  inherited;
  SetIndexvariable(IX_Templatevariables, AddIndex(TBoldTemplatevariableIndex.Create));
end;

function TBoldTemplateVariables.GetVariableByName(const name: String): TBoldTemplateVariable;
begin
  result := TBoldTemplatevariableIndex(Indexes[IX_Templatevariables]).FindByString(name) as TBoldTemplateVariable;
end;

procedure TBoldTemplateVariables.Add(const Name, Value: string; Flags: TBoldVariableFlags);
var
  variable: TBoldTemplateVariable;
begin
  variable := VariableByName[Name];
  if not assigned(variable) then
    ForceAdd(Name, Value, Flags);
end;

procedure TBoldTemplateVariables.Change(const Name, Value: string);
var
  Variable: TBoldTemplatevariable;
begin
  Variable := VariableByName[Name];
  if assigned(Variable) then
    Variable.Value := Value;
end;

function TBoldTemplateVariables.Exists(const Name: string): Boolean;
var
  Variable: TBoldTemplatevariable;
begin
  variable := VariableByName[Name];
  Result := assigned(Variable);
end;

function TBoldTemplateVariables.ExpandString(const Source: string): string;
begin
  Change('DATETIME', DateTimeToStr(now));
  Change('DATE', DateToStr(now));
  Change('TIME', TimeToStr(now));
  result := BoldExpandTemplate(Source, Self);
end;

{
function TBoldTemplateVariables.GetName(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Count) then
    Result := (items[Index] as TBoldTemplatevariable).Name
  else
    Result := '';
end;
}

function TBoldTemplateVariables.GetValue(const Name: string): string;
var
  variable: TBoldTemplatevariable;
begin
  Variable := VariableByName[Name];
  if assigned(variable) then
    result := Variable.value
  else
    result := '';
end;

procedure TBoldTemplateVariables.Remove(const Name: string);
var
  variable: TBoldTemplatevariable;
begin
  Variable := VariableByName[Name];
  if assigned(variable) then
    inherited remove(variable);
end;

const
  MacroBegin = '$(';
  MacroEnd = ')';
  MacroModifiers  = [',', '+', '-', '<', '>', '?'];
  MacroNumbers = ['0'..'9'];

function BoldExpandTemplate(const Source: string; Variables: TBoldTemplateVariables; MacroNamePad: String): string;
var
  Mb, Me, J, i, p: Integer;
  BarPos: integer;
  Temp, TempSource, MacroName, MacroValue, Number, Pad: string;
  Modifier: Char;
  LoopText: String;
  OldLoopValue, EndLoopPos: integer;
  MatchMacroName: String;
  ColonPos, CommaPos: integer;
  CaseText: TStringList;
  MacroText: string;
  CaseValue: String;
  elsevalue: string;
  CurrentCasetag: String;
  value: string;

  function GetVarvalue(MacroName: String): String;
  begin
    result := Variables.Values[MacroName];
    while (Result = '') and (pos('.', MacroName) <> 0) do
    begin
      i := length(MacroName);
      While MacroName[i] <> '.' do dec(i);
      Delete(MacroName, i, maxint);
      result := Variables.Values[MacroName];
    end;
  end;

begin
  Variables.InitializeDateTimeMacros;
  TempSource := Source;
  Result := '';
  Mb := Pos(MacroBegin,TempSource);
  while (Mb > 0) do
  begin
    Temp := Copy(TempSource, Mb + 2, MaxInt);
    Me := Pos(MacroEnd,Temp);
    if Me > 0 then
    begin
      Result := Result + Copy(TempSource, 1, Mb - 1);
      if not CharInSet(temp[1], ['?', ',']) then // temp cant be empty since MAcroEnd is inside temp
        MacroName := Copy(Temp, 1, Me - 1) + MacroNamePad
      else
        MacroName := Copy(Temp, 1, Me - 1);
      if (Length(MacroName) > 0) and CharInSet(MacroName[1], MacroModifiers) then
      begin
        Modifier := MacroName[1];
        MacroName := Copy(MacroName, 2, MaxInt);
        case Modifier of
          ',':
          begin
            Value := copy(MacroName, pos(':', MacroName) + 1, MaxInt);
            macroName := copy(MacroName, 1, pos(':', MacroName) - 1);
            if GetVarValue(MacroName + MacroNamePad) <> '0' then
              result := result + Value
          end;

          '+':
            Result := Result + AnsiUppercase(GetVarvalue(MacroName));
          '-':
            Result := Result + AnsiLowercase(GetVarvalue(MacroName));
          '<','>':
          begin
            Number := '';
            while CharInSet(MacroName[1], MacroNumbers) do
            begin
              Number := Number + MacroName[1];
              MacroName := Copy(MacroName, 2, MaxInt);
            end;
            J := StrToIntDef(Number, 0);
            MacroValue := GetVarvalue(MacroName);
            if (J > 0) and (J > Length(MacroValue)) then
            begin
              case Modifier of
                '<':
                  Pad := MacroValue + StringOfChar(' ', J - Length(MacroValue));
                '>':
                  Pad := StringOfChar(' ', J - Length(MacroValue)) + MacroValue;
              end;
            end
            else
              Pad := MacroValue;
            Result := Result + Pad;
          end;
          '?': begin
            ColonPos := pos(':', MacroName);
            CommaPos:= pos(',', MacroName);
            if GetVarvalue(copy(MacroName, 1, ColonPos - 1) + MacroNamePad) = '1' then
              result := result + copy(MacroName, ColonPos + 1, commaPos - ColonPos - 1)
            else
              result := result + copy(MacroName, CommaPos + 1, MaxInt);
          end;
        end;
      end
      else
      begin
        EndLoopPos := 0;
        if Pos('LOOP', MacroName) = 1 then
        begin
          MacroName := Copy(MacroName, 5, maxint);
          MatchMacroName := MacroName;
          if pos('.', MatchMacroName) <> 0 then
            Delete(MatchMacroName, pos('.', MatchMacroName), MaxInt);
          MatchMacroname := UpperCase(MacroBegin + 'ENDLOOP' + MatchMacroName + MacroEnd);

          EndLoopPos := pos(MatchMacroName, UpperCase(temp));
          if EndLoopPos <> 0 then
          begin
            LoopText := copy(temp, me + 1, EndLoopPos - me - 1);
            me := EndLoopPos+length(MatchMacroname) - 1;
            OldLoopValue := StrToIntDef(GetVarvalue(MacroName), 0);
            for i := 0 to OldLoopValue - 1 do
            begin
              Variables.SetVariable(Macroname, IntToStr(i));
              result := result + BoldExpandTemplate(LoopText, Variables, MacroNamePad + '.' + IntToStr(i));
            end;
            Variables.SetVariable(Macroname, IntToStr(OldLoopValue));
          end
          else
            raise Exception.CreateFmt(sUnterminatedLoop, [MacroName]);

        end
        else if Pos('CASE', macroName) = 1 then
        begin
          MacroName := Copy(MacroName, 5, maxint);
          MatchMacroName := MacroName;
          if pos('.', MatchMacroName) <> 0 then
            Delete(MatchMacroName, pos('.', MatchMacroName), MaxInt);
          MatchMacroname := UpperCase(MacroBegin + 'ENDCASE' + MatchMacroName + MacroEnd);

          EndLoopPos := pos(MatchMacroName, UpperCase(temp));
          if EndLoopPos <> 0 then
          begin
            CaseText := TStringList.Create;
            CaseText.Text := trim(copy(temp, me + 1, EndLoopPos - me - 1));
            CaseValue := UpperCase(GetVarvalue(MacroName));
            ElseValue := '';
            for i := CaseText.Count-1 downto 0 do
            begin
              BarPos := pos('|', CaseText[i]);
              if (barpos <> 0) then
              begin
                CurrentCaseTag := UpperCase(BoldExpandTemplate(uppercase(trim(copy(CaseText[i], 1, barpos - 1))), variables, MacroNamePad));
                if currentCaseTag <> CaseValue then
                begin
                  if CurrentCasetag = 'ELSE' then
                    ElseValue := copy(CaseText[i], BarPos + 1, maxint) + BOLDCRLF + ElseValue;
                  CaseText.Delete(i);
                end
                else
                begin
                  CaseText[i] := copy(CaseText[i], BarPos + 1, maxint);
                  if trim(CaseText[i]) = '\' then
                    CaseText.Delete(i);
                end;
              end
              else
                if trim(CaseText[i]) = '\' then
                  CaseText.Delete(i);

            end;
            if CaseText.Count = 0 then
              CaseText.text := ElseValue;

            me := EndLoopPos + Length(MatchMacroName) - 1;
            result := result + BoldExpandTemplate(CaseText.Text, Variables, MacroNamePad);
            CaseText.Free;
          end 
          else
            raise Exception.CreateFmt(sUnterminatedCase, [MacroName]);
        end
        else if Pos('MACROSTART', MacroName) = 1 then
        begin
          MacroName := trim(Copy(MacroName, 11, maxint));
          MatchMacroName := MacroName;
          if pos('.', MatchMacroName) <> 0 then
            Delete(MatchMacroName, pos('.', MatchMacroName), MaxInt);
          MatchMacroname := UpperCase(MacroBegin + 'MACROEND ' + MatchMacroName + MacroEnd);

          EndLoopPos := pos(MatchMacroName, UpperCase(temp));
          if EndLoopPos <> 0 then
          begin
            MacroText := copy(temp, me + 1, EndLoopPos - me - 1);
            me := EndLoopPos + Length(MatchMacroname) - 1;
            Variables.SetVariable(Macroname, MacroText);
          end
          else
            raise Exception.CreateFmt(sUnterminatedMacro, [MacroName]);
        end;

        if pos('CRLF', upperCase(MacroName)) = 1 then
          result := result + BOLDCRLF
        else if endLoopPos = 0 then
        begin
          value := Variables.Values[MacroName];
          while (value = '') and (pos('.', MacroName) <> 0) do
          begin
            i := length(MacroName);
            While MacroName[i] <> '.' do dec(i);
            Delete(MacroName, i, maxint);
            value := Variables.Values[MacroName];
          end;
          Result := Result + BoldExpandTemplate(Value, variables, MacroNamePad);
        end;
      end;
      TempSource := Copy(Temp, Me + 1, MaxInt);
      Mb := Pos(MacroBegin, TempSource);
    end
    else
      Break;
  end;
  Result := Result + TempSource;
  p := pos('\' + BOLDCRLF, result);
  while p <> 0 do
  begin
    Delete(result, p, 3);
    p := pos('\' + BOLDCRLF, result);
  end;
end;

{ TBoldTemplateHolder }

constructor TBoldTemplateHolder.Create(aOwner: Tcomponent);
begin
  inherited;
  fVariables := TBoldTemplateVariables.Create;
  fTemplate := TStringList.Create;
  fTemplate.OnChange := TemplateChanged;
  fExpandedTemplate := TStringList.Create;
end;

destructor TBoldTemplateHolder.Destroy;
begin
  FreeAndNil(fVariables);
  FreeAndNil(fTemplate);
  FreeAndNil(fExpandedTemplate);
  inherited;
end;

procedure TBoldTemplateHolder.ExpandTemplate;
begin
  fExpandedTemplate.Text := trim(Variables.ExpandString(Template.text));
end;

function TBoldTemplateHolder.GetExpandedTemplate: TStringList;
begin
  ExpandTemplate;
  result := fExpandedTemplate;
end;

procedure TBoldTemplateHolder.SetTemplate(const Value: TStringList);
begin
  fTemplate.assign(Value);
end;

procedure TBoldTemplateVariables.SetVariable(const Name, Value: string);
var
  variable: TBoldTemplatevariable;
begin
  Variable := VariableByName[Name];
  if assigned(variable) then
    Variable.Value := Value
  else
    Forceadd(name, value, []);
end;

procedure TBoldTemplateVariables.ForceAdd(const Name, Value: string; Flags: TBoldVariableFlags);
var
  variable: TBoldTemplateVariable;
begin
  variable := TBoldTemplateVariable.Create(name, value, flags);
  inherited add(variable);
end;

function TBoldTemplateHolder.GetExpandedFileName: String;
begin
  result := BoldExpandTemplate(FileName, Variables);
end;

procedure TBoldTemplateHolder.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TBoldTemplateHolder.SetFileName(const Value: String);
begin
  FFileName := Value;
end;

procedure TBoldTemplateVariables.InitializeDateTimeMacros;
begin
  SetVariable('DATETIME', DateTimeToStr(now));
  SetVariable('DATE', DateToStr(now));
  SetVariable('TIME', TimeToStr(now));
end;

procedure TBoldTemplateHolder.TemplateChanged(sender: TObject);
begin
  while (fTemplate.Count > 0) and (trim(fTemplate[fTemplate.Count - 1]) = '') do
    fTemplate.Delete(fTemplate.Count - 1);
end;

{ TBoldTemplatevariable }

constructor TBoldTemplatevariable.create(const name, value: string; Flags: TBoldVariableFlags);
begin
  fFlags := flags;
  fName := Name;
  fValue := Value;
end;

{ TBoldTemplatevariableIndex }

function TBoldTemplatevariableIndex.ItemAsKeyString(Item: TObject): string;
begin
  result := TBoldTemplatevariable(Item).Name;
end;



{ TBoldTemplateList }

function TBoldTemplateList.GetItems(index: integer): TBoldTemplateHolder;
begin
  result := TBoldTemplateHolder(inherited items[index]);
end;

end.
