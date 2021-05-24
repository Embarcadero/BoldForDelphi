
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLDTDData;

interface

uses
  SysUtils, Classes, BoldTemplateExpander,
  BoldStringList;

type
  TdmUML13DTD = class(TDataModule)
    UML13DTD: TBoldTemplateHolder;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  TBoldUMLDTDReader = class
  private
    fLookup: TBoldStringList;
    fCurrentElementName: string;
    fCurrentCps: string;
    procedure StartElement(Name: string);
    procedure EndElement;
    procedure FoundCpName(Name: string);
    function IsClass(Name: string): Boolean;
  public
    constructor CreateFromFile(Name: string);
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(Name: string);
    function MembersOfClass(ClassName: string): string;
  end;


var
  dmUML13DTD: TdmUML13DTD;

implementation

{$R *.dfm}

uses
  BoldUtils,
  BoldDTDParser;

{ TBoldUMLDTDReader }

constructor TBoldUMLDTDReader.Create;
var
  dm: TdmUML13DTD;
begin
  fLookup := TBoldStringList.Create;
  dm := TdmUML13DTD.Create(nil);
  try
    fLookup.Text := dm.UML13DTD.Template.Text;
  finally
    dm.Free;
  end;
  fLookup.Sort;
end;

constructor TBoldUMLDTDReader.CreateFromFile(Name: string);
var
  parser: TBoldDTDParser;
  dtd: TStrings;
begin
  fLookup := TBoldStringList.Create;
  parser := TBoldDTDParser.Create;
  parser.OnStartElement := StartElement;
  parser.OnEndElement := EndElement;
  parser.OnFoundCpName := FoundCpName;

  dtd := TStringList.Create;
  dtd.LoadFromFile(Name);
  parser.Parse(dtd.Text);
  fLookup.Sort;
end;

destructor TBoldUMLDTDReader.Destroy;
begin
  FreeAndNil(fLookup);
  inherited;
end;

procedure TBoldUMLDTDReader.EndElement;
begin
  assert(fCurrentElementName <> '');
  if IsClass(fCurrentElementName) then
    fLookup.FastValues[fCurrentElementName] := fCurrentCps;                                        
  fCurrentElementName := '';
  fCurrentCps := '';
end;

procedure TBoldUMLDTDReader.FoundCpName(Name: string);
begin
  assert(fCurrentElementName <> '');
  if fCurrentCps <> '' then
    fCurrentCps := fCurrentCps + ', ';
  fCurrentCps := fCurrentCps + Name;
end;

function TBoldUMLDTDReader.IsClass(Name: string): Boolean;
begin
  Result := CharInSet(Name[LastDelimiter('.', Name)+1], ['A'..'Z']);
end;

function TBoldUMLDTDReader.MembersOfClass(ClassName: string): string;
begin
  result := fLookup.FastValues[ClassName];
end;

procedure TBoldUMLDTDReader.SaveToFile(Name: string);
begin
  fLookup.SaveToFile(Name);
end;

procedure TBoldUMLDTDReader.StartElement(Name: string);
begin
  assert(fCurrentElementName = '');
  fCurrentElementName := Name;
end;

end.
