{ Global compiler directives }
{$include bold.inc}
unit BoldIndexCollection;

interface

uses
  Classes;

type
  TBoldIndexCollection = class;
  TBoldIndexCollectionClass = class of TBoldIndexCollection;

  TBoldIndexDefintion = class(TCollectionItem)
  strict private
    FTableName: String;
    fColumns: String;
    fUnique: Boolean;
    fRemove: Boolean;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AsString: string read GetAsString write SetAsString;
    function Equals(value: TBoldIndexDefintion): Boolean; reintroduce;
  published
    property TableName: String read FTableName write FTableName;
    property Columns: String read fColumns write fColumns;
    property Unique: Boolean read fUnique write fUnique;
    property Remove: Boolean read fRemove write fRemove;
  end;

  TBoldIndexCollection = class(TCollection)
  private
    fOwner: TComponent;
    function GetDefinition(Index: integer): TBoldIndexDefintion;
    procedure SaveToStringList(StrList: TStringList);
    procedure LoadFromStringList(StrList: TStringList);
  protected
   function GetOwner: TPersistent; override;
  public
    constructor Create(Owner: TComponent);
    property IndexDefinition[Index: integer]: TBoldIndexDefintion read GetDefinition; default;
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    function AddIndexDefintion: TBoldIndexDefintion;
  end;


implementation

uses
  SysUtils,
  BoldDefs,
  BoldNameExpander,
  BoldTaggedValueSupport;

{ TBoldTypeNameDictionary }

function TBoldIndexCollection.AddIndexDefintion: TBoldIndexDefintion;
begin
  result := Add as TBoldIndexDefintion;
end;

constructor TBoldIndexCollection.Create(Owner: TComponent);
begin
  inherited Create(TBoldIndexDefintion);
  fOwner := Owner;
end;

function TBoldIndexCollection.GetDefinition(
  Index: integer): TBoldIndexDefintion;
begin
  result := GetItem(index) as TBoldIndexDefintion;
end;

function TBoldIndexCollection.GetOwner: TPersistent;
begin
  result := fOwner;
end;

procedure TBoldIndexCollection.LoadFromFile(const FileName: String);
var
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  try
    StrList.LoadFromFile(FileName);
    LoadFromStringList(StrList);
  finally
    StrList.Free;
  end;
end;

procedure TBoldIndexCollection.LoadFromStringList(StrList: TStringList);
var
  i: integer;
begin
  Clear;
  for i := 0 to StrList.Count-1 do
    if trim(StrList[i]) <> '' then
    begin
      Add;
      IndexDefinition[Count-1].AsString := Trim(StrList[i]);
    end;
end;

procedure TBoldIndexCollection.SaveToFile(const FileName: String);
var
  StrList: TStringLIst;
begin
  StrList := TStringList.Create;
  try
    SaveToStringList(StrList);
    StrList.SaveToFile(FileName);
  finally
    StrList.Free;
  end;
end;

procedure TBoldIndexCollection.SaveToStringList(StrList: TStringList);
var
  i: integer;
begin
  for i := 0 to Count-1 do
    StrList.Add(IndexDefinition[i].AsString);
end;

{ TBoldTypeNameMapping }

function TBoldIndexDefintion.Equals(value: TBoldIndexDefintion): Boolean;
begin
  result := (TableName = value.TableName) and (Columns = value.Columns) and
  (Unique = value.Unique);

end;

function TBoldIndexDefintion.GetAsString: string;
begin
  Result := Format('TableName=%s,Columns=%s',
                    [TableName,
                     Columns]);
end;

function TBoldIndexDefintion.GetDisplayName: string;
begin
  Result := Format('%s(%s)', [TableName, Columns]);
  if Unique then
    Result := Result + '[U]';
end;

procedure TBoldIndexDefintion.SetAsString(const Value: string);
begin
  with TStringList.Create do
  try
    CommaText       := value;
    TableName       := Values['TableName'];
    Columns        := Values['Columns'];
  finally
    Free;
  end;
end;

procedure TBoldIndexDefintion.AssignTo(Dest: TPersistent);
begin
  if dest is TBoldIndexDefintion then
    with dest as TBoldIndexDefintion do begin
      TableName := self.TableName;
      Columns := self.Columns;
      Unique := self.Unique;
      Remove := self.remove;
    end
  else
   inherited;
end;

end.
