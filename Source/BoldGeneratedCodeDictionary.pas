
{ Global compiler directives }
{$include bold.inc}
unit BoldGeneratedCodeDictionary;

interface

uses
  BoldIndexableList;

type
  TBoldGeneratedCodeList = class;
  TBoldGeneratedCodeDescriptor = class;
  TBoldGeneratedClassList = class;
  TBoldGeneratedClassDescriptor = class;

  TInstallBusinessClasses = procedure (BoldObjectClasses: TBoldGeneratedClassList);
  TInstallObjectListClasses = procedure (BoldObjectListClasses: TBoldGeneratedClassList);


  { TBoldGeneratedCodeList }
  TBoldGeneratedCodeList = class(TBoldIndexableList)
  private
    function GetDescriptorByExpressionName(const ExpressionName: string): TBoldGeneratedCodeDescriptor;
    function GetModelDescriptors(index: Integer): TBoldGeneratedCodeDescriptor;
  public
    constructor Create;
    function AddGeneratedCodeDescriptorWithFunc(const ExpressionName: string; InstBusiClasses: TInstallBusinessClasses; InstObjListClasses: TInstallObjectListClasses; CRC: string = ''):TBoldGeneratedCodeDescriptor;
    property DescriptorByExpressionName[const ExpressionName: string]: TBoldGeneratedCodeDescriptor read GetDescriptorByExpressionName;
    property ModelEntries[index: Integer]: TBoldGeneratedCodeDescriptor read GetModelDescriptors;
  end;

  { TBoldGeneratedCodeDescriptor }
  TBoldGeneratedCodeDescriptor = class
  private
    fExpressionName: string;
    fCRC: string;
    fInstallBusinessClasses: TInstallBusinessClasses;
    fInstallObjectListClasses: TInstallObjectListClasses;
  public
    constructor Create(const ExpressionName: string; InstallBusinessClasses: TInstallBusinessClasses; InstallObjectListClasses: TInstallObjectListClasses; CRC: string);
    property InstallBusinessClasses: TInstallBusinessClasses read fInstallBusinessClasses;
    property InstallObjectListClasses: TInstallObjectListClasses read fInstallObjectListClasses;
    property ExpressionName: string read fExpressionName;
    property CRC: string read fCRC;
  end;

  { TBoldGeneratedClassList }
  TBoldGeneratedClassList = class(TBoldIndexableList)
  private
    function GetDescriptorByExpressionName(const ExpressionName: string): TBoldGeneratedClassDescriptor;
  public
    constructor Create;
    procedure AddEntry(BoldObjectClassEntry: TBoldGeneratedClassDescriptor);
    procedure AddObjectEntry(const ExpressionName: string; AClass: TClass);
    property EntryByExpressionName[const ExpressionName: string]: TBoldGeneratedClassDescriptor read GetDescriptorByExpressionName;
  end;

  { TBoldGeneratedClassDescriptor }
  TBoldGeneratedClassDescriptor = class
  private
    fExpressionName: string;
    fClass: TClass;
  public
    constructor Create(const ExpressionName: string; AClass: TClass);
    property ExpressionName: string read fExpressionName;
    property TheClass: TClass read fClass;
  end;

function GeneratedCodes: TBoldGeneratedCodeList;
function BoldGeneratedCodesAssigned: Boolean;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldHashIndexes,
  BoldIndex
  ;

var
  G_BoldGeneratedCodes: TBoldGeneratedCodeList = nil;
  IX_GeneratedClassExpressionName: integer = -1;
  IX_GeneratedCodeExpressionName: integer = -1;
  
type
  { TGeneratedCodeExpressionNameIndex }
  TGeneratedCodeExpressionNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;
  
function TGeneratedCodeExpressionNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldGeneratedCodeDescriptor(Item).ExpressionName;
end;

{--Global access methods---}
function GeneratedCodes: TBoldGeneratedCodeList;
begin
  if not Assigned(G_BoldGeneratedCodes) then
    raise EBoldInternal.Create('GeneratedCodes: BoldGeneratedCodeDictionary not initialized');
  Result := G_BoldGeneratedCodes;
end;

function BoldGeneratedCodesAssigned: Boolean;
begin
  Result := Assigned(G_BoldGeneratedCodes);
end;

{ TBoldGeneratedCodeList }
constructor TBoldGeneratedCodeList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_GeneratedCodeExpressionName, AddIndex(TGeneratedCodeExpressionNameIndex.Create));
end;

function TBoldGeneratedCodeList.GetDescriptorByExpressionName(const ExpressionName: string): TBoldGeneratedCodeDescriptor;
begin
  Result := TBoldGeneratedCodeDescriptor(TGeneratedCodeExpressionNameIndex(Indexes[IX_GeneratedCodeExpressionName]).FindByString(ExpressionName))
end;

function TBoldGeneratedCodeList.GetModelDescriptors(index: Integer): TBoldGeneratedCodeDescriptor;
begin
  Result := TBoldGeneratedCodeDescriptor(Items[index]);
end;

function TBoldGeneratedCodeList.AddGeneratedCodeDescriptorWithFunc(const ExpressionName: string; InstBusiClasses: TInstallBusinessClasses; InstObjListClasses: TInstallObjectListClasses; CRC: string = ''): TBoldGeneratedCodeDescriptor;
begin
  result := TBoldGeneratedCodeDescriptor.Create(ExpressionName, InstBusiClasses, InstObjListClasses, CRC);
  Add(result);
end;

{ TBoldGeneratedCodeDescriptor }
constructor TBoldGeneratedCodeDescriptor.Create(const ExpressionName: string; InstallBusinessClasses: TInstallBusinessClasses; InstallObjectListClasses: TInstallObjectListClasses; CRC: String);
begin
  fExpressionName := ExpressionName;
  fInstallBusinessClasses := InstallBusinessClasses;
  fInstallObjectListClasses := InstallObjectListClasses;
  fCRC := CRC;
end;

type
  { TGeneratedClassExpressionNameIndex }
  TGeneratedClassExpressionNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

function TGeneratedClassExpressionNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldGeneratedClassDescriptor(Item).ExpressionName;
end;

{ TBoldGeneratedClassList }

constructor TBoldGeneratedClassList.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_GeneratedClassExpressionName, AddIndex(TGeneratedClassExpressionNameIndex.Create));
end;

function TBoldGeneratedClassList.GetDescriptorByExpressionName(const ExpressionName: string): TBoldGeneratedClassDescriptor;
begin
  Result := TBoldGeneratedClassDescriptor(TGeneratedClassExpressionNameIndex(Indexes[IX_GeneratedClassExpressionName]).FindByString(ExpressionName));
end;

procedure TBoldGeneratedClassList.AddEntry(BoldObjectClassEntry: TBoldGeneratedClassDescriptor);
begin
  Add(BoldObjectClassEntry);
end;

procedure TBoldGeneratedClassList.AddObjectEntry(const ExpressionName: string; AClass: TClass);
begin
  AddEntry(TBoldGeneratedClassDescriptor.Create(ExpressionName, AClass));
end;

{ TBoldObjectClassEntry }
constructor TBoldGeneratedClassDescriptor.Create(const ExpressionName: string; AClass: TClass);
begin
  fClass := AClass;
  fExpressionName := ExpressionName;
end;

initialization
  G_BoldGeneratedCodes := TBoldGeneratedCodeList.Create;

finalization
  FreeAndNil(G_BoldGeneratedCodes);

end.
