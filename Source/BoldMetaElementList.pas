{ Global compiler directives }
{$include bold.inc}
unit BoldMetaElementList;

interface

uses
  BoldElements,
  BoldIndexableList,
  BoldHashIndexes,
  SysUtils
  ;

type
  { forward declarations }
  TBoldMetaElementList = class;
  TBoldElementTypeInfoList = class;

  TBoldMetaElementListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldMetaElement;
    property Current: TBoldMetaElement read GetCurrent;
  end;

  { TBoldMetaElementList }
  TBoldMetaElementList = class(TBoldIndexableList)
  private
    function GetItem(index: Integer): TBoldMetaElement;
    function GetItemByExpressionName(const ExpressionName: string): TBoldMetaElement;
    function GetItemByDelphiName(const DelphiName: string): TBoldMetaElement;
    function GetItemByModelName(const ModelName: string): TBoldMetaElement;
    class var IX_ExpressionName: integer;
    class var IX_DelphiName: integer;
    class var IX_ModelName: integer;
  public
    constructor Create;
    function GetEnumerator: TBoldMetaElementListTraverser;
    procedure Add(Item: TBoldMetaElement);
    property Items[index: Integer]: TBoldMetaElement read GetItem; default;
    property ItemsByExpressionName[const ExpressionName: string]: TBoldMetaElement read GetItemByExpressionName;
    property ItemsByDelphiName[const ExpressionName: string]: TBoldMetaElement read GetItemByDelphiName;
    property ItemsByModelName[const ModelName: string]: TBoldMetaElement read GetItemByModelName;
  end;

  TBoldElementTypeInfoListTraverser = class(TBoldIndexableListTraverser)
  public
    function GetCurrent: TBoldElementTypeInfo;
    property Current: TBoldElementTypeInfo read GetCurrent;
  end;

  { TBoldElementTypeInfoList }
  TBoldElementTypeInfoList = class(TBoldMetaElementList)
  private
    function GetItem(index: Integer): TBoldElementTypeInfo;
    function GetItemByExpressionName(const ExpressionName: string): TBoldElementTypeInfo;
    function GetItemByDelphiName(const DelphiName: string): TBoldElementTypeInfo;
    function GetItemByModelName(const ModelName: string): TBoldElementTypeInfo;
  public
    function GetEnumerator: TBoldElementTypeInfoListTraverser;
    procedure Add(Item: TBoldElementTypeInfo);
    property Items[index: Integer]: TBoldElementTypeInfo read GetItem; default;
    property ItemsByExpressionName[const ExpressionName: string]: TBoldElementTypeInfo read GetItemByExpressionName;
    property ItemsByDelphiName[const ExpressionName: string]: TBoldElementTypeInfo read GetItemByDelphiName;
    property ItemsByModelName[const ModelName: string]: TBoldElementTypeInfo read GetItemByModelName;
  end;

implementation

uses
  BoldIndex,
  BoldDefs
  ;

type
  {---TExpressionNameIndex---}
  TBoldValueTypeNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  {---TDelphiNameIndex---}
  TDelphiNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  TModelNameIndex = class(TBoldCaseSensitiveStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

{---TExpressionNameIndex---}
function TBoldValueTypeNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(item is TBoldMetaElement, 'Element is no MetaElement');
  Result := TBoldMetaElement(Item).ExpressionName;
end;

{---TDelphiNameIndex---}
function TDelphiNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(item is TBoldMetaElement, 'Element is no MetaElement');
  Result := TBoldMetaElement(Item).DelphiName;
end;

function TModelNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  assert(item is TBoldMetaElement, 'Element is no MetaElement');
  Result := TBoldMetaElement(Item).ModelName;
end;

{---TBoldMetaElementList---}
constructor TBoldMetaElementList.Create;
begin
  inherited;
  SetIndexCapacity(3);
  SetIndexVariable(IX_ExpressionName, AddIndex(TBoldValueTypeNameIndex.Create));
  SetIndexVariable(IX_DelphiName, AddIndex(TDelphiNameIndex.Create));
  SetIndexVariable(IX_ModelName, AddIndex(TModelNameIndex.Create));
end;

function TBoldMetaElementList.GetEnumerator: TBoldMetaElementListTraverser;
begin
  result := CreateTraverser as TBoldMetaElementListTraverser;
end;

function TBoldMetaElementList.GetItem(index: Integer): TBoldMetaElement;
begin
  Result := TBoldMetaElement(inherited Items[index]);
end;

function TBoldMetaElementList.GetItemByExpressionName(const ExpressionName: string): TBoldMetaElement;
begin
  Result := TBoldMetaElement(TBoldStringHashIndex(Indexes[IX_ExpressionName]).FindByString(ExpressionName));
end;

function TBoldMetaElementList.GetItemByDelphiName(const DelphiName: string): TBoldMetaElement;
begin
  Result := TBoldMetaElement(TBoldStringHashIndex(Indexes[IX_DelphiName]).FindByString(DelphiName));
end;

procedure TBoldMetaElementList.Add(Item: TBoldMetaElement);
begin
  inherited Add(Item);
end;

function TBoldMetaElementList.GetItemByModelName(const ModelName: string): TBoldMetaElement;
begin
  Result := TBoldMetaElement(TBoldStringHashIndex(Indexes[IX_ModelName]).FindByString(ModelName));
end;

{ TBoldElementTypeInfoList }

procedure TBoldElementTypeInfoList.Add(Item: TBoldElementTypeInfo);
begin
  inherited add(item);
end;

function TBoldElementTypeInfoList.GetEnumerator: TBoldElementTypeInfoListTraverser;
begin
  result := CreateTraverser as TBoldElementTypeInfoListTraverser;
end;

function TBoldElementTypeInfoList.GetItem(index: Integer): TBoldElementTypeInfo;
begin
  result := TBoldElementTypeInfo(inherited GetItem(index));
end;

function TBoldElementTypeInfoList.GetItemByDelphiName(const DelphiName: string): TBoldElementTypeInfo;
begin
  result := TBoldElementTypeInfo(inherited GetItemByDelphiName(DelphiName));
end;

function TBoldElementTypeInfoList.GetItemByExpressionName(const ExpressionName: string): TBoldElementTypeInfo;
begin
  result := TBoldElementTypeInfo(inherited GetItemByExpressionName(ExpressionName));
end;

function TBoldElementTypeInfoList.GetItemByModelName(const ModelName: string): TBoldElementTypeInfo;
begin
  result := TBoldElementTypeInfo(inherited GetItemByModelName(ModelName));
end;

{ TBoldMetaElementListTraverser }

function TBoldMetaElementListTraverser.GetCurrent: TBoldMetaElement;
begin
  result := inherited GetItem as TBoldMetaElement;
end;

{ TBoldElementTypeInfoListTraverser }

function TBoldElementTypeInfoListTraverser.GetCurrent: TBoldElementTypeInfo;
begin
  result := inherited GetItem as TBoldElementTypeInfo;
end;


initialization
  TBoldMetaElementList.IX_ExpressionName := -1;
  TBoldMetaElementList.IX_DelphiName := -1;
  TBoldMetaElementList.IX_ModelName := -1;

end.

