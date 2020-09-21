unit BoldHashIndexes;

interface

uses
  Classes,
  BoldIndex;

type

  TBoldStringCompareMode = (bscCaseDependent, bscCaseIndependent, bscLocaleCaseIndependent);
  TBoldStringKey = class;
  TBoldStringHashIndex = class;
  TBoldCaseSensitiveStringHashIndex = class;
  TBoldObjectHashIndex = class;
  TBoldClassHashIndex = class;
  TBoldCardinalHashIndex = class;

  {---TBoldStringKey---}
  TBoldStringKey = class
  public
    class function HashString(const KeyString: String; CompareMode: TBoldStringCompareMode): Cardinal;
    class function HashBuffer(P: PChar; length: integer; CompareMode: TBoldStringCompareMode): Cardinal;
  end;

  {---TBoldStringHashIndex---}
  TBoldStringHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
   public
    function FindByString(const KeyString: string): TObject;
    procedure FindAllByString(const KeyString: string; List: TList);
  end;

  {---TBoldCaseSensitiveStringHashIndex---}
  TBoldCaseSensitiveStringHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
  public
    function FindByString(const KeyString: string): TObject;
  end;

  {---TBoldObjectHashIndex---}
  TBoldObjectHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyObject(Item: TObject): TObject; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    class function HashObject(KeyObject: TObject): Cardinal;
    function FindByObject(KeyObject: TObject): TObject;
    procedure FindAllByObject(const KeyObject: TObject; List: TList);
  end;

  {---TBoldClassHashIndex---}
  TBoldClassHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyClass(O: TObject): TClass; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    class function HashClass(KeyClass: TClass): Cardinal;
    function FindByClass(KeyClass: TClass): TObject;
 end;

   {---TBoldGuidHashIndex---}
  TBoldGUIDHashIndex = class(TBoldHashIndex)
  private
    function GuidEqual(const Guid1, Guid2: TGuid): Boolean;
  protected
    function ItemASKeyGUID(O: TObject): TGUID; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function HashGUID(const KeyGUID: TGUID): Cardinal;
    function FindByGUID(const KeyGUID: TGUID): TObject;
 end;

  {---TBoldCardinalHashIndex---}
  TBoldCardinalHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyCardinal(Item: TObject): cardinal; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function FindByCardinal(const KeyCardinal: Cardinal): TObject;
  end;


implementation

uses
  Windows,  // CharUpperBuff, fixme move to BoldUtils.
  SysUtils,
  BoldUtils;

class function TBoldStringKey.HashBuffer(P: PChar; Length: integer; CompareMode: TBoldStringCompareMode): Cardinal;
  function LIHash: Cardinal; // Separate function to avoid string handling overhead in main func
  var
    PUpper: PChar;
  begin
    GetMem(PUpper, Length);
    try
      Move(P^, PUpper^, Length);
      CharUpperBuff(PUpper, Length);  // FIXME provide method in BoldUtils
      Result := HashBuffer(PUpper, Length, bscCaseDependent);
    finally
      Freemem(PUpper);
    end;
  end;
var
  i: integer;
begin
   Result := 0;
   case CompareMode of
     bscCaseDependent:
       for i := 0 to Length-1 do
         Result := ((Result shl 3) and 2147483647) or
                   (Result shr (32-3)) xor ord(P[i]);
     bscCaseIndependent:
       for i := 0 to Length-1 do
         Result := ((Result shl 3) and 2147483647) or
                   (Result shr (32-3)) xor (ord(P[i]) or 32);
    else
      Result := LIHash;
   end;
end;

class function TBoldStringKey.HashString(const KeyString: String; CompareMode: TBoldStringCompareMode): Cardinal;
begin
  Result := HashBuffer(PChar(KeyString), Length(KeyString), Comparemode);
end;

{---TBoldStringHashIndex---}
function TBoldStringHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := TBoldStringKey.Hashstring(ItemAsKeyString(Item), bscLocaleCaseIndependent);
end;

function TBoldStringHashIndex.Match(const Key; Item:TObject):Boolean;
begin
  Result :=  BoldAnsiEqual(ItemAsKeyString(Item), string(Key));
end;

function TBoldStringHashIndex.FindByString(const KeyString: string): TObject;
begin
  Result := Find(KeyString);
end;

{---TBoldCaseSensitiveStringHashIndex---}
function TBoldCaseSensitiveStringHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := TBoldStringKey.Hashstring(ItemAsKeyString(Item), bscCaseDependent);
end;

function TBoldCaseSensitiveStringHashIndex.Match(const Key; Item:TObject):Boolean;
begin
 Result := ItemAsKeyString(Item) = string(Key);
end;

function TBoldCaseSensitiveStringHashIndex.FindByString(const KeyString: string): TObject;
begin
  Result := Find(KeyString);
end;

{---TBoldObjectHashIndex---}
function TBoldObjectHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := HashObject(ItemAsKeyObject(Item));
end;

function TBoldObjectHashIndex.Match(const Key; Item:TObject):Boolean;
begin
  Result := TObject(Key) = ItemAsKeyObject(Item);
end;

function TBoldObjectHashIndex.FindByObject(KeyObject: TObject): TObject;
begin
  Result := Find(KeyObject);
end;

{---TBoldClassHashIndex---}
function TBoldClassHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := HashClass(ItemAsKeyClass(Item));
end;

function TBoldClassHashIndex.Match(const Key; Item: TObject):Boolean;
begin
  Result := TClass(Key) = ItemAsKeyClass(Item);
end;

function TBoldClassHashIndex.FindByClass(KeyClass: TClass): TObject;
begin
  Result := Find(KeyClass);
end;

{---TBoldGUIDHashIndex---}
function TBoldGUIDHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := HashGUID(ItemAsKeyGUID(Item));
end;

function TBoldGUIDHashIndex.GuidEqual(const Guid1, Guid2: TGuid): Boolean;
var
  i: integer;
begin
  result := (Guid1.D1 = Guid2.D1) and
            (Guid1.D2 = Guid2.D2) and
            (Guid1.D3 = Guid2.D3);
  for i := 0 to 7 do
    result := result and (Guid1.D4[i] = Guid2.D4[i]);
end;


function TBoldGUIDHashIndex.Match(const Key; Item: TObject):Boolean;
begin
  Result := GuidEqual(TGUID(Key), ItemAsKeyGUID(Item));
end;

function TBoldGUIDHashIndex.FindByGUID(const KeyGUID: TGUID): TObject;
begin
  Result := Find(KeyGUID);
end;

{---TBoldCardinalHashIndex---}
function TBoldCardinalHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := ItemAsKeyCardinal(Item);
end;

function TBoldCardinalHashIndex.Match(const Key; Item: TObject): Boolean;
begin
  Result := Cardinal(Key) = ItemAsKeyCardinal(Item);
end;

function TBoldCardinalHashIndex.FindByCardinal(const KeyCardinal: Cardinal): TObject;
begin
  Result := Find(KeyCardinal);
end;

procedure TBoldStringHashIndex.FindAllByString(const KeyString: string; List: TList);
begin
  FindAll(KeyString, List);
end;

procedure TBoldObjectHashIndex.FindAllByObject(const KeyObject: TObject; List: TList);
begin
  FindAll(KeyObject, List);
end;

function TBoldStringHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldStringKey.HashString(string(Key), bscLocaleCaseIndependent);
end;

function TBoldCaseSensitiveStringHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldStringKey.HashString(string(Key), bscCaseDependent);
end;

function TBoldObjectHashIndex.Hash(const Key): Cardinal;
begin
   Result := Cardinal(Key);
end;

function TBoldClassHashIndex.Hash(const Key): Cardinal;
begin
  Result := Cardinal(Key);
end;

function TBoldGUIDHashIndex.Hash(const Key): Cardinal;
begin
  Result := HashGUid(TGuid(Key));
end;

function TBoldCardinalHashIndex.Hash(const Key): Cardinal;
begin
  Result := Cardinal(Key);
end;

class function TBoldObjectHashIndex.HashObject(
  KeyObject: TObject): Cardinal;
begin
  Result := Cardinal(KeyObject);
end;

class function TBoldClassHashIndex.HashClass(KeyClass: TClass): Cardinal;
begin
  Result := Cardinal(KeyClass);
end;

function TBoldGUIDHashIndex.HashGUID(const KeyGUID: TGUID): Cardinal;
var
  I: integer;
begin
  with KeyGUID do
  begin
    Result := D1 xor d2 xor (d3 shl 8);
    for i := 0 to 7 do
      result := result xor D4[I];
  end;
end;

end.
