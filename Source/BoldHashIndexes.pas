
{ Global compiler directives }
{$include bold.inc}
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
    class function HashString(const KeyString: String; CompareMode: TBoldStringCompareMode): Cardinal;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    class function HashBuffer(const P: PChar; const Length: Integer; const CompareMode: TBoldStringCompareMode): Cardinal;
  end;

  {---TBoldStringHashIndex---}
  TBoldStringHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
   public
    function FindByString(const KeyString: string): TObject; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure FindAllByString(const KeyString: string; List: TList); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  end;

  {---TBoldCaseSensitiveStringHashIndex---}
  TBoldCaseSensitiveStringHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
  public
    function FindByString(const KeyString: string): TObject;  {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  end;

  {---TBoldObjectHashIndex---}
  TBoldObjectHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyObject(Item: TObject): TObject; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    class function HashObject(KeyObject: TObject): Cardinal; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  public    
    function FindByObject(KeyObject: TObject): TObject; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
    procedure FindAllByObject(const KeyObject: TObject; List: TList); {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  end;

  {---TBoldClassHashIndex---}
  TBoldClassHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyClass(O: TObject): TClass; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    class function HashClass(KeyClass: TClass): Cardinal; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  public
    function FindByClass(KeyClass: TClass): TObject; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
 end;

   {---TBoldGuidHashIndex---}
  TBoldGUIDHashIndex = class(TBoldHashIndex)
  private
    function GuidEqual(const Guid1, Guid2: TGuid): Boolean;
  protected
    function ItemASKeyGUID(Item: TObject): TGUID; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function HashGUID(const KeyGUID: TGUID): Cardinal; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  public
    function FindByGUID(const KeyGUID: TGUID): TObject; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
 end;

  {---TBoldCardinalHashIndex---}
  TBoldCardinalHashIndex = class(TBoldHashIndex)
  protected
    function ItemAsKeyCardinal(Item: TObject): cardinal; virtual; abstract;
    function HashItem(Item: TObject): Cardinal; override;
    function Match(const Key; Item:TObject):Boolean; override;
    function Hash(const Key): Cardinal; override;
    function FindByCardinal(const KeyCardinal: Cardinal): TObject; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
  end;


implementation

uses
  Windows,
  BoldUtils,
  BoldRev;

{$IFDEF USEGLOBALCHARBUFFER}
const
  InitialBufferSize = 256;
var
  _PBuffer: PChar;
  _PBufferLength: integer = InitialBufferSize;
{$ENDIF}

class function TBoldStringKey.HashBuffer(const P: PChar; const Length: Integer; const CompareMode: TBoldStringCompareMode): Cardinal;
{$IFNDEF USEGLOBALCHARBUFFER}
  function LIHash: Cardinal; // Separate function to avoid string handling overhead in main func
  var
    PUpper: PChar;
  begin
    GetMem(PUpper, Length * SizeOf(Char));
    try
      Move(P^, PUpper^, Length * SizeOf(Char));
      CharUpperBuff(PUpper, Length);  // FIXME provide method in BoldUtils
      Result := HashBuffer(PUpper, Length, bscCaseDependent);
    finally
      Freemem(PUpper);
    end;
  end;
{$ENDIF}
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
      {$IFDEF USEGLOBALCHARBUFFER}
      if Length > _PBufferLength then begin
        Freemem(_PBuffer);
        GetMem(_PBuffer, Length * SizeOf(Char));
        _PBufferLength := Length;
      end;
      Move(P^, _PBuffer^, Length * SizeOf(Char));
      CharUpperBuff(_PBuffer, Length);  // FIXME provide method in BoldUtils
      Result := HashBuffer(_PBuffer, Length, bscCaseDependent);
      {$ELSE}
      Result := LIHash;
      {$ENDIF}
  end;
end;

class function TBoldStringKey.HashString(const KeyString: String; CompareMode: TBoldStringCompareMode): Cardinal;
begin
  Result := HashBuffer(PChar(KeyString), Length(KeyString), Comparemode);
end;

{---TBoldStringHashIndex---}

function TBoldStringHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldStringKey.HashString(string(Key), bscLocaleCaseIndependent);
end;

function TBoldStringHashIndex.HashItem(Item: TObject): Cardinal;
begin
  Result := TBoldStringKey.Hashstring(ItemAsKeyString(Item), bscLocaleCaseIndependent);
end;

function TBoldStringHashIndex.Match(const Key; Item:TObject):Boolean;
begin
  Result := BoldAnsiEqual(ItemAsKeyString(Item), string(Key));
end;

function TBoldStringHashIndex.FindByString(const KeyString: string): TObject;
begin
  Result := Find(KeyString);
end;

{---TBoldCaseSensitiveStringHashIndex---}

function TBoldCaseSensitiveStringHashIndex.Hash(const Key): Cardinal;
begin
  Result := TBoldStringKey.HashString(string(Key), bscCaseDependent);
end;

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

class function TBoldObjectHashIndex.HashObject(KeyObject: TObject): Cardinal;
begin
  Result := Cardinal(KeyObject);
end;

function TBoldObjectHashIndex.Hash(const Key): Cardinal;
begin
   Result := Cardinal(Key);
end;

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

class function TBoldClassHashIndex.HashClass(KeyClass: TClass): Cardinal;
begin
  Result := Cardinal(KeyClass);
end;

function TBoldClassHashIndex.Hash(const Key): Cardinal;
begin
  Result := Cardinal(Key);
end;

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

function TBoldGUIDHashIndex.Hash(const Key): Cardinal;
begin
  Result := HashGUid(TGuid(Key));
end;

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

function TBoldCardinalHashIndex.Hash(const Key): Cardinal;
begin
  Result := Cardinal(Key);
end;

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

initialization
{$IFDEF USEGLOBALCHARBUFFER}
  GetMem(_PBuffer, InitialBufferSize * SizeOf(Char));
{$ENDIF}
finalization
{$IFDEF USEGLOBALCHARBUFFER}
  Freemem(_PBuffer);
  _PBufferLength := 0;
{$ENDIF}

end.

