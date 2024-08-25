
{ Global compiler directives }
{$include bold.inc}
unit BoldRegistry;

interface

uses
  Windows,
  Registry;

{** Usage (kala 990714)
 *
 * Create an instance of TBoldRegistry.
 * Set the desired registrymode, rmDesigntime or rmRuntime.
 * Call OpenKey with the "Reg" value set to only your specific subkey
 * for example '\UMLModel'.
 * Write Your values.
 * Remember to call CloseKey after You are finished.
 * Free the instance.
 * (See unit BoldUMLModelEditForm.pas for exaple of usage                  *
 *                                                                        **
}

type
  { forward declarations }
  TBoldRegistry = class;

  TRegistryMode = (rmDesigntime, rmRuntime);

  { TBoldRegistry }
  TBoldRegistry = class(TObject)
  private
    fRegistry: TRegistry;
    fRegistryMode: TRegistryMode;
  public
    constructor Create;
    destructor Destroy; override;
    function OpenKey(const Key: String): Boolean;
    function OpenNotDefaultKey(const Key: String): Boolean;
    procedure CloseKey;
    function ReadString(const Name: String; DefaultValue: String): String;
    function ReadBool(const Name: String; DefaultValue: Boolean): Boolean;
    function ReadInteger(const Name: String; DefaultValue: Integer): Integer;
    procedure WriteString(const Name: String; Value: String);
    procedure WriteBool(const Name: String; Value: Boolean);
    procedure WriteInteger(const Name: String; Value: Integer);
    function ValueExists(const Name: String): Boolean;
    function DeleteKey(const Key: String): Boolean;
    property RegistryMode: TRegistryMode read fRegistryMode write fRegistryMode;
  end;

function GetBoldLibPath: string;
function GetBoldResPath: string;
function GetBoldBasePath: string;


const
  RegPathExtention: array[0..1] of String = ('\Designtime', '\Runtime');

implementation

uses
  SysUtils,
  BoldGuard,
  BoldUtils;

{ TBoldRegistry }

function GetBoldBasePath: string;
var
  Reg: TRegistry;
  Guard: IBoldGuard;
begin
  Guard := TBoldguard.Create(Reg);
  Reg := TRegistry.Create;
  result := '';
  Reg.RootKey := HKEY_LOCAL_MACHINE;
  if Reg.OpenKey(BoldRootRegistryKey, False) then
    result := Reg.ReadString('RootDir');
end;

function GetBoldLibPath: string;
begin
  result := GetBoldBasePath;
  if result <> '' then
    result := result + '\Lib';
end;

function GetBoldResPath: string;
begin
  result := GetBoldBasePath;
  if result <> '' then
    result := result + '\Res';
end;


procedure TBoldRegistry.CloseKey;
begin
  fRegistry.CloseKey;
end;

constructor TBoldRegistry.Create;
begin
  inherited Create;
  fRegistry := TRegistry.Create;
  fRegistryMode := rmDesigntime;
end;

function TBoldRegistry.DeleteKey(const Key: String): Boolean;
begin
  Result := fRegistry.DeleteKey(Key);
end;

destructor TBoldRegistry.Destroy;
begin
  FreeAndNil(fRegistry);
  inherited Destroy;
end;

function TBoldRegistry.OpenKey(const Key: String): Boolean;
begin
  Result := fRegistry.OpenKey(BoldRootRegistryKey + RegPathExtention[Integer(RegistryMode)] + Key, True);
end;

function TBoldRegistry.OpenNotDefaultKey(const Key: String): Boolean;
begin
  Result := fRegistry.OpenKey(Key, True);
end;

function TBoldRegistry.ReadBool(const Name: String; DefaultValue: Boolean): Boolean;
begin
  if fRegistry.ValueExists(Name) then
    Result := fRegistry.ReadBool(Name)
  else
    Result := DefaultValue;
end;

function TBoldRegistry.ReadInteger(const Name: String; DefaultValue: Integer): Integer;
begin
  if fRegistry.ValueExists(Name) then
    Result := fRegistry.ReadInteger(Name)
  else
    Result := DefaultValue;
end;

function TBoldRegistry.ReadString(const Name: String; DefaultValue: String): String;
begin
  if fRegistry.ValueExists(Name) then
    Result := fRegistry.ReadString(Name)
  else
    Result := DefaultValue;
end;

function TBoldRegistry.ValueExists(const Name: String): Boolean;
begin
  Result := fRegistry.ValueExists(Name)
end;

procedure TBoldRegistry.WriteBool(const Name: String; Value: Boolean);
begin
  fRegistry.WriteBool(Name, Value);
end;

procedure TBoldRegistry.WriteInteger(const Name: String; Value: Integer);
begin
  fRegistry.WriteInteger(Name, Value);
end;

procedure TBoldRegistry.WriteString(const Name: String; Value: String);
begin
  fRegistry.WriteString(Name, Value);
end;

end.
