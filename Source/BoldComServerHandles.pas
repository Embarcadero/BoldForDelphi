
{ Global compiler directives }
{$include bold.inc}
unit BoldComServerHandles;

interface

uses
  Classes,
  BoldBase,
  BoldContainers,
  BoldServerHandles,
  BoldComServer;

type
  { forward declarations }
  TBoldComClass = class;
  TBoldComClasses = class;
  TBoldComObjectProvider = class;
  TBoldComServerHandle = class;
  TBoldComExportHandle = class;
  TBoldComServerObjectHandle = class;

  {-- TBoldComClass --}
  TBoldComClass = class(TCollectionItem)
  private
    FCLSID: string;
    FDescription: string;
    FActive: Boolean;
    FName: string;
    function IsDesignTime: Boolean;
    function IsLoading: Boolean;
    procedure SetCLSID(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetActive(Value: Boolean);
    procedure SetName(const Value: string);
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property CLSID: string read FCLSID write SetCLSID;
    property Description: string read FDescription write SetDescription;
    property Active: Boolean read FActive write SetActive default True;
    property Name: string read FName write SetName;
  end;

  {-- TBoldComClasses --}
  TBoldComClasses = class(TCollection)
  private
    FServerHandle: TBoldComServerHandle;
    function ClassByName(const Name: string): TBoldComClass;
    function ClassExists(const Name: string): Boolean;
    function FirstClassName: string;
    function GetItem(Index: Integer): TBoldComClass;
    function GetUniqueName: string;
    procedure SetItem(Index: Integer; Value: TBoldComClass);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ServerHandle: TBoldComServerHandle);
    function Add: TBoldComClass;
    procedure GetClassNames(Strings: TStrings);
    property Items[Index: Integer]: TBoldComClass read GetItem write SetItem; default;
  end;

  {-- TBoldComObjectProvider --}
  TBoldComObjectProvider = class(TBoldNonRefCountedObject,IBoldComServerObjectProvider)
  private
    FCookie: Integer;
    FHandles: TBoldObjectArray;
    FServerHandle: TBoldComServerHandle;
    procedure AddHandle(Handle: TBoldComExportHandle);
    procedure RemoveHandle(Handle: TBoldComExportHandle);
    property ServerHandle: TBoldComServerHandle read FServerHandle;
  protected
    { IBoldComServerObjectProvider }
    function CreateObject(const ClassId: TGUID; const ClassName: string): IUnknown;
    function GetObject(const ClassId: TGUID; const ObjectName: string): IUnknown;
    procedure GetObjectInfo(const ClassId: TGUID; ObjectNames, ClassNames: TStrings);
  public
    constructor Create(ServerHandle: TBoldComServerHandle);
    destructor Destroy; override;
  end;

  {-- TBoldComServerHandle --}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldComServerHandle = class(TBoldServerHandle)
  private
    FClasses: TBoldComClasses;
    FObjectProvider: TBoldComObjectProvider;
    function GetObjectProvider: TBoldComObjectProvider;
    procedure SetClasses(Value: TBoldComClasses);
    property ObjectProvider: TBoldComObjectProvider read GetObjectProvider;
  protected
    function GetHandledObject: TObject; override;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active;
    property Classes: TBoldComClasses read FClasses write SetClasses;
  end;

  {-- TBoldComExportHandle --}
  TBoldComExportHandle = class(TBoldExportHandle)
  private
    FServerClass: string;
    FServerHandle: TBoldComServerHandle;
    function GetEffectiveActive: Boolean;
    function CanProvideThroughClass(const ClassId: TGUID): Boolean;
    procedure RegisterHandle;
    procedure RevokeHandle;
    procedure SetServerClass(const Value: string);
    procedure SetServerHandle(Value: TBoldComServerHandle);
    procedure UpdateServerClass(Connect: Boolean);
  protected
    function GetComObject: IUnknown; virtual; abstract;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ComObject: IUnknown read GetComObject;
    property EffectiveActive: Boolean read GetEffectiveActive;
  published
    property Active;
    property ObjectName;
    property ServerClass: string read FServerClass write SetServerClass;
    property ServerHandle: TBoldComServerHandle read FServerHandle write SetServerHandle;
  end;

  TBoldComGetComObjectEvent = procedure(Sender: TObject; out Obj: IUnknown) of object;
  
  {-- TBoldComServerObjectHandle --}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldComServerObjectHandle = class(TBoldComExportHandle)
  private
    FOnGetComObject: TBoldComGetComObjectEvent;
  protected
    function GetComObject: IUnknown; override;
    function GetHandledObject: TObject; override;
  published
    property OnGetComObject: TBoldComGetComObjectEvent read FOnGetComObject write FOnGetComObject;
  end;

implementation

uses
  SysUtils,
  ActiveX,
  ComObj,
  BoldHandle,
  BoldComUtils,
  BoldGUIDUtils;

const
  DefaultClassName = 'BoldServer';
  DefaultClassDescription = 'Bold Server Application';

{-- TBoldComClass ---------------------------------------------------------}

constructor TBoldComClass.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FActive := True;
  if not IsLoading then
  begin
    FCLSID := BoldCreateGUIDAsString;
    with Collection as TBoldComClasses do
      FName := GetUniqueName;
  end;
end;

procedure TBoldComClass.Assign(Source: TPersistent);
begin
  if Source is TBoldComClass then
  begin
    Active := TBoldComClass(Source).Active;
    CLSID := TBoldComClass(Source).CLSID;
    Description := TBoldComClass(Source).Description;
    Name := TBoldComClass(Source).Name;
  end
  else
    inherited Assign(Source);
end;

function TBoldComClass.GetDisplayName: string;
begin
  Result := Name;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TBoldComClass.IsLoading: Boolean;
begin
  with Collection as TBoldComClasses do
    Result := (csLoading in FServerHandle.ComponentState);
end;

function TBoldComClass.IsDesignTime: Boolean;
begin
  with Collection as TBoldComClasses do
    Result := (csDesigning in FServerHandle.ComponentState);
end;

procedure TBoldComClass.SetCLSID(const Value: string);
var
  GUID: TGUID;
begin
  if FCLSID <> Value then
  begin
    if not IsLoading and not IsDesignTime then
      if Active then
        raise EBoldCom.CreateFmt('%s: Cannot change CLSID at run-time',[ClassName]);
    try
      GUID := StringToGUID(Value);
      FCLSID := Value;
      Changed(False);
    except
      on Exception do
        raise EBoldCom.CreateFmt('%s: Invalid CLSID',[ClassName]);
    end;
  end;
end;

procedure TBoldComClass.SetDescription(const Value: string);
begin
  if FDescription <> Value then
  begin
    if not IsLoading and not IsDesignTime then
      raise EBoldCom.CreateFmt('%s: Cannot change Description at run-time',[ClassName]);
    FDescription := Value;
    Changed(False);
  end;
end;

procedure TBoldComClass.SetActive(Value: Boolean);
begin
  if FActive <> Value then
    FActive := Value;
end;

procedure TBoldComClass.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    if not IsLoading and not IsDesignTime then
      raise EBoldCom.CreateFmt('%s: Cannot change Name at run-time',[ClassName]);
    if IsValidIdent(Value) then
    begin
      FName := Value;
      Changed(False);
    end
    else
      raise EBoldCom.CreateFmt('%s: Invalid Name',[ClassName]);
  end;
end;

{-- TBoldComClasses --------------------------------------------------------}

constructor TBoldComClasses.Create(ServerHandle: TBoldComServerHandle);
begin
  inherited Create(TBoldComClass);
  FServerHandle := ServerHandle;
end;

function TBoldComClasses.Add: TBoldComClass;
begin
  Result := TBoldComClass(inherited Add);
end;

function TBoldComClasses.ClassByName(const Name: string): TBoldComClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    if AnsiCompareText(Name,Items[I].Name) = 0 then
    begin
      Result := Items[I];
      Break;
    end;
  end;
end;

function TBoldComClasses.ClassExists(const Name: string): Boolean;
begin
  Result := Assigned(ClassByName(Name));
end;

function TBoldComClasses.FirstClassName: string;
begin
  if Count = 0 then
    Result := ''
  else
    Result := Items[0].Name;
end;

function TBoldComClasses.GetItem(Index: Integer): TBoldComClass;
begin
  Result := TBoldComClass(inherited GetItem(Index));
end;

procedure TBoldComClasses.GetClassNames(Strings: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Strings.Add(Items[I].Name);
end;

function TBoldComClasses.GetOwner: TPersistent;
begin
  Result := FServerHandle;
end;

function TBoldComClasses.GetUniqueName: string;
var
  I: Integer;
begin
  I := 0;
  repeat
    Inc(I);
    Result := Format('Class%d',[I]);
  until not ClassExists(Result);
end;

procedure TBoldComClasses.SetItem(Index: Integer; Value: TBoldComClass);
begin
  inherited SetItem(Index, Value);
end;

procedure TBoldComClasses.Update(Item: TCollectionItem);
begin
  if Item <> nil then
  else
end;

{-- TBoldComObjectProvider ----------------------------------------------------}

constructor TBoldComObjectProvider.Create(ServerHandle: TBoldComServerHandle);
begin
  inherited Create;
  FServerHandle := ServerHandle;
  FHandles := TBoldObjectArray.Create(8,[]);
  FCookie := TBoldComServer.Instance.RegisterObjectProvider(Self);
end;

destructor TBoldComObjectProvider.Destroy;
begin
  TBoldComServer.Instance.RevokeObjectProvider(self, FCookie);
  FHandles.Free;
  inherited Destroy;
end;

procedure TBoldComObjectProvider.AddHandle(Handle: TBoldComExportHandle);
begin
  FHandles.Add(Handle);
end;

function TBoldComObjectProvider.CreateObject(const ClassId: TGUID; const ClassName: string): IUnknown;
begin
  Result := nil;
end;

function TBoldComObjectProvider.GetObject(const ClassId: TGUID; const ObjectName: string): IUnknown;
var
  I: Integer;
  Handle: TBoldComExportHandle;
begin
  Result := nil;
  if ServerHandle.Active then
  begin
    for I := 0 to FHandles.Count - 1 do
    begin
      Handle := FHandles[I] as TBoldComExportHandle;
      if (AnsiCompareText(ObjectName,Handle.ObjectName) = 0) then
      begin
        if Handle.EffectiveActive and Handle.CanProvideThroughClass(ClassId) then
          Result := Handle.ComObject;
        Break;
      end;
    end;
  end;
end;

procedure TBoldComObjectProvider.GetObjectInfo(const ClassId: TGUID; ObjectNames, ClassNames: TStrings);
var
  I: Integer;
  Handle: TBoldComExportHandle;
begin
  if ServerHandle.Active then
  begin
    for I := 0 to FHandles.Count - 1 do
    begin
      Handle := FHandles[I] as TBoldComExportHandle;
      if Handle.EffectiveActive and Handle.CanProvideThroughClass(ClassId) then
        ObjectNames.Add(Handle.ObjectName);
    end;
  end;
end;

procedure TBoldComObjectProvider.RemoveHandle(Handle: TBoldComExportHandle);
begin
  FHandles.Remove(Handle);
end;

{-- TBoldComServerHandle ------------------------------------------------------}

constructor TBoldComServerHandle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClasses := TBoldComClasses.Create(Self);
  if (csDesigning in ComponentState) and
    not (csLoading in ComponentState) then
      with FClasses.Add do
      begin
        Name := DefaultClassName;
        Description := DefaultClassDescription;
      end;
end;

destructor TBoldComServerHandle.Destroy;
begin
  if Assigned(FObjectProvider) then
    FObjectProvider.Free;
  FClasses.Free;
  inherited Destroy;
end;

function TBoldComServerHandle.GetHandledObject: TObject;
begin
  Result := TBoldComServer.Instance;
end;

function TBoldComServerHandle.GetObjectProvider: TBoldComObjectProvider;
begin
  if not Assigned(FObjectProvider) then
    FObjectProvider := TBoldComObjectProvider.Create(Self);
  Result := FObjectProvider;
end;

procedure TBoldComServerHandle.Loaded;
begin
  inherited;
end;

procedure TBoldComServerHandle.SetClasses(Value: TBoldComClasses);
begin
  FClasses.Assign(Value);
end;

{-- TBoldComExportHandle ------------------------------------------------------}

constructor TBoldComExportHandle.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TBoldComExportHandle.Destroy;
begin
  RevokeHandle;
  inherited;
end;

function TBoldComExportHandle.CanProvideThroughClass(const ClassId: TGUID): Boolean;
var
  SClass: TBoldComClass;
begin
  if (ServerClass = '') or IsEqualGUID(GUID_NULL,ClassId) then
    Result := True
  else
  begin
    SClass := ServerHandle.Classes.ClassByName(ServerClass);
    Result := Assigned(SClass) and (CompareText(SClass.CLSID,GUIDToString(ClassId)) = 0);
  end;
end;

function TBoldComExportHandle.GetEffectiveActive: Boolean;
var
  SClass: TBoldComClass;
begin
  Result := Assigned(ServerHandle) and ServerHandle.Active;
  if Result then
  begin
    if ServerClass = '' then
      Result := True
    else
    begin
      SClass := ServerHandle.Classes.ClassByName(ServerClass);
      Result := Assigned(SClass) and SClass.Active;
    end;
    if Result then
      Result := Active;
  end;
end;

procedure TBoldComExportHandle.Loaded;
begin
  inherited;
  RegisterHandle;
end;

procedure TBoldComExportHandle.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServerHandle) then
    ServerHandle := nil;
end;

procedure TBoldComExportHandle.RegisterHandle;
begin
  if Assigned(ServerHandle) and not (csLoading in ComponentState) then
  begin
    ServerHandle.ObjectProvider.AddHandle(Self);
    ServerHandle.FreeNotification(Self);
  end;
end;

procedure TBoldComExportHandle.RevokeHandle;
begin
  if Assigned(ServerHandle) then
    ServerHandle.ObjectProvider.RemoveHandle(Self);
end;

procedure TBoldComExportHandle.SetName(const NewName: TComponentName);
begin
  inherited;
end;

procedure TBoldComExportHandle.SetServerClass(const Value: string);
begin
  if Value <> FServerClass then
    FServerClass := Value;
end;

procedure TBoldComExportHandle.SetServerHandle(Value: TBoldComServerHandle);
begin
  if Value <> FServerHandle then
  begin
    RevokeHandle;
    UpdateServerClass(False);
    FServerHandle := Value;
    RegisterHandle;
    UpdateServerClass(True);
  end;
end;

procedure TBoldComExportHandle.UpdateServerClass(Connect: Boolean);
begin
  if Assigned(ServerHandle) and not (csLoading in ComponentState) then
  begin
    if Connect then
    begin
      if ServerClass = '' then
        ServerClass := ServerHandle.Classes.FirstClassName;
    end
    else
    begin
      if ServerHandle.Classes.ClassExists(ServerClass) then
        ServerClass := '';
    end;
  end;
end;

{-- TBoldComServerObjectHandle ------------------------------------------------}

function TBoldComServerObjectHandle.GetComObject: IUnknown;
begin
  if Assigned(OnGetComObject) then
    OnGetComObject(Self,Result)
  else
    Result := nil;
end;

function TBoldComServerObjectHandle.GetHandledObject: TObject;
begin
  Result := nil;
end;

end.