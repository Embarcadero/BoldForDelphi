
{ Global compiler directives }
{$include bold.inc}
unit BoldAFPPluggable;

interface

uses
  Classes,
  Forms,
  BoldBase,
  BoldIndexableList,
  BoldElements,
  BoldSystemRT,
  BoldReferenceHandle,
  BoldAFPDefault;

type
  {---forward declarations---}
  TBoldPlaceableAFP = class;
  TBoldRegisterAFP = class;
  TBoldInstallableAFP = class;
  TBoldInstallableFormsDescriptor = class;
  TBoldInstallableFormsRegistry = class;

  TBoldGetFormClassEvent = procedure(Element: TBoldElement; var Result: TFormClass) of object;
  TBoldRetrieveHandleEvent = procedure(Form: TForm; var Result: TBoldReferenceHandle) of object;
  TBoldMemberShouldBeDisplayedEvent = procedure(Member: TBoldMemberRTInfo; var ShowMember: boolean) of object;
  TBoldHandleLocatorStyle = (hfByName, hfByType);

  {---TBoldPlaceableAFP---}
  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldPlaceableAFP = class(TComponent)
  private
    fOnGetFormClass: TBoldGetFormClassEvent;
    fOnRetrieveHandle: TBoldRetrieveHandleEvent;
    fOnMemberShouldBeDisplayed: TBoldMemberShouldBeDisplayedEvent;
  protected
    function GetFormClass(Element: TBoldElement): TFormClass;
    function RetrieveHandle(Form: TForm): TBoldReferenceHandle;
    function MemberShouldBeDisplayed(Member: TBoldMemberRTInfo): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnGetFormClass: TBoldGetFormClassEvent read fOnGetFormClass write fOnGetFormClass;
    property OnRetrieveHandle: TBoldRetrieveHandleEvent read fOnRetrieveHandle write fOnRetrieveHandle;
    property OnMemberShouldBeDisplayed: TBoldMemberShouldBeDisplayedEvent read fOnMemberShouldBeDisplayed write fOnMemberShouldBeDisplayed;
  end;

  {---TBoldPluggableAFP---}
  TBoldPluggableAFP = class(TBoldDefaultObjectAutoFormProvider)
  private
    fDefaultBehaviour: boolean;
  protected
    function RetrieveHandle: TBoldReferenceHandle; virtual; abstract;
    function RetrieveFormClass: TFormClass; virtual; abstract;
    procedure EnsureHandle; override;
    procedure EnsureComponents; override;
    function GetFormClass: TFormClass; override;
    procedure PostEnsureForm; override;
    property DefaultBehaviour: boolean read fDefaultBehaviour write fDefaultBehaviour;
  end;

  {---TBoldRegisterAFP---}
  TBoldRegisterAFP = class(TBoldPluggableAFP)
  protected
    function RetrieveHandle: TBoldReferenceHandle; override;
    function RetrieveFormClass: TFormClass; override;
    function MemberShouldBeDisplayed(Member: TBoldMemberRTInfo): Boolean; override;
  end;

  {---TBoldInstallableAFP---}
  TBoldInstallableAFP = class(TBoldPluggableAFP)
  protected
    function FindHandleByName: TBoldReferenceHandle;
    function FindHandleByType: TBoldReferenceHandle;
    function RetrieveFormClass: TFormClass; override;
    function RetrieveHandle: TBoldReferenceHandle; override;
  end;

  {---TBoldInstallableFormsDescriptor---}
  TBoldInstallableFormsDescriptor = class(TBoldMemoryManagedObject)
  private
    fFormClass: TFormClass;
    fBOClassName: string;
    fHandleName: string;
    fHandleLocatorStyle: TBoldHandleLocatorStyle;
  public
    constructor Create(const BOClassName: string; const FormClass: TFormClass; const HandleLocatorStyle: TBoldHandleLocatorStyle; const HandleName: string);
    property FormClass: TFormClass read fFormClass;
    property BOClassName: string read fBOClassName;
    property HandleName: string read fHandleName;
    property HandleLocatorStyle: TBoldHandleLocatorStyle read fHandleLocatorStyle;
  end;

  {---TBoldInstallableFormsRegistry---}
  TBoldInstallableFormsRegistry = class(TBoldIndexableList)
  protected
    function GetDescriptorsByBOClassName(const BOClassName: string): TBoldInstallableFormsDescriptor;
  public
    constructor Create;
    procedure RegisterInstallableFormsDescriptor(const BOClassName: string; const FormClass: TFormClass; const HandleLocatorStyle: TBoldHandleLocatorStyle; const HandleName: string);
    procedure UnRegisterInstallableFormsDescriptor(const BOClassName: string);
    property DescriptorsByBOClassName[const BOClassName: string]: TBoldInstallableFormsDescriptor read GetDescriptorsByBOClassName;
  end;

var
  AllowDefaultForm: boolean = True;

function InstallableFormsRegistry: TBoldInstallableFormsRegistry;

implementation

uses
  SysUtils,
  BoldSystem,
  BoldAFP,
  BoldHashIndexes;

var
  IX_BOClassName: integer = -1;

type
  {---TBOClassNameIndex---}
  TBOClassNameIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

var
  G_AFPList: TList;
  G_InstallableForms: TBoldInstallableFormsRegistry;

{---Global Access Methods---}
function InstallableFormsRegistry: TBoldInstallableFormsRegistry;
begin
  if not assigned(G_InstallableForms) then
    G_InstallableForms := TBoldInstallableFormsRegistry.Create;
  Result := G_InstallableForms;
end;

{---TBOClassNameIndex---}
function TBOClassNameIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldInstallableFormsDescriptor(Item).BOClassName;
end;

{---TBoldInstallableFormsDescriptor---}
constructor TBoldInstallableFormsDescriptor.Create(const BOClassName: string;
                                                   const FormClass: TFormClass;
                                                   const HandleLocatorStyle: TBoldHandleLocatorStyle;
                                                   const HandleName: string);
begin
  inherited create;
  fBOClassName := BOClassName;
  fFormClass := FormClass;
  fHandleLocatorStyle := HandleLocatorStyle;
  fHandleName := HandleName;
end;

{---TBoldInstallableFormsRegistry---}
constructor TBoldInstallableFormsRegistry.Create;
begin
  inherited;
  SetIndexCapacity(1);
  SetIndexVariable(IX_BOClassName, AddIndex(TBOClassNameIndex.Create));
end;

procedure TBoldInstallableFormsRegistry.RegisterInstallableFormsDescriptor(const BOClassName: string;
                                                                           const FormClass: TFormClass;
                                                                           const HandleLocatorStyle: TBoldHandleLocatorStyle;
                                                                           const HandleName: string);
begin
  Add(TBoldInstallableFormsDescriptor.Create(BOClassName, FormClass, HandleLocatorStyle, HandleName));
end;

procedure TBoldInstallableFormsRegistry.UnRegisterInstallableFormsDescriptor(const BOClassName: string);
begin
  Remove(DescriptorsByBOClassName[BOClassName]);
end;

function TBoldInstallableFormsRegistry.GetDescriptorsByBOClassName(const BOClassName: string): TBoldInstallableFormsDescriptor;
begin
  Result := TBoldInstallableFormsDescriptor(TBOClassNameIndex(Indexes[IX_BOClassName]).FindByString(BOClassName));
end;


{---TBoldPlaceableAFP---}
constructor TBoldPlaceableAFP.Create(AOwner: TComponent);
begin
  inherited;
  G_AFPList.Add(Self);
  // Handle all BoldObjects
  AutoFormProviderRegistry.RegisterProvider(bvtClass, TBoldObject, TBoldRegisterAFP);
end;

destructor TBoldPlaceableAFP.Destroy;
begin
  AutoFormProviderRegistry.UnregisterProvider(TBoldRegisterAFP);
  if Assigned(G_AFPList) then
    G_AFPList.Remove(Self);
  inherited;
end;

function TBoldPlaceableAFP.GetFormClass(Element: TBoldElement): TFormClass;
begin
  Result := nil;
  if Assigned(fOnGetFormClass) then
    OnGetFormClass(Element, result);
end;

function TBoldPlaceableAFP.MemberShouldBeDisplayed(
  Member: TBoldMemberRTInfo): Boolean;
begin
  Result := True;
  if Assigned(fOnMemberShouldBeDisplayed) then
    OnMemberShouldBeDisplayed(Member, Result);
end;

function TBoldPlaceableAFP.RetrieveHandle(Form: TForm): TBoldReferenceHandle;
begin
  Result := nil;
  if Assigned(Form) and Assigned(fOnRetrieveHandle) then
    OnRetrieveHandle(Form, result);
end;

{---TBoldPluggableAFP---}
function TBoldPluggableAFP.GetFormClass: TFormClass;
begin
  Result := RetrieveFormClass;

  DefaultBehaviour := (not Assigned(Result)) and AllowDefaultForm;
  if DefaultBehaviour then
    Result := inherited GetFormClass;
end;

procedure TBoldPluggableAFP.PostEnsureForm;
begin
end;

procedure TBoldPluggableAFP.EnsureHandle;
begin
  if DefaultBehaviour then
    inherited
  else
  begin
    BoldHandle := RetrieveHandle;
    if not Assigned(BoldHandle) then
      inherited;
  end;
end;

procedure TBoldPluggableAFP.EnsureComponents;
begin
  if DefaultBehaviour then
    inherited;
end;

{---TBoldRegisterAFP---}
function TBoldRegisterAFP.MemberShouldBeDisplayed(
  Member: TBoldMemberRTInfo): Boolean;
var
  i: integer;
begin
  Result := inherited MemberShouldBeDisplayed(Member);
  if Result then
    for i := G_AFPList.Count - 1 downto 0 do
    begin
      Result := TBoldPlaceableAFP(G_AFPList[i]).MemberShouldBeDisplayed(Member);
      if not Result then Break;
    end;
end;

function TBoldRegisterAFP.RetrieveFormClass: TFormClass;
var
  i: integer;
begin
  Result := nil;
  for i := G_AFPList.Count - 1 downto 0 do
  begin
    Result := TBoldPlaceableAFP(G_AFPList[i]).GetFormClass(Element);
    if Assigned(Result) then Break;
  end;
end;

function TBoldRegisterAFP.RetrieveHandle: TBoldReferenceHandle;
var
  i: integer;
begin
  Result := nil;
  for i := G_AFPList.Count - 1 downto 0 do
  begin
    Result := TBoldPlaceableAFP(G_AFPList[i]).RetrieveHandle(Form);
    if Assigned(Result) then Break;
  end;
end;

{---TBoldInstallableAFP---}
function TBoldInstallableAFP.RetrieveFormClass: TFormClass;
var
  Descriptor: TBoldInstallableFormsDescriptor;
begin
  Descriptor := InstallableFormsRegistry.DescriptorsByBOClassName[Element.BoldType.ModelName];
  if Assigned(Descriptor) then
    Result := Descriptor.FormClass
  else
    Result := nil;
end;

function TBoldInstallableAFP.RetrieveHandle: TBoldReferenceHandle;
begin
  Result := nil;
  case InstallableFormsRegistry.DescriptorsByBOClassName[Element.BoldType.ModelName].HandleLocatorStyle of
    hfByName: Result := FindHandleByName;
    hfByType: Result := FindHandleByType;
  end;
end;

function TBoldInstallableAFP.FindHandleByName: TBoldReferenceHandle;
var
  Component: TComponent;
begin
  Component := Form.FindComponent(InstallableFormsRegistry.DescriptorsByBOClassName[Element.BoldType.ModelName].HandleName);
  if component is TBoldReferenceHandle then
    Result := TBoldReferenceHandle(Component)
  else
    Result := nil;
end;

function TBoldInstallableAFP.FindHandleByType: TBoldReferenceHandle;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Form.ComponentCount - 1 do
    if Form.Components[i] is TBoldReferenceHandle then
    begin
      // if we find one, lets use it, if we find more, drop the first and exit.
      if not assigned(result) then
        Result := TBoldReferenceHandle(Form.Components[i])
      else
      begin
        result := nil;
        break;
      end;
    end;
end;

initialization
  G_AFPList := TList.Create;
  AutoFormProviderRegistry.RegisterProvider(bvtClass, TBoldObject, TBoldInstallableAFP);

finalization
  FreeAndNil(G_AFPList);
  FreeAndNil(G_InstallableForms);

end.

