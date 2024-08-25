{ Global compiler directives }
{$include bold.inc}
unit BoldUMLModelLink;

interface

uses
  Dialogs,
  Forms,
  Classes,
  BoldUMLModel,
  BoldHandle,
  BoldAbstractModel,
  BoldSubscription,
  BoldIndexableList;

type
  {---Forward declaration of classes---}
  TBoldUMLModelLinkList = class;
  TBoldUMLModelLinkDescriptor = class;
  TBoldUMLModelLink = class;
  TBoldUMLModelLinkClass = class of TBoldUMLModelLink;

  {---TBoldModelLink---}
  TBoldUMLModelLink = class(TBoldHandle)
  private
    fBoldModel: TBoldAbstractModel;
    fSubscriber: TBoldPassThroughSubscriber;
    procedure SetBoldModel(const Value: TBoldAbstractModel);
    procedure _Receive(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
  protected
    function GetCanExport: Boolean; virtual;
    function GetDisplayName: string; virtual; abstract;
    function GetFileName: string; virtual;
    procedure SetFileName(const Value: string); virtual;
    function GetHandledObject: TObject; override;
  public
    constructor Create(owner: TComponent); override;
    destructor Destroy; override;
    function ExportModel(UMLModel: TUMLModel): Boolean; virtual; abstract;
    function ImportModel(UMLModel: TUMLModel): Boolean; virtual; abstract;
    property CanExport: Boolean read GetCanExport;
    property DisplayName: string read GetDisplayName;
    property FileName: string read GetFileName write SetFileName;
    property BoldModel: TBoldAbstractModel read fBoldModel write SetBoldModel;
  end;

  {---TBoldModelLinkDescriptor---}
  TBoldUMLModelLinkDescriptor = class
  private
    fDescription: string;
    fUMLModelLinkClass: TBoldUMLModelLinkClass;
    fSuffix: string;
    function GetFilterString: string;
  public
    constructor Create(const Suffix, Description: string; ModelLinkClass: TBoldUMLModelLinkClass);
    property Description: string read fDescription;
    property FilterString: string read GetFilterString;
    property ModelLinkClass: TBoldUMLModelLinkClass read fUMLModelLinkClass;
    property Suffix: string read fSuffix;
    function CanExport: Boolean;
  end;

  {---TBoldModelLinkList---}
  TBoldUMLModelLinkList = class(TBoldIndexableList)
  private
    function GetDescriptorByIndex(const Index: integer): TBoldUMLModelLinkDescriptor;
    function GetDescriptorByLinkClass(ModelLinkClass: TBoldUMLModelLinkClass): TBoldUMLModelLinkDescriptor;
    function GetDescriptorBySuffix(const Suffix: string): TBoldUMLModelLinkDescriptor;
    function GetFileFilter: string;
    function GetLinkClasses(const Index: integer; const Suffix: string): TBoldUMLModelLinkClass;
    function GetExportableFileFilter: string;
  protected
    property Descriptors[const index: integer]: TBoldUMLModelLinkDescriptor read GetDescriptorByIndex;
    property LinkDescriptorBySuffix[const Suffix: string]: TBoldUMLModelLinkDescriptor read GetDescriptorBySuffix;
  public
    constructor Create;
    procedure AddLink(const Suffix, Description: string; ModelLinkClass: TBoldUMLModelLinkClass);
    procedure RemoveLink(const ModelLinkClass: TBoldUMLModelLinkClass);
    property FileFilter: string read GetFileFilter;
    property ExportableFileFilter: string read GetExportableFileFilter;
    property LinkClasses[const index: integer; const Suffix: string]: TBoldUMLModelLinkClass read GetLinkClasses;
    property LinkDescriptorByLinkClass[ModelLinkClass: TBoldUMLModelLinkClass]: TBoldUMLModelLinkDescriptor read GetDescriptorByLinkClass;
  end;

function BoldUMLModelLinkList: TBoldUMLModelLinkList;
function BoldUMLModelLinkListAssigned: boolean;
function BoldGetUMLModelLinkForComponent(BoldModel: TBoldAbstractModel): TBoldUMLModelLink;

implementation

uses
  SysUtils,
  BoldDefs,
  BoldHashIndexes;

var
  {Index in list for the indexes, must correspond to creation order}
  IX_Suffix: integer = -1;
  IX_LinkClass: integer = -1;

var
  G_BoldUMLModelLinkList: TBoldUMLModelLinkList = nil;

type
  {---TSuffixIndex---}
  TSuffixIndex = class(TBoldStringHashIndex)
  protected
    function ItemAsKeyString(Item: TObject): string; override;
  end;

  {---TLinkClassIndex---}
  TLinkClassIndex = class(TBoldClassHashIndex)
  protected
    function ItemAsKeyClass(Item: TObject): TClass; override;
  end;

{---TSuffixIndex---}
function TSuffixIndex.ItemAsKeyString(Item: TObject): string;
begin
  Result := TBoldUMLModelLinkDescriptor(Item).Suffix;
end;

{---TLinkClassIndex---}
function TLinkClassIndex.ItemAsKeyClass(Item: TObject): TClass;
begin
  Result := TBoldUMLModelLinkDescriptor(Item).ModelLinkClass;
end;

{---Access methods---}
procedure EnsureList;
begin
  if not Assigned(G_BoldUMLModelLinkList) then
    G_BoldUMLModelLinkList := TBoldUMLModelLinkList.Create;
end;

function BoldUMLModelLinkList: TBoldUMLModellinkList;
begin
  EnsureList;
  Result := G_BoldUMLModelLinkList;
end;

function BoldUMLModelLinkListAssigned: boolean;
begin
  Result := Assigned(G_BoldUMLModelLinkList);
end;

function BoldGetUMLModelLinkForComponent(BoldModel: TBoldAbstractModel): TBoldUMLModelLink;
var
  i: integer;
begin
  result := nil;
  for i := 0 to BoldHandleList.count - 1 do
    if (BoldHandleList[i] is TBoldUMLModelLink) and
       ((BoldHandleList[i] as TBoldUMLModelLink).BoldModel = BoldModel) then
    begin
      result := BoldHandleList[i] as TBoldUMLModelLink;
      break;
    end;
end;

{---TBoldModelLinkList---}
constructor TBoldUMLModelLinkList.Create;
begin
  inherited;
  SetIndexCapacity(2);
  SetIndexVariable(IX_Suffix, AddIndex(TSuffixIndex.Create));
  SetIndexVariable(IX_LinkClass, AddIndex(TLinkClassIndex.Create));
end;

function TBoldUMLModelLinkList.GetDescriptorByIndex(const Index: integer): TBoldUMLModelLinkDescriptor;
begin
  Result := Items[Index] as TBoldUMLModelLinkDescriptor;
end;

function TBoldUMLModelLinkList.GetDescriptorBySuffix(const Suffix: string): TBoldUMLModelLinkDescriptor;
begin
  Result := TBoldUMLModelLinkDescriptor(TSuffixIndex(Indexes[IX_Suffix]).FindByString(Suffix))
end;

function TBoldUMLModelLinkList.GetDescriptorByLinkClass(ModelLinkClass: TBoldUMLModelLinkClass): TBoldUMLModelLinkDescriptor;
begin
  Result := TBoldUMLModelLinkDescriptor(TLinkClassIndex(Indexes[IX_LinkClass]).FindByClass(ModelLinkClass))
end;

function TBoldUMLModelLinkList.GetFileFilter: string;
var
  i: integer;
  AllSupportedFilter: string;
begin
  Result := '';
  AllSupportedFilter := 'All Supported|';
  for i := 0 to Count - 1  do
  begin
    Result := Result + Descriptors[i].FilterString + '|';
    AllSupportedFilter := AllSupportedFilter + '*' + Descriptors[i].Suffix;
    if i < count-1 then
      AllSupportedFilter := AllSupportedFilter + ';';
  end;
  Result := AllSupportedFilter + '|' + Result + ALLFILESFILTER;
end;

function TBoldUMLModelLinkList.GetLinkClasses(const Index: integer; const Suffix: string): TBoldUMLModelLinkClass;
var
  Descriptor: TBoldUMLModelLinkDescriptor;
begin
  if (Index >= 0) and (Index < Count) then
    Result := Descriptors[Index].ModelLinkClass
  else
  begin
    Descriptor := LinkDescriptorBySuffix[Suffix];
    if assigned(Descriptor) then
      Result := descriptor.ModelLinkClass
    else
      result := nil;
  end;
end;

procedure TBoldUMLModelLinkList.AddLink(const Suffix, Description: string; ModelLinkClass: TBoldUMLModelLinkClass);
begin
  Add(TBoldUMLModelLinkDescriptor.Create(Suffix, Description, ModelLinkClass));
end;

procedure TBoldUMLModelLinkList.RemoveLink(const ModelLinkClass: TBoldUMLModelLinkClass);
var
  Descriptor: TBoldUMLModelLinkDescriptor;
begin
  Descriptor := LinkDescriptorByLinkClass[ModelLinkClass];
  if Assigned(Descriptor) then
    Remove(Descriptor);
end;

{---TBoldModelLinkDescriptor---}
function TBoldUMLModelLinkDescriptor.CanExport: Boolean;
begin
  with ModelLinkClass.create(nil) do
    try
      Result := CanExport;
    finally
      Free;
    end;
end;

constructor TBoldUMLModelLinkDescriptor.Create(const suffix, description: string; modelLinkClass: TBoldUMLModelLinkClass);
begin
  inherited Create;
  fSuffix := suffix;
  fDescription := description;
  fUMLModelLinkClass := modelLinkClass;
end;

function TBoldUMLModelLinkDescriptor.GetFilterString: string;
begin
  Result := Format('%s (*%s)|*%1:s', [Description, Suffix]);
end;

{---TBoldModelLink---}
procedure TBoldUMLModelLink.SetFileName(const Value: string);
begin
  raise EBoldInternal.CreateFmt('%s.SetFileName: Link can''t import from file', [ClassName]);
end;

function TBoldUMLModelLink.GetFileName: string;
begin
  raise EBoldInternal.CreateFmt('%s.GetFileName: Link can''t import from file', [ClassName]);
  Result := '';
end;

function TBoldUMLModelLink.GetCanExport: Boolean;
begin
  Result := False;
end;

function TBoldUMLModelLink.GetHandledObject: TObject;
begin
  result := nil;
end;

procedure TBoldUMLModelLink.SetBoldModel(const Value: TBoldAbstractModel);
begin
  fSubscriber.CancelAllSubscriptions;
  if assigned(Value) then
  begin
    Value.AddSmallSubscription(fSubscriber, [beDestroying], beDestroying);
  end;
  fBoldModel := Value;
end;

constructor TBoldUMLModelLink.Create(owner: TComponent);
begin
  inherited;
  fsubscriber := TBoldPassthroughSubscriber.Create(_Receive);
end;

procedure TBoldUMLModelLink._Receive(Originator: TObject;
  OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);
begin
  if (Originator = fBoldModel) and (OriginalEvent = beDestroying) then
    fBoldModel := nil;
end;

destructor TBoldUMLModelLink.Destroy;
begin
  inherited;
  FreeAndNil(fSubscriber);
end;

function TBoldUMLModelLinkList.GetExportableFileFilter: string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to Count - 1  do
    if Descriptors[i].CanExport then
      Result := Result + Descriptors[i].FilterString + '|';

  Result := Result + ALLFILESFILTER;
end;


initialization

finalization
  FreeAndNil(G_BoldUMLModelLinkList);

end.
