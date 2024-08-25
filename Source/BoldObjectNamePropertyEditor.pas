
{ Global compiler directives }
{$include bold.inc}
unit BoldObjectNamePropertyEditor;

{$Include Bold.inc}

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  Buttons,
  ComCtrls,
  BoldComClientHandles,
  ActnList,
  BoldAbstractPropertyEditors,
  DesignIntf,
  TypInfo, System.Actions;

const
  INDEX_OBJECTNAME = 0;
  INDEX_CLASSNAME = 1;

type
  {forward declarations}
  TObjectNamePropEditFrm = class;
  TBoldObjectNameProperty = class;
  TBoldProviderObjectInfo = class;

  TBoldProviderObjectInfo = class
  private
    fClassNames: TStringList;
    fObjectNames: TStringList;
    function GetClassNames(Index: integer): string;
    function GetCount: integer;
    function GetObjectNames(Index: integer): string;
  public
    constructor Create(ClassNames, ObjectNames: TStringList);
    destructor Destroy; override;
    procedure AddStrings(ClassNames, ObjectNames: TStringList);
    property Count: integer read GetCount;
    property ClassNames[Index: integer]: string read GetClassNames ;
    property ObjectNames[Index: integer]: string read GetObjectNames;
  end;

  TObjectNamePropEditFrm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    lvObjects: TListView;
    BitBtn3: TBitBtn;
    ActionList1: TActionList;
    RefreshAction: TAction;
    procedure RefreshActionExecute(Sender: TObject);
  private
    fConnectionHandle: TBoldComConnectionHandle;
    function GetObjectName: string;
    procedure RefreshGUI(ObjectInfo: TBoldProviderObjectInfo);
    procedure GetObjectsFromServer;
    property ConnectionHandle: TBoldComConnectionHandle read fConnectionHandle;
    { Private declarations }
  public
    { Public declarations }
    function Display(pConnectionHandle: TObject): Boolean;
    property ObjectName: string read GetObjectName;
  end;

  TBoldObjectNameProperty = class(TBoldStringProperty)
  protected
    function FileFilter: string; virtual;
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;


var
  ObjectNamePropEditFrm: TObjectNamePropEditFrm;

implementation

uses
  BoldComUtils,
  Variants,
  dialogs;

{$R *.DFM}

var
  G_ProviderObjectInfoList: TStringList;

function ObjectServers: TStringList;
begin
  if not Assigned(G_ProviderObjectInfoList) then
    G_ProviderObjectInfoList := TStringList.Create;
  Result := G_ProviderObjectInfoList;
end;

procedure FreeObjectServers;
var
  i: integer;
  CurObjectInfo: TObject;
begin
  if Assigned(G_ProviderObjectInfoList) then
  begin
    for i := 0 to G_ProviderObjectInfoList.Count - 1 do
    begin
      CurObjectInfo := G_ProviderObjectInfoList.Objects[i];
      FreeAndNil(CurObjectInfo);
    end;
    FreeAndNil(G_ProviderObjectInfoList);
  end;
end;

{ TObjectNamePropEditFrm }

function TObjectNamePropEditFrm.Display(pConnectionHandle: TObject): Boolean;
var
  CurrentInfo: TBoldProviderObjectInfo;
begin
  Result := false;
  fConnectionHandle := pConnectionHandle as TBoldComConnectionHandle;
  if Assigned(ConnectionHandle) then
  begin
    Caption := Format('Objects in %s', [ConnectionHandle.ServerName]);
    if (ObjectServers.IndexOf(ConnectionHandle.ServerCLSID) = -1) and
    (MessageDlg(Format('Would you like to start the server %s to get the list of exported objects?', [ConnectionHandle.ServerName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
      GetObjectsFromServer;
    CurrentInfo := nil;
    if (ObjectServers.IndexOf(ConnectionHandle.ServerCLSID) <> -1) then
      CurrentInfo := ObjectServers.Objects[ObjectServers.IndexOf(ConnectionHandle.ServerCLSID)] as TBoldProviderObjectInfo;
    RefreshGUI(CurrentInfo);
    Result := (ShowModal = mrOk);
  end;
  fConnectionHandle := nil;
end;

procedure TObjectNamePropEditFrm.GetObjectsFromServer;
var
  j, idx: integer;
  ObjectNames, ClassNames: TStringList;
  V, VObjs, VClassNames: OleVariant;
begin
  ConnectionHandle.Connected := True;
  if ConnectionHandle.Connected then
  begin
    try
      ObjectNames := TStringList.Create;
      ClassNames := TStringList.Create;
      V := ConnectionHandle.BoldProvider.ObjectInfo;
      if VarIsArray(V) and (VarArrayDimCount(V) = 1) and (VarArrayHighBound(V, 1) = 1) then
      begin
        VObjs := V[0];
        VClassNames := V[1];
        if VarIsArray(VObjs) and VarIsArray(VClassNames) then
        begin
          for j := VarArrayLowBound(VObjs, 1) to VarArrayHighBound(VObjs, 1) do
            ObjectNames.Add(VObjs[j]);
          for j := VarArrayLowBound(VClassNames, 1) to VarArrayHighBound(VClassNames, 1) do
            ClassNames.Add(VarToStr(VClassNames[j]));
        end;
      end;
      idx := ObjectServers.IndexOf(ConnectionHandle.ServerCLSID);
      if (idx = -1) then
      begin
        ObjectServers.AddObject(ConnectionHandle.ServerCLSID, TBoldProviderObjectInfo.Create(ClassNames, ObjectNames));
      end
      else
        (ObjectServers.Objects[idx] as TBoldProviderObjectInfo).AddStrings(ClassNames, ObjectNames);
    finally
      FreeAndNil(ObjectNames);
      FreeAndNil(ClassNames);
    end;
  end;
  ConnectionHandle.Connected := false;
end;


procedure TObjectNamePropEditFrm.RefreshGUI(ObjectInfo: TBoldProviderObjectInfo);
var
  i:integer;
  ListItem: TListItem;
begin
  if Assigned(ObjectInfo) then
  begin
    try
      lvObjects.Items.BeginUpdate;
      lvObjects.Items.Clear;
      for i:= 0 to ObjectInfo.Count - 1 do
      begin
        ListItem := lvObjects.Items.Add;
        ListItem.Caption := ObjectInfo.ObjectNames[i];
        ListItem.SubItems.Add(ObjectInfo.ClassNames[i]);
      end;
    finally
      lvObjects.Items.EndUpdate;
    end;
  end;
end;

function TObjectNamePropEditFrm.GetObjectName: string;
begin
  if lvObjects.SelCount > 0 then
    Result := lvObjects.Selected.Caption
  else
    Result := '';
end;


{ TBoldProviderObjectInfo }

procedure TBoldProviderObjectInfo.AddStrings(ClassNames,
  ObjectNames: TStringList);
begin
  fClassNames.Clear;
  fObjectNames.Clear;
  fClassNames.AddStrings(ClassNames);
  fObjectNames.AddStrings(ObjectNames);
end;

constructor TBoldProviderObjectInfo.Create(ClassNames, ObjectNames: TStringList);
begin
  inherited Create;
  Assert(ObjectNames.Count <= ClassNames.Count);
  fClassNames := TStringList.Create;
  fObjectNames := TStringList.Create;
  fClassNames.AddStrings(ClassNames);
  fObjectNames.AddStrings(ObjectNames);
end;

destructor TBoldProviderObjectInfo.Destroy;
begin
  FreeAndNil(fClassNames);
  FreeAndNil(fObjectNames);
  inherited;
end;

function TBoldProviderObjectInfo.GetClassNames(Index: integer): string;
begin
  Assert((Index <= fClassNames.Count) and (Index > -1));
  Result := fClassNames[Index];
end;

function TBoldProviderObjectInfo.GetCount: integer;
begin
  Assert(fObjectNames.Count <= fClassNames.Count);
  Result := fObjectNames.Count;
end;

function TBoldProviderObjectInfo.GetObjectNames(Index: integer): string;
begin
  Assert((Index <= fObjectNames.Count) and (Index > -1));
  Result := fObjectNames[Index];
end;

procedure TObjectNamePropEditFrm.RefreshActionExecute(Sender: TObject);
var
  CurrentInfo: TBoldProviderObjectInfo;
begin
  if (MessageDlg(Format('Connect to server %s and retrieve exported objects?', [ConnectionHandle.ServerName]), mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    GetObjectsFromServer;
    CurrentInfo := nil;
    if (ObjectServers.IndexOf(ConnectionHandle.ServerCLSID) <> -1) then
      CurrentInfo := ObjectServers.Objects[ObjectServers.IndexOf(ConnectionHandle.ServerCLSID)] as TBoldProviderObjectInfo;
    RefreshGUI(CurrentInfo);
  end;
end;

{ TBoldObjectNameProperty }

procedure TBoldObjectNameProperty.Edit;
var
  obj: TObject;
  i: integer;
  SelectedPersistent : TPersistent;
begin
  inherited;
  SelectedPersistent := nil;
  for i := 0 to PropCount - 1 do
    SelectedPersistent := GetComponent(i);
  obj := GetObjectProp(SelectedPersistent, 'ConnectionHandle');
  if Assigned(obj) then
  begin
    with TObjectNamePropEditFrm.Create(Application) do
    begin
      if Display(obj) then
        SetValue(ObjectName);
      Free;
    end;
  end
  else
    MessageDlg('Cannot find a ConnectionHandle.',mtInformation, [mbOk], 0);
end;

function TBoldObjectNameProperty.FileFilter: string;
begin
  Result := '';
end;

function TBoldObjectNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

initialization

finalization
  FreeObjectServers;

end.
