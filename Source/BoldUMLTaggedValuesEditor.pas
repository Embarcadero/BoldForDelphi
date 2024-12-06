{ Global compiler directives }
{$include bold.inc}
unit BoldUMLTaggedValuesEditor;

interface

uses
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  BoldSubscription,
  BoldHandles,
  BoldReferenceHandle,
  BoldTreeView,
  BoldUMLModel,
  BoldAbstractListHandle,
  BoldCursorHandle,
  BoldRootedHandles,
  BoldDerivedHandle,
  BoldElements,
  BoldSystemRT,
  BoldSystem,
  BoldListBox,
  BoldMemo,
  BoldListHandle,
  BoldExpressionHandle,
  BoldControlPack,
  BoldStringControlPack,
  BoldPlaceableSubscriber,
  ToolWin, Menus, ImgList, System.ImageList;

const
  WM_REFRESHGUI = WM_USER + 123;

type
  TfrmBoldUMLTaggedValuesEditor = class(TForm)
    Panel1: TPanel;
    bdhAllTools: TBoldDerivedHandle;
    bchAllTools: TBoldCursorHandle;
    bdhSelectedTVs: TBoldDerivedHandle;
    blhSelectedTVs: TBoldListHandle;
    tcTools: TTabControl;
    bmValue: TBoldMemo;
    lbTagName: TBoldListBox;
    behRoot: TBoldExpressionHandle;
    Splitter1: TSplitter;
    BoldAsStringRenderer1: TBoldAsStringRenderer;
    MainMenu1: TMainMenu;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    Cut1: TMenuItem;
    Paste1: TMenuItem;
    mnuTools: TMenuItem;
    Addtaggedvalue1: TMenuItem;
    Deletetaggedvalue1: TMenuItem;
    ImageList1: TImageList;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure bdhAllToolsDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    function GetStringListType(RootType: TBoldElementTypeInfo): TBoldLIstTYpeInfo;
    procedure bdhSelectedTVsDeriveAndSubscribe(Sender: TComponent;
      RootValue: TBoldElement; ResultElement: TBoldIndirectElement;
      Subscriber: TBoldSubscriber);
    procedure tcToolsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btAddTVClick(Sender: TObject);
    procedure btDeleteTVClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Cut1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    function BoldAsStringRenderer1GetAsString(aFollower: TBoldFollower): string;
    procedure BoldAsStringRenderer1Subscribe(aFollower: TBoldFollower;
      Subscriber: TBoldSubscriber);
  private
    fRootSubscriber: TBoldPassThroughSubscriber;
    PreviousSelectedTab: Integer;
    procedure RecieveRootChanged(Originator: TObject; OriginalEvent: TBoldEvent; RequestedEvent: TBoldRequestedEvent);

    function GetToolName(tag: string): string;
    procedure CreateTabs;
    function GetTagName(tag: string): string;
    procedure PlaceSubscriptions;
    procedure RefreshGUI(var Message: TMessage); message WM_REFRESHGUI;
    procedure DoMessage(Control: TControl; Msg: Cardinal);
  public
    destructor Destroy; override;
    procedure SwitchMode(ReadOnly: Boolean);
  end;

procedure ShowTaggedValuesEditor(UMLElement: TUMLModelElement);

implementation

uses
  SysUtils,
  BoldUtils,
  BoldUMLModelDataModule,
  BoldUMLAddTV;

{$R *.dfm}

procedure ShowTaggedValuesEditor(UMLElement: TUMLModelElement);
begin
  TfrmBoldUMLTaggedValuesEditor.Create(nil);
end;

procedure TfrmBoldUMLTaggedValuesEditor.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

function TfrmBoldUMLTaggedValuesEditor.GetToolName(tag: string): string;
var
  p: integer;
begin
  p := pos('.', tag);
  if p = 0 then
    Result := '<Default>'
  else
    result := copy(tag, 1, p-1);
end;

function TfrmBoldUMLTaggedValuesEditor.GetTagName(tag: string): string;
var
  p: integer;
begin
  p := pos('.', tag);
  if p = 0 then
    Result := tag
  else
    result := copy(tag, p + 1, MaxInt);
end;

procedure TfrmBoldUMLTaggedValuesEditor.bdhAllToolsDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
var
  tool: string;
  i: integer;
  ResultList: TBoldMemberList;
  StringLIst: TStringLIst;
begin
  if Assigned(RootValue) then
  begin
    with RootValue as TUMLModelElement do
    begin
      resultList := TBoldMemberFactory.CreateMemberFromBoldType(GetStringListType(BoldType)) as TBoldMemberList;
      StringLIst := TStringLIst.Create;
      for i := 0 to M_TaggedValue.Count-1 do
      begin
        tool := GetToolName(M_TaggedValue[i].Tag);
        if StringLIst.IndexOf(Tool) = -1 then begin
          ResultList.AddNew.AsString := Tool;
          StringLIst.Add(Tool);
        end;
      end;
      StringLIst.Free;
    end;
    RootValue.SubscribeToExpression('taggedValue.tag', subscriber, false);
    ResultElement.SetOwnedValue(ResultLIst);
  end;
end;

function TfrmBoldUMLTaggedValuesEditor.GetStringListType(RootTYpe: TBoldElementTypeINfo): TBoldLIstTYpeInfo;
var
  StringTypeINfo: TBoldAttributeTypeINfo;
  SystemTypeINfo: TBoldSystemTypeINfo;
begin
  SystemTypeINfo := RootType.SystemTypeInfo as TBoldSystemTypeINfo;
  stringTypeINfo := SystemTypeInfo.AttributeTypeInfoByExpressionName['string'];
  Result := SystemTypeInfo.ListTypeInfoByElement[StringTypeINfo];
end;

procedure TfrmBoldUMLTaggedValuesEditor.bdhSelectedTVsDeriveAndSubscribe(
  Sender: TComponent; RootValue: TBoldElement;
  ResultElement: TBoldIndirectElement; Subscriber: TBoldSubscriber);
var
  i: integer;
  ResultLIst: TUMLTaggedValueList;
begin
  if assigned(RootValue) then
  begin
    with behRoot.value as TUMLModelElement do
    begin
      REsultList := TBoldMemberFactory.CreateMemberFromBoldType(M_TaggedValue.BoldTYpe) as TUMLTaggedValueList;
      for i := 0 to M_taggedValue.count-1 do
        if GetToolName(M_taggedValue[i].tag) = rootValue.asstring then
          ResultLIst.Add(M_TaggedValue[i]);

      SubscribetoExpression('taggedValue.tag', subscriber, true);
    end;
  end
  else
    ResultList := nil;
  ResultElement.SetOwnedValue(ResultLIst);
end;

procedure TfrmBoldUMLTaggedValuesEditor.CreateTabs;
var
  i: Integer;
begin
  tcTools.Tabs.Clear;
  if assigned(bchAllTools.List) then
    for i := 0 to bchAllTools.List.Count-1 do
      tcTools.Tabs.Add(bchAllTools.List[i].AsString);
  if tcTools.tabs.count > 0 then
  begin
    tcTools.TabIndex := 0;
    tcToolsChange(nil);
  end;
  if (tcTools.Tabs.Count - 1) >= PreviousSelectedTab then
  begin
    tcTools.TabIndex := PreviousSelectedTab;
    tcToolsChange(nil);
  end;
end;

procedure TfrmBoldUMLTaggedValuesEditor.tcToolsChange(Sender: TObject);
begin
  bchAllTools.CurrentIndex := tcTools.TabIndex;
end;

function TfrmBoldUMLTaggedValuesEditor.BoldAsStringRenderer1GetAsString(
  aFollower: TBoldFollower): string;
begin
  Result := '';
  if Assigned(aFollower.Element) then
  begin
    Result := GetTagName((aFollower.Element as TUMLTaggedValue).Tag);
  end;
end;


procedure TfrmBoldUMLTaggedValuesEditor.BoldAsStringRenderer1Subscribe(
  aFollower: TBoldFollower; Subscriber: TBoldSubscriber);
begin
  (aFollower.Element as TUMLTaggedValue).M_Tag.DefaultSubscribe(Subscriber, breReEvaluate);
end;

procedure TfrmBoldUMLTaggedValuesEditor.PlaceSubscriptions;
begin
  if assigned(fRootSubscriber) then
  begin
    fRootSubscriber.CancelAllSubscriptions;
    behRoot.AddSubscription( fRootSubscriber, beValueIdentityChanged, breReSubscribe);
    if assigned(behRoot.Value) then
      behRoot.Value.SubscribeToExpression('taggedvalue.tag', fRootSubscriber, false);
  end;
end;

procedure TfrmBoldUMLTaggedValuesEditor.RecieveRootChanged(
  Originator: TObject; OriginalEvent: TBoldEvent;
  RequestedEvent: TBoldRequestedEvent);
begin
  case RequestedEvent of
    breReSubscribe: PlaceSubscriptions;
  end;
  PostMessage(Handle, WM_REFRESHGUI, 0, 0);
end;

procedure TfrmBoldUMLTaggedValuesEditor.FormCreate(Sender: TObject);
begin
  fRootSubscriber := TBoldPassThroughSubscriber.Create(RecieveRootChanged);
  PlaceSubscriptions;
end;

procedure TfrmBoldUMLTaggedValuesEditor.btAddTVClick(Sender: TObject);
begin
  PreviousSelectedTab := tcTools.TabIndex;
  ShowAddTVDialog(tcTools.Tabs[tcTools.TabIndex], behRoot.Value as TUMLModelElement);
end;

procedure TfrmBoldUMLTaggedValuesEditor.btDeleteTVClick(Sender: TObject);
begin
  PreviousSelectedTab := tcTools.TabIndex;
  (behRoot.Value as TUMLModelElement).DeleteTaggedValue((blhSelectedTVs.CurrentElement as TUMLTaggedValue).Tag);
end;

procedure TfrmBoldUMLTaggedValuesEditor.RefreshGUI(var Message: TMessage);
begin
  PlaceSubscriptions;
  CreateTabs;
  Caption := Format('Tagged values for the %s %s.', [LowerCase(Copy(behRoot.Value.Classname, 5, MaxInt)) , (behRoot.Value as TUMLModelElement).Name]);
end;

procedure TfrmBoldUMLTaggedValuesEditor.SwitchMode(ReadOnly: Boolean);
begin
  mnuTools.Visible := not ReadOnly;
  bmValue.ReadOnly := ReadOnly;
  if ReadOnly then
  begin
    lbTagName.Color := clInactiveCaptionText;
    bmValue.Color := clInactiveCaptionText;
  end
  else
  begin
    lbTagName.Color := clCaptionText;
    bmValue.Color := clCaptionText;
  end;
end;

destructor TfrmBoldUMLTaggedValuesEditor.destroy;
begin
  FreeAndNil(fRootSubscriber);
  inherited;
end;

procedure TfrmBoldUMLTaggedValuesEditor.Copy1Click(Sender: TObject);
begin
  DoMessage(ActiveControl, WM_COPY);
end;

procedure TfrmBoldUMLTaggedValuesEditor.DoMessage(Control: TControl; Msg: Cardinal);
begin
  if Assigned(Control) then
    Control.Perform(Msg, 0, 0);
end;

procedure TfrmBoldUMLTaggedValuesEditor.Cut1Click(Sender: TObject);
begin
  DoMessage(ActiveControl, WM_CUT);
end;

procedure TfrmBoldUMLTaggedValuesEditor.Paste1Click(Sender: TObject);
begin
  DoMessage(ActiveControl, WM_PASTE);
end;

end.