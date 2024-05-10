
{ Global compiler directives }
{$include bold.inc}
unit BoldWACustomAttrForm1;

interface

uses
  Windows,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  Grids,
  Buttons,
  ToolWin,
  Classes,
  ComCtrls,
  BoldWACustomAttr,
  BoldWAInterfaces,
  BoldWCodeInformer,
  BoldTemplateExpander,
  BoldWAMethodInfo,
  BoldWAStringGridManager,
  BoldTypeNameDictionary,
  ActnList;

const
  YES = '1';
  NO = '0';
  COL_METHOD_VISIBILITY = 0;
  COL_METHOD_TYPE = 1;
  COL_METHOD_NAME = 2;
  COL_METHOD_PARAMS = 3;
  COL_METHOD_RETURNTYPE = 4;
  COL_PROPERTY_NAME = 0;
  COL_PROPERTY_TYPE = 1;
  COL_PROPERTY_ACCESSTYPE = 2;

  BaseInterfaceName = 'IBoldNullableValue';
  cTBoldAttribute = 'TBoldAttribute';
  defaultMapperName = 'TBoldMemberDefaultMapper';
  defaultSingleMapperName = 'TBoldSingleColumnMember';

type
  TCustomAttrForm1 = class(TForm, IWizardForm)
    PageControl1: TPageControl;
    tsClassDef: TTabSheet;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label7: TLabel;
    Label4: TLabel;
    Label8: TLabel;
    Label6: TLabel;
    Label2: TLabel;
    cbParent: TComboBox;
    edExpressionName: TEdit;
    edDelphiName: TEdit;
    edUnitname: TEdit;
    edModelName: TEdit;
    tsProperties: TTabSheet;
    GroupBox4: TGroupBox;
    ToolBar1: TToolBar;
    bbPropertyAdd: TBitBtn;
    bbPropertyDelete: TBitBtn;
    bbPropertyMoveDown: TBitBtn;
    bbPropertyMoveUp: TBitBtn;
    StringGridProperties: TStringGrid;
    cbPropertyTypes: TComboBox;
    cbAccessTypes: TComboBox;
    edPropertyName: TEdit;
    tsMethods: TTabSheet;
    Label5: TLabel;
    GroupBox3: TGroupBox;
    ToolBar2: TToolBar;
    bbNewMethod: TBitBtn;
    bbDeleteMethod: TBitBtn;
    bbMethodMoveDown: TBitBtn;
    bbMethodMoveUp: TBitBtn;
    StringGridMethods: TStringGrid;
    cbVisibility: TComboBox;
    edMethodName: TEdit;
    cbReturnTypes: TComboBox;
    edMethodSignature: TEdit;
    cbMethodTypes: TComboBox;
    tsOverride: TTabSheet;
    GroupBox2: TGroupBox;
    CheckListBoxOverride: TCheckListBox;
    ActionList: TActionList;
    ActionStringGridInsert: TAction;
    ActionStringGridDelete: TAction;
    MemoMethodsToOverride: TMemo;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    procedure edExpressionNameChange(Sender: TObject);
    procedure tsClassDefShow(Sender: TObject);
    procedure tsPropertiesShow(Sender: TObject);
    procedure tsMethodsShow(Sender: TObject);
    procedure sbPropertyAddClick(Sender: TObject);
    procedure sbPropertyDeleteClick(Sender: TObject);
    procedure ActionStringGridInsertExecute(Sender: TObject);
    procedure ActionStringGridDeleteExecute(Sender: TObject);
    procedure cbParentChange(Sender: TObject);
    procedure CheckListBoxOverrideClickCheck(Sender: TObject);
    procedure bbPropertyMoveDownClick(Sender: TObject);
    procedure bbPropertyMoveUpClick(Sender: TObject);
    procedure bbMethodMoveDownClick(Sender: TObject);
    procedure bbMethodMoveUpClick(Sender: TObject);
    procedure edDelphiNameChange(Sender: TObject);
    procedure StringGridPropertiesDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure StringGridMethodsDrawCell(Sender: TObject; ACol,
      ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure FormDestroy(Sender: TObject);
    function getTypeDictionary: TBoldTypeNameDictionary;    
    procedure edUnitnameChange(Sender: TObject);
    function DelphiToBDEType(const DelphiType: string): string;
    function DelphiToAccessorType(const DelphiType: string): string;
    property TypeDictionary: TBoldTypeNameDictionary read getTypeDictionary;
    procedure tsOverrideShow(Sender: TObject);
  private
    { Private declarations }
    fSteppedBack: Boolean;
    fUnitGenerator: IUnitGenerator;
    fOverrideMethodsParser: TClassParser;
    fTypeDictionary: TBoldTypeNameDictionary;
    fEnableNext: TEnableNextEvent;
    fMgrStringGridProperties: TStringGridManager;
    fMgrStringGridMethods: TStringGridManager;
    fUnitName_Locked, fDelphiName_Locked: Boolean;
    procedure DisplayOverrideMethods(List: TStrings);
    procedure AssignProperties(var NewAttribute: TCustomAttribute; var Template: TBoldTemplateHolder);
    procedure GetBaseClasses(list: TStrings);
    procedure setEnableNext(Value: TEnableNextEvent);
    procedure EnableNextBtn(const Enable: Boolean);
    function ValidateProperty: Boolean;
    function ValidateMethods: Boolean;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
    destructor Destroy; override;
    procedure Finalize;

    procedure AssignParent(aParent: TWinControl);
    procedure ClearParent;
    procedure Initialize;
    function Next: integer;
    function Back: integer;
    procedure Cancel;
    procedure Finish;
    function GetSteppedBack: Boolean;
    procedure setUnitGeneratorIntf(Intf: IUnitGenerator);
    function getUnitGeneratorIntf: IUnitGenerator;

    procedure GetNewAttribute(var NewAttribute: TCustomAttribute);
    function IsValidClassDef: Boolean;
    function GetDefaultMapperName(const ClassName: string; const NumOfProperties: integer): string;
    property SteppedBack: Boolean read GetSteppedBack default false;
    property UnitGenerator: IUnitGenerator read getUnitGeneratorIntf write setUnitGeneratorIntf;
    property EnableNext: TEnableNextEvent write setEnableNext;
  end;
  function BooleanToStr(value: Boolean): string;

var
  CustomAttrForm1: TCustomAttrForm1;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldWAClassInfo,
  BoldVclUtils,
  BoldWAdmTemplates,
  BoldGUIDUtils;

{$R *.dfm}


function BooleanToStr(value: Boolean): string;
begin
  if value then
    Result := 'true'
  else
    Result := 'false';
end;

procedure TCustomAttrForm1.EnableNextBtn(const Enable: Boolean);
begin
  if Assigned(fEnableNext) then
    fEnableNext(Enable);
end;

constructor TCustomAttrForm1.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fMgrStringGridProperties := TStringGridManager.Create(StringGridProperties);
  fMgrStringGridMethods := TStringGridManager.Create(StringGridMethods);
  tsClassDef.TabVisible := false;
  tsProperties.TabVisible := false;
  tsMethods.TabVisible := false;
  tsOverride.TabVisible := false;
  fUnitName_Locked := true;
  fDelphiName_Locked := true;
end;

destructor TCustomAttrForm1.Destroy;
begin
  FreeAndNil(fMgrStringGridProperties);
  FreeAndNil(fMgrStringGridMethods);
  inherited destroy;
end;

procedure TCustomAttrForm1.Initialize;
begin
  Align := alClient;
  cbPropertyTypes.Visible := false;
  cbAccessTypes.Visible := false;
  cbMethodTypes.Visible := false;
  cbReturnTypes.Visible := false;
  cbVisibility.Visible := false;
  edPropertyName.Visible := false;
  edMethodName.Visible := false;
  edMethodSignature.Visible := false;

  { StringGridMethods }
  StringGridMethods.Cells[COL_METHOD_TYPE,0] := 'Method type';
  StringGridMethods.Cells[COL_METHOD_NAME,0] := 'Name';
  StringGridMethods.Cells[COL_METHOD_PARAMS,0] := 'Parameters';
  StringGridMethods.Cells[COL_METHOD_RETURNTYPE,0] := 'Return Type';
  StringGridMethods.Cells[COL_METHOD_VISIBILITY,0] := 'Visibility';
  fMgrStringGridMethods.addCtrlForColumn(cbMethodTypes, TComboBox, COL_METHOD_TYPE);
  fMgrStringGridMethods.addCtrlForColumn(edMethodName, TEdit, COL_METHOD_NAME);
  fMgrStringGridMethods.addCtrlForColumn(edMethodSignature, TEdit, COL_METHOD_PARAMS);
  fMgrStringGridMethods.addCtrlForColumn(cbReturnTypes, TComboBox, COL_METHOD_RETURNTYPE);
  fMgrStringGridMethods.addCtrlForColumn(cbVisibility, TComboBox, COL_METHOD_VISIBILITY);

  { StringGridProperties  }
  StringGridProperties.Cells[COL_PROPERTY_NAME,0] := 'Name';
  StringGridProperties.Cells[COL_PROPERTY_TYPE,0] := 'Type';
  StringGridProperties.Cells[COL_PROPERTY_ACCESSTYPE,0] := 'Access type';
  fMgrStringGridProperties.addCtrlForColumn(edPropertyName, TEdit, COL_PROPERTY_NAME);
  fMgrStringGridProperties.addCtrlForColumn(cbPropertyTypes, TComboBox, COL_PROPERTY_TYPE);
  fMgrStringGridProperties.addCtrlForColumn(cbAccessTypes, TComboBox, COL_PROPERTY_ACCESSTYPE);

  fOverrideMethodsParser := TClassParser.Create(TStringStream.Create(MemoMethodsToOverride.Lines.Text));
  fOverrideMethodsParser.Start;
  GetBaseClasses(cbParent.Items);

  DisplayOverrideMethods(CheckListBoxOverride.Items);
  PageControl1.ActivePage := tsClassDef;
end;

procedure TCustomAttrForm1.Finalize;
begin
  FreeAndNil(fOverrideMethodsParser);
  FreeAndNil(fTypeDictionary);
end;

procedure TCustomAttrForm1.AssignParent(aParent: TWinControl);
begin
  PageControl1.Parent := aParent;
  if (aParent is TForm) then
    (aParent as TForm).helpcontext := helpcontext;
end;

procedure TCustomAttrForm1.ClearParent;
begin
  PageControl1.Parent := self;
end;

function TCustomAttrForm1.Next: integer;
begin
  if PageControl1.ActivePage = tsClassDef then
  begin
    Result := wfaNext;
    if IsValidClassDef then
    begin
      PageControl1.ActivePage := tsProperties;
      EnableNextBtn(true);
      Result := wfaNext;
    end;
    Exit;
  end
  else if PageControl1.ActivePage = tsProperties then
  begin
    PageControl1.ActivePage := tsMethods;
    EnableNextBtn(true);
    Result := wfaNext;
    Exit;
  end
  else if PageControl1.ActivePage = tsMethods then
  begin
    PageControl1.ActivePage := tsOverride;
    EnableNextBtn(true);
    Result := wfaLast;
    Exit;
  end;
  if (MessageDlg('The Wizard will now generate code for your new attribute!' + #13 +
     '                               Continue? ',mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    Result := wfaFinish
  else
    Result := wfaIgnore;
end;

procedure TCustomAttrForm1.Finish;
var
  NewAttribute: TCustomAttribute;
  InterfaceName,
  MapperName,
  InterfaceUnitName,
  PMapperUnitName: string;
  UnitNamePrefix: string;
begin
  NewAttribute := TCustomAttribute.Create;
  GetNewAttribute(NewAttribute);
  if IsEmptyStr(NewAttribute.UnitName) then
    UnitNamePrefix := NewAttribute.ExpressionName
  else
    UnitNamePrefix := NewAttribute.UnitName;
  PMapperUnitName := Format('%sPMapper',[UnitNamePrefix]);
  InterfaceUnitName := Format('%sInterface',[UnitNamePrefix]);
  InterfaceName := Format('I%s',[NewAttribute.ExpressionName]);
  MapperName :=Format('%sPMapper',[NewAttribute.DelphiName]);
  try
    with attrdatamodule.AttributeTemplate do
    begin
      Variables.Add('UNITNAME', NewAttribute.UnitName, []);
      Variables.Add('EXPRESSIONNAME', NewAttribute.ExpressionName, []);
      Variables.Add('DELPHINAME', NewAttribute.DelphiName, []);
      Variables.Add('SUPERCLASS', NewAttribute.Parent, []);
      Variables.Add('INTERFACENAME', InterfaceName, []);
      Variables.Add('INTERFACEUNITNAME', InterfaceUnitName, []);
      if (NewAttribute.Properties.Count > 0) then
      begin
        Variables.Add('MAPPERNAME',MapperName,[]);
        Variables.Add('CONTENTNAME', Format('ContentName_%s', [NewAttribute.ExpressionName]), []);
      end
      else
      begin
        Variables.Add('MAPPERNAME',Format('<same as for %s>',[NewAttribute.Parent]),[]);
        Variables.Add('CONTENTNAME', Format('<same as for %s>', [NewAttribute.Parent]), []);
      end
    end;
    NewAttribute.AssignMethodsToTemplate(attrdatamodule.AttributeTemplate);
    AssignProperties(NewAttribute, attrdatamodule.AttributeTemplate);
    if (NewAttribute.Properties.Count > 0) then
    begin
      with attrdatamodule.InterfaceTemplate do
      begin
        Variables.Add('SUPERINTERFACENAME', BaseInterfaceName, []);
        Variables.Add('INTERFACENAME',InterfaceName, []);
        Variables.Add('UNITNAME',InterfaceUnitName, []);
        Variables.Add('INTERFACEGUID',BoldCreateGUIDAsString, []);
        Variables.Add('FREESTANDINGDELPHINAME','TBFS' + NewAttribute.ExpressionName,[]);
        Variables.Add('FREESTANDINGSUPERCLASS','TBoldFreeStandingNullableValue',[]);
        Variables.Add('INTERFACEUNITNAME',Format('%sInterface',[NewAttribute.UnitName]), []);
        Variables.Add('EXPRESSIONNAME', NewAttribute.ExpressionName, []);
      end;
      AssignProperties(NewAttribute, attrdatamodule.InterfaceTemplate);
      with attrdatamodule.MapperTemplate do
      begin
        Variables.Add('MAPPERNAME',NewAttribute.DelphiName+ 'PMapper',[]);
        Variables.Add('SUPERMAPPERNAME',getDefaultMapperName(NewAttribute.parent, NewAttribute.Properties.Count) ,[]);
        Variables.Add('STREAMCONSTANT', 'StreamName',[]);
        Variables.Add('INTERFACEUNITNAME',InterfaceUnitName, []);
        Variables.Add('UNITNAME', PMapperUnitName, []);
        Variables.Add('INTERFACENAME',InterfaceName, []);
      end;
      AssignProperties(NewAttribute, attrdatamodule.MapperTemplate);
    end;
    if Assigned(UnitGenerator) then
      UnitGenerator.GenerateUnit(NewAttribute.UnitName, attrdatamodule.AttributeTemplate);
      if (NewAttribute.Properties.Count > 0) then
      begin
        UnitGenerator.GenerateUnit(InterfaceUnitName, attrdatamodule.InterfaceTemplate);
        UnitGenerator.GenerateUnit(PMapperUnitName, attrdatamodule.MapperTemplate);
      end;
  finally
    FreeAndNil(NewAttribute);
  end;
end;

function TCustomAttrForm1.Back: integer;
begin
  Result := wfaPrevious;
  fSteppedBack := false;
  if (PageControl1.ActivePage = tsProperties) then
  begin
    PageControl1.ActivePage := tsClassDef;
    tsClassDef.Visible := true;
    Exit;
  end
  else if (PageControl1.ActivePage = tsMethods) then
  begin
    PageControl1.ActivePage := tsProperties;
    Exit;
  end
  else if (PageControl1.ActivePage = tsOverride) then
  begin
    PageControl1.ActivePage := tsMethods;
    Exit;
  end;
  fSteppedBack:= true;
  Result := wfaBack;
end;

procedure TCustomAttrForm1.Cancel;
begin
  fSteppedBack := false;
end;

procedure TCustomAttrForm1.setEnableNext(Value: TEnableNextEvent);
begin
  fEnableNext := Value;
end;

function TCustomAttrForm1.GetSteppedBack: Boolean;
begin
  Result := fSteppedBack;
end;

procedure TCustomAttrForm1.setUnitGeneratorIntf(Intf: IUnitGenerator);
begin
  fUnitGenerator := Intf;
end;

function TCustomAttrForm1.getUnitGeneratorIntf: IUnitGenerator;
begin
  Result := fUnitGenerator;
end;

procedure TCustomAttrForm1.GetNewAttribute(var NewAttribute: TCustomAttribute);
var
  NewMethod, temp: TMethodInfo;
  i: Integer;
begin
  with NewAttribute do
  begin
    Parent := Trim(cbParent.Text);
    DelphiName := edDelphiName.Text;
    ExpressionName := edExpressionName.Text;
    UnitName := trim(edUnitname.Text);
    Properties.Clear;

    for i:= 1 to StringGridProperties.RowCount - 1 do
      if not IsEmptyStr(StringGridProperties.Rows[i].Text) then
      begin
        Properties.Add(TPropertyInfo.Create(StringGridProperties.Cells[0,i], StringGridProperties.Cells[1,i],
                            StrToAccessType(StringGridProperties.Cells[2,i])));
      end;
    Methods.Clear;

    for i := 1 to StringGridMethods.RowCount - 1 do
      if not IsEmptyStr(StringGridMethods.Rows[i].Text) then
      begin
        NewMethod := TMethodInfo.Create;








        NewMethod.Assign(TMethodInfo.StrToMethodType(Trim(StringGridMethods.Cells[COL_METHOD_TYPE,i])),
          Trim(StringGridMethods.Cells[COL_METHOD_NAME,i]), Trim(StringGridMethods.Cells[COL_METHOD_PARAMS,i]),
          Trim(StringGridMethods.Cells[COL_METHOD_RETURNTYPE, i]), TMethodInfo.StrToVisibility(Trim(StringGridMethods.Cells[COL_METHOD_VISIBILITY, i])), []);
        Methods.Add(NewMethod);
      end;

    for i:= 0 to CheckListBoxOverride.Items.Count - 1 do
    if CheckListBoxOverride.Checked[i] then
    begin
      NewMethod := TMethodInfo.Create;
      temp := fOverrideMethodsParser.getMethodbyTreeIndex(i);
      NewMethod.Assign(temp.methodType, temp.Name, temp.Params, temp.ReturnType,
                          temp.Visibility, [mdOverride]);
      Methods.Add(NewMethod);
    end;
  end;
end;

  { FormCreate  }
procedure TCustomAttrForm1.edExpressionNameChange(Sender: TObject);
begin
  EnableNextBtn(IsValidClassDef);
  if fDelphiName_Locked then
    edDelphiName.Text := 'T' + edExpressionName.Text;
  edModelName.Text := LowerCase(edExpressionName.Text) ;
  if fUnitName_Locked then
    edUnitName.Text := edExpressionName.Text;
end;

procedure TCustomAttrForm1.tsClassDefShow(Sender: TObject);
begin
  cbParent.SetFocus;
  tsClassDef.Parent.HelpContext := tsClassDef.HelpContext;
end;

procedure TCustomAttrForm1.tsPropertiesShow(Sender: TObject);
begin
  bbPropertyAdd.SetFocus;
  tsProperties.Parent.HelpContext := tsProperties.HelpContext;
end;

procedure TCustomAttrForm1.tsMethodsShow(Sender: TObject);
begin
  bbNewMethod.SetFocus;
  tsMethods.Parent.HelpContext := tsMethods.HelpContext;
end;

procedure TCustomAttrForm1.DisplayOverrideMethods(List: TStrings);
var
  i, j: integer;
  CurrentClass: TClassInfo;
  Item: TMethodInfo;
  Index: integer;
begin
  i:= 0;
  List.BeginUpdate;
  List.Clear;
  while(fOverrideMethodsParser.getClasses(CurrentClass, i)) do
  begin
    if Assigned(CurrentClass) then
    j:= 0;
    while CurrentClass.getMethods(stAll,Item,j)  do
      if Assigned(Item) then
      begin
        Index := List.Add(Item.methodTypeAsString + ' ' +item.Name + item.Params);
        if (Item.methodType = mtFunction) then
          List[Index] := List[Index] + ': '+ item.ReturnType;
      end;
  end;
  List.EndUpdate;
end;

procedure TCustomAttrForm1.AssignProperties(var NewAttribute: TCustomAttribute; var Template: TBoldTemplateHolder);
var
  i: integer;
  aProperty: TPropertyInfo;
begin
  for i:= 0 to NewAttribute.Properties.Count - 1 do
    begin
      aProperty := NewAttribute.Properties[i];
      Template.Variables.Add(Format('FIELDNAME.%d',[i]), aProperty.Name, []);
      Template.Variables.Add(Format('FIELDTYPE.%d',[i]), aProperty.pType, []);
      Template.Variables.Add(Format('FIELDBDETYPE.%d',[i]), DelphiToBDEType(aProperty.pType), []);
      Template.Variables.Add(Format('FIELDACCESSORTYPE.%d',[i]), DelphiToAccessorType(aProperty.pType), []);
      if (aProperty.AccessType  = atWriteOnly)  then
        Template.Variables.Add(Format('FIELDREADABLE.%d',[i]), NO, [])
      else
        Template.Variables.Add(Format('FIELDREADABLE.%d',[i]), YES, []);
      if (aProperty.AccessType  = atReadOnly)  then
        Template.Variables.Add(Format('FIELDWRITABLE.%d',[i]), NO, [])
      else
        Template.Variables.Add(Format('FIELDWRITABLE.%d',[i]), YES, []);
    end;
  Template.Variables.Add('FIELDCOUNT', IntToStr(NewAttribute.Properties.Count), []);
  if (NewAttribute.Properties.Count > 0) then
  begin
    Template.Variables.SetVariable('PUBLIC', 'true');
    Template.Variables.SetVariable('PRIVATE', 'true');
    Template.Variables.SetVariable('PROTECTED', 'true');    
    Template.Variables.Add('INTERFACEDCLASS',YES, []);
  end
  else
    Template.Variables.Add('INTERFACEDCLASS', NO, []);
end;

procedure TCustomAttrForm1.GetBaseClasses(list: TStrings);
var
  i: integer;
begin
{  // create parser object, free in finalizer, classes hard coded
  fAttributeClassParser := TClassParser.Create(TStringStream.Create(MemoBoldClasses.Lines.Text));
  fAttributeClassParser.Start;
  i:= 0;
  while fAttributeClassParser.getClasses(ClassInfo, i) do
    if Assigned(ClassInfo) then
      cbParent.Items.Add(ClassInfo.DelphiName);}
  TypeDictionary.AddDefaultMappings;
  list.BeginUpdate;
  list.Clear;
  list.Add(cTBoldAttribute);
  for i:= 0 to TypeDictionary.Count - 1  do
  begin
    if list.IndexOf(TypeDictionary.Mapping[i].ExpandedDelphiName) = -1 then
    begin
      if (length(TypeDictionary.Mapping[i].ExpandedMapperName) <> 0) then
        list.Add(TypeDictionary.Mapping[i].ExpandedDelphiName);
    end;
  end;
  list.EndUpdate;
end;

procedure TCustomAttrForm1.sbPropertyAddClick(Sender: TObject);
begin
  fMgrStringGridProperties.Add;
end;

procedure TCustomAttrForm1.sbPropertyDeleteClick(Sender: TObject);
begin
  fMgrStringGridProperties.Delete;
end;

function TCustomAttrForm1.IsValidClassDef: Boolean;
begin
  Result := not (IsEmptyStr(trim(cbParent.Text)) OR IsEmptyStr(Trim(edExpressionName.Text)) OR IsEmptyStr(Trim(edDelphiName.Text)));
end;

function TCustomAttrForm1.GetDefaultMapperName(const ClassName: string; const NumOfProperties: integer): string;
var
  i: integer;
begin
  if (CompareText(ClassName, cTBoldAttribute) = 0) then
  begin
    if (NumOfProperties = 1) then
      Result := defaultSingleMapperName
    else
      Result := defaultMapperName;
    Exit;
  end;
  for i:= 0 to TypeDictionary.Count - 1 do
    if (CompareText(TypeDictionary.Mapping[i].ExpandedDelphiName ,ClassName) = 0) then
    begin
      Result := TypeDictionary.Mapping[i].ExpandedMapperName;
      Break;
    end;
end;

procedure TCustomAttrForm1.ActionStringGridInsertExecute(Sender: TObject);
begin
  if tsProperties.Visible then
    if ValidateProperty then
      fMgrStringGridProperties.Add
    else
      MessageDlg('Invalid property entry', mtInformation, [mbOk], 0)
  else if tsMethods.Visible then
    if ValidateMethods then
      fMgrStringGridMethods.Add
    else
      MessageDlg('Invalid method entry', mtInformation, [mbOk], 0);

end;

procedure TCustomAttrForm1.ActionStringGridDeleteExecute(Sender: TObject);
begin
  if tsProperties.Visible then
    fMgrStringGridProperties.Delete
  else if tsMethods.Visible then
    fMgrStringGridMethods.Delete;
end;


procedure TCustomAttrForm1.cbParentChange(Sender: TObject);
var
  i: integer;
begin
  EnableNextBtn(IsValidClassDef);
  CheckListBoxOverride.Items.BeginUpdate;
  if (AnsiCompareText(cbParent.Text, cTBoldAttribute) = 0) then
    for i:= 0 to CheckListBoxOverride.Items.Count - 1  do
    begin
      if MethodIsOverriden(fOverrideMethodsParser.getMethodbyTreeIndex(i).name) then
        CheckListBoxOverride.State[i] := cbChecked;
    end
  else
    for i:= 0 to CheckListBoxOverride.Items.Count - 1 do
      if (CheckListBoxOverride.State[i] = cbChecked) then
        CheckListBoxOverride.State[i] := cbUnchecked;
   CheckListBoxOverride.Items.EndUpdate;
end;

procedure TCustomAttrForm1.CheckListBoxOverrideClickCheck(
  Sender: TObject);
var
  i: integer;
begin
  if (AnsiCompareText(cbParent.Text, cTBoldAttribute) = 0) then
  begin
    i:= CheckListBoxOverride.ItemIndex;
    if MethodIsOverriden(fOverrideMethodsParser.getMethodbyTreeIndex(i).Name) then
      CheckListBoxOverride.State[i] := cbChecked;
  end;
end;

procedure TCustomAttrForm1.bbPropertyMoveDownClick(Sender: TObject);
begin
  fMgrStringGridProperties.MoveDown;
end;

procedure TCustomAttrForm1.bbPropertyMoveUpClick(Sender: TObject);
begin
  fMgrStringGridProperties.MoveUp;
end;

procedure TCustomAttrForm1.bbMethodMoveDownClick(Sender: TObject);
begin
  fMgrStringGridMethods.MoveDown;
end;

procedure TCustomAttrForm1.bbMethodMoveUpClick(Sender: TObject);
begin
  fMgrStringGridMethods.MoveUp;
end;

procedure TCustomAttrForm1.edDelphiNameChange(Sender: TObject);
begin
  fDelphiName_Locked := (Trim(edDelphiName.Text) = 'T' + Trim(edExpressionName.Text));
  EnableNextBtn(IsValidClassDef);
end;

function TCustomAttrForm1.ValidateProperty: Boolean;
var
  EmptyColCount: integer;
  i: integer;
begin
  Result := true;
  for i:= 1 to StringGridProperties.RowCount - 1 do
  begin
    EmptyColCount := 0;
    if IsEmptyStr(StringGridProperties.Cells[COL_PROPERTY_NAME, i]) then
      Inc(EmptyColCount);
    if IsEmptyStr(StringGridProperties.Cells[COL_PROPERTY_TYPE, i]) then
      Inc(EmptyColCount);
    if IsEmptyStr(StringGridProperties.Cells[COL_PROPERTY_ACCESSTYPE, i]) then
      Inc(EmptyColCount);
    if not ((EmptyColCount mod 3)= 0) then
    begin
      Result := false;
      Break;
    end;
  end;
  EnableNextBtn(result);
end;

function TCustomAttrForm1.ValidateMethods: Boolean;
var
  EmptyColCount: integer;
  Enable: Boolean;
  i: integer;
begin
  Result := true;
  for i:= 1 to StringGridMethods.RowCount - 1 do
  begin
    EmptyColCount := 0;
    Enable := true;
    if IsEmptyStr(StringGridMethods.Cells[COL_METHOD_VISIBILITY, i]) then
      Inc(EmptyColCount);
    if IsEmptyStr(StringGridMethods.Cells[COL_METHOD_TYPE, i]) then
      Inc(EmptyColCount);
    if IsEmptyStr(StringGridMethods.Cells[COL_METHOD_NAME, i]) then
      Inc(EmptyColCount);
    if (TMethodInfo.StrToMethodType(StringGridMethods.Cells[COL_METHOD_TYPE, i])= mtFunction) and
       IsEmptyStr(StringGridMethods.Cells[COL_METHOD_RETURNTYPE, i]) then
       Enable := false;
    if (TMethodInfo.StrToMethodType(StringGridMethods.Cells[COL_METHOD_TYPE, i]) <>  mtFunction) and
       not IsEmptyStr(StringGridMethods.Cells[COL_METHOD_RETURNTYPE, i]) then
       Enable := false;
    if not (((EmptyColCount mod 3)= 0) AND Enable) then
    begin
      Result := false;
      Break;
    end;
  end;
  EnableNextBtn(result);
end;

procedure TCustomAttrForm1.StringGridPropertiesDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (ARow >= 1) then
    ValidateProperty;
end;

procedure TCustomAttrForm1.StringGridMethodsDrawCell(Sender: TObject;
  ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if (ARow >= 1) then
    ValidateMethods;
end;

procedure TCustomAttrForm1.FormDestroy(Sender: TObject);
begin
  if Assigned(fUnitGenerator) then
    fUnitGenerator := nil;
end;

function TCustomAttrForm1.getTypeDictionary: TBoldTypeNameDictionary;
begin
  if not Assigned(fTypeDictionary) then
    fTypeDictionary  := TBoldCurrentTypeNameDictionaryClass.Create(nil);
  Result := fTypeDictionary;
end;

procedure TCustomAttrForm1.edUnitnameChange(Sender: TObject);
begin
  fUnitName_Locked := (Trim(edUnitname.Text) = Trim(edExpressionName.Text));
end;

function TCustomAttrForm1.DelphiToBDEType(const DelphiType: string): string;
begin
  if (CompareText(DelphiType,'integer') = 0)then
    Result := 'ftInteger'
  else if (CompareText(DelphiType,'real') = 0) then
    Result := 'ftFloat'
  else if (CompareText(DelphiType,'extended') = 0) then
    Result := 'ftFloat'
  else if (CompareText(DelphiType,'cardinal')= 0) then
    Result := 'ftInteger'
  else if (CompareText(DelphiType,'char') = 0) then
    Result := 'ftFixedChar'
  else if (CompareText(DelphiType,'string')= 0) then
    Result := 'ftString'
  else if (CompareText(DelphiType,'boolean')= 0) then
    Result := 'ftBoolean'
  else
    Result := '<BDEDataType>';
end;

function TCustomAttrForm1.DelphiToAccessorType(const DelphiType: string): string;
begin
  if (CompareText(DelphiType,'integer') = 0) then
    Result := 'Integer'
  else if (CompareText(DelphiType,'real') = 0) then
    Result := 'Double'
  else if (CompareText(DelphiType,'extended') = 0) then
    Result := 'Double'
  else if (CompareText(DelphiType,'cardinal') = 0) then
    Result := 'Integer'
  else if (CompareText(DelphiType, 'char') = 0) then
    Result := 'Char'
  else if (CompareText(DelphiType,'string') = 0) then
    Result := 'String'
  else if (CompareText(DelphiType,'boolean') = 0) then
    Result := 'Boolean'
  else
    Result := '<DataType>';
end;

procedure TCustomAttrForm1.tsOverrideShow(Sender: TObject);
begin
  CheckListBoxOverride.SetFocus;
end;

end.
