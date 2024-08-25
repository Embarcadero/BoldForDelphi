
{ Global compiler directives }
{$include bold.inc}
unit BoldUMLRose98Support;

{$WARN SYMBOL_PLATFORM OFF}


interface

uses
  Classes,
  BoldLogHandler,
  RationalRose98_TLB,
  BoldUMLModel,
  BoldTaggedValueList,
  BoldRose98Support;

const
  BOLDTOOLNAME = 'Bold';
  BOLDSTDUMLTOOLNAME = 'BoldStdUML';

type
  { forward declarations }
  TBoldUMLRose98Support = class;
  TBoldUMLRose98Properties = class;

  { TBoldUMLRose98Support }
  TBoldUMLRose98Support = class(TBoldRose98Support)
  end;

  { TBoldUMLRose98Properties }
  TBoldUMLRose98Properties = class(TBoldRose98Properties)
  private
    fEffectiveDefaults: TBoldTaggedValuePerClassList;
    function GetToolProps(RoseItem: IRoseItem; Toolname: String): IRosePropertyCollection;
    procedure TaggedValueToElement(UmlElement: TUMLModelElement; RoseProp: IRoseProperty; Toolname: String);
    function GetEffectiveDefaults: TBoldTaggedValuePerClassList;
    function GetBoldDefaultsForClass(const UMLModelName: string; RoseItem: IRoseItem): TBoldTaggedValueList;
    property EffectiveDefaults: TBoldTaggedValuePerClassList read GetEffectiveDefaults;      
  public
    destructor Destroy; override;
    procedure GetTaggedValues(RoseItem: IRoseItem; UMLElement: TUMLModelElement; AdditionalTools: TStrings);
    procedure SetTaggedValues(RoseItem: IRoseItem; UMLElement: TUMLModelElement; AdditionalTools: TStrings);
    procedure SetTaggedValuesAsDefaultProps(RoseItem: IRoseItem; UMLElement: TUMLModelElement; AdditionalTools: TStrings);
    procedure GetTaggedValuesForTool(RoseItem: IRoseItem; UMLElement: TUMLModelElement; aToolName: string);
    procedure SetTaggedValuesForTool(RoseItem: IRoseItem; UMLElement: TUMLModelElement; aToolName: string);
    procedure SetTaggedValuesForToolAsDefaultProps(RoseItem: IRoseItem; UMLElement: TUMLModelElement; aToolName: String);
    procedure ClearEffectiveDefaults;
    function StripToolName(var Tag: string): string;
    class function LogName(UMLElement: TUMLModelElement): string;
  end;

implementation

uses
  SysUtils,
  BoldUtils,
  BoldDefs,
  BoldDefaultTaggedValues,
  BoldUMLModelSupport;

class function TBoldUMLRose98Properties.LogName(UMLElement: TUMLModelElement): string;
  function EffectiveName(AssociationEnd: TUMLAssociationEnd): string;
  begin
    Result := AssociationEnd.Name;
    if Result = '' then
      Result := '(:' + AssociationEnd.otherEnd.type_.name + ')';
  end;
var
  Assocation: TUMLAssociation;
begin
  if (UMLElement is TUMLAssociation) and (UMLElement.name = '') then
  begin
    Assocation := TUMLAssociation(UMLElement);
    if Assocation.connection.Count = 2 then
      Result := EffectiveName(Assocation.connection[0]) + EffectiveName(Assocation.connection[1])
    else
      Result := '<Unnamed Association>';  
  end
  else if (UMLElement is TUMLAssociationEnd) then
  begin
    Result := LogName(TUMLAssociationEnd(UMLElement).association) + '.' + EffectiveName(TUMLAssociationEnd(UMLElement));
  end
  else
    Result := UMLElement.qualifiedName
end;

function TBoldUMLRose98Properties.GetToolProps(RoseItem: IRoseItem; Toolname: String): IRosePropertyCollection;
begin
  result := RoseItem.GetToolProperties(ToolName);
end;

procedure TBoldUMLRose98Properties.TaggedValueToElement(UmlElement: TUMLModelElement; RoseProp: IRoseProperty; Toolname: String);
begin
  TBoldUMLSupport.EnsuredTaggedValue(UMLElement, ToolName +'.' + RoseProp.Name).value := RoseProp.Value;
end;

procedure TBoldUMLRose98Properties.GetTaggedValuesForTool(RoseItem: IRoseItem; UMLElement: TUMLModelElement; aToolName:string);
var
  Index: Integer;
  PropCollection: IRosePropertyCollection;
  BoldTaggedValues: TBoldTaggedValueList;
  i: integer;
  UMLModelName: String;
begin
  UMLModelName := TBoldUMLSupport.UMLModelNameToUMLName(UMLElement.BoldClassTypeInfo.ExpressionName);
  BoldTaggedValues := GetBoldDefaultsForClass(UMLModelName, ROseItem);
  if aToolName = BOLDTOOLNAME then
  begin
    for i := 0 to BoldTaggedValues.Count-1 do
      if (RoseItem.IsOverriddenProperty(aToolName, BoldTaggedValues.Definition[i].Tag)) then
      begin
        TBoldUMLSupport.EnsuredTaggedValue(UMLElement,BOLDTOOLNAME + '.'+ BoldTaggedValues.Definition[i].Tag).value :=
          RoseItem.GetPropertyValue(BOLDTOOLNAME, BoldTaggedValues.Definition[i].Tag);
      end
      else
        TBoldUMLSupport.EnsuredTaggedValue(UMLElement, BOLDTOOLNAME + '.'+ BoldTaggedValues.Definition[i].tag).Value :=
         BoldTaggedValues.Definition[i].DefaultValue;
  end
  else
  begin
    PropCollection := GetToolProps(RoseItem, aToolName);
    for Index := 1 to PropCollection.Count do
      TaggedValueToElement(UmlElement, PropCollection.GetAt(Index), aToolname);
  end;
end;

procedure TBoldUMLRose98Properties.SetTaggedValuesForTool(RoseItem: IRoseItem; UMLElement: TUMLModelElement; aToolName:string);
var
  BoldTaggedValues: TBoldTaggedValueList;
  TaggedValue: TUMLTaggedValue;
  Tag: string;
  UMLValue: string;
  RoseValue: string;
  i: integer;
  UMLModelName: String;
begin
  UMLModelName := TBoldUMLSupport.UMLModelNameToUMLName(UMLElement.BoldClassTypeInfo.ExpressionName);
  BoldTaggedValues := GetBoldDefaultsForClass(UMLModelName, ROseItem);
  if aToolName = BOLDTOOLNAME then
  begin
    for i := 0 to BoldTaggedValues.Count-1 do
    begin
      Tag := BoldTaggedValues.Definition[i].Tag;
      UMLValue :=  UMLElement.GetBoldTV(Tag);
      if (RoseItem.IsOverriddenProperty(aToolName, Tag) or
        (UMLValue <> BoldTaggedValues.Definition[i].DefaultValue)) then
      begin
        RoseValue := RoseItem.GetPropertyValue(BOLDTOOLNAME, Tag);
        if RoseValue <> UMLValue then
        begin
          BoldLog.LogFmt('Changing property %s.%s from "%s" to "%s"', [LogName(UMLElement), Tag, RoseValue, UMLValue]);
          try
            RoseItem.OverrideProperty(aToolName, Tag, UMLValue);
          except
            on e: Exception do
              BoldLog.LogFmt('Failed to set property %s.%s to "%s": %s', [LogName(UMLElement), Tag, UMLValue, e.Message], ltWarning);
          end;
        end;
      end;
    end;
  end
  else
  begin
    for i := 0 to UMLElement.M_taggedValue.Count-1 do
    begin
      TaggedValue := UMLElement.M_taggedValue[i];
      Tag := TaggedValue.Tag;
      if StripToolName(Tag) = aToolName then
      begin
        RoseValue := RoseItem.GetPropertyValue(aToolName, Tag);
        UMLValue := TaggedValue.Value;
        if RoseValue <> UMLValue then
          begin
            BoldLog.LogFmt('Changing property %s.%s from %s to %s', [LogName(UMLElement), Tag, RoseValue, UMLValue]);
            RoseItem.OverrideProperty(aToolName, Tag, UMLValue);
          end;
        end;
    end;
  end;
end;

procedure TBoldUMLRose98Properties.SetTaggedValuesForToolAsDefaultProps(
  RoseItem: IRoseItem; UMLElement: TUMLModelElement; aToolName: String);
var
  BoldTaggedValues: TBoldTaggedValueList;
  TaggedValue: TUMLTaggedValue;
  Tag: string;
  UMLValue: string;
  RoseValue: string;
  i: integer;
  UMLModelName: String;
begin
  UMLModelName := TBoldUMLSupport.UMLModelNameToUMLName(UMLElement.BoldClassTypeInfo.ExpressionName);
  BoldTaggedValues := GetBoldDefaultsForClass(UMLModelName, ROseItem);
  if aToolName = BOLDTOOLNAME then
  begin
    for i := 0 to BoldTaggedValues.Count-1 do
    begin
      Tag := BoldTaggedValues.Definition[i].Tag;
      UMLValue :=  UMLElement.GetBoldTV(Tag);
      if (RoseItem.IsOverriddenProperty(aToolName, Tag) or
        (UMLValue <> BoldTaggedValues.Definition[i].DefaultValue))then
      begin
        RoseValue := RoseItem.GetPropertyValue(BOLDTOOLNAME, Tag);
        if RoseValue <> UMLValue then
        begin
          BoldLog.LogFmt('Changing property %s.%s from %s to %s', [LogName(UMLElement), Tag, RoseValue, UMLValue]);
          RoseItem.FindDefaultProperty(aToolName, Tag).Value := UMLValue;
        end;
      end;
    end;
  end
  else
  begin
    for i := 0 to UMLElement.M_taggedValue.Count-1 do
    begin
      TaggedValue := UMLElement.M_taggedValue[i];
      Tag := TaggedValue.Tag;
      if StripToolName(Tag) = aToolName then
      begin
        RoseValue := RoseItem.GetPropertyValue(aToolName, Tag);
        UMLValue := TaggedValue.Value;
        if RoseValue <> UMLValue then
          begin
            BoldLog.LogFmt('Changing property %s.%s from %s to %s', [LogName(UMLElement), Tag, RoseValue, UMLValue]);
            RoseItem.FindDefaultProperty(aToolName, Tag).Value := UMLValue;
          end;
        end;
    end;
  end;

end;

procedure TBoldUMLRose98Properties.GetTaggedValues(RoseItem: IRoseItem;
  UMLElement: TUMLModelElement; AdditionalTools: TStrings);
var
  i: integer;
{ part of the slower strategy
  Tools: TStringList;
  PropCollection: IRosePropertyCollection;
  DefaultProp: IRoseProperty;}
begin
  
  GetTaggedValuesForTool(RoseItem, UMLElement, ToolName);
  GetTaggedValuesForTool(RoseItem, UMLElement, BOLDSTDUMLTOOLNAME);
  for i := 0 to AdditionalTools.Count-1 do
    GetTaggedValuesForTool(RoseItem, UMLElement, AdditionalTools[i])

{ this strategy was slower...
  Tools := TStringList.Create;
  Tools.Assign(AdditionalTools);
  Tools.Add(ToolName);
  Tools.Add('BoldStdUML'); // empty string = std UML tagged values.

  PropCollection := RoseItem.GetAllProperties;
  for i := 1 to PropCollection.Count do
  begin
    DefaultProp := PropCollection.GetAt(i);
    ToolName := DefaultProp.ToolName;

    if Tools.IndexOf(ToolName) <> -1 then
    begin
      if ToolName <> '' then
        UMLElement.SetTaggedValue(ToolName + '.' + Defaultprop.Name, DefaultProp.Value)
      else
        UMLElement.SetTaggedValue(Defaultprop.Name, DefaultProp.Value)
    end;
  end;
  Tools.Free;  }
end;

procedure TBoldUMLRose98Properties.SetTaggedValues(RoseItem: IRoseItem;
  UMLElement: TUMLModelElement; AdditionalTools: TStrings);
var
  i: integer;
begin
  SetTaggedValuesForTool(RoseItem, UMLElement, ToolName);
  for i := 0 to AdditionalTools.Count-1 do
    SetTaggedValuesForTool(RoseItem, UMLElement, AdditionalTools[i])
end;

procedure TBoldUMLRose98Properties.SetTaggedValuesAsDefaultProps(
  RoseItem: IRoseItem; UMLElement: TUMLModelElement; AdditionalTools: TStrings);
var
  i: integer;
begin
  SetTaggedValuesForToolAsDefaultProps(RoseItem, UMLElement, ToolName);
  for i := 0 to AdditionalTools.Count-1 do
    SetTaggedValuesForToolAsDefaultProps(RoseItem, UMLElement, AdditionalTools[i])
end;

destructor TBoldUMLRose98Properties.Destroy;
begin
  FreeAndNil(fEffectiveDefaults);
  inherited;
end;


procedure TBoldUMLRose98Properties.ClearEffectiveDefaults;
begin
  FreeAndNil(fEffectiveDefaults);
end;


function TBoldUMLRose98Properties.GetEffectiveDefaults: TBoldTaggedValuePerClassList;
begin
  if not assigned(fEffectiveDefaults) then
    fEffectiveDefaults :=  TBoldTaggedValuePerClassList.Create;
  Result := fEffectiveDefaults;
end;

function TBoldUMLRose98Properties.GetBoldDefaultsForClass(const UMLModelName: string; RoseItem: IRoseItem): TBoldTaggedValueList;
var
  i: integer;
  BoldDefaultList: TBoldTaggedValueList;
begin
  Result := EffectiveDefaults.ListForClassName[UMLModelName];
  BoldDefaultList := BoldDefaultTaggedValueList.ListForClassName[UMLModelName];
  if Result.Count = 0 then
    for i := 0 to BoldDefaultList.Count-1 do
      Result.Add(BoldDefaultList.Definition[i].TypeName, BoldDefaultList.Definition[i].Tag, RoseItem.GetDefaultPropertyValue(BOLDTOOLNAME , BoldDefaultList.Definition[i].Tag));
end;

function TBoldUMLRose98Properties.StripToolName(
  var Tag: string): string;
var
  DotPos: integer;
begin
  DotPos := Pos('.', Tag);
  if DotPos= 0 then
    Result := BOLDSTDUMLTOOLNAME
  else
  begin
    Result := Copy(Tag, 0,  DotPos-1);
    Tag := Copy(Tag, DotPos+1, MAXINT);
  end;
end;

end.
