{ Global compiler directives }
{$include bold.inc}
unit BoldXMLRequests;

interface

uses
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  Classes,
  BoldStringList,
  BoldDefs;

const
  DEFAULT_ACTION_PATH = '/SOAP:Envelope/SOAP:Body';
  DEFAULT_ACTION_NAME = 'BoldAction';
  DEFAULT_DOCUMENT_ELEMENT_NAME = 'BoldDocument';
  DEFAULT_IDSTRING_TAG = 'BoldId';
  DEFAULT_IDENTIFIEDVALUE_TAG = 'BoldObject';
  DEFAULT_VERSION_NO = '1.0';
  DEFAULT_ENCODING = 'iso-8859-1';
  DEFAULT_STANDALONE = TRUE;

type
  { forward declarations }
  TBoldXMLRequest = class;

  { method prototypes }
  TBoldXMLMethodEvent = procedure (const request: TBoldXMLRequest;
            out response: string) of object;

  { TBoldXMLRequest }
  TBoldXMLRequest = class
  private
    FIsReadOnly: Boolean;
    FDomDocument :IXMLDomDocument;
    FActionPath: string;
    FActionElement: IXMLDomElement;
    FParams: TBoldStringList;
    FIdentifiedValues: TBoldStringList;
    function getParams: TBoldStringList;
    function getIdentifiedValues: TBoldStringList;
    function getActionName: string;
  protected
    function GetActionElement: IXMLDomElement;
    function getActionPath: string; virtual;
    procedure setActionPath(const Value: string); virtual;
    procedure ExtractParams(Params: TBoldStringList); virtual;
    procedure ExtractIdentifiedValues(Values: TBoldStringList; const IdStringTag: string = DEFAULT_IDSTRING_TAG); virtual;
    procedure LoadIdentifiedValues(ParentElement:IXMLDomElement; const Values: TStrings;
                          const DomElementTag: string; const AttributeTag: string); virtual;
    procedure LoadParams(ParentElement: IXMLDomElement; const Params: TStrings); virtual;
  public
    constructor CreateFromXML(const XML: WideString); virtual;
    constructor CreateInitialized(const VersionNo: string = DEFAULT_VERSION_NO; const Encoding: string = DEFAULT_ENCODING;
                    const StandAlone: Boolean = DEFAULT_STANDALONE);
    constructor Create;
    procedure SetIdentifiedValues(const Values: TStrings; const DomElementTag: string = DEFAULT_IDENTIFIEDVALUE_TAG;
                const AttributeTag: string = DEFAULT_IDSTRING_TAG);
    procedure SetParams(const Params: TStrings);
    procedure EnsureRoot(const TagName: string);
    function SetAction(const ActionName: string; const ActionPath: string = DEFAULT_ACTION_PATH): IXMLDomElement;
    procedure DeleteAction;
    procedure AddParam(const Name: string; const Value: string);
    procedure AddIdentifiedValue(const IdString: string; const Value: string;
        const DomElementTag: string = DEFAULT_IDENTIFIEDVALUE_TAG;
        const AttributeTag: string = DEFAULT_IDSTRING_TAG);
    procedure ReloadIdentifiedValues;    
    property DomDocument: IXMLDomDocument read FDomDocument;
    property Params: TBoldStringList read getParams;
    property IdentifiedValues: TBoldStringList read getIdentifiedValues;
    property ActionElement: IXMLDomElement read getActionElement;
    property ActionPath: string read getActionPath write setActionPath;
    property ActionName: string read getActionName;
  end;

implementation

uses
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  Windows;

  {TBoldXMLRequest}

constructor TBoldXMLRequest.CreateFromXML(const XML: WideString);
begin
  FIsReadOnly := true;
  FDomDocument := CoDOMDocument.Create;
  FDomDocument.async := false;
  FActionPath := DEFAULT_ACTION_PATH;
  if FDomDocument.loadXML(XML) then
    ActionPath := DEFAULT_ACTION_PATH
  else
    raise EBoldXMLLoadError.Create(FDomDocument.parseError.reason);
end;

procedure TBoldXMLRequest.ExtractIdentifiedValues(Values: TBoldStringList;
      const IdStringTag: string = DEFAULT_IDSTRING_TAG);
var
  i: integer;
  ChildNodes: IXMLDomNodeList;
  CurrentElement: IXMLDomElement;
  AttributeNode: IXMLDomAttribute;
begin
  if Assigned(ActionElement) and Assigned(Values) then
  begin
    ChildNodes := ActionElement.selectNodes('./*');
    if Assigned(ChildNodes) then
      for i:= 0 to ChildNodes.Get_length - 1 do
      begin
        if (ChildNodes[i].QueryInterface(IID_IXMLDOMElement, CurrentElement) = S_OK) then
        begin
          AttributeNode := CurrentElement.getAttributeNode(IdStringTag);
          if Assigned(AttributeNode) and  (AttributeNode.text <> '') then
            Values.Values[AttributeNode.text] := CurrentElement.text;
        end;
      end;
  end;
end;

procedure TBoldXMLRequest.ExtractParams(Params: TBoldStringList);
var
  i: integer;
  ChildNodes: IXMLDomNodeList;
  CurrentElement: IXMLDomElement;
begin
  if Assigned(ActionElement) and Assigned(Params) then
  begin
    ChildNodes := ActionElement.selectNodes('./*');
    if Assigned(ChildNodes) then
      for i:= 0 to ChildNodes.Get_length - 1 do
        if (ChildNodes[i].QueryInterface(IID_IXMLDOMElement, CurrentElement) = S_OK) then
          Params.Values[CurrentElement.tagName] := CurrentElement.text;
  end;
end;

function TBoldXMLRequest.GetActionElement: IXMLDomElement;
var
  elem: IXMLDomElement;
begin
  if FIsReadOnly then
    if not Assigned(FActionElement) then
    begin
      elem := DomDocument.documentElement.selectSingleNode(Format('.%s', [FActionPath])) as IXMLDomElement;
      if Assigned(Elem) then
        FActionElement := Elem.firstChild as IXMLDomElement;
    end;
  Result := FActionElement;
end;

procedure TBoldXMLRequest.SetIdentifiedValues(const Values: TStrings;
  const DomElementTag, AttributeTag: string);
begin
  if not FIsReadOnly then
  begin
    if Assigned(ActionElement) then
      LoadIdentifiedValues(ActionElement, Values, DomElementTag, AttributeTag)
    else
      raise EBold.CreateFmt(sSOAPActionNotSet, [ClassName, 'SetIdentifiedValues']); // do not localize
  end
  else
    raise EBold.CreateFmt(sCannotSetPropertyWhenReadOnly, [ClassName, 'SetIdentifiedValues']); // do not localize
end;

procedure TBoldXMLRequest.SetParams(const Params: TStrings);
begin
  if not FIsReadOnly then
  begin
    if Assigned(ActionElement) then
      LoadParams(ActionElement, Params)
    else
      raise EBold.CreateFmt(sSOAPActionNotSet, [ClassName, 'SetParams']); // do not localize
  end
  else
    raise EBold.CreateFmt(sCannotSetPropertyWhenReadOnly, [ClassName, 'SetParams']); // do not localize
end;

function TBoldXMLRequest.getActionName: string;
begin
  if Assigned(ActionElement) then
    Result := ActionElement.baseName;
end;

function TBoldXMLRequest.getActionPath: string;
begin
  if FIsReadOnly then
    Result := FActionPath
  else if Assigned(ActionElement) then
    Result := FActionPath;
end;

function TBoldXMLRequest.getIdentifiedValues: TBoldStringList;
begin
  if not Assigned(FIdentifiedValues) then
  begin
    FIdentifiedValues := TBoldStringList.Create;
    ExtractIdentifiedValues(FIdentifiedValues);
  end;
  Result := FIdentifiedValues;
end;

function TBoldXMLRequest.getParams: TBoldStringList;
begin
  if not Assigned(FParams) then
    FParams := TBoldStringList.Create
  else
    FParams.Clear;
  ExtractParams(FParams);
  Result := FParams;
end;

procedure TBoldXMLRequest.setActionPath(const Value: string);
begin
  if FIsReadOnly then
  begin
    if (Value <> FActionPath) then
    begin
      if (Trim(Value) = '') then
        FActionPath := DEFAULT_ACTION_PATH
      else
        FActionPath := Value;
      FActionElement := nil;
    end
  end
  else
    raise EBold.CreateFmt(sCannotSetPropertyWhenReadOnly, [ClassName, 'ActionPath']);; // do not localize
end;

constructor TBoldXMLRequest.CreateInitialized(const VersionNo: string = DEFAULT_VERSION_NO; const Encoding: string = DEFAULT_ENCODING;
                    const StandAlone: Boolean = DEFAULT_STANDALONE);
  function BooleanToStr(value: Boolean): string;
  begin
    if Value then
      result := 'yes'
    else
      result := 'no';
  end;
var
  ProcessingInstruction: IXMLDOMProcessingInstruction;
begin
  inherited Create;
  FIsReadOnly := false;
  FDomDocument := CoDOMDocument.Create;
  FDomDocument.async := FALSE;
  ProcessingInstruction := FDomDocument.createProcessingInstruction('xml', Format('version="%s" encoding="%s" standalone="%s"', [VersionNo, Encoding, BooleantoStr(StandAlone)]));
  FDomDocument.appendChild(ProcessingInstruction);
  FActionPath := DEFAULT_ACTION_PATH;  
end;

function TBoldXMLRequest.SetAction( const ActionName: string; const ActionPath: string = DEFAULT_ACTION_PATH): IXMLDomElement;
var
  ActionNode: IXMLDomNode;
  NewElement, ChildElement: IXMLDomElement;
  temp, Tag: string;
  p: integer;
  IsDefault: Boolean;
begin
  if not FIsReadOnly then
  begin
    if (Trim(ActionPath) = '') or (Trim(ActionName) = '') then
    begin
      Result := nil;
      raise EBold.CreateFmt(sSetActionInvalidArgs, [ClassName]);
    end;
    if ((Trim(ActionPath) <> FActionPath) and (ActionName <> self.ActionName)) or
      not Assigned(ActionElement) then
    begin
      FActionPath := ActionPath;
      EnsureRoot('');
      ActionNode := DomDocument.DocumentElement.selectSingleNode(Format('.%s/%s',[ActionPath, ActionName]));
      IsDefault := (ActionPath = DEFAULT_ACTION_PATH);
      if not Assigned(ActionNode)then
      begin
        temp := Format('%s/%s', [ActionPath, ActionName]);
        p := LastDelimiter('/', temp);
        while (p > 0) do
        begin
          Tag := Copy(temp, p+1, Maxint);
          temp := Copy(temp, 1, p - 1);
          NewElement := DomDocument.CreateElement(Tag);
          if IsDefault and (Tag = 'SOAP:Envelope') then
            NewElement.setAttribute('xmlns:SOAP', 'urn:schemas-xmlssoap-org:soap.v1');
          if IsDefault and (Pos('m:', Tag) > 0) then
            NewElement.setAttribute('xmlns:m', 'www.boldsoft.com/products/boldfordelphi');
          if Assigned(ChildElement) then
            NewElement.appendChild(ChildElement);
          ChildElement := NewElement;
          if (temp = '') then
          begin
            DomDocument.DocumentElement.appendChild(ChildElement);
            ActionNode := DomDocument.DocumentElement.selectSingleNode(Format('.%s/%s',[ActionPath, ActionName]));
          end;
          p := LastDelimiter('/', temp);
        end;
      end;
      Result := ActionNode as IXMLDomElement;
      FActionElement := Result;
    end
    else
      Result := FActionElement;
  end
  else
    Result := nil;
end;

procedure TBoldXMLRequest.EnsureRoot(const TagName: string);
var
  RootElement: IXMLDomElement;
  aTag: string;
begin
  if not FIsReadOnly then
  begin
    if not assigned(DomDocument) then
      raise EBold.CreateFmt(sSOAPDOMDocumentMissing, [classname]);
    RootElement := DomDocument.Get_documentElement;
    if not Assigned(RootElement) then
    begin
      aTag := Trim(TagName);
      if (aTag = '') then
        aTag := DEFAULT_DOCUMENT_ELEMENT_NAME;
      DomDocument.documentElement := DomDocument.createElement(aTag);
    end;
  end;
end;

procedure TBoldXMLRequest.LoadIdentifiedValues(ParentElement:IXMLDomElement; const Values: TStrings;
                                    const DomElementTag: string; const AttributeTag: string);
var
  i: integer;
  NewElement: IXMLDomElement;
begin
  if Assigned(Values) then
    for i:= 0 to Values.Count - 1 do
    begin
      NewElement := DomDocument.createElement(DomElementTag);
      NewElement.setAttribute(AttributeTag, Values.Names[i]);
      NewElement.Set_text(Values.Values[Values.Names[i]]);
      ParentElement.appendChild(NewElement);
    end;
end;

procedure TBoldXMLRequest.LoadParams(ParentElement:IXMLDomElement; const Params: TStrings);
var
  i: integer;
  NewElement: IXMLDomElement;
begin
  if Assigned(Params) then
    for i:= 0 to Params.Count - 1 do
    begin
      NewElement := DomDocument.createElement(Params.Names[i]);
      NewElement.Set_text(Params.Values[Params.Names[i]]);
      ParentElement.appendChild(NewElement);
    end;
end;

procedure TBoldXMLRequest.AddParam(const Name, Value: string);
var
  NewElement: IXMLDomElement;
begin
  if FIsReadOnly then
    raise EBold.CreateFmt(sCannotPerformInReadOnlyMode, [ClassName, 'AddParam']) // do not localize
  else if Assigned(ActionElement) then
  begin
    NewElement := DomDocument.createElement(Name);
    NewElement.Set_text(Value);
    ActionElement.appendChild(NewElement);
  end
  else
    raise EBold.CreateFmt(sSOAPActionNotSet, [ClassName, 'AddParam']); // do not localize
end;

procedure TBoldXMLRequest.DeleteAction;
var
  parentNode: IXMLDomNode;
begin
  if FIsReadOnly then
    raise EBold.CreateFmt(sCannotPerformInReadOnlyMode, [ClassName, 'DeleteAction']) // do not localize
  else
  begin
    parentNode := ActionElement.parentNode;
    parentNode.removeChild(ActionElement as IXMLDomNode);
    FActionElement := nil;
    FActionPath := '';
  end;
end;

procedure TBoldXMLRequest.AddIdentifiedValue(const IdString, Value, DomElementTag,
  AttributeTag: string);
var
  NewElement: IXMLDomElement;
begin
  if FIsReadOnly then
    raise EBold.CreateFmt(sCannotPerformInReadOnlyMode, [ClassName, 'AddIdentifiedValue']) // do not localize
  else if Assigned(ActionElement) then
  begin
    NewElement := DomDocument.createElement(DomElementTag);
    NewElement.setAttribute(AttributeTag, IdString);
    NewElement.Set_text(Value);
    ActionElement.appendChild(NewElement);
  end
  else
    raise EBold.CreateFmt(sSOAPActionNotSet, [ClassName, 'AddIdentifiedValue']); // do not localize
end;

procedure TBoldXMLRequest.ReloadIdentifiedValues;
begin
  if not Assigned(FIdentifiedValues) then
    FIdentifiedValues := TBoldStringList.Create
  else
    FIdentifiedValues.Clear;
  ExtractIdentifiedValues(FIdentifiedValues);
end;

constructor TBoldXMLRequest.Create;
begin
  raise EBold.CreateFmt(sSOAPBadConstructorToUse, [ClassName]);
end;

end.
