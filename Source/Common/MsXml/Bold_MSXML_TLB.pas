
{ Global compiler directives }
{$include bold.inc}
unit Bold_MSXML_TLB;



































{$TYPEDADDRESS OFF}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;






const
  MSXMLMajorVersion = 2;
  MSXMLMinorVersion = 0;

  LIBID_MSXML: TGUID = '{D63E0CE2-A0A2-11D0-9C02-00C04FC99C8E}';

  IID_IXMLDOMImplementation: TGUID = '{2933BF8F-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMNode: TGUID = '{2933BF80-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMNodeList: TGUID = '{2933BF82-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMNamedNodeMap: TGUID = '{2933BF83-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMDocument: TGUID = '{2933BF81-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMDocumentType: TGUID = '{2933BF8B-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMElement: TGUID = '{2933BF86-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMAttribute: TGUID = '{2933BF85-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMDocumentFragment: TGUID = '{3EFAA413-272F-11D2-836F-0000F87A7782}';
  IID_IXMLDOMCharacterData: TGUID = '{2933BF84-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMText: TGUID = '{2933BF87-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMComment: TGUID = '{2933BF88-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMCDATASection: TGUID = '{2933BF8A-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMProcessingInstruction: TGUID = '{2933BF89-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMEntityReference: TGUID = '{2933BF8E-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMParseError: TGUID = '{3EFAA426-272F-11D2-836F-0000F87A7782}';
  IID_IXMLDOMNotation: TGUID = '{2933BF8C-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLDOMEntity: TGUID = '{2933BF8D-7B36-11D2-B20E-00C04F983E60}';
  IID_IXTLRuntime: TGUID = '{3EFAA425-272F-11D2-836F-0000F87A7782}';
  DIID_XMLDOMDocumentEvents: TGUID = '{3EFAA427-272F-11D2-836F-0000F87A7782}';
  CLASS_DOMDocument: TGUID = '{2933BF90-7B36-11D2-B20E-00C04F983E60}';
  CLASS_DOMFreeThreadedDocument: TGUID = '{2933BF91-7B36-11D2-B20E-00C04F983E60}';
  IID_IXMLHttpRequest: TGUID = '{ED8C108D-4349-11D2-91A4-00C04F7969E8}';
  CLASS_XMLHTTPRequest: TGUID = '{ED8C108E-4349-11D2-91A4-00C04F7969E8}';
  IID_IXMLDSOControl: TGUID = '{310AFA62-0575-11D2-9CA9-0060B0EC3D39}';
  CLASS_XMLDSOControl: TGUID = '{550DDA30-0541-11D2-9CA9-0060B0EC3D39}';
  IID_IXMLElementCollection: TGUID = '{65725580-9B5D-11D0-9BFE-00C04FC99C8E}';
  IID_IXMLDocument: TGUID = '{F52E2B61-18A1-11D1-B105-00805F49916B}';
  IID_IXMLElement: TGUID = '{3F7F31AC-E15F-11D0-9C25-00C04FC99C8E}';
  IID_IXMLDocument2: TGUID = '{2B8DE2FE-8D2D-11D1-B2FC-00C04FD915A9}';
  IID_IXMLElement2: TGUID = '{2B8DE2FF-8D2D-11D1-B2FC-00C04FD915A9}';
  IID_IXMLAttribute: TGUID = '{D4D4A0FC-3B73-11D1-B2B4-00C04FB92596}';
  IID_IXMLError: TGUID = '{948C5AD3-C58D-11D0-9C0B-00C04FC99C8E}';
  CLASS_XMLDocument: TGUID = '{CFC399AF-D876-11D0-9C10-00C04FC99C8E}';



type
  tagDOMNodeType = TOleEnum;
const
  NODE_INVALID = $00000000;
  NODE_ELEMENT = $00000001;
  NODE_ATTRIBUTE = $00000002;
  NODE_TEXT = $00000003;
  NODE_CDATA_SECTION = $00000004;
  NODE_ENTITY_REFERENCE = $00000005;
  NODE_ENTITY = $00000006;
  NODE_PROCESSING_INSTRUCTION = $00000007;
  NODE_COMMENT = $00000008;
  NODE_DOCUMENT = $00000009;
  NODE_DOCUMENT_TYPE = $0000000A;
  NODE_DOCUMENT_FRAGMENT = $0000000B;
  NODE_NOTATION = $0000000C;
type
  tagXMLEMEM_TYPE = TOleEnum;
const
  XMLELEMTYPE_ELEMENT = $00000000;
  XMLELEMTYPE_TEXT = $00000001;
  XMLELEMTYPE_COMMENT = $00000002;
  XMLELEMTYPE_DOCUMENT = $00000003;
  XMLELEMTYPE_DTD = $00000004;
  XMLELEMTYPE_PI = $00000005;
  XMLELEMTYPE_OTHER = $00000006;

type


  IXMLDOMImplementation = interface;
  IXMLDOMImplementationDisp = dispinterface;
  IXMLDOMNode = interface;
  IXMLDOMNodeDisp = dispinterface;
  IXMLDOMNodeList = interface;
  IXMLDOMNodeListDisp = dispinterface;
  IXMLDOMNamedNodeMap = interface;
  IXMLDOMNamedNodeMapDisp = dispinterface;
  IXMLDOMDocument = interface;
  IXMLDOMDocumentDisp = dispinterface;
  IXMLDOMDocumentType = interface;
  IXMLDOMDocumentTypeDisp = dispinterface;
  IXMLDOMElement = interface;
  IXMLDOMElementDisp = dispinterface;
  IXMLDOMAttribute = interface;
  IXMLDOMAttributeDisp = dispinterface;
  IXMLDOMDocumentFragment = interface;
  IXMLDOMDocumentFragmentDisp = dispinterface;
  IXMLDOMCharacterData = interface;
  IXMLDOMCharacterDataDisp = dispinterface;
  IXMLDOMText = interface;
  IXMLDOMTextDisp = dispinterface;
  IXMLDOMComment = interface;
  IXMLDOMCommentDisp = dispinterface;
  IXMLDOMCDATASection = interface;
  IXMLDOMCDATASectionDisp = dispinterface;
  IXMLDOMProcessingInstruction = interface;
  IXMLDOMProcessingInstructionDisp = dispinterface;
  IXMLDOMEntityReference = interface;
  IXMLDOMEntityReferenceDisp = dispinterface;
  IXMLDOMParseError = interface;
  IXMLDOMParseErrorDisp = dispinterface;
  IXMLDOMNotation = interface;
  IXMLDOMNotationDisp = dispinterface;
  IXMLDOMEntity = interface;
  IXMLDOMEntityDisp = dispinterface;
  IXTLRuntime = interface;
  IXTLRuntimeDisp = dispinterface;
  XMLDOMDocumentEvents = dispinterface;
  IXMLHttpRequest = interface;
  IXMLHttpRequestDisp = dispinterface;
  IXMLDSOControl = interface;
  IXMLDSOControlDisp = dispinterface;
  IXMLElementCollection = interface;
  IXMLElementCollectionDisp = dispinterface;
  IXMLDocument = interface;
  IXMLDocumentDisp = dispinterface;
  IXMLElement = interface;
  IXMLElementDisp = dispinterface;
  IXMLDocument2 = interface;
  IXMLElement2 = interface;
  IXMLElement2Disp = dispinterface;
  IXMLAttribute = interface;
  IXMLAttributeDisp = dispinterface;
  IXMLError = interface;



  DOMDocument = IXMLDOMDocument;
  DOMFreeThreadedDocument = IXMLDOMDocument;
  XMLHTTPRequest = IXMLHttpRequest;
  XMLDSOControl = IXMLDSOControl;
  XMLDocument = IXMLDocument2;


  PUserType1 = ^_xml_error; {*}

  DOMNodeType = tagDOMNodeType; 

  _xml_error = packed record
    _nLine: SYSUINT;
    _pchBuf: WideString;
    _cchBuf: SYSUINT;
    _ich: SYSUINT;
    _pszFound: WideString;
    _pszExpected: WideString;
    _reserved1: LongWord;
    _reserved2: LongWord;
  end;

  XMLELEM_TYPE = tagXMLEMEM_TYPE;




  IXMLDOMImplementation = interface(IDispatch)
    ['{2933BF8F-7B36-11D2-B20E-00C04F983E60}']
    function  hasFeature(const feature: WideString; const version: WideString): WordBool; safecall;
  end;




  IXMLDOMImplementationDisp = dispinterface
    ['{2933BF8F-7B36-11D2-B20E-00C04F983E60}']
    function  hasFeature(const feature: WideString; const version: WideString): WordBool; dispid 145;
  end;




  IXMLDOMNode = interface(IDispatch)
    ['{2933BF80-7B36-11D2-B20E-00C04F983E60}']
    function  Get_nodeName: WideString; safecall;
    function  Get_nodeValue: OleVariant; safecall;
    procedure Set_nodeValue(value: OleVariant); safecall;
    function  Get_nodeType: DOMNodeType; safecall;
    function  Get_parentNode: IXMLDOMNode; safecall;
    function  Get_childNodes: IXMLDOMNodeList; safecall;
    function  Get_firstChild: IXMLDOMNode; safecall;
    function  Get_lastChild: IXMLDOMNode; safecall;
    function  Get_previousSibling: IXMLDOMNode; safecall;
    function  Get_nextSibling: IXMLDOMNode; safecall;
    function  Get_attributes: IXMLDOMNamedNodeMap; safecall;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; safecall;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; safecall;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; safecall;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; safecall;
    function  hasChildNodes: WordBool; safecall;
    function  Get_ownerDocument: IXMLDOMDocument; safecall;
    function  cloneNode(deep: WordBool): IXMLDOMNode; safecall;
    function  Get_nodeTypeString: WideString; safecall;
    function  Get_text: WideString; safecall;
    procedure Set_text(const text: WideString); safecall;
    function  Get_specified: WordBool; safecall;
    function  Get_definition: IXMLDOMNode; safecall;
    function  Get_nodeTypedValue: OleVariant; safecall;
    procedure Set_nodeTypedValue(typedValue: OleVariant); safecall;
    function  Get_dataType: OleVariant; safecall;
    procedure Set_dataType(const dataTypeName: WideString); safecall;
    function  Get_xml: WideString; safecall;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; safecall;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; safecall;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; safecall;
    function  Get_parsed: WordBool; safecall;
    function  Get_namespaceURI: WideString; safecall;
    function  Get_prefix: WideString; safecall;
    function  Get_baseName: WideString; safecall;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); safecall;
    property nodeName: WideString read Get_nodeName;
    property nodeValue: OleVariant read Get_nodeValue write Set_nodeValue;
    property nodeType: DOMNodeType read Get_nodeType;
    property parentNode: IXMLDOMNode read Get_parentNode;
    property childNodes: IXMLDOMNodeList read Get_childNodes;
    property firstChild: IXMLDOMNode read Get_firstChild;
    property lastChild: IXMLDOMNode read Get_lastChild;
    property previousSibling: IXMLDOMNode read Get_previousSibling;
    property nextSibling: IXMLDOMNode read Get_nextSibling;
    property attributes: IXMLDOMNamedNodeMap read Get_attributes;
    property ownerDocument: IXMLDOMDocument read Get_ownerDocument;
    property nodeTypeString: WideString read Get_nodeTypeString;
    property text: WideString read Get_text write Set_text;
    property specified: WordBool read Get_specified;
    property definition: IXMLDOMNode read Get_definition;
    property nodeTypedValue: OleVariant read Get_nodeTypedValue write Set_nodeTypedValue;
    property xml: WideString read Get_xml;
    property parsed: WordBool read Get_parsed;
    property namespaceURI: WideString read Get_namespaceURI;
    property prefix: WideString read Get_prefix;
    property baseName: WideString read Get_baseName;
  end;




  IXMLDOMNodeDisp = dispinterface
    ['{2933BF80-7B36-11D2-B20E-00C04F983E60}']
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMNodeList = interface(IDispatch)
    ['{2933BF82-7B36-11D2-B20E-00C04F983E60}']
    function  Get_item(index: Integer): IXMLDOMNode; safecall;
    function  Get_length: Integer; safecall;
    function  nextNode: IXMLDOMNode; safecall;
    procedure reset; safecall;
    function  Get__newEnum: IUnknown; safecall;
    property item[index: Integer]: IXMLDOMNode read Get_item; default;
    property length: Integer read Get_length;
    property _newEnum: IUnknown read Get__newEnum;
  end;




  IXMLDOMNodeListDisp = dispinterface
    ['{2933BF82-7B36-11D2-B20E-00C04F983E60}']
    property item[index: Integer]: IXMLDOMNode readonly dispid 0; default;
    property length: Integer readonly dispid 74;
    function  nextNode: IXMLDOMNode; dispid 76;
    procedure reset; dispid 77;
    property _newEnum: IUnknown readonly dispid -4;
  end;




  IXMLDOMNamedNodeMap = interface(IDispatch)
    ['{2933BF83-7B36-11D2-B20E-00C04F983E60}']
    function  getNamedItem(const name: WideString): IXMLDOMNode; safecall;
    function  setNamedItem(const newItem: IXMLDOMNode): IXMLDOMNode; safecall;
    function  removeNamedItem(const name: WideString): IXMLDOMNode; safecall;
    function  Get_item(index: Integer): IXMLDOMNode; safecall;
    function  Get_length: Integer; safecall;
    function  getQualifiedItem(const baseName: WideString; const namespaceURI: WideString): IXMLDOMNode; safecall;
    function  removeQualifiedItem(const baseName: WideString; const namespaceURI: WideString): IXMLDOMNode; safecall;
    function  nextNode: IXMLDOMNode; safecall;
    procedure reset; safecall;
    function  Get__newEnum: IUnknown; safecall;
    property item[index: Integer]: IXMLDOMNode read Get_item; default;
    property length: Integer read Get_length;
    property _newEnum: IUnknown read Get__newEnum;
  end;




  IXMLDOMNamedNodeMapDisp = dispinterface
    ['{2933BF83-7B36-11D2-B20E-00C04F983E60}']
    function  getNamedItem(const name: WideString): IXMLDOMNode; dispid 83;
    function  setNamedItem(const newItem: IXMLDOMNode): IXMLDOMNode; dispid 84;
    function  removeNamedItem(const name: WideString): IXMLDOMNode; dispid 85;
    property item[index: Integer]: IXMLDOMNode readonly dispid 0; default;
    property length: Integer readonly dispid 74;
    function  getQualifiedItem(const baseName: WideString; const namespaceURI: WideString): IXMLDOMNode; dispid 87;
    function  removeQualifiedItem(const baseName: WideString; const namespaceURI: WideString): IXMLDOMNode; dispid 88;
    function  nextNode: IXMLDOMNode; dispid 89;
    procedure reset; dispid 90;
    property _newEnum: IUnknown readonly dispid -4;
  end;




  IXMLDOMDocument = interface(IXMLDOMNode)
    ['{2933BF81-7B36-11D2-B20E-00C04F983E60}']
    function  Get_doctype: IXMLDOMDocumentType; safecall;
    function  Get_implementation_: IXMLDOMImplementation; safecall;
    function  Get_documentElement: IXMLDOMElement; safecall;
    procedure Set_documentElement(const DOMElement: IXMLDOMElement); safecall;
    function  createElement(const tagName: WideString): IXMLDOMElement; safecall;
    function  createDocumentFragment: IXMLDOMDocumentFragment; safecall;
    function  createTextNode(const data: WideString): IXMLDOMText; safecall;
    function  createComment(const data: WideString): IXMLDOMComment; safecall;
    function  createCDATASection(const data: WideString): IXMLDOMCDATASection; safecall;
    function  createProcessingInstruction(const target: WideString; const data: WideString): IXMLDOMProcessingInstruction; safecall;
    function  createAttribute(const name: WideString): IXMLDOMAttribute; safecall;
    function  createEntityReference(const name: WideString): IXMLDOMEntityReference; safecall;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList; safecall;
    function  createNode(type_: OleVariant; const name: WideString; const namespaceURI: WideString): IXMLDOMNode; safecall;
    function  nodeFromID(const idString: WideString): IXMLDOMNode; safecall;
    function  load(xmlSource: OleVariant): WordBool; safecall;
    function  Get_readyState: Integer; safecall;
    function  Get_parseError: IXMLDOMParseError; safecall;
    function  Get_url: WideString; safecall;
    function  Get_async: WordBool; safecall;
    procedure Set_async(isAsync: WordBool); safecall;
    procedure abort; safecall;
    function  loadXML(const bstrXML: WideString): WordBool; safecall;
    procedure save(desination: OleVariant); safecall;
    function  Get_validateOnParse: WordBool; safecall;
    procedure Set_validateOnParse(isValidating: WordBool); safecall;
    function  Get_resolveExternals: WordBool; safecall;
    procedure Set_resolveExternals(isResolving: WordBool); safecall;
    function  Get_preserveWhiteSpace: WordBool; safecall;
    procedure Set_preserveWhiteSpace(isPreserving: WordBool); safecall;
    procedure Set_onreadystatechange(Param1: OleVariant); safecall;
    procedure Set_ondataavailable(Param1: OleVariant); safecall;
    procedure Set_ontransformnode(Param1: OleVariant); safecall;
    property doctype: IXMLDOMDocumentType read Get_doctype;
    property implementation_: IXMLDOMImplementation read Get_implementation_;
    property documentElement: IXMLDOMElement read Get_documentElement write Set_documentElement;
    property readyState: Integer read Get_readyState;
    property parseError: IXMLDOMParseError read Get_parseError;
    property url: WideString read Get_url;
    property async: WordBool read Get_async write Set_async;
    property validateOnParse: WordBool read Get_validateOnParse write Set_validateOnParse;
    property resolveExternals: WordBool read Get_resolveExternals write Set_resolveExternals;
    property preserveWhiteSpace: WordBool read Get_preserveWhiteSpace write Set_preserveWhiteSpace;
    property onreadystatechange: OleVariant write Set_onreadystatechange;
    property ondataavailable: OleVariant write Set_ondataavailable;
    property ontransformnode: OleVariant write Set_ontransformnode;
  end;




  IXMLDOMDocumentDisp = dispinterface
    ['{2933BF81-7B36-11D2-B20E-00C04F983E60}']
    property doctype: IXMLDOMDocumentType readonly dispid 38;
    property implementation_: IXMLDOMImplementation readonly dispid 39;
    property documentElement: IXMLDOMElement dispid 40;
    function  createElement(const tagName: WideString): IXMLDOMElement; dispid 41;
    function  createDocumentFragment: IXMLDOMDocumentFragment; dispid 42;
    function  createTextNode(const data: WideString): IXMLDOMText; dispid 43;
    function  createComment(const data: WideString): IXMLDOMComment; dispid 44;
    function  createCDATASection(const data: WideString): IXMLDOMCDATASection; dispid 45;
    function  createProcessingInstruction(const target: WideString; const data: WideString): IXMLDOMProcessingInstruction; dispid 46;
    function  createAttribute(const name: WideString): IXMLDOMAttribute; dispid 47;
    function  createEntityReference(const name: WideString): IXMLDOMEntityReference; dispid 49;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList; dispid 50;
    function  createNode(type_: OleVariant; const name: WideString; const namespaceURI: WideString): IXMLDOMNode; dispid 54;
    function  nodeFromID(const idString: WideString): IXMLDOMNode; dispid 56;
    function  load(xmlSource: OleVariant): WordBool; dispid 58;
    property readyState: Integer readonly dispid -525;
    property parseError: IXMLDOMParseError readonly dispid 59;
    property url: WideString readonly dispid 60;
    property async: WordBool dispid 61;
    procedure abort; dispid 62;
    function  loadXML(const bstrXML: WideString): WordBool; dispid 63;
    procedure save(desination: OleVariant); dispid 64;
    property validateOnParse: WordBool dispid 65;
    property resolveExternals: WordBool dispid 66;
    property preserveWhiteSpace: WordBool dispid 67;
    property onreadystatechange: OleVariant writeonly dispid 68;
    property ondataavailable: OleVariant writeonly dispid 69;
    property ontransformnode: OleVariant writeonly dispid 70;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMDocumentType = interface(IXMLDOMNode)
    ['{2933BF8B-7B36-11D2-B20E-00C04F983E60}']
    function  Get_name: WideString; safecall;
    function  Get_entities: IXMLDOMNamedNodeMap; safecall;
    function  Get_notations: IXMLDOMNamedNodeMap; safecall;
    property name: WideString read Get_name;
    property entities: IXMLDOMNamedNodeMap read Get_entities;
    property notations: IXMLDOMNamedNodeMap read Get_notations;
  end;




  IXMLDOMDocumentTypeDisp = dispinterface
    ['{2933BF8B-7B36-11D2-B20E-00C04F983E60}']
    property name: WideString readonly dispid 131;
    property entities: IXMLDOMNamedNodeMap readonly dispid 132;
    property notations: IXMLDOMNamedNodeMap readonly dispid 133;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMElement = interface(IXMLDOMNode)
    ['{2933BF86-7B36-11D2-B20E-00C04F983E60}']
    function  Get_tagName: WideString; safecall;
    function  getAttribute(const name: WideString): OleVariant; safecall;
    procedure setAttribute(const name: WideString; value: OleVariant); safecall;
    procedure removeAttribute(const name: WideString); safecall;
    function  getAttributeNode(const name: WideString): IXMLDOMAttribute; safecall;
    function  setAttributeNode(const DOMAttribute: IXMLDOMAttribute): IXMLDOMAttribute; safecall;
    function  removeAttributeNode(const DOMAttribute: IXMLDOMAttribute): IXMLDOMAttribute; safecall;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList; safecall;
    procedure normalize; safecall;
    property tagName: WideString read Get_tagName;
  end;




  IXMLDOMElementDisp = dispinterface
    ['{2933BF86-7B36-11D2-B20E-00C04F983E60}']
    property tagName: WideString readonly dispid 97;
    function  getAttribute(const name: WideString): OleVariant; dispid 99;
    procedure setAttribute(const name: WideString; value: OleVariant); dispid 100;
    procedure removeAttribute(const name: WideString); dispid 101;
    function  getAttributeNode(const name: WideString): IXMLDOMAttribute; dispid 102;
    function  setAttributeNode(const DOMAttribute: IXMLDOMAttribute): IXMLDOMAttribute; dispid 103;
    function  removeAttributeNode(const DOMAttribute: IXMLDOMAttribute): IXMLDOMAttribute; dispid 104;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList; dispid 105;
    procedure normalize; dispid 106;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMAttribute = interface(IXMLDOMNode)
    ['{2933BF85-7B36-11D2-B20E-00C04F983E60}']
    function  Get_name: WideString; safecall;
    function  Get_value: OleVariant; safecall;
    procedure Set_value(attributeValue: OleVariant); safecall;
    property name: WideString read Get_name;
    property value: OleVariant read Get_value write Set_value;
  end;




  IXMLDOMAttributeDisp = dispinterface
    ['{2933BF85-7B36-11D2-B20E-00C04F983E60}']
    property name: WideString readonly dispid 118;
    property value: OleVariant dispid 120;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMDocumentFragment = interface(IXMLDOMNode)
    ['{3EFAA413-272F-11D2-836F-0000F87A7782}']
  end;




  IXMLDOMDocumentFragmentDisp = dispinterface
    ['{3EFAA413-272F-11D2-836F-0000F87A7782}']
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMCharacterData = interface(IXMLDOMNode)
    ['{2933BF84-7B36-11D2-B20E-00C04F983E60}']
    function  Get_data: WideString; safecall;
    procedure Set_data(const data: WideString); safecall;
    function  Get_length: Integer; safecall;
    function  substringData(offset: Integer; count: Integer): WideString; safecall;
    procedure appendData(const data: WideString); safecall;
    procedure insertData(offset: Integer; const data: WideString); safecall;
    procedure deleteData(offset: Integer; count: Integer); safecall;
    procedure replaceData(offset: Integer; count: Integer; const data: WideString); safecall;
    property data: WideString read Get_data write Set_data;
    property length: Integer read Get_length;
  end;




  IXMLDOMCharacterDataDisp = dispinterface
    ['{2933BF84-7B36-11D2-B20E-00C04F983E60}']
    property data: WideString dispid 109;
    property length: Integer readonly dispid 110;
    function  substringData(offset: Integer; count: Integer): WideString; dispid 111;
    procedure appendData(const data: WideString); dispid 112;
    procedure insertData(offset: Integer; const data: WideString); dispid 113;
    procedure deleteData(offset: Integer; count: Integer); dispid 114;
    procedure replaceData(offset: Integer; count: Integer; const data: WideString); dispid 115;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMText = interface(IXMLDOMCharacterData)
    ['{2933BF87-7B36-11D2-B20E-00C04F983E60}']
    function  splitText(offset: Integer): IXMLDOMText; safecall;
  end;




  IXMLDOMTextDisp = dispinterface
    ['{2933BF87-7B36-11D2-B20E-00C04F983E60}']
    function  splitText(offset: Integer): IXMLDOMText; dispid 123;
    property data: WideString dispid 109;
    property length: Integer readonly dispid 110;
    function  substringData(offset: Integer; count: Integer): WideString; dispid 111;
    procedure appendData(const data: WideString); dispid 112;
    procedure insertData(offset: Integer; const data: WideString); dispid 113;
    procedure deleteData(offset: Integer; count: Integer); dispid 114;
    procedure replaceData(offset: Integer; count: Integer; const data: WideString); dispid 115;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMComment = interface(IXMLDOMCharacterData)
    ['{2933BF88-7B36-11D2-B20E-00C04F983E60}']
  end;




  IXMLDOMCommentDisp = dispinterface
    ['{2933BF88-7B36-11D2-B20E-00C04F983E60}']
    property data: WideString dispid 109;
    property length: Integer readonly dispid 110;
    function  substringData(offset: Integer; count: Integer): WideString; dispid 111;
    procedure appendData(const data: WideString); dispid 112;
    procedure insertData(offset: Integer; const data: WideString); dispid 113;
    procedure deleteData(offset: Integer; count: Integer); dispid 114;
    procedure replaceData(offset: Integer; count: Integer; const data: WideString); dispid 115;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMCDATASection = interface(IXMLDOMText)
    ['{2933BF8A-7B36-11D2-B20E-00C04F983E60}']
  end;




  IXMLDOMCDATASectionDisp = dispinterface
    ['{2933BF8A-7B36-11D2-B20E-00C04F983E60}']
    function  splitText(offset: Integer): IXMLDOMText; dispid 123;
    property data: WideString dispid 109;
    property length: Integer readonly dispid 110;
    function  substringData(offset: Integer; count: Integer): WideString; dispid 111;
    procedure appendData(const data: WideString); dispid 112;
    procedure insertData(offset: Integer; const data: WideString); dispid 113;
    procedure deleteData(offset: Integer; count: Integer); dispid 114;
    procedure replaceData(offset: Integer; count: Integer; const data: WideString); dispid 115;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMProcessingInstruction = interface(IXMLDOMNode)
    ['{2933BF89-7B36-11D2-B20E-00C04F983E60}']
    function  Get_target: WideString; safecall;
    function  Get_data: WideString; safecall;
    procedure Set_data(const value: WideString); safecall;
    property target: WideString read Get_target;
    property data: WideString read Get_data write Set_data;
  end;




  IXMLDOMProcessingInstructionDisp = dispinterface
    ['{2933BF89-7B36-11D2-B20E-00C04F983E60}']
    property target: WideString readonly dispid 127;
    property data: WideString dispid 128;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMEntityReference = interface(IXMLDOMNode)
    ['{2933BF8E-7B36-11D2-B20E-00C04F983E60}']
  end;




  IXMLDOMEntityReferenceDisp = dispinterface
    ['{2933BF8E-7B36-11D2-B20E-00C04F983E60}']
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMParseError = interface(IDispatch)
    ['{3EFAA426-272F-11D2-836F-0000F87A7782}']
    function  Get_errorCode: Integer; safecall;
    function  Get_url: WideString; safecall;
    function  Get_reason: WideString; safecall;
    function  Get_srcText: WideString; safecall;
    function  Get_line: Integer; safecall;
    function  Get_linepos: Integer; safecall;
    function  Get_filepos: Integer; safecall;
    property errorCode: Integer read Get_errorCode;
    property url: WideString read Get_url;
    property reason: WideString read Get_reason;
    property srcText: WideString read Get_srcText;
    property line: Integer read Get_line;
    property linepos: Integer read Get_linepos;
    property filepos: Integer read Get_filepos;
  end;




  IXMLDOMParseErrorDisp = dispinterface
    ['{3EFAA426-272F-11D2-836F-0000F87A7782}']
    property errorCode: Integer readonly dispid 0;
    property url: WideString readonly dispid 179;
    property reason: WideString readonly dispid 180;
    property srcText: WideString readonly dispid 181;
    property line: Integer readonly dispid 182;
    property linepos: Integer readonly dispid 183;
    property filepos: Integer readonly dispid 184;
  end;




  IXMLDOMNotation = interface(IXMLDOMNode)
    ['{2933BF8C-7B36-11D2-B20E-00C04F983E60}']
    function  Get_publicId: OleVariant; safecall;
    function  Get_systemId: OleVariant; safecall;
    property publicId: OleVariant read Get_publicId;
    property systemId: OleVariant read Get_systemId;
  end;




  IXMLDOMNotationDisp = dispinterface
    ['{2933BF8C-7B36-11D2-B20E-00C04F983E60}']
    property publicId: OleVariant readonly dispid 136;
    property systemId: OleVariant readonly dispid 137;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXMLDOMEntity = interface(IXMLDOMNode)
    ['{2933BF8D-7B36-11D2-B20E-00C04F983E60}']
    function  Get_publicId: OleVariant; safecall;
    function  Get_systemId: OleVariant; safecall;
    function  Get_notationName: WideString; safecall;
    property publicId: OleVariant read Get_publicId;
    property systemId: OleVariant read Get_systemId;
    property notationName: WideString read Get_notationName;
  end;




  IXMLDOMEntityDisp = dispinterface
    ['{2933BF8D-7B36-11D2-B20E-00C04F983E60}']
    property publicId: OleVariant readonly dispid 140;
    property systemId: OleVariant readonly dispid 141;
    property notationName: WideString readonly dispid 142;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  IXTLRuntime = interface(IXMLDOMNode)
    ['{3EFAA425-272F-11D2-836F-0000F87A7782}']
    function  uniqueID(const pNode: IXMLDOMNode): Integer; safecall;
    function  depth(const pNode: IXMLDOMNode): Integer; safecall;
    function  childNumber(const pNode: IXMLDOMNode): Integer; safecall;
    function  ancestorChildNumber(const bstrNodeName: WideString; const pNode: IXMLDOMNode): Integer; safecall;
    function  absoluteChildNumber(const pNode: IXMLDOMNode): Integer; safecall;
    function  formatIndex(lIndex: Integer; const bstrFormat: WideString): WideString; safecall;
    function  formatNumber(dblNumber: Double; const bstrFormat: WideString): WideString; safecall;
    function  formatDate(varDate: OleVariant; const bstrFormat: WideString; 
                         varDestLocale: OleVariant): WideString; safecall;
    function  formatTime(varTime: OleVariant; const bstrFormat: WideString; 
                         varDestLocale: OleVariant): WideString; safecall;
  end;




  IXTLRuntimeDisp = dispinterface
    ['{3EFAA425-272F-11D2-836F-0000F87A7782}']
    function  uniqueID(const pNode: IXMLDOMNode): Integer; dispid 187;
    function  depth(const pNode: IXMLDOMNode): Integer; dispid 188;
    function  childNumber(const pNode: IXMLDOMNode): Integer; dispid 189;
    function  ancestorChildNumber(const bstrNodeName: WideString; const pNode: IXMLDOMNode): Integer; dispid 190;
    function  absoluteChildNumber(const pNode: IXMLDOMNode): Integer; dispid 191;
    function  formatIndex(lIndex: Integer; const bstrFormat: WideString): WideString; dispid 192;
    function  formatNumber(dblNumber: Double; const bstrFormat: WideString): WideString; dispid 193;
    function  formatDate(varDate: OleVariant; const bstrFormat: WideString; 
                         varDestLocale: OleVariant): WideString; dispid 194;
    function  formatTime(varTime: OleVariant; const bstrFormat: WideString; 
                         varDestLocale: OleVariant): WideString; dispid 195;
    property nodeName: WideString readonly dispid 2;
    property nodeValue: OleVariant dispid 3;
    property nodeType: DOMNodeType readonly dispid 4;
    property parentNode: IXMLDOMNode readonly dispid 6;
    property childNodes: IXMLDOMNodeList readonly dispid 7;
    property firstChild: IXMLDOMNode readonly dispid 8;
    property lastChild: IXMLDOMNode readonly dispid 9;
    property previousSibling: IXMLDOMNode readonly dispid 10;
    property nextSibling: IXMLDOMNode readonly dispid 11;
    property attributes: IXMLDOMNamedNodeMap readonly dispid 12;
    function  insertBefore(const newChild: IXMLDOMNode; refChild: OleVariant): IXMLDOMNode; dispid 13;
    function  replaceChild(const newChild: IXMLDOMNode; const oldChild: IXMLDOMNode): IXMLDOMNode; dispid 14;
    function  removeChild(const childNode: IXMLDOMNode): IXMLDOMNode; dispid 15;
    function  appendChild(const newChild: IXMLDOMNode): IXMLDOMNode; dispid 16;
    function  hasChildNodes: WordBool; dispid 17;
    property ownerDocument: IXMLDOMDocument readonly dispid 18;
    function  cloneNode(deep: WordBool): IXMLDOMNode; dispid 19;
    property nodeTypeString: WideString readonly dispid 21;
    property text: WideString dispid 24;
    property specified: WordBool readonly dispid 22;
    property definition: IXMLDOMNode readonly dispid 23;
    property nodeTypedValue: OleVariant dispid 25;
    function  dataType: OleVariant; dispid 26;
    property xml: WideString readonly dispid 27;
    function  transformNode(const stylesheet: IXMLDOMNode): WideString; dispid 28;
    function  selectNodes(const queryString: WideString): IXMLDOMNodeList; dispid 29;
    function  selectSingleNode(const queryString: WideString): IXMLDOMNode; dispid 30;
    property parsed: WordBool readonly dispid 31;
    property namespaceURI: WideString readonly dispid 32;
    property prefix: WideString readonly dispid 33;
    property baseName: WideString readonly dispid 34;
    procedure transformNodeToObject(const stylesheet: IXMLDOMNode; outputObject: OleVariant); dispid 35;
  end;




  XMLDOMDocumentEvents = dispinterface
    ['{3EFAA427-272F-11D2-836F-0000F87A7782}']
    procedure ondataavailable; dispid 198;
    procedure onreadystatechange; dispid -609;
  end;




  IXMLHttpRequest = interface(IDispatch)
    ['{ED8C108D-4349-11D2-91A4-00C04F7969E8}']
    procedure open(const bstrMethod: WideString; const bstrUrl: WideString; varAsync: OleVariant; 
                   bstrUser: OleVariant; bstrPassword: OleVariant); safecall;
    procedure setRequestHeader(const bstrHeader: WideString; const bstrValue: WideString); safecall;
    function  getResponseHeader(const bstrHeader: WideString): WideString; safecall;
    function  getAllResponseHeaders: WideString; safecall;
    procedure send(varBody: OleVariant); safecall;
    procedure abort; safecall;
    function  Get_status: Integer; safecall;
    function  Get_statusText: WideString; safecall;
    function  Get_responseXML: IDispatch; safecall;
    function  Get_responseText: WideString; safecall;
    function  Get_responseBody: OleVariant; safecall;
    function  Get_responseStream: OleVariant; safecall;
    function  Get_readyState: Integer; safecall;
    procedure Set_onreadystatechange(const Param1: IDispatch); safecall;
    property status: Integer read Get_status;
    property statusText: WideString read Get_statusText;
    property responseXML: IDispatch read Get_responseXML;
    property responseText: WideString read Get_responseText;
    property responseBody: OleVariant read Get_responseBody;
    property responseStream: OleVariant read Get_responseStream;
    property readyState: Integer read Get_readyState;
    property onreadystatechange: IDispatch write Set_onreadystatechange;
  end;




  IXMLHttpRequestDisp = dispinterface
    ['{ED8C108D-4349-11D2-91A4-00C04F7969E8}']
    procedure open(const bstrMethod: WideString; const bstrUrl: WideString; varAsync: OleVariant; 
                   bstrUser: OleVariant; bstrPassword: OleVariant); dispid 1;
    procedure setRequestHeader(const bstrHeader: WideString; const bstrValue: WideString); dispid 2;
    function  getResponseHeader(const bstrHeader: WideString): WideString; dispid 3;
    function  getAllResponseHeaders: WideString; dispid 4;
    procedure send(varBody: OleVariant); dispid 5;
    procedure abort; dispid 6;
    property status: Integer readonly dispid 7;
    property statusText: WideString readonly dispid 8;
    property responseXML: IDispatch readonly dispid 9;
    property responseText: WideString readonly dispid 10;
    property responseBody: OleVariant readonly dispid 11;
    property responseStream: OleVariant readonly dispid 12;
    property readyState: Integer readonly dispid 13;
    property onreadystatechange: IDispatch writeonly dispid 14;
  end;




  IXMLDSOControl = interface(IDispatch)
    ['{310AFA62-0575-11D2-9CA9-0060B0EC3D39}']
    function  Get_XMLDocument: IXMLDOMDocument; safecall;
    procedure Set_XMLDocument(const ppDoc: IXMLDOMDocument); safecall;
    function  Get_JavaDSOCompatible: Integer; safecall;
    procedure Set_JavaDSOCompatible(fJavaDSOCompatible: Integer); safecall;
    function  Get_readyState: Integer; safecall;
    property XMLDocument: IXMLDOMDocument read Get_XMLDocument write Set_XMLDocument;
    property JavaDSOCompatible: Integer read Get_JavaDSOCompatible write Set_JavaDSOCompatible;
    property readyState: Integer read Get_readyState;
  end;




  IXMLDSOControlDisp = dispinterface
    ['{310AFA62-0575-11D2-9CA9-0060B0EC3D39}']
    property XMLDocument: IXMLDOMDocument dispid 65537;
    property JavaDSOCompatible: Integer dispid 65538;
    property readyState: Integer readonly dispid -525;
  end;




  IXMLElementCollection = interface(IDispatch)
    ['{65725580-9B5D-11D0-9BFE-00C04FC99C8E}']
    procedure Set_length(p: Integer); safecall;
    function  Get_length: Integer; safecall;
    function  Get__newEnum: IUnknown; safecall;
    function  item(var1: OleVariant; var2: OleVariant): IDispatch; safecall;
    property length: Integer read Get_length write Set_length;
    property _newEnum: IUnknown read Get__newEnum;
  end;




  IXMLElementCollectionDisp = dispinterface
    ['{65725580-9B5D-11D0-9BFE-00C04FC99C8E}']
    property length: Integer dispid 65537;
    property _newEnum: IUnknown readonly dispid -4;
    function  item(var1: OleVariant; var2: OleVariant): IDispatch; dispid 65539;
  end;




  IXMLDocument = interface(IDispatch)
    ['{F52E2B61-18A1-11D1-B105-00805F49916B}']
    function  Get_root: IXMLElement; safecall;
    function  Get_fileSize: WideString; safecall;
    function  Get_fileModifiedDate: WideString; safecall;
    function  Get_fileUpdatedDate: WideString; safecall;
    function  Get_url: WideString; safecall;
    procedure Set_url(const p: WideString); safecall;
    function  Get_mimeType: WideString; safecall;
    function  Get_readyState: Integer; safecall;
    function  Get_charset: WideString; safecall;
    procedure Set_charset(const p: WideString); safecall;
    function  Get_version: WideString; safecall;
    function  Get_doctype: WideString; safecall;
    function  Get_dtdURL: WideString; safecall;
    function  createElement(vType: OleVariant; var1: OleVariant): IXMLElement; safecall;
    property root: IXMLElement read Get_root;
    property fileSize: WideString read Get_fileSize;
    property fileModifiedDate: WideString read Get_fileModifiedDate;
    property fileUpdatedDate: WideString read Get_fileUpdatedDate;
    property url: WideString read Get_url write Set_url;
    property mimeType: WideString read Get_mimeType;
    property readyState: Integer read Get_readyState;
    property charset: WideString read Get_charset write Set_charset;
    property version: WideString read Get_version;
    property doctype: WideString read Get_doctype;
    property dtdURL: WideString read Get_dtdURL;
  end;




  IXMLDocumentDisp = dispinterface
    ['{F52E2B61-18A1-11D1-B105-00805F49916B}']
    property root: IXMLElement readonly dispid 65637;
    property fileSize: WideString readonly dispid 65638;
    property fileModifiedDate: WideString readonly dispid 65639;
    property fileUpdatedDate: WideString readonly dispid 65640;
    property url: WideString dispid 65641;
    property mimeType: WideString readonly dispid 65642;
    property readyState: Integer readonly dispid 65643;
    property charset: WideString dispid 65645;
    property version: WideString readonly dispid 65646;
    property doctype: WideString readonly dispid 65647;
    property dtdURL: WideString readonly dispid 65648;
    function  createElement(vType: OleVariant; var1: OleVariant): IXMLElement; dispid 65644;
  end;




  IXMLElement = interface(IDispatch)
    ['{3F7F31AC-E15F-11D0-9C25-00C04FC99C8E}']
    function  Get_tagName: WideString; safecall;
    procedure Set_tagName(const p: WideString); safecall;
    function  Get_parent: IXMLElement; safecall;
    procedure setAttribute(const strPropertyName: WideString; PropertyValue: OleVariant); safecall;
    function  getAttribute(const strPropertyName: WideString): OleVariant; safecall;
    procedure removeAttribute(const strPropertyName: WideString); safecall;
    function  Get_children: IXMLElementCollection; safecall;
    function  Get_type_: Integer; safecall;
    function  Get_text: WideString; safecall;
    procedure Set_text(const p: WideString); safecall;
    procedure addChild(const pChildElem: IXMLElement; lIndex: Integer; lReserved: Integer); safecall;
    procedure removeChild(const pChildElem: IXMLElement); safecall;
    property tagName: WideString read Get_tagName write Set_tagName;
    property parent: IXMLElement read Get_parent;
    property children: IXMLElementCollection read Get_children;
    property type_: Integer read Get_type_;
    property text: WideString read Get_text write Set_text;
  end;




  IXMLElementDisp = dispinterface
    ['{3F7F31AC-E15F-11D0-9C25-00C04FC99C8E}']
    property tagName: WideString dispid 65737;
    property parent: IXMLElement readonly dispid 65738;
    procedure setAttribute(const strPropertyName: WideString; PropertyValue: OleVariant); dispid 65739;
    function  getAttribute(const strPropertyName: WideString): OleVariant; dispid 65740;
    procedure removeAttribute(const strPropertyName: WideString); dispid 65741;
    property children: IXMLElementCollection readonly dispid 65742;
    property type_: Integer readonly dispid 65743;
    property text: WideString dispid 65744;
    procedure addChild(const pChildElem: IXMLElement; lIndex: Integer; lReserved: Integer); dispid 65745;
    procedure removeChild(const pChildElem: IXMLElement); dispid 65746;
  end;




  IXMLDocument2 = interface(IDispatch)
    ['{2B8DE2FE-8D2D-11D1-B2FC-00C04FD915A9}']
    function  Get_root(out p: IXMLElement2): HResult; stdcall;
    function  Get_fileSize(out p: WideString): HResult; stdcall;
    function  Get_fileModifiedDate(out p: WideString): HResult; stdcall;
    function  Get_fileUpdatedDate(out p: WideString): HResult; stdcall;
    function  Get_url(out p: WideString): HResult; stdcall;
    function  Set_url(const p: WideString): HResult; stdcall;
    function  Get_mimeType(out p: WideString): HResult; stdcall;
    function  Get_readyState(out pl: Integer): HResult; stdcall;
    function  Get_charset(out p: WideString): HResult; stdcall;
    function  Set_charset(const p: WideString): HResult; stdcall;
    function  Get_version(out p: WideString): HResult; stdcall;
    function  Get_doctype(out p: WideString): HResult; stdcall;
    function  Get_dtdURL(out p: WideString): HResult; stdcall;
    function  createElement(vType: OleVariant; var1: OleVariant; out ppElem: IXMLElement2): HResult; stdcall;
    function  Get_async(out pf: WordBool): HResult; stdcall;
    function  Set_async(pf: WordBool): HResult; stdcall;
  end;




  IXMLElement2 = interface(IDispatch)
    ['{2B8DE2FF-8D2D-11D1-B2FC-00C04FD915A9}']
    function  Get_tagName: WideString; safecall;
    procedure Set_tagName(const p: WideString); safecall;
    function  Get_parent: IXMLElement2; safecall;
    procedure setAttribute(const strPropertyName: WideString; PropertyValue: OleVariant); safecall;
    function  getAttribute(const strPropertyName: WideString): OleVariant; safecall;
    procedure removeAttribute(const strPropertyName: WideString); safecall;
    function  Get_children: IXMLElementCollection; safecall;
    function  Get_type_: Integer; safecall;
    function  Get_text: WideString; safecall;
    procedure Set_text(const p: WideString); safecall;
    procedure addChild(const pChildElem: IXMLElement2; lIndex: Integer; lReserved: Integer); safecall;
    procedure removeChild(const pChildElem: IXMLElement2); safecall;
    function  Get_attributes: IXMLElementCollection; safecall;
    property tagName: WideString read Get_tagName write Set_tagName;
    property parent: IXMLElement2 read Get_parent;
    property children: IXMLElementCollection read Get_children;
    property type_: Integer read Get_type_;
    property text: WideString read Get_text write Set_text;
    property attributes: IXMLElementCollection read Get_attributes;
  end;




  IXMLElement2Disp = dispinterface
    ['{2B8DE2FF-8D2D-11D1-B2FC-00C04FD915A9}']
    property tagName: WideString dispid 65737;
    property parent: IXMLElement2 readonly dispid 65738;
    procedure setAttribute(const strPropertyName: WideString; PropertyValue: OleVariant); dispid 65739;
    function  getAttribute(const strPropertyName: WideString): OleVariant; dispid 65740;
    procedure removeAttribute(const strPropertyName: WideString); dispid 65741;
    property children: IXMLElementCollection readonly dispid 65742;
    property type_: Integer readonly dispid 65743;
    property text: WideString dispid 65744;
    procedure addChild(const pChildElem: IXMLElement2; lIndex: Integer; lReserved: Integer); dispid 65745;
    procedure removeChild(const pChildElem: IXMLElement2); dispid 65746;
    property attributes: IXMLElementCollection readonly dispid 65747;
  end;




  IXMLAttribute = interface(IDispatch)
    ['{D4D4A0FC-3B73-11D1-B2B4-00C04FB92596}']
    function  Get_name: WideString; safecall;
    function  Get_value: WideString; safecall;
    property name: WideString read Get_name;
    property value: WideString read Get_value;
  end;




  IXMLAttributeDisp = dispinterface
    ['{D4D4A0FC-3B73-11D1-B2B4-00C04FB92596}']
    property name: WideString readonly dispid 65937;
    property value: WideString readonly dispid 65938;
  end;




  IXMLError = interface(IUnknown)
    ['{948C5AD3-C58D-11D0-9C0B-00C04FC99C8E}']
    function  GetErrorInfo(var pErrorReturn: _xml_error): HResult; stdcall;
  end;






  CoDOMDocument = class
    class function Create: IXMLDOMDocument;
    class function CreateRemote(const MachineName: string): IXMLDOMDocument;
  end;








{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TDOMDocumentProperties= class;
{$ENDIF}
  TDOMDocument = class(TOleServer)
  private
    FOnondataavailable: TNotifyEvent;
    FOnonreadystatechange: TNotifyEvent;
    FIntf:        IXMLDOMDocument;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TDOMDocumentProperties;
    function      GetServerProperties: TDOMDocumentProperties;
{$ENDIF}
    function      GetDefaultInterface: IXMLDOMDocument;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get_doctype: IXMLDOMDocumentType;
    function  Get_implementation_: IXMLDOMImplementation;
    function  Get_documentElement: IXMLDOMElement;
    procedure Set_documentElement(const DOMElement: IXMLDOMElement);
    function  Get_readyState: Integer;
    function  Get_parseError: IXMLDOMParseError;
    function  Get_url: WideString;
    function  Get_async: WordBool;
    procedure Set_async(isAsync: WordBool);
    function  Get_validateOnParse: WordBool;
    procedure Set_validateOnParse(isValidating: WordBool);
    function  Get_resolveExternals: WordBool;
    procedure Set_resolveExternals(isResolving: WordBool);
    function  Get_preserveWhiteSpace: WordBool;
    procedure Set_preserveWhiteSpace(isPreserving: WordBool);
    procedure Set_onreadystatechange(Param1: OleVariant);
    procedure Set_ondataavailable(Param1: OleVariant);
    procedure Set_ontransformnode(Param1: OleVariant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IXMLDOMDocument);
    procedure Disconnect; override;
    function  createElement(const tagName: WideString): IXMLDOMElement;
    function  createDocumentFragment: IXMLDOMDocumentFragment;
    function  createTextNode(const data: WideString): IXMLDOMText;
    function  createComment(const data: WideString): IXMLDOMComment;
    function  createCDATASection(const data: WideString): IXMLDOMCDATASection;
    function  createProcessingInstruction(const target: WideString; const data: WideString): IXMLDOMProcessingInstruction;
    function  createAttribute(const name: WideString): IXMLDOMAttribute;
    function  createEntityReference(const name: WideString): IXMLDOMEntityReference;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList;
    function  createNode(type_: OleVariant; const name: WideString; const namespaceURI: WideString): IXMLDOMNode;
    function  nodeFromID(const idString: WideString): IXMLDOMNode;
    function  load(xmlSource: OleVariant): WordBool;
    procedure abort;
    function  loadXML(const bstrXML: WideString): WordBool;
    procedure save(desination: OleVariant);
    property  DefaultInterface: IXMLDOMDocument read GetDefaultInterface;
    property doctype: IXMLDOMDocumentType read Get_doctype;
    property implementation_: IXMLDOMImplementation read Get_implementation_;
    property documentElement: IXMLDOMElement read Get_documentElement write Set_documentElement;
    property readyState: Integer read Get_readyState;
    property parseError: IXMLDOMParseError read Get_parseError;
    property url: WideString read Get_url;
    property onreadystatechange: OleVariant write Set_onreadystatechange;
    property ondataavailable: OleVariant write Set_ondataavailable;
    property ontransformnode: OleVariant write Set_ontransformnode;
    property async: WordBool read Get_async write Set_async;
    property validateOnParse: WordBool read Get_validateOnParse write Set_validateOnParse;
    property resolveExternals: WordBool read Get_resolveExternals write Set_resolveExternals;
    property preserveWhiteSpace: WordBool read Get_preserveWhiteSpace write Set_preserveWhiteSpace;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TDOMDocumentProperties read GetServerProperties;
{$ENDIF}
    property Onondataavailable: TNotifyEvent read FOnondataavailable write FOnondataavailable;
    property Ononreadystatechange: TNotifyEvent read FOnonreadystatechange write FOnonreadystatechange;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}





 TDOMDocumentProperties = class(TPersistent)
  private
    FServer:    TDOMDocument;
    function    GetDefaultInterface: IXMLDOMDocument;
    constructor Create(AServer: TDOMDocument);
  protected
    function  Get_doctype: IXMLDOMDocumentType;
    function  Get_implementation_: IXMLDOMImplementation;
    function  Get_documentElement: IXMLDOMElement;
    procedure Set_documentElement(const DOMElement: IXMLDOMElement);
    function  Get_readyState: Integer;
    function  Get_parseError: IXMLDOMParseError;
    function  Get_url: WideString;
    function  Get_async: WordBool;
    procedure Set_async(isAsync: WordBool);
    function  Get_validateOnParse: WordBool;
    procedure Set_validateOnParse(isValidating: WordBool);
    function  Get_resolveExternals: WordBool;
    procedure Set_resolveExternals(isResolving: WordBool);
    function  Get_preserveWhiteSpace: WordBool;
    procedure Set_preserveWhiteSpace(isPreserving: WordBool);
    procedure Set_onreadystatechange(Param1: OleVariant);
    procedure Set_ondataavailable(Param1: OleVariant);
    procedure Set_ontransformnode(Param1: OleVariant);
  public
    property DefaultInterface: IXMLDOMDocument read GetDefaultInterface;
  published
    property async: WordBool read Get_async write Set_async;
    property validateOnParse: WordBool read Get_validateOnParse write Set_validateOnParse;
    property resolveExternals: WordBool read Get_resolveExternals write Set_resolveExternals;
    property preserveWhiteSpace: WordBool read Get_preserveWhiteSpace write Set_preserveWhiteSpace;
  end;
{$ENDIF}






  CoDOMFreeThreadedDocument = class
    class function Create: IXMLDOMDocument;
    class function CreateRemote(const MachineName: string): IXMLDOMDocument;
  end;








{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TDOMFreeThreadedDocumentProperties= class;
{$ENDIF}
  TDOMFreeThreadedDocument = class(TOleServer)
  private
    FOnondataavailable: TNotifyEvent;
    FOnonreadystatechange: TNotifyEvent;
    FIntf:        IXMLDOMDocument;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TDOMFreeThreadedDocumentProperties;
    function      GetServerProperties: TDOMFreeThreadedDocumentProperties;
{$ENDIF}
    function      GetDefaultInterface: IXMLDOMDocument;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get_doctype: IXMLDOMDocumentType;
    function  Get_implementation_: IXMLDOMImplementation;
    function  Get_documentElement: IXMLDOMElement;
    procedure Set_documentElement(const DOMElement: IXMLDOMElement);
    function  Get_readyState: Integer;
    function  Get_parseError: IXMLDOMParseError;
    function  Get_url: WideString;
    function  Get_async: WordBool;
    procedure Set_async(isAsync: WordBool);
    function  Get_validateOnParse: WordBool;
    procedure Set_validateOnParse(isValidating: WordBool);
    function  Get_resolveExternals: WordBool;
    procedure Set_resolveExternals(isResolving: WordBool);
    function  Get_preserveWhiteSpace: WordBool;
    procedure Set_preserveWhiteSpace(isPreserving: WordBool);
    procedure Set_onreadystatechange(Param1: OleVariant);
    procedure Set_ondataavailable(Param1: OleVariant);
    procedure Set_ontransformnode(Param1: OleVariant);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IXMLDOMDocument);
    procedure Disconnect; override;
    function  createElement(const tagName: WideString): IXMLDOMElement;
    function  createDocumentFragment: IXMLDOMDocumentFragment;
    function  createTextNode(const data: WideString): IXMLDOMText;
    function  createComment(const data: WideString): IXMLDOMComment;
    function  createCDATASection(const data: WideString): IXMLDOMCDATASection;
    function  createProcessingInstruction(const target: WideString; const data: WideString): IXMLDOMProcessingInstruction;
    function  createAttribute(const name: WideString): IXMLDOMAttribute;
    function  createEntityReference(const name: WideString): IXMLDOMEntityReference;
    function  getElementsByTagName(const tagName: WideString): IXMLDOMNodeList;
    function  createNode(type_: OleVariant; const name: WideString; const namespaceURI: WideString): IXMLDOMNode;
    function  nodeFromID(const idString: WideString): IXMLDOMNode;
    function  load(xmlSource: OleVariant): WordBool;
    procedure abort;
    function  loadXML(const bstrXML: WideString): WordBool;
    procedure save(desination: OleVariant);
    property  DefaultInterface: IXMLDOMDocument read GetDefaultInterface;
    property doctype: IXMLDOMDocumentType read Get_doctype;
    property implementation_: IXMLDOMImplementation read Get_implementation_;
    property documentElement: IXMLDOMElement read Get_documentElement write Set_documentElement;
    property readyState: Integer read Get_readyState;
    property parseError: IXMLDOMParseError read Get_parseError;
    property url: WideString read Get_url;
    property onreadystatechange: OleVariant write Set_onreadystatechange;
    property ondataavailable: OleVariant write Set_ondataavailable;
    property ontransformnode: OleVariant write Set_ontransformnode;
    property async: WordBool read Get_async write Set_async;
    property validateOnParse: WordBool read Get_validateOnParse write Set_validateOnParse;
    property resolveExternals: WordBool read Get_resolveExternals write Set_resolveExternals;
    property preserveWhiteSpace: WordBool read Get_preserveWhiteSpace write Set_preserveWhiteSpace;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TDOMFreeThreadedDocumentProperties read GetServerProperties;
{$ENDIF}
    property Onondataavailable: TNotifyEvent read FOnondataavailable write FOnondataavailable;
    property Ononreadystatechange: TNotifyEvent read FOnonreadystatechange write FOnonreadystatechange;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}





 TDOMFreeThreadedDocumentProperties = class(TPersistent)
  private
    FServer:    TDOMFreeThreadedDocument;
    function    GetDefaultInterface: IXMLDOMDocument;
    constructor Create(AServer: TDOMFreeThreadedDocument);
  protected
    function  Get_doctype: IXMLDOMDocumentType;
    function  Get_implementation_: IXMLDOMImplementation;
    function  Get_documentElement: IXMLDOMElement;
    procedure Set_documentElement(const DOMElement: IXMLDOMElement);
    function  Get_readyState: Integer;
    function  Get_parseError: IXMLDOMParseError;
    function  Get_url: WideString;
    function  Get_async: WordBool;
    procedure Set_async(isAsync: WordBool);
    function  Get_validateOnParse: WordBool;
    procedure Set_validateOnParse(isValidating: WordBool);
    function  Get_resolveExternals: WordBool;
    procedure Set_resolveExternals(isResolving: WordBool);
    function  Get_preserveWhiteSpace: WordBool;
    procedure Set_preserveWhiteSpace(isPreserving: WordBool);
    procedure Set_onreadystatechange(Param1: OleVariant);
    procedure Set_ondataavailable(Param1: OleVariant);
    procedure Set_ontransformnode(Param1: OleVariant);
  public
    property DefaultInterface: IXMLDOMDocument read GetDefaultInterface;
  published
    property async: WordBool read Get_async write Set_async;
    property validateOnParse: WordBool read Get_validateOnParse write Set_validateOnParse;
    property resolveExternals: WordBool read Get_resolveExternals write Set_resolveExternals;
    property preserveWhiteSpace: WordBool read Get_preserveWhiteSpace write Set_preserveWhiteSpace;
  end;
{$ENDIF}






  CoXMLHTTPRequest = class
    class function Create: IXMLHttpRequest;
    class function CreateRemote(const MachineName: string): IXMLHttpRequest;
  end;








{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TXMLHTTPRequestProperties= class;
{$ENDIF}
  TXMLHTTPRequest = class(TOleServer)
  private
    FIntf:        IXMLHttpRequest;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TXMLHTTPRequestProperties;
    function      GetServerProperties: TXMLHTTPRequestProperties;
{$ENDIF}
    function      GetDefaultInterface: IXMLHttpRequest;
  protected
    procedure InitServerData; override;
    function  Get_status: Integer;
    function  Get_statusText: WideString;
    function  Get_responseXML: IDispatch;
    function  Get_responseText: WideString;
    function  Get_responseBody: OleVariant;
    function  Get_responseStream: OleVariant;
    function  Get_readyState: Integer;
    procedure Set_onreadystatechange(const Param1: IDispatch);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IXMLHttpRequest);
    procedure Disconnect; override;
    procedure open(const bstrMethod: WideString; const bstrUrl: WideString); overload;
    procedure open(const bstrMethod: WideString; const bstrUrl: WideString; varAsync: OleVariant); overload;
    procedure open(const bstrMethod: WideString; const bstrUrl: WideString; varAsync: OleVariant; 
                   bstrUser: OleVariant); overload;
    procedure open(const bstrMethod: WideString; const bstrUrl: WideString; varAsync: OleVariant; 
                   bstrUser: OleVariant; bstrPassword: OleVariant); overload;
    procedure setRequestHeader(const bstrHeader: WideString; const bstrValue: WideString);
    function  getResponseHeader(const bstrHeader: WideString): WideString;
    function  getAllResponseHeaders: WideString;
    procedure send; overload;
    procedure send(varBody: OleVariant); overload;
    procedure abort;
    property  DefaultInterface: IXMLHttpRequest read GetDefaultInterface;
    property status: Integer read Get_status;
    property statusText: WideString read Get_statusText;
    property responseXML: IDispatch read Get_responseXML;
    property responseText: WideString read Get_responseText;
    property responseBody: OleVariant read Get_responseBody;
    property responseStream: OleVariant read Get_responseStream;
    property readyState: Integer read Get_readyState;
    property onreadystatechange: IDispatch write Set_onreadystatechange;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TXMLHTTPRequestProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}





 TXMLHTTPRequestProperties = class(TPersistent)
  private
    FServer:    TXMLHTTPRequest;
    function    GetDefaultInterface: IXMLHttpRequest;
    constructor Create(AServer: TXMLHTTPRequest);
  protected
    function  Get_status: Integer;
    function  Get_statusText: WideString;
    function  Get_responseXML: IDispatch;
    function  Get_responseText: WideString;
    function  Get_responseBody: OleVariant;
    function  Get_responseStream: OleVariant;
    function  Get_readyState: Integer;
    procedure Set_onreadystatechange(const Param1: IDispatch);
  public
    property DefaultInterface: IXMLHttpRequest read GetDefaultInterface;
  published
  end;
{$ENDIF}






  CoXMLDSOControl = class
    class function Create: IXMLDSOControl;
    class function CreateRemote(const MachineName: string): IXMLDSOControl;
  end;








{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TXMLDSOControlProperties= class;
{$ENDIF}
  TXMLDSOControl = class(TOleServer)
  private
    FIntf:        IXMLDSOControl;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TXMLDSOControlProperties;
    function      GetServerProperties: TXMLDSOControlProperties;
{$ENDIF}
    function      GetDefaultInterface: IXMLDSOControl;
  protected
    procedure InitServerData; override;
    function  Get_XMLDocument: IXMLDOMDocument;
    procedure Set_XMLDocument(const ppDoc: IXMLDOMDocument);
    function  Get_JavaDSOCompatible: Integer;
    procedure Set_JavaDSOCompatible(fJavaDSOCompatible: Integer);
    function  Get_readyState: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IXMLDSOControl);
    procedure Disconnect; override;
    property  DefaultInterface: IXMLDSOControl read GetDefaultInterface;
    property readyState: Integer read Get_readyState;
    property XMLDocument: IXMLDOMDocument read Get_XMLDocument write Set_XMLDocument;
    property JavaDSOCompatible: Integer read Get_JavaDSOCompatible write Set_JavaDSOCompatible;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TXMLDSOControlProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}





 TXMLDSOControlProperties = class(TPersistent)
  private
    FServer:    TXMLDSOControl;
    function    GetDefaultInterface: IXMLDSOControl;
    constructor Create(AServer: TXMLDSOControl);
  protected
    function  Get_XMLDocument: IXMLDOMDocument;
    procedure Set_XMLDocument(const ppDoc: IXMLDOMDocument);
    function  Get_JavaDSOCompatible: Integer;
    procedure Set_JavaDSOCompatible(fJavaDSOCompatible: Integer);
    function  Get_readyState: Integer;
  public
    property DefaultInterface: IXMLDSOControl read GetDefaultInterface;
  published
    property XMLDocument: IXMLDOMDocument read Get_XMLDocument write Set_XMLDocument;
    property JavaDSOCompatible: Integer read Get_JavaDSOCompatible write Set_JavaDSOCompatible;
  end;
{$ENDIF}






  CoXMLDocument = class
    class function Create: IXMLDocument2;
    class function CreateRemote(const MachineName: string): IXMLDocument2;
  end;








{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TXMLDocumentProperties= class;
{$ENDIF}
  TXMLDocument = class(TOleServer)
  private
    FIntf:        IXMLDocument2;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TXMLDocumentProperties;
    function      GetServerProperties: TXMLDocumentProperties;
{$ENDIF}
    function      GetDefaultInterface: IXMLDocument2;
  protected
    procedure InitServerData; override;
    function  Get_root(out p: IXMLElement2): HResult;
    function  Get_url(out p: WideString): HResult;
    function  Set_url(const p: WideString): HResult;
    function  Get_readyState(out pl: Integer): HResult;
    function  Get_charset(out p: WideString): HResult;
    function  Set_charset(const p: WideString): HResult;
    function  Get_version(out p: WideString): HResult;
    function  Get_doctype(out p: WideString): HResult;
    function  Get_async(out pf: WordBool): HResult;
    function  Set_async(pf: WordBool): HResult;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IXMLDocument2);
    procedure Disconnect; override;
    function  createElement(vType: OleVariant; var1: OleVariant; out ppElem: IXMLElement2): HResult;
    property  DefaultInterface: IXMLDocument2 read GetDefaultInterface;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TXMLDocumentProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}





 TXMLDocumentProperties = class(TPersistent)
  private
    FServer:    TXMLDocument;
    function    GetDefaultInterface: IXMLDocument2;
    constructor Create(AServer: TXMLDocument);
  protected
    function  Get_root(out p: IXMLElement2): HResult;
    function  Get_url(out p: WideString): HResult;
    function  Set_url(const p: WideString): HResult;
    function  Get_readyState(out pl: Integer): HResult;
    function  Get_charset(out p: WideString): HResult;
    function  Set_charset(const p: WideString): HResult;
    function  Get_version(out p: WideString): HResult;
    function  Get_doctype(out p: WideString): HResult;
    function  Get_async(out pf: WordBool): HResult;
    function  Set_async(pf: WordBool): HResult;
  public
    property DefaultInterface: IXMLDocument2 read GetDefaultInterface;
  published
  end;
{$ENDIF}


implementation

uses ComObj;

class function CoDOMDocument.Create: IXMLDOMDocument;
begin
  Result := CreateComObject(CLASS_DOMDocument) as IXMLDOMDocument;
end;

class function CoDOMDocument.CreateRemote(const MachineName: string): IXMLDOMDocument;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DOMDocument) as IXMLDOMDocument;
end;

procedure TDOMDocument.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{2933BF90-7B36-11D2-B20E-00C04F983E60}';
    IntfIID:   '{2933BF81-7B36-11D2-B20E-00C04F983E60}';
    EventIID:  '{3EFAA427-272F-11D2-836F-0000F87A7782}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TDOMDocument.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IXMLDOMDocument;
  end;
end;

procedure TDOMDocument.ConnectTo(svrIntf: IXMLDOMDocument);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TDOMDocument.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TDOMDocument.GetDefaultInterface: IXMLDOMDocument;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TDOMDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TDOMDocumentProperties.Create(Self);
{$ENDIF}
end;

destructor TDOMDocument.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TDOMDocument.GetServerProperties: TDOMDocumentProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TDOMDocument.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;
   198: if Assigned(FOnondataavailable) then
            FOnondataavailable(Self);
   -609: if Assigned(FOnonreadystatechange) then
            FOnonreadystatechange(Self);
  end; {case DispID}
end;

function  TDOMDocument.Get_doctype: IXMLDOMDocumentType;
begin
  Result := DefaultInterface.Get_doctype;
end;

function  TDOMDocument.Get_implementation_: IXMLDOMImplementation;
begin
  Result := DefaultInterface.Get_implementation_;
end;

function  TDOMDocument.Get_documentElement: IXMLDOMElement;
begin
  Result := DefaultInterface.Get_documentElement;
end;

procedure TDOMDocument.Set_documentElement(const DOMElement: IXMLDOMElement);
begin
  DefaultInterface.Set_documentElement(DOMElement);
end;

function  TDOMDocument.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

function  TDOMDocument.Get_parseError: IXMLDOMParseError;
begin
  Result := DefaultInterface.Get_parseError;
end;

function  TDOMDocument.Get_url: WideString;
begin
  Result := DefaultInterface.Get_url;
end;

function  TDOMDocument.Get_async: WordBool;
begin
  Result := DefaultInterface.Get_async;
end;

procedure TDOMDocument.Set_async(isAsync: WordBool);
begin
  DefaultInterface.Set_async(isAsync);
end;

function  TDOMDocument.Get_validateOnParse: WordBool;
begin
  Result := DefaultInterface.Get_validateOnParse;
end;

procedure TDOMDocument.Set_validateOnParse(isValidating: WordBool);
begin
  DefaultInterface.Set_validateOnParse(isValidating);
end;

function  TDOMDocument.Get_resolveExternals: WordBool;
begin
  Result := DefaultInterface.Get_resolveExternals;
end;

procedure TDOMDocument.Set_resolveExternals(isResolving: WordBool);
begin
  DefaultInterface.Set_resolveExternals(isResolving);
end;

function  TDOMDocument.Get_preserveWhiteSpace: WordBool;
begin
  Result := DefaultInterface.Get_preserveWhiteSpace;
end;

procedure TDOMDocument.Set_preserveWhiteSpace(isPreserving: WordBool);
begin
  DefaultInterface.Set_preserveWhiteSpace(isPreserving);
end;

procedure TDOMDocument.Set_onreadystatechange(Param1: OleVariant);
begin
  DefaultInterface.Set_onreadystatechange(Param1);
end;

procedure TDOMDocument.Set_ondataavailable(Param1: OleVariant);
begin
  DefaultInterface.Set_ondataavailable(Param1);
end;

procedure TDOMDocument.Set_ontransformnode(Param1: OleVariant);
begin
  DefaultInterface.Set_ontransformnode(Param1);
end;

function  TDOMDocument.createElement(const tagName: WideString): IXMLDOMElement;
begin
  Result := DefaultInterface.createElement(tagName);
end;

function  TDOMDocument.createDocumentFragment: IXMLDOMDocumentFragment;
begin
  Result := DefaultInterface.createDocumentFragment;
end;

function  TDOMDocument.createTextNode(const data: WideString): IXMLDOMText;
begin
  Result := DefaultInterface.createTextNode(data);
end;

function  TDOMDocument.createComment(const data: WideString): IXMLDOMComment;
begin
  Result := DefaultInterface.createComment(data);
end;

function  TDOMDocument.createCDATASection(const data: WideString): IXMLDOMCDATASection;
begin
  Result := DefaultInterface.createCDATASection(data);
end;

function  TDOMDocument.createProcessingInstruction(const target: WideString; const data: WideString): IXMLDOMProcessingInstruction;
begin
  Result := DefaultInterface.createProcessingInstruction(target, data);
end;

function  TDOMDocument.createAttribute(const name: WideString): IXMLDOMAttribute;
begin
  Result := DefaultInterface.createAttribute(name);
end;

function  TDOMDocument.createEntityReference(const name: WideString): IXMLDOMEntityReference;
begin
  Result := DefaultInterface.createEntityReference(name);
end;

function  TDOMDocument.getElementsByTagName(const tagName: WideString): IXMLDOMNodeList;
begin
  Result := DefaultInterface.getElementsByTagName(tagName);
end;

function  TDOMDocument.createNode(type_: OleVariant; const name: WideString; 
                                  const namespaceURI: WideString): IXMLDOMNode;
begin
  Result := DefaultInterface.createNode(type_, name, namespaceURI);
end;

function  TDOMDocument.nodeFromID(const idString: WideString): IXMLDOMNode;
begin
  Result := DefaultInterface.nodeFromID(idString);
end;

function  TDOMDocument.load(xmlSource: OleVariant): WordBool;
begin
  Result := DefaultInterface.load(xmlSource);
end;

procedure TDOMDocument.abort;
begin
  DefaultInterface.abort;
end;

function  TDOMDocument.loadXML(const bstrXML: WideString): WordBool;
begin
  Result := DefaultInterface.loadXML(bstrXML);
end;

procedure TDOMDocument.save(desination: OleVariant);
begin
  DefaultInterface.save(desination);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TDOMDocumentProperties.Create(AServer: TDOMDocument);
begin
  inherited Create;
  FServer := AServer;
end;

function TDOMDocumentProperties.GetDefaultInterface: IXMLDOMDocument;
begin
  Result := FServer.DefaultInterface;
end;

function  TDOMDocumentProperties.Get_doctype: IXMLDOMDocumentType;
begin
  Result := DefaultInterface.Get_doctype;
end;

function  TDOMDocumentProperties.Get_implementation_: IXMLDOMImplementation;
begin
  Result := DefaultInterface.Get_implementation_;
end;

function  TDOMDocumentProperties.Get_documentElement: IXMLDOMElement;
begin
  Result := DefaultInterface.Get_documentElement;
end;

procedure TDOMDocumentProperties.Set_documentElement(const DOMElement: IXMLDOMElement);
begin
  DefaultInterface.Set_documentElement(DOMElement);
end;

function  TDOMDocumentProperties.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

function  TDOMDocumentProperties.Get_parseError: IXMLDOMParseError;
begin
  Result := DefaultInterface.Get_parseError;
end;

function  TDOMDocumentProperties.Get_url: WideString;
begin
  Result := DefaultInterface.Get_url;
end;

function  TDOMDocumentProperties.Get_async: WordBool;
begin
  Result := DefaultInterface.Get_async;
end;

procedure TDOMDocumentProperties.Set_async(isAsync: WordBool);
begin
  DefaultInterface.Set_async(isAsync);
end;

function  TDOMDocumentProperties.Get_validateOnParse: WordBool;
begin
  Result := DefaultInterface.Get_validateOnParse;
end;

procedure TDOMDocumentProperties.Set_validateOnParse(isValidating: WordBool);
begin
  DefaultInterface.Set_validateOnParse(isValidating);
end;

function  TDOMDocumentProperties.Get_resolveExternals: WordBool;
begin
  Result := DefaultInterface.Get_resolveExternals;
end;

procedure TDOMDocumentProperties.Set_resolveExternals(isResolving: WordBool);
begin
  DefaultInterface.Set_resolveExternals(isResolving);
end;

function  TDOMDocumentProperties.Get_preserveWhiteSpace: WordBool;
begin
  Result := DefaultInterface.Get_preserveWhiteSpace;
end;

procedure TDOMDocumentProperties.Set_preserveWhiteSpace(isPreserving: WordBool);
begin
  DefaultInterface.Set_preserveWhiteSpace(isPreserving);
end;

procedure TDOMDocumentProperties.Set_onreadystatechange(Param1: OleVariant);
begin
  DefaultInterface.Set_onreadystatechange(Param1);
end;

procedure TDOMDocumentProperties.Set_ondataavailable(Param1: OleVariant);
begin
  DefaultInterface.Set_ondataavailable(Param1);
end;

procedure TDOMDocumentProperties.Set_ontransformnode(Param1: OleVariant);
begin
  DefaultInterface.Set_ontransformnode(Param1);
end;

{$ENDIF}

class function CoDOMFreeThreadedDocument.Create: IXMLDOMDocument;
begin
  Result := CreateComObject(CLASS_DOMFreeThreadedDocument) as IXMLDOMDocument;
end;

class function CoDOMFreeThreadedDocument.CreateRemote(const MachineName: string): IXMLDOMDocument;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_DOMFreeThreadedDocument) as IXMLDOMDocument;
end;

procedure TDOMFreeThreadedDocument.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{2933BF91-7B36-11D2-B20E-00C04F983E60}';
    IntfIID:   '{2933BF81-7B36-11D2-B20E-00C04F983E60}';
    EventIID:  '{3EFAA427-272F-11D2-836F-0000F87A7782}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TDOMFreeThreadedDocument.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IXMLDOMDocument;
  end;
end;

procedure TDOMFreeThreadedDocument.ConnectTo(svrIntf: IXMLDOMDocument);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TDOMFreeThreadedDocument.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TDOMFreeThreadedDocument.GetDefaultInterface: IXMLDOMDocument;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TDOMFreeThreadedDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TDOMFreeThreadedDocumentProperties.Create(Self);
{$ENDIF}
end;

destructor TDOMFreeThreadedDocument.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TDOMFreeThreadedDocument.GetServerProperties: TDOMFreeThreadedDocumentProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TDOMFreeThreadedDocument.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;
   198: if Assigned(FOnondataavailable) then
            FOnondataavailable(Self);
   -609: if Assigned(FOnonreadystatechange) then
            FOnonreadystatechange(Self);
  end; {case DispID}
end;

function  TDOMFreeThreadedDocument.Get_doctype: IXMLDOMDocumentType;
begin
  Result := DefaultInterface.Get_doctype;
end;

function  TDOMFreeThreadedDocument.Get_implementation_: IXMLDOMImplementation;
begin
  Result := DefaultInterface.Get_implementation_;
end;

function  TDOMFreeThreadedDocument.Get_documentElement: IXMLDOMElement;
begin
  Result := DefaultInterface.Get_documentElement;
end;

procedure TDOMFreeThreadedDocument.Set_documentElement(const DOMElement: IXMLDOMElement);
begin
  DefaultInterface.Set_documentElement(DOMElement);
end;

function  TDOMFreeThreadedDocument.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

function  TDOMFreeThreadedDocument.Get_parseError: IXMLDOMParseError;
begin
  Result := DefaultInterface.Get_parseError;
end;

function  TDOMFreeThreadedDocument.Get_url: WideString;
begin
  Result := DefaultInterface.Get_url;
end;

function  TDOMFreeThreadedDocument.Get_async: WordBool;
begin
  Result := DefaultInterface.Get_async;
end;

procedure TDOMFreeThreadedDocument.Set_async(isAsync: WordBool);
begin
  DefaultInterface.Set_async(isAsync);
end;

function  TDOMFreeThreadedDocument.Get_validateOnParse: WordBool;
begin
  Result := DefaultInterface.Get_validateOnParse;
end;

procedure TDOMFreeThreadedDocument.Set_validateOnParse(isValidating: WordBool);
begin
  DefaultInterface.Set_validateOnParse(isValidating);
end;

function  TDOMFreeThreadedDocument.Get_resolveExternals: WordBool;
begin
  Result := DefaultInterface.Get_resolveExternals;
end;

procedure TDOMFreeThreadedDocument.Set_resolveExternals(isResolving: WordBool);
begin
  DefaultInterface.Set_resolveExternals(isResolving);
end;

function  TDOMFreeThreadedDocument.Get_preserveWhiteSpace: WordBool;
begin
  Result := DefaultInterface.Get_preserveWhiteSpace;
end;

procedure TDOMFreeThreadedDocument.Set_preserveWhiteSpace(isPreserving: WordBool);
begin
  DefaultInterface.Set_preserveWhiteSpace(isPreserving);
end;

procedure TDOMFreeThreadedDocument.Set_onreadystatechange(Param1: OleVariant);
begin
  DefaultInterface.Set_onreadystatechange(Param1);
end;

procedure TDOMFreeThreadedDocument.Set_ondataavailable(Param1: OleVariant);
begin
  DefaultInterface.Set_ondataavailable(Param1);
end;

procedure TDOMFreeThreadedDocument.Set_ontransformnode(Param1: OleVariant);
begin
  DefaultInterface.Set_ontransformnode(Param1);
end;

function  TDOMFreeThreadedDocument.createElement(const tagName: WideString): IXMLDOMElement;
begin
  Result := DefaultInterface.createElement(tagName);
end;

function  TDOMFreeThreadedDocument.createDocumentFragment: IXMLDOMDocumentFragment;
begin
  Result := DefaultInterface.createDocumentFragment;
end;

function  TDOMFreeThreadedDocument.createTextNode(const data: WideString): IXMLDOMText;
begin
  Result := DefaultInterface.createTextNode(data);
end;

function  TDOMFreeThreadedDocument.createComment(const data: WideString): IXMLDOMComment;
begin
  Result := DefaultInterface.createComment(data);
end;

function  TDOMFreeThreadedDocument.createCDATASection(const data: WideString): IXMLDOMCDATASection;
begin
  Result := DefaultInterface.createCDATASection(data);
end;

function  TDOMFreeThreadedDocument.createProcessingInstruction(const target: WideString; 
                                                               const data: WideString): IXMLDOMProcessingInstruction;
begin
  Result := DefaultInterface.createProcessingInstruction(target, data);
end;

function  TDOMFreeThreadedDocument.createAttribute(const name: WideString): IXMLDOMAttribute;
begin
  Result := DefaultInterface.createAttribute(name);
end;

function  TDOMFreeThreadedDocument.createEntityReference(const name: WideString): IXMLDOMEntityReference;
begin
  Result := DefaultInterface.createEntityReference(name);
end;

function  TDOMFreeThreadedDocument.getElementsByTagName(const tagName: WideString): IXMLDOMNodeList;
begin
  Result := DefaultInterface.getElementsByTagName(tagName);
end;

function  TDOMFreeThreadedDocument.createNode(type_: OleVariant; const name: WideString; 
                                              const namespaceURI: WideString): IXMLDOMNode;
begin
  Result := DefaultInterface.createNode(type_, name, namespaceURI);
end;

function  TDOMFreeThreadedDocument.nodeFromID(const idString: WideString): IXMLDOMNode;
begin
  Result := DefaultInterface.nodeFromID(idString);
end;

function  TDOMFreeThreadedDocument.load(xmlSource: OleVariant): WordBool;
begin
  Result := DefaultInterface.load(xmlSource);
end;

procedure TDOMFreeThreadedDocument.abort;
begin
  DefaultInterface.abort;
end;

function  TDOMFreeThreadedDocument.loadXML(const bstrXML: WideString): WordBool;
begin
  Result := DefaultInterface.loadXML(bstrXML);
end;

procedure TDOMFreeThreadedDocument.save(desination: OleVariant);
begin
  DefaultInterface.save(desination);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TDOMFreeThreadedDocumentProperties.Create(AServer: TDOMFreeThreadedDocument);
begin
  inherited Create;
  FServer := AServer;
end;

function TDOMFreeThreadedDocumentProperties.GetDefaultInterface: IXMLDOMDocument;
begin
  Result := FServer.DefaultInterface;
end;

function  TDOMFreeThreadedDocumentProperties.Get_doctype: IXMLDOMDocumentType;
begin
  Result := DefaultInterface.Get_doctype;
end;

function  TDOMFreeThreadedDocumentProperties.Get_implementation_: IXMLDOMImplementation;
begin
  Result := DefaultInterface.Get_implementation_;
end;

function  TDOMFreeThreadedDocumentProperties.Get_documentElement: IXMLDOMElement;
begin
  Result := DefaultInterface.Get_documentElement;
end;

procedure TDOMFreeThreadedDocumentProperties.Set_documentElement(const DOMElement: IXMLDOMElement);
begin
  DefaultInterface.Set_documentElement(DOMElement);
end;

function  TDOMFreeThreadedDocumentProperties.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

function  TDOMFreeThreadedDocumentProperties.Get_parseError: IXMLDOMParseError;
begin
  Result := DefaultInterface.Get_parseError;
end;

function  TDOMFreeThreadedDocumentProperties.Get_url: WideString;
begin
  Result := DefaultInterface.Get_url;
end;

function  TDOMFreeThreadedDocumentProperties.Get_async: WordBool;
begin
  Result := DefaultInterface.Get_async;
end;

procedure TDOMFreeThreadedDocumentProperties.Set_async(isAsync: WordBool);
begin
  DefaultInterface.Set_async(isAsync);
end;

function  TDOMFreeThreadedDocumentProperties.Get_validateOnParse: WordBool;
begin
  Result := DefaultInterface.Get_validateOnParse;
end;

procedure TDOMFreeThreadedDocumentProperties.Set_validateOnParse(isValidating: WordBool);
begin
  DefaultInterface.Set_validateOnParse(isValidating);
end;

function  TDOMFreeThreadedDocumentProperties.Get_resolveExternals: WordBool;
begin
  Result := DefaultInterface.Get_resolveExternals;
end;

procedure TDOMFreeThreadedDocumentProperties.Set_resolveExternals(isResolving: WordBool);
begin
  DefaultInterface.Set_resolveExternals(isResolving);
end;

function  TDOMFreeThreadedDocumentProperties.Get_preserveWhiteSpace: WordBool;
begin
  Result := DefaultInterface.Get_preserveWhiteSpace;
end;

procedure TDOMFreeThreadedDocumentProperties.Set_preserveWhiteSpace(isPreserving: WordBool);
begin
  DefaultInterface.Set_preserveWhiteSpace(isPreserving);
end;

procedure TDOMFreeThreadedDocumentProperties.Set_onreadystatechange(Param1: OleVariant);
begin
  DefaultInterface.Set_onreadystatechange(Param1);
end;

procedure TDOMFreeThreadedDocumentProperties.Set_ondataavailable(Param1: OleVariant);
begin
  DefaultInterface.Set_ondataavailable(Param1);
end;

procedure TDOMFreeThreadedDocumentProperties.Set_ontransformnode(Param1: OleVariant);
begin
  DefaultInterface.Set_ontransformnode(Param1);
end;

{$ENDIF}

class function CoXMLHTTPRequest.Create: IXMLHttpRequest;
begin
  Result := CreateComObject(CLASS_XMLHTTPRequest) as IXMLHttpRequest;
end;

class function CoXMLHTTPRequest.CreateRemote(const MachineName: string): IXMLHttpRequest;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XMLHTTPRequest) as IXMLHttpRequest;
end;

procedure TXMLHTTPRequest.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{ED8C108E-4349-11D2-91A4-00C04F7969E8}';
    IntfIID:   '{ED8C108D-4349-11D2-91A4-00C04F7969E8}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TXMLHTTPRequest.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IXMLHttpRequest;
  end;
end;

procedure TXMLHTTPRequest.ConnectTo(svrIntf: IXMLHttpRequest);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TXMLHTTPRequest.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TXMLHTTPRequest.GetDefaultInterface: IXMLHttpRequest;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TXMLHTTPRequest.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TXMLHTTPRequestProperties.Create(Self);
{$ENDIF}
end;

destructor TXMLHTTPRequest.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TXMLHTTPRequest.GetServerProperties: TXMLHTTPRequestProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TXMLHTTPRequest.Get_status: Integer;
begin
  Result := DefaultInterface.Get_status;
end;

function  TXMLHTTPRequest.Get_statusText: WideString;
begin
  Result := DefaultInterface.Get_statusText;
end;

function  TXMLHTTPRequest.Get_responseXML: IDispatch;
begin
  Result := DefaultInterface.Get_responseXML;
end;

function  TXMLHTTPRequest.Get_responseText: WideString;
begin
  Result := DefaultInterface.Get_responseText;
end;

function  TXMLHTTPRequest.Get_responseBody: OleVariant;
begin
  Result := DefaultInterface.Get_responseBody;
end;

function  TXMLHTTPRequest.Get_responseStream: OleVariant;
begin
  Result := DefaultInterface.Get_responseStream;
end;

function  TXMLHTTPRequest.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

procedure TXMLHTTPRequest.Set_onreadystatechange(const Param1: IDispatch);
begin
  DefaultInterface.Set_onreadystatechange(Param1);
end;

procedure TXMLHTTPRequest.open(const bstrMethod: WideString; const bstrUrl: WideString);
begin
  DefaultInterface.open(bstrMethod, bstrUrl, EmptyParam, EmptyParam, EmptyParam);
end;

procedure TXMLHTTPRequest.open(const bstrMethod: WideString; const bstrUrl: WideString; 
                               varAsync: OleVariant);
begin
  DefaultInterface.open(bstrMethod, bstrUrl, varAsync, EmptyParam, EmptyParam);
end;

procedure TXMLHTTPRequest.open(const bstrMethod: WideString; const bstrUrl: WideString; 
                               varAsync: OleVariant; bstrUser: OleVariant);
begin
  DefaultInterface.open(bstrMethod, bstrUrl, varAsync, bstrUser, EmptyParam);
end;

procedure TXMLHTTPRequest.open(const bstrMethod: WideString; const bstrUrl: WideString; 
                               varAsync: OleVariant; bstrUser: OleVariant; bstrPassword: OleVariant);
begin
  DefaultInterface.open(bstrMethod, bstrUrl, varAsync, bstrUser, bstrPassword);
end;

procedure TXMLHTTPRequest.setRequestHeader(const bstrHeader: WideString; const bstrValue: WideString);
begin
  DefaultInterface.setRequestHeader(bstrHeader, bstrValue);
end;

function  TXMLHTTPRequest.getResponseHeader(const bstrHeader: WideString): WideString;
begin
  Result := DefaultInterface.getResponseHeader(bstrHeader);
end;

function  TXMLHTTPRequest.getAllResponseHeaders: WideString;
begin
  Result := DefaultInterface.getAllResponseHeaders;
end;

procedure TXMLHTTPRequest.send;
begin
  DefaultInterface.send(EmptyParam);
end;

procedure TXMLHTTPRequest.send(varBody: OleVariant);
begin
  DefaultInterface.send(varBody);
end;

procedure TXMLHTTPRequest.abort;
begin
  DefaultInterface.abort;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TXMLHTTPRequestProperties.Create(AServer: TXMLHTTPRequest);
begin
  inherited Create;
  FServer := AServer;
end;

function TXMLHTTPRequestProperties.GetDefaultInterface: IXMLHttpRequest;
begin
  Result := FServer.DefaultInterface;
end;

function  TXMLHTTPRequestProperties.Get_status: Integer;
begin
  Result := DefaultInterface.Get_status;
end;

function  TXMLHTTPRequestProperties.Get_statusText: WideString;
begin
  Result := DefaultInterface.Get_statusText;
end;

function  TXMLHTTPRequestProperties.Get_responseXML: IDispatch;
begin
  Result := DefaultInterface.Get_responseXML;
end;

function  TXMLHTTPRequestProperties.Get_responseText: WideString;
begin
  Result := DefaultInterface.Get_responseText;
end;

function  TXMLHTTPRequestProperties.Get_responseBody: OleVariant;
begin
  Result := DefaultInterface.Get_responseBody;
end;

function  TXMLHTTPRequestProperties.Get_responseStream: OleVariant;
begin
  Result := DefaultInterface.Get_responseStream;
end;

function  TXMLHTTPRequestProperties.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

procedure TXMLHTTPRequestProperties.Set_onreadystatechange(const Param1: IDispatch);
begin
  DefaultInterface.Set_onreadystatechange(Param1);
end;

{$ENDIF}

class function CoXMLDSOControl.Create: IXMLDSOControl;
begin
  Result := CreateComObject(CLASS_XMLDSOControl) as IXMLDSOControl;
end;

class function CoXMLDSOControl.CreateRemote(const MachineName: string): IXMLDSOControl;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XMLDSOControl) as IXMLDSOControl;
end;

procedure TXMLDSOControl.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{550DDA30-0541-11D2-9CA9-0060B0EC3D39}';
    IntfIID:   '{310AFA62-0575-11D2-9CA9-0060B0EC3D39}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TXMLDSOControl.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IXMLDSOControl;
  end;
end;

procedure TXMLDSOControl.ConnectTo(svrIntf: IXMLDSOControl);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TXMLDSOControl.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TXMLDSOControl.GetDefaultInterface: IXMLDSOControl;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TXMLDSOControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TXMLDSOControlProperties.Create(Self);
{$ENDIF}
end;

destructor TXMLDSOControl.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TXMLDSOControl.GetServerProperties: TXMLDSOControlProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TXMLDSOControl.Get_XMLDocument: IXMLDOMDocument;
begin
  Result := DefaultInterface.Get_XMLDocument;
end;

procedure TXMLDSOControl.Set_XMLDocument(const ppDoc: IXMLDOMDocument);
begin
  DefaultInterface.Set_XMLDocument(ppDoc);
end;

function  TXMLDSOControl.Get_JavaDSOCompatible: Integer;
begin
  Result := DefaultInterface.Get_JavaDSOCompatible;
end;

procedure TXMLDSOControl.Set_JavaDSOCompatible(fJavaDSOCompatible: Integer);
begin
  DefaultInterface.Set_JavaDSOCompatible(fJavaDSOCompatible);
end;

function  TXMLDSOControl.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TXMLDSOControlProperties.Create(AServer: TXMLDSOControl);
begin
  inherited Create;
  FServer := AServer;
end;

function TXMLDSOControlProperties.GetDefaultInterface: IXMLDSOControl;
begin
  Result := FServer.DefaultInterface;
end;

function  TXMLDSOControlProperties.Get_XMLDocument: IXMLDOMDocument;
begin
  Result := DefaultInterface.Get_XMLDocument;
end;

procedure TXMLDSOControlProperties.Set_XMLDocument(const ppDoc: IXMLDOMDocument);
begin
  DefaultInterface.Set_XMLDocument(ppDoc);
end;

function  TXMLDSOControlProperties.Get_JavaDSOCompatible: Integer;
begin
  Result := DefaultInterface.Get_JavaDSOCompatible;
end;

procedure TXMLDSOControlProperties.Set_JavaDSOCompatible(fJavaDSOCompatible: Integer);
begin
  DefaultInterface.Set_JavaDSOCompatible(fJavaDSOCompatible);
end;

function  TXMLDSOControlProperties.Get_readyState: Integer;
begin
  Result := DefaultInterface.Get_readyState;
end;

{$ENDIF}

class function CoXMLDocument.Create: IXMLDocument2;
begin
  Result := CreateComObject(CLASS_XMLDocument) as IXMLDocument2;
end;

class function CoXMLDocument.CreateRemote(const MachineName: string): IXMLDocument2;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_XMLDocument) as IXMLDocument2;
end;

procedure TXMLDocument.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{CFC399AF-D876-11D0-9C10-00C04FC99C8E}';
    IntfIID:   '{2B8DE2FE-8D2D-11D1-B2FC-00C04FD915A9}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TXMLDocument.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IXMLDocument2;
  end;
end;

procedure TXMLDocument.ConnectTo(svrIntf: IXMLDocument2);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TXMLDocument.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TXMLDocument.GetDefaultInterface: IXMLDocument2;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TXMLDocument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TXMLDocumentProperties.Create(Self);
{$ENDIF}
end;

destructor TXMLDocument.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TXMLDocument.GetServerProperties: TXMLDocumentProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function  TXMLDocument.Get_root(out p: IXMLElement2): HResult;
begin
  Result := DefaultInterface.Get_root(p);
end;

function  TXMLDocument.Get_url(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_url(p);
end;

function  TXMLDocument.Set_url(const p: WideString): HResult;
begin
  Result := DefaultInterface.Set_url(p);
end;

function  TXMLDocument.Get_readyState(out pl: Integer): HResult;
begin
  Result := DefaultInterface.Get_readyState(pl);
end;

function  TXMLDocument.Get_charset(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_charset(p);
end;

function  TXMLDocument.Set_charset(const p: WideString): HResult;
begin
  Result := DefaultInterface.Set_charset(p);
end;

function  TXMLDocument.Get_version(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_version(p);
end;

function  TXMLDocument.Get_doctype(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_doctype(p);
end;

function  TXMLDocument.Get_async(out pf: WordBool): HResult;
begin
  Result := DefaultInterface.Get_async(pf);
end;

function  TXMLDocument.Set_async(pf: WordBool): HResult;
begin
  Result := DefaultInterface.Set_async(pf);
end;

function  TXMLDocument.createElement(vType: OleVariant; var1: OleVariant; out ppElem: IXMLElement2): HResult;
begin
  Result := DefaultInterface.createElement(vType, var1, ppElem);
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TXMLDocumentProperties.Create(AServer: TXMLDocument);
begin
  inherited Create;
  FServer := AServer;
end;

function TXMLDocumentProperties.GetDefaultInterface: IXMLDocument2;
begin
  Result := FServer.DefaultInterface;
end;

function  TXMLDocumentProperties.Get_root(out p: IXMLElement2): HResult;
begin
  Result := DefaultInterface.Get_root(p);
end;

function  TXMLDocumentProperties.Get_url(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_url(p);
end;

function  TXMLDocumentProperties.Set_url(const p: WideString): HResult;
begin
  Result := DefaultInterface.Set_url(p);
end;

function  TXMLDocumentProperties.Get_readyState(out pl: Integer): HResult;
begin
  Result := DefaultInterface.Get_readyState(pl);
end;

function  TXMLDocumentProperties.Get_charset(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_charset(p);
end;

function  TXMLDocumentProperties.Set_charset(const p: WideString): HResult;
begin
  Result := DefaultInterface.Set_charset(p);
end;

function  TXMLDocumentProperties.Get_version(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_version(p);
end;

function  TXMLDocumentProperties.Get_doctype(out p: WideString): HResult;
begin
  Result := DefaultInterface.Get_doctype(p);
end;

function  TXMLDocumentProperties.Get_async(out pf: WordBool): HResult;
begin
  Result := DefaultInterface.Get_async(pf);
end;

function  TXMLDocumentProperties.Set_async(pf: WordBool): HResult;
begin
  Result := DefaultInterface.Set_async(pf);
end;

{$ENDIF}

{
procedure Register;
begin
  RegisterComponents('ActiveX',[TDOMDocument, TDOMFreeThreadedDocument, TXMLHTTPRequest, TXMLDSOControl,
    TXMLDocument]);
end;
}

end.
