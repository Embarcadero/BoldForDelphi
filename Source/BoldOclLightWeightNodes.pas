
{ Global compiler directives }
{$include bold.inc}
unit BoldOclLightWeightNodes;

interface
uses
  Classes,
  BoldStreams,
  BoldBase,
  BoldId,
  BoldCondition;

type
  TBoldOLWNode = class;
  TBoldOLWTypeNode = class;
  TBoldOLWListCoercion = class;
  TBoldOLWOperation = class;
  TBoldOLWIteration = class;
  TBoldOLWMember = class;
  TBoldOLWVariableBinding = class;
  TBoldOLWVariableReference = class;
  TBoldOLWLiteral = class;
  TBoldOLWStrLiteral = class;
  TBoldOLWFloatLiteral = class;
  TBoldOLWIntLiteral = class;
  TBoldOLWTimeLiteral = class;
  TBoldOLWDateLiteral = class;
  TBoldOLWEnumLiteral = class;

  TBoldOLWNodeList = class;

  TBoldOclCondition = class(TBoldConditionWithClass)
  private
    fRootNode: TBoldOLWNode;
    fContext: TBoldObjectIdList;
    fEnv: TBoldOLWNodeList;
    fOclExpr: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GetStreamName: string; override;
    property RootNode: TBoldOLWNode read fRootNode write fRootNode;
    property Context: TBoldObjectIdList read fContext;
    property Env: TBoldOLWNodeList read fEnv;
    property OclExpr: string read fOclExpr write fOclExpr;
  end;


  TBoldOLWNodeVisitor = class(TObject)
  protected
    procedure VisitTBoldOLWNode(N: TBoldOLWNode); virtual;
    procedure VisitTBoldOLWListCoercion(N: TBoldOLWListCoercion); virtual;
    procedure VisitTBoldOLWOperation(N: TBoldOLWOperation); virtual;
    procedure VisitTBoldOLWIteration(N: TBoldOLWIteration); virtual;
    procedure VisitTBoldOLWMember(N: TBoldOLWMember); virtual;
    procedure VisitTBoldOLWLiteral(N: TBoldOLWLiteral); virtual;
    procedure VisitTBoldOLWStrLiteral(N: TBoldOLWStrLiteral); virtual;
    procedure VisitTBoldOLWDateLiteral(N: TBoldOLWDateLiteral); virtual;
    procedure VisitTBoldOLWTimeLiteral(N: TBoldOLWTimeLiteral); virtual;
    procedure VisitTBoldOLWFloatLiteral(N: TBoldOLWFloatLiteral); virtual;
    procedure VisitTBoldOLWEnumLiteral(N: TBoldOLWEnumLiteral); virtual;
    procedure VisitTBoldOLWIntLiteral(N: TBoldOLWIntLiteral); virtual;
    procedure VisitTBoldOLWVariableBinding(N: TBoldOLWVariableBinding); virtual;
    procedure VisitTBoldOLWVariableReference(N: TBoldOLWVariableReference); virtual;
    procedure VisitTBoldOLWTypeNode(N: TBoldOLWTypeNode); virtual;
  end;

  TBoldOLWNodeList = class(TBoldNonRefCountedObject, IBoldStreamable)
  private
    fList: TList;
    fOwnsObjects: Boolean;
    function GetItem(index: Integer): TBoldOLWNode;
    procedure PutItem(index: Integer; Value: TBoldOLWNode);
    function GetStreamName: string;
    function GetCount: integer;
    function GetIndexOf(Node: TBoldOLWNode): integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TBoldOLWNode): Integer;
    procedure TraverseList(V: TBoldOLWNodeVisitor); virtual;
    property Items[index: Integer]: TBoldOLWNode read GetItem write PutItem; default;
    property Count: integer read GetCount;
    property IndexOf[Node: TBoldOLWNode]: integer read GetIndexOf;
    property OwnsObjects: Boolean read fOwnsObjects write fOwnsObjects;
  end;


  TBoldOLWNode = class(TBoldNonRefCountedObject, IBoldStreamable)
  private
    fPosition: integer;
  protected
    function GetStreamName: string; virtual; abstract;
   public
    constructor Create(Position: integer);
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); virtual;
    property Position: Integer read fPosition;
  end;


  TBoldOLWTypeNode = class(TBoldOLWNode)
  private
    fTypeName: String;
    fTopSortedIndex: Integer;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; const TypeName: string; TopSortedIndex: integer);
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property TypeName: String read fTypeName;
    property TopSortedIndex: Integer read fTopSortedIndex;
  end;

  TBoldOLWListCoercion = class(TBoldOLWNode)
  private
    fChild: TBoldOLWNode;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; Child: TBoldOLWNode);
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property Child: TBoldOLWNode read fChild;
  end;

  TBoldOLWOperation = class(TBoldOLWNode)
  private
    fArgs: TBoldOLWNodeList;
    fOperationName: string;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; const OperationName: String);
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property Args: TBoldOLWNodeList read fArgs;
    property OperationName: string read fOperationName;
  end;

  TBoldOLWIteration = class(TBoldOLWOperation)
  private
    fLoopVar: TBoldOLWVariableBinding;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; const OperationName: String; LoopVar: TBoldOLWVariableBinding);
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property LoopVar: TBoldOLWVariableBinding read fLoopVar;
  end;


  TBoldOLWMember = class(TBoldOLWNode)
  private
    fMemberIndex: Integer;
    fMemberName: string;
    fmemberOf: TBoldOLWNode;
    fQualifier: TBoldOLWNodeList;
    fIsBoolean: Boolean;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; const memberName: string; MemberIndex: Integer; MemberOf: TBoldOLWNode; IsBoolean: Boolean);
    destructor Destroy; override;
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property MemberOf: TBoldOLWNode read fmemberOf;
    property MemberName: string read fMemberName;
    property MemberIndex: Integer read fMemberIndex;
    property Qualifier: TBoldOLWNodeList read fQualifier;
    property IsBoolean: Boolean read fIsBoolean;
  end;

  TBoldOLWVariableBinding = class(TBoldOLWNode)
  private
    fVariableName: string;
    fTopSortedIndex: integer;
    fExternalVarValue: Variant;
    fIsLoopVar: Boolean;
    fRefCount: integer;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; const VariableName: String; TopSortedIndex: integer);
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    procedure AddRef;
    property VariableName: string read fVariableName;
    property TopSortedIndex: integer read fTopSortedIndex;
    property ExternalVarvalue: Variant read fExternalVarValue write fExternalVarValue;
    property IsLoopVar: Boolean read fIsLoopVar write fIsLoopVar;
  end;

  TBoldOLWVariableReference = class(TBoldOLWNode)
  private
    fVariableBinding: TBoldOLWVariableBinding;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; VariableBinding: TBoldOLWVariableBinding);
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property VariableBinding: TBoldOLWVariableBinding read fVariableBinding;
  end;

  TBoldOLWLiteral = class(TBoldOLWNode)
  public
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
  end;

  TBoldOLWStrLiteral = class(TBoldOLWLiteral)
  private
    fStrValue: String;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; const StrValue: String);
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property StrValue: String read fStrValue;
  end;

  TBoldOLWMomentLiteral = class(TBoldOLWLiteral)
  protected
    fMomentValue: TDateTime;
  public
    constructor Create(Position: integer; MomentValue: TDateTime);
  end;

  TBoldOLWDateLiteral = class(TBoldOLWMomentLiteral)
  private
  protected
    function GetStreamName: string; override;
  public
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property DateValue: TDateTime read fMomentValue write fMomentValue;
  end;

  TBoldOLWTimeLiteral = class(TBoldOLWMomentLiteral)
  private
  protected
    function GetStreamName: string; override;
  public
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property TimeValue: TDateTime read fMomentValue write fMomentValue;
  end;

  TBoldOLWFloatLiteral = class(TBoldOLWLiteral)
  private
    fFloatValue: Double;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; FloatValue: Double);
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property FloatValue: Double read fFloatValue;
  end;

  TBoldOLWIntLiteral = class(TBoldOLWLiteral)
  private
    fIntValue: Integer;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; IntValue: Integer);
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
    property IntValue: Integer read fIntValue;
  end;

  TBoldOLWEnumLiteral = class(TBoldOLWLiteral)
  private
    fName: string;
    fIntValue: integer;
  protected
    function GetStreamName: string; override;
  public
    constructor Create(Position: integer; const Name: String);
    property Name: string read fName;
    property Intvalue: integer read fIntValue write fIntvalue;
    procedure AcceptVisitor(V: TBoldOLWNodeVisitor); override;
  end;

implementation

uses
  SysUtils,
  {$IFDEF OXML}OXmlPDOM{$ELSE}Bold_MSXML_TLB{$ENDIF},
  BoldCoreConsts,
  BoldXMLStreaming,
  BoldDefaultStreamNames,
  BoldDefs;

const
  OCLConditionStreamName = 'OCLCondition';
  OLWNodeListStreamName = 'OLWNodeList';
  OLWNodeStreamName = 'OLWNode';
  OLWTypeNodeStreamName = 'OLWTypeNode';
  OLWListCoercionStreamName = 'OLWListCoercion';
  OLWOperationStreamName = 'OLWOperation';
  OLWIterationStreamName = 'OLWIteration';
  OLWMemberStreamName = 'OLWMember';
  OLWVariableBindingStreamName = 'OLWVariableBinding';
  OLWVariableReferenceStreamName = 'OLWVariableReference';
  OLWStrLiteralStreamName = 'OLWStrLiteral';
  OLWFloatLiteralStreamName = 'OLWFloatLiteral';
  OLWIntLiteralStreamName = 'OLWIntLiteral';
  OLWEnumLiteralStreamName = 'OLWEnumLiteral';
  OLWDateLiteralStreamName = 'OLWDateLiteral';
  OLWTimeLiteralStreamName = 'OLWTimeLiteral';


type
  TBoldXMLOCLConditionStreamer = class(TBoldXMLConditionWithClassStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWNodeListStreamer = class(TBoldXMLObjectStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;


  TBoldXMLOLWNodeStreamer = class(TBoldXMLObjectStreamer)
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
  end;

  TBoldXMLOLWTypeNodeStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWListCoercionStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWOperationStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWIterationStreamer = class(TBoldXMLOLWOperationStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWMemberStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWVariableBindingStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;


  TBoldXMLOLWVariableReferenceStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWStrLiteralStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWFloatLiteralStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWIntLiteralStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWDateLiteralStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;

  TBoldXMLOLWTimeLiteralStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;



  TBoldXMLOLWEnumLiteralStreamer = class(TBoldXMLOLWNodeStreamer)
  protected
    function GetStreamName: string; override;
  public
    procedure WriteObject(Obj: TBoldInterfacedObject; Node: TBoldXMLNode); override;
    procedure ReadObject(Obj: TObject; Node: TBoldXMLNode); override;
    function CreateObject: TObject; override;
  end;


{ TBoldOLWOperation }


function TBoldOLWOperation.GetStreamName: string;
begin
  result := OLWOperationStreamName;
end;

procedure TBoldOLWOperation.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWOperation(self);
end;

constructor TBoldOLWOperation.create(Position: integer; const OperationName: String);
begin
  inherited create(position);
  fOperationName := OperationName;
  fArgs := TBoldOLWNodeList.Create;
end;

destructor TBoldOLWOperation.Destroy;
begin
  FreeAndNil(fArgs);
  inherited;          
end;


{ TBoldOLWNode }

procedure TBoldOLWNode.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  v.VisitTBoldOLWNode(self);
end;

constructor TBoldOLWNode.Create(Position: integer);
begin
  inherited create;
  fPosition := Position;
end;

{ TBoldOLWTypeNode }

procedure TBoldOLWTypeNode.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWTypeNode(self);
end;

constructor TBoldOLWTypeNode.Create(Position: integer; const TypeName: string; TopSortedIndex: integer);
begin
  inherited create(Position);
  fTypeName := TypeName;
  fTopSortedIndex := TopSortedIndex;
end;

function TBoldOLWTypeNode.GetStreamName: string;
begin
  result := OLWTypeNodeStreamName;
end;

{ TBoldOLWIteration }

procedure TBoldOLWIteration.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWIteration(self);
end;

constructor TBoldOLWIteration.Create(Position: integer; const OperationName: String; LoopVar: TBoldOLWVariableBinding);
begin
  inherited Create(Position, OperationName);
  fLoopVar := LoopVar;
end;

destructor TBoldOLWIteration.Destroy;
begin
  inherited;
  FreeandNil(fLoopVar);
end;

function TBoldOLWIteration.GetStreamName: string;
begin
  result := OLWIterationStreamName;
end;

{ TBoldOLWMember }

procedure TBoldOLWMember.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWMember(self);
end;

constructor TBoldOLWMember.Create(Position: integer; const memberName: String; MemberIndex: Integer; MemberOf: TBoldOLWNode; IsBoolean: Boolean);
begin
  inherited Create(Position);
  fMemberName := memberName;
  fMemberIndex := MemberIndex;
  fMemberOf := MemberOf;
  fQualifier := TBoldOLWNodeList.Create;
  fIsBoolean := IsBoolean;
end;

destructor TBoldOLWMember.Destroy;
begin
  FreeAndNil(fMemberOf);
  FreeAndNil(fQualifier);
  inherited;
end;

function TBoldOLWMember.GetStreamName: string;
begin
  result := OLWMemberStreamName;
end;

{ TBoldOLWNodeList }

function TBoldOLWNodeList.Add(Item: TBoldOLWNode): Integer;
begin
  result := fList.add(item);
end;


constructor TBoldOLWNodeList.Create;
begin
  fList := TList.create;
  fOwnsObjects := true;
end;

destructor TBoldOLWNodeList.Destroy;
var
  i: integer;
begin
  if OwnsObjects then
    for i := 0 to Count-1 do
      Items[i].Free;
  FreeAndNil(fList);
  inherited;
end;

function TBoldOLWNodeList.GetCount: integer;
begin
  result := fList.Count;
end;

function TBoldOLWNodeList.GetIndexOf(Node: TBoldOLWNode): integer;
begin
  result := fList.IndexOf(Node);
end;

function TBoldOLWNodeList.GetItem(index: Integer): TBoldOLWNode;
begin
  result := TObject(flist[index]) as TBoldOLWNode;
end;

function TBoldOLWNodeList.GetStreamName: string;
begin
  result := OLWNodeListStreamName;
end;

procedure TBoldOLWNodeList.PutItem(index: Integer; Value: TBoldOLWNode);
begin
  flist[index] := value;
end;

procedure TBoldOLWNodeList.TraverseList(V: TBoldOLWNodeVisitor);
var
  i: integer;
begin
  for i := 0 to Count-1 do
    items[i].AcceptVisitor(v);
end;

{ TBoldOLWVariableBinding }

procedure TBoldOLWVariableBinding.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWVariableBinding(self);
end;


procedure TBoldOLWVariableBinding.AddRef;
begin
  inc(fRefCount);
  if (fRefCount > 1) and not fIsLoopVar then
    raise EBold.Create(sExternalVarsCanOnlyBeReferencedOnce);
end;

constructor TBoldOLWVariableBinding.Create(Position: integer; const VariableName: String; TopSortedIndex: integer);
begin
  inherited Create(Position);
  fVariableName := VariableName;
  fTopSortedIndex := TopSortedIndex;
end;

function TBoldOLWVariableBinding.GetStreamName: string;
begin
  result := OLWVariableBindingStreamName;
end;

{ TBoldOLWStrLiteral }

procedure TBoldOLWStrLiteral.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  v.VisitTBoldOLWStrLiteral(self);
end;


constructor TBoldOLWStrLiteral.Create(Position: integer; const StrValue: String);
begin
  inherited Create(Position);
  fStrValue := StrValue;
end;


function TBoldOLWStrLiteral.GetStreamName: string;
begin
  result := OLWStrLiteralStreamName;
end;

{ TBoldOLWNodeVisitor }

procedure TBoldOLWNodeVisitor.VisitTBoldOLWEnumLiteral(N: TBoldOLWEnumLiteral);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWIntLiteral(N: TBoldOLWIntLiteral);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWIteration(N: TBoldOLWIteration);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWListCoercion(N: TBoldOLWListCoercion);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWLiteral(N: TBoldOLWLiteral);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWMember(N: TBoldOLWMember);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWNode(N: TBoldOLWNode);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWFloatLiteral(N: TBoldOLWFloatLiteral);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWOperation(N: TBoldOLWOperation);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWStrLiteral(N: TBoldOLWStrLiteral);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWTypeNode(N: TBoldOLWTypeNode);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWVariableBinding(N: TBoldOLWVariableBinding);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWVariableReference(N: TBoldOLWVariableReference);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWDateLiteral(
  N: TBoldOLWDateLiteral);
begin

end;

procedure TBoldOLWNodeVisitor.VisitTBoldOLWTimeLiteral(
  N: TBoldOLWTimeLiteral);
begin

end;

{ TBoldOLWVariableReference }

procedure TBoldOLWVariableReference.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWVariableReference(self);
end;

constructor TBoldOLWVariableReference.Create(Position: integer; VariableBinding: TBoldOLWVariableBinding);
begin
  inherited Create(Position);
  fVariableBinding := VariableBinding;
end;


function TBoldOLWVariableReference.GetStreamName: string;
begin
  result := OLWVariableReferenceStreamName;
end;

{ TBoldOLWEnumLiteral }

procedure TBoldOLWEnumLiteral.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWEnumLiteral(self);
end;

{ TBoldOLWListCoercion }

constructor TBoldOLWListCoercion.Create(Position: integer; Child: TBoldOLWNode);
begin
  inherited create(Position);
  fChild := Child;
end;

destructor TBoldOLWListCoercion.Destroy;
begin
  FreeAndNil(fChild);
  inherited;
end;

procedure TBoldOLWListCoercion.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWListCoercion(self);
end;

function TBoldOLWListCoercion.GetStreamName: string;
begin
  result := OLWListCoercionStreamName;
end;

constructor TBoldOLWEnumLiteral.Create(Position: integer; const Name: String);
begin
  inherited Create(Position);
  fName := Name;
end;

function TBoldOLWEnumLiteral.GetStreamName: string;
begin
  result := OLWEnumLiteralStreamName;
end;

{ TBoldOLWLiteral }

procedure TBoldOLWLiteral.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWLiteral(self);
end;


{ TBoldOLWFloatLiteral }

procedure TBoldOLWFloatLiteral.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWFloatLiteral(self);
end;

constructor TBoldOLWFloatLiteral.Create(Position: integer; FloatValue: Double);
begin
  inherited Create(Position);
  fFloatValue := FloatValue;
end;

function TBoldOLWFloatLiteral.GetStreamName: string;
begin
  result := OLWFloatLiteralStreamName;
end;

{ TBoldOLWIntLiteral }

procedure TBoldOLWIntLiteral.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWIntLiteral(self);
end;

constructor TBoldOLWIntLiteral.Create(Position, IntValue: Integer);
begin
  inherited Create(Position);
  fIntValue := IntValue;
end;

function TBoldOLWIntLiteral.GetStreamName: string;
begin
  result := OLWIntLiteralStreamName;
end;

{ TBoldOclCondition }

constructor TBoldOclCondition.create;
begin
  inherited;
  fContext := TBoldObjectIdList.Create;
  fEnv := TBoldOLWNodeList.Create;
end;

destructor TBoldOclCondition.Destroy;
begin
  FreeAndNil(fContext);
  FreeAndNil(fEnv);
  FreeAndNil(fRootNode);
  inherited;
end;

function TBoldOclCondition.GetStreamName: string;
begin
  result := OCLConditionStreamName;
end;

{ TBoldXMLOCLConditionStreamer }

function TBoldXMLOCLConditionStreamer.CreateObject: TObject;
begin
  result := TBoldOclCondition.create;
end;

function TBoldXMLOCLConditionStreamer.GetStreamName: string;
begin
  result := OCLConditionStreamName;
end;

procedure TBoldXMLOCLConditionStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  Condition: TBoldOclCondition;
  Bindings: TBoldOLWNodeList;
begin
  inherited;
  Condition := Obj as TBoldOclCondition;
  Bindings := TBoldOLWNodeList.Create;
  Bindings.OwnsObjects := false;
  Node.AddStateObject('Bindings', Bindings);
  Condition.fEnv := Node.ReadSubnodeObject('Env', OLWNodeListStreamName) as TBoldOLWNodeList;
  Condition.fRootNode := Node.ReadSubnodeObject('RootNode', '') as TBoldOLWNode;
  Condition.fContext := Node.ReadSubnodeObject('Context', BOLDOBJECTIDLISTNAME) as TBoldObjectidList;
  Condition.fOclExpr := Node.ReadSubNodeString('OCL');
  Node.RemoveStateObject('Bindings');
  Bindings.Free;
end;

procedure TBoldXMLOCLConditionStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  Condition: TBoldOclCondition;
  Bindings: TBoldOLWNodeList;
begin
  inherited;
  Condition := Obj as TBoldOclCondition;
  Bindings := TBoldOLWNodeList.Create;
  Bindings.OwnsObjects := false;
  Node.AddStateObject('Bindings', Bindings);

  Node.WriteSubnodeObject('Env', OLWNodeListStreamName, Condition.fEnv);
  Node.WriteSubNodeObject('RootNode', '', Condition.fRootNode);
  Node.WriteSubnodeObject('Context', BOLDOBJECTIDLISTNAME, Condition.fContext);
  Node.WriteSubNodeString('OCL', Condition.fOclExpr);

  Node.RemoveStateObject('Bindings');
  Bindings.free;
end;



{ TBoldXMLOLWNodeStreamer }

procedure TBoldXMLOLWNodeStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWNode: TBoldOLWNode;
begin
  inherited;
  OLWNode := obj as TBoldOLWNode;
  OLWNode.fPosition := Node.ReadSubNodeInteger('Position');
end;

procedure TBoldXMLOLWNodeStreamer.WriteObject(Obj: TBoldInterfacedObject;
  Node: TBoldXMLNode);
var
  OLWNode: TBoldOLWNode;
begin
  inherited;
  OLWNode := obj as TBoldOLWNode;
  Node.WriteSubNodeInteger('Position', OLWNode.fPosition);
end;

{ TBoldXMLOLWTypeNodeStreamer }

function TBoldXMLOLWTypeNodeStreamer.CreateObject: TObject;
begin
  result := TBoldOLWTypeNode.Create(0, '', -1);
end;

function TBoldXMLOLWTypeNodeStreamer.GetStreamName: string;
begin
  result := OLWTypeNodeStreamName;
end;

procedure TBoldXMLOLWTypeNodeStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWTypeNode: TBoldOLWTypeNode;
begin
  inherited;
  OLWTypeNode := Obj as TBoldOLWTypeNode;
  OLWTypeNode.fTypeName := Node.ReadSubNodeString('TypeName');
  OLWTypeNode.fTopSortedIndex := Node.ReadSubNodeInteger('TopSortedIndex');
end;

procedure TBoldXMLOLWTypeNodeStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  OLWTypeNode: TBoldOLWTypeNode;
begin
  inherited;
  OLWTypeNode := Obj as TBoldOLWTypeNode;
  Node.WriteSubNodeString('TypeName', OLWTypeNode.TypeName);
  Node.WriteSubNodeInteger('TopSortedIndex', OLWTypeNode.TopSortedIndex);
end;

{ TBoldXMLOLWListCoercionStreamer }

function TBoldXMLOLWListCoercionStreamer.CreateObject: TObject;
begin
  result := TBoldOLWListCoercion.Create(0, nil);
end;

function TBoldXMLOLWListCoercionStreamer.GetStreamName: string;
begin
  result := OLWListCoercionStreamName;
end;

procedure TBoldXMLOLWListCoercionStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  ListCoercion: TBoldOLWListcoercion;
begin
  inherited;
  ListCoercion := obj as TBoldOLWListCoercion;
  LIstCoercion.fChild := Node.readSubNodeObject('Child', '') as TBoldOLWNode;
end;

procedure TBoldXMLOLWListCoercionStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  ListCoercion: TBoldOLWListcoercion;
begin
  inherited;
  ListCoercion := obj as TBoldOLWListCoercion;
  Node.WriteSubNodeObject('Child', '', ListCoercion.fChild);
end;

{ TBoldXMLOLWOperationStreamer }

function TBoldXMLOLWOperationStreamer.CreateObject: TObject;
begin
  result := TBoldOLWOperation.Create(0, '');
end;

function TBoldXMLOLWOperationStreamer.GetStreamName: string;
begin
  result := OLWOperationStreamName;
end;

procedure TBoldXMLOLWOperationStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWOperation: TBoldOLWOperation;
begin
  inherited;
  OLWOperation := Obj as TBoldOLWOperation;
  OLWOperation.fArgs := Node.readSubNodeObject('Args', '') as TBoldOLWNodeList;
  OLWOperation.fOperationName := Node.ReadSubNodeString('OperationName');
end;

procedure TBoldXMLOLWOperationStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  OLWOperation: TBoldOLWOperation;
begin
  inherited;
  OLWOperation := Obj as TBoldOLWOperation;
  Node.WriteSubNodeObject('Args', '', OLWOperation.fArgs);
  Node.WriteSubNodeString('OperationName', OLWOperation.fOperationName);
end;

{ TBoldXMLOLWIterationStreamer }

function TBoldXMLOLWIterationStreamer.CreateObject: TObject;
begin
  result := TBoldOLWIteration.Create(0, '', nil);
end;

function TBoldXMLOLWIterationStreamer.GetStreamName: string;
begin
  result := OLWIterationStreamName;
end;

procedure WriteBindingToStream(Binding: TBoldOLWVariableBinding; Node: TBoldXMLNode);
var
  BindingIndex: integer;
  Bindings: TBoldOLWNodeList;
begin
  Bindings := Node.GetStateObject('Bindings') as TBoldOLWNodeList;
  BindingIndex := Bindings.IndexOf[Binding];
  Node.WriteSubNodeInteger('BindingIndex', BindingIndex);
  if BindingIndex = -1 then
  begin
    Node.WriteSubNodeObject('Binding', OLWVariableBindingStreamName, Binding);
    Bindings.Add(Binding);
  end;
end;

function ReadBindingFromStream(Node: TBoldXMLNode): TBoldOLWVariableBinding;
var
  BindingIndex: integer;
  Bindings: TBoldOLWNodeList;
begin
  Bindings := Node.GetStateObject('Bindings') as TBoldOLWNodeList;
  BindingIndex := Node.ReadSubNodeInteger('BindingIndex');
  if BindingIndex <> -1 then
  begin
    Result := Bindings[BindingIndex] as tBoldOLWVariableBinding;
  end
  else
  begin
    Result := Node.ReadSubNodeObject('Binding', OLWVariableBindingStreamName) as TBoldOLWVariableBinding;
    Bindings.Add(result);
  end;
end;

procedure TBoldXMLOLWIterationStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWIteration: TBoldOLWIteration;
begin
  inherited;
  OLWIteration := Obj as TBoldOLWIteration;
  OLWIteration.fLoopVar := ReadBindingFromStream(Node);
end;

procedure TBoldXMLOLWIterationStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  OLWIteration: TBoldOLWIteration;
begin
  inherited;
  OLWIteration := Obj as TBoldOLWIteration;
  WriteBindingToStream(OLWIteration.fLoopVar, Node);
end;

{ TBoldXMLOLWMemberStreamer }

function TBoldXMLOLWMemberStreamer.CreateObject: TObject;
begin
  result := TBoldOLWMember.Create(0, '', -1, nil, false);
end;

function TBoldXMLOLWMemberStreamer.GetStreamName: string;
begin
  result := OLWMemberStreamName;
end;

procedure TBoldXMLOLWMemberStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWMember: TBoldOLWMember;
begin
  inherited;
  OLWMember := Obj as TBoldOLWMember;
  OLWMember.fMemberIndex := Node.ReadSubNodeInteger('MemberIndex');
  OLWMember.fMemberName := Node.ReadSubNodeString('MemberName');
  OLWMember.fMemberOf := Node.ReadSubNodeObject('MemberOf', '') as TBoldOLWNode;
  OLWMember.fQualifier := Node.ReadSubNodeObject('Qualifier', OLWNodeListStreamName) as TBoldOLWNodeList;
  OLWMember.fIsBoolean := Node.ReadSubNodeBoolean('IsBoolean');
end;

procedure TBoldXMLOLWMemberStreamer.WriteObject(Obj: TBoldInterfacedObject;
  Node: TBoldXMLNode);
var
  OLWMember: TBoldOLWMember;
begin
  inherited;
  OLWMember := Obj as TBoldOLWMember;
  Node.WriteSubNodeInteger('MemberIndex', OLWMember.fMemberIndex);
  Node.WriteSubNodeString('MemberName', OLWMember.fMemberName);
  Node.WriteSubNodeObject('MemberOf', '', OLWMember.fMemberOf);
  Node.WriteSubNodeObject('Qualifier', OLWNodeListStreamName, OLWMember.fQualifier);
  Node.WriteSubNodeBoolean('IsBoolean', OLWMember.fIsBoolean);
end;

{ TBoldXMLOLWVariableBindingStreamer }

function TBoldXMLOLWVariableBindingStreamer.CreateObject: TObject;
begin
  result := TBoldOLWVariableBinding.Create(0, '', -1);
end;

function TBoldXMLOLWVariableBindingStreamer.GetStreamName: string;
begin
  result := OLWVariableBindingStreamName;
end;

procedure TBoldXMLOLWVariableBindingStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWBinding: TBoldOLWVariableBinding;
begin
  inherited;
  OLWBinding := Obj as TBoldOLWVariableBinding;
  OLWBinding.fVariableName := Node.ReadSubNodeString('VariableName');
  OLWBinding.fTopSortedIndex := Node.ReadSubNodeInteger('TopSortedIndex');
  OLWBinding.fIsLoopVar := Node.ReadSubNodeBoolean('IsLoopVar');
  OLWBinding.fRefCount := Node.ReadSubNodeInteger('RefCount');
  OLWBinding.fExternalVarValue := Node.ReadSubNodeString('ExternalVarValue');
end;

procedure TBoldXMLOLWVariableBindingStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  OLWBinding: TBoldOLWVariableBinding;
begin
  inherited;
  OLWBinding := Obj as TBoldOLWVariableBinding;
  Node.WriteSubNodeString('VariableName', OLWBinding.fVariableName);
  Node.WriteSubNodeInteger('TopSortedIndex', OLWBinding.fTopSortedIndex);
  Node.WriteSubNodeBoolean('IsLoopVar', OLWBinding.fIsLoopVar);
  Node.WriteSubNodeInteger('RefCount', OLWBinding.fRefCount);
  Node.WriteSubNodeString('ExternalVarValue', OLWBinding.fExternalVarValue);
end;

{ TBoldXMLOLWVariableReferenceStreamer }

function TBoldXMLOLWVariableReferenceStreamer.CreateObject: TObject;
begin
  result := TBoldOLWVariableReference.Create(0, nil);
end;

function TBoldXMLOLWVariableReferenceStreamer.GetStreamName: string;
begin
  result := OLWVariableReferenceStreamName;
end;

procedure TBoldXMLOLWVariableReferenceStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWReference: TBoldOLWVariableReference;
begin
  inherited;
  OLWReference := obj as TBoldOLWVariableReference;
  OLWReference.fVariableBinding := ReadBindingFromStream(Node);
end;

procedure TBoldXMLOLWVariableReferenceStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  OLWReference: TBoldOLWVariableReference;
begin
  inherited;
  OLWReference := obj as TBoldOLWVariableReference;
  WriteBindingToStream(OLWReference.fVariableBinding, Node);
end;

{ TBoldXMLOLWStrLiteralStreamer }

function TBoldXMLOLWStrLiteralStreamer.CreateObject: TObject;
begin
  result := TBoldOLWStrLiteral.Create(0, '');
end;

function TBoldXMLOLWStrLiteralStreamer.GetStreamName: string;
begin
  result := OLWStrLiteralStreamName;
end;

procedure TBoldXMLOLWStrLiteralStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  StrLiteral: TBoldOLWStrLiteral;
begin
  inherited;
  StrLiteral := obj as TBoldOLWStrLiteral;
  StrLiteral.fStrValue := Node.ReadSubNodeString('StrValue');
end;

procedure TBoldXMLOLWStrLiteralStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  StrLiteral: TBoldOLWStrLiteral;
begin
  inherited;
  StrLiteral := obj as TBoldOLWStrLiteral;
  Node.WriteSubNodeString('StrValue', StrLiteral.fStrValue);
end;

{ TBoldXMLOLWIntLiteralStreamer }

function TBoldXMLOLWIntLiteralStreamer.CreateObject: TObject;
begin
  result := TBoldOLWIntLiteral.Create(0, 0);
end;

function TBoldXMLOLWIntLiteralStreamer.GetStreamName: string;
begin
  result := OLWIntLiteralStreamName;
end;

procedure TBoldXMLOLWIntLiteralStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  IntLiteral: TBoldOLWIntLiteral;
begin
  inherited;
  IntLiteral := obj as TBoldOLWIntLiteral;
  IntLiteral.fIntValue := Node.ReadSubNodeInteger('IntValue');
end;

procedure TBoldXMLOLWIntLiteralStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  IntLiteral: TBoldOLWIntLiteral;
begin
  inherited;
  IntLiteral := obj as TBoldOLWIntLiteral;
  Node.WriteSubNodeInteger('IntValue', IntLiteral.fIntValue);
end;

{ TBoldXMLOLWFloatLiteralStreamer }

function TBoldXMLOLWFloatLiteralStreamer.CreateObject: TObject;
begin
  result := TBoldOLWFloatLiteral.Create(0, 0);
end;

function TBoldXMLOLWFloatLiteralStreamer.GetStreamName: string;
begin
  result := OLWFloatLiteralStreamName;
end;

procedure TBoldXMLOLWFloatLiteralStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  FloatLiteral: TBoldOLWFloatLiteral;
begin
  inherited;
  FloatLiteral := obj as TBoldOLWFloatLiteral;
  FloatLiteral.fFloatValue := Node.ReadSubNodeFloat('FloatValue');
end;

procedure TBoldXMLOLWFloatLiteralStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  FloatLiteral: TBoldOLWFloatLiteral;
begin
  inherited;
  FloatLiteral := obj as TBoldOLWFloatLiteral;
  Node.WriteSubNodeFloat('FloatValue', FloatLiteral.fFloatValue);
end;


{ TBoldXMLOLWEnumLiteralStreamer }

function TBoldXMLOLWEnumLiteralStreamer.CreateObject: TObject;
begin
  result := TBoldOLWEnumLiteral.Create(0, '');
end;

function TBoldXMLOLWEnumLiteralStreamer.GetStreamName: string;
begin
  result := OLWEnumLiteralStreamName;
end;

procedure TBoldXMLOLWEnumLiteralStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  EnumLiteral: TBoldOLWEnumLiteral;
begin
  inherited;
  EnumLiteral := obj as TBoldOLWEnumLiteral;
  EnumLiteral.fName := Node.ReadSubNodeString('Name');
  EnumLiteral.fIntValue := Node.ReadSubNodeInteger('IntValue');
end;

procedure TBoldXMLOLWEnumLiteralStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  EnumLiteral: TBoldOLWEnumLiteral;
begin
  inherited;
  EnumLiteral := obj as TBoldOLWEnumLiteral;
  Node.WriteSubNodeString('Name', EnumLIteral.fName);
  Node.WriteSubNodeInteger('IntValue', EnumLIteral.fIntValue);
end;

{ TBoldXMLOLWNodeListStreamer }

function TBoldXMLOLWNodeListStreamer.CreateObject: TObject;
begin
  result := TBoldOLWNodeList.Create;
end;

function TBoldXMLOLWNodeListStreamer.GetStreamName: string;
begin
  result := OLWNodeListStreamName;
end;

procedure TBoldXMLOLWNodeListStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  OLWList: TBoldOLWNodeList;
  {$IFDEF OXML}
  aNodeEnumerator: TXMLChildNodeListEnumerator;
  aNode: PXMLNode;
  {$ELSE}
  aNodeList: IXMLDOMNodeList;
  aNode: IXMLDOMNode;
  {$ENDIF}
  aSubNode: TBoldXMLNode;
begin
  inherited;
  OLWList := Obj as TBoldOLWNodeList;
  {$IFDEF OXML}
  aNodeEnumerator := Node.XMLDomElement.ChildNodes.GetEnumerator;
  try
    while aNodeEnumerator.MoveNext do
    begin
      aNode := aNodeEnumerator.Current;
      aSubNode := Node.MakeNodeForElement(aNode);
      if aSubNode.Accessor = 'Node' then // do not localize
        OLWList.Add(aSubNode.ReadObject('') as TBoldOLWNode);
      aSubNode.Free;
    end;
  finally
    aNodeEnumerator.Free;
  end;
  {$ELSE}
  aNodeList := Node.XMLDomElement.childNodes;
  aNode := aNodeList.nextNode;
  while assigned(aNode) do
  begin
    aSubNode := Node.MakeNodeForElement(aNode as IXMLDOMElement);
    if aSubNode.Accessor = 'Node' then
      OLWList.Add(aSubNode.ReadObject('') as TBoldOLWNode);
    aSubNode.Free;
    aNode := aNodeList.nextNode;
  end;
  {$ENDIF}
end;

procedure TBoldXMLOLWNodeListStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  OLWList: TBoldOLWNodeList;
  i: Integer;
  aSubNode: TBoldXMLNode;
begin
  inherited;
  OLWList := Obj as TBoldOLWNodeList;
  for i := 0 to OLWList.Count-1 do
  begin
    aSubNode := Node.NewSubNode('Node');
    aSubNode.WriteObject('', OLWList[i]);
    aSubNode.Free;
  end;
end;

{ TBoldXMLOLWDateLiteralStreamer }

function TBoldXMLOLWDateLiteralStreamer.CreateObject: TObject;
begin
  result := TBoldOLWDateLiteral.Create(0, 0);
end;

function TBoldXMLOLWDateLiteralStreamer.GetStreamName: string;
begin
  result := OLWDateLIteralStreamName;
end;

procedure TBoldXMLOLWDateLiteralStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  DateLiteral: TBoldOLWDateLiteral;
begin
  inherited;
  DateLiteral := obj as TBoldOLWDateLiteral;
  DateLiteral.DateValue := Node.ReadSubNodeDate('DateValue');
end;


procedure TBoldXMLOLWDateLiteralStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  DateLiteral: TBoldOLWDateLiteral;
begin
  inherited;
  DateLiteral := obj as TBoldOLWDateLiteral;
  Node.WriteSubNodeDate('DateValue', DateLIteral.DateValue);
end;

{ TBoldXMLOLWTimeLiteralStreamer }

function TBoldXMLOLWTimeLiteralStreamer.CreateObject: TObject;
begin
  result := TBoldOLWTimeLiteral.Create(0, 0);
end;

function TBoldXMLOLWTimeLiteralStreamer.GetStreamName: string;
begin
  result := OLWTimeLIteralStreamName;
end;

procedure TBoldXMLOLWTimeLiteralStreamer.ReadObject(Obj: TObject;
  Node: TBoldXMLNode);
var
  TimeLiteral: TBoldOLWTimeLiteral;
begin
  inherited;
  TimeLiteral := obj as TBoldOLWTimeLiteral;
  TimeLiteral.TimeValue := Node.ReadSubNodeTime('TimeValue');
end;


procedure TBoldXMLOLWTimeLiteralStreamer.WriteObject(
  Obj: TBoldInterfacedObject; Node: TBoldXMLNode);
var
  TimeLiteral: TBoldOLWTimeLiteral;
begin
  inherited;
  TimeLiteral := obj as TBoldOLWTimeLiteral;
  Node.WriteSubNodeTime('TimeValue', TimeLIteral.TimeValue);
end;

{ TBoldOLWMomentLiteral }

constructor TBoldOLWMomentLiteral.Create(Position: integer;
  MomentValue: TDateTime);
begin
  inherited Create(Position);
  fMomentValue := MomentValue
end;

{ TBoldOLWDateLiteral }

procedure TBoldOLWDateLiteral.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWDateLiteral(self);
end;

function TBoldOLWDateLiteral.GetStreamName: string;
begin
  result := OLWDateLiteralStreamName;
end;

{ TBoldOLWTimeLiteral }

procedure TBoldOLWTimeLiteral.AcceptVisitor(V: TBoldOLWNodeVisitor);
begin
  inherited;
  v.VisitTBoldOLWTimeLiteral(self);
end;

function TBoldOLWTimeLiteral.GetStreamName: string;
begin
  result := OLWTimeLiteralStreamName;
end;

initialization
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOCLConditionStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWNodeListStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWTypeNodeStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWListCoercionStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWOperationStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWIterationStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWMemberStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWVariableBindingStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWVariableReferenceStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWStrLiteralStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWDateLiteralStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWTimeLiteralStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWFloatLiteralStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWIntLiteralStreamer.Create);
  TBoldXMLStreamerRegistry.MainStreamerRegistry.RegisterStreamer(TBoldXMLOLWEnumLiteralStreamer.Create);

end.
