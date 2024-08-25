
{ Global compiler directives }
{$include bold.inc}
unit BoldOclEvaluator;

interface

uses
  BoldSystem,
  BoldSystemRT,
  BoldElements,
  BoldAttributes,
  BoldSubscription,
  BoldOclClasses;

type
  TBoldOclEvaluatorVisitor = class(TBoldOclVisitor)
  private
    CurrentSubscriber: TBoldSubscriber;
    CurrentSystemTypeInfo: TBoldSystemTypeInfo;
    fStringType: TBoldAttributeTypeInfo;
    fIntegerType: TBoldAttributeTypeInfo;
    fTimeType: TBoldAttributeTypeInfo;
    fDateType: TBoldAttributeTypeInfo;
    fFloatType: TBoldAttributeTypeInfo;
    fBooleanType: TBoldAttributeTypeInfo;
    fTrueBool: TBABoolean;
    CurrentSystem: tBoldSystem;
    fResubscribeAll: Boolean;
    function MakeNewString: TBAString;
    function MakeNewDate: TBADate;
    function MakeNewTime: TBATime;
    function MakeNewInteger: TBAInteger;
    function MakeNewFloat: TBAFloat;
    function CreateNewMember(BoldType: TBoldElementTypeInfo): TBoldMember;
  public
    constructor Create(Subscriber: TBoldSubscriber; ResubscribeAll: Boolean; SystemTypeInfo: TBoldSystemTypeInfo; BoldSystem: TBoldSystem; TrueBool: TBABoolean; BooleanType, StringType, IntegerType, FloatType, DateType, TimeType: TBoldAttributeTypeInfo);
    procedure VisitTBoldOclListCoercion(N: TBoldOclListCoercion); override;
    procedure VisitTBoldOclMethod(N: TBoldOclmethod); override;

    procedure VisitTBoldOclOperation(N: TBoldOclOperation); override;
    procedure VisitTBoldOclIteration(N: TBoldOclIteration); override;
    procedure VisitTBoldOclMember(N: TBoldOclMember); override;
    procedure VisitTBoldOclVariableReference(N: TBoldOclVariableReference); override;
    procedure VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral); override;
    procedure VisitTBoldOclStrLiteral(N: TBoldOclStrLiteral); override;
    procedure VisitTBoldOclNumericLiteral(N: TBoldOclNumericLiteral); override;
    procedure VisitTBoldOclDateLiteral(N: TBoldOclDateLiteral); override;
    procedure VisitTBoldOclTimeLiteral(N: TBoldOclTimeLiteral); override;
    procedure VisitTBoldOclIntLiteral(N: TBoldOclIntLiteral); override;
    procedure VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLiteral); override;
    procedure SubScribeToElem(N: TBoldOclNode);
  end;

var
  OclUseTemporaryDummyValue: boolean = true;

implementation

uses
  Classes,
  SysUtils,

  BoldCoreConsts,
  BoldDefs,
  BoldOclError,
  BoldContainers,
  BoldBase;

constructor TBoldOclEvaluatorVisitor.Create(Subscriber: TBoldSubscriber; ResubscribeAll: Boolean; SystemTypeInfo: TBoldSystemTypeInfo;
  BoldSystem: TBoldSystem; TrueBool: TBABoolean; BooleanType, StringType, IntegerType, FloatType, DateType, TimeType: TBoldAttributeTypeInfo);
begin
  inherited Create;
  CurrentSubscriber := Subscriber;
  CurrentSystemTypeInfo := SystemTypeInfo;
  CurrentSystem := BoldSystem;
  fStringType := StringType;
  fIntegerType := IntegerType;
  fFloatType := FloatType;
  fDAteType := DateType;
  fTimeType := TimeType;
  fBooleanType := BooleanType;
  fTrueBool := TrueBool;
  fResubscribeAll := ResubscribeAll;
end;

function MapResubscribe(Resubscribe: Boolean): TBoldRequestedEvent; {$IFDEF BOLD_INLINE} inline; {$ENDIF}
begin
  if Resubscribe then
    Result := breReSubscribe
  else
    Result := breReEvaluate;
end;

type
  PBoldOCLSortData = ^TBoldOCLSortData;
  TBoldOCLSortData = record
    SortArg: TBoldElement;
    SortObj: TBoldElement;
  end;

  TBoldOCLSortArray = class(TBoldArray)
  private
    function Get(Index: Integer): TBoldOCLSortData;
    procedure Put(Index: Integer; Item: TBoldOCLSortData);
  protected
    function GetItemSize: Integer; override;
  public
    property Items[Index: Integer]: TBoldOCLSortData read Get write Put; default;
  end;

function TBoldOCLSortArray.Get(Index: Integer): TBoldOCLSortData;
begin
  inherited Get(Index,Result);
end;

function TBoldOCLSortArray.GetItemSize: Integer;
begin
  Result := SizeOf(TBoldOCLSortData);
end;

procedure TBoldOCLSortArray.Put(Index: Integer; Item: TBoldOCLSortData);
begin
  inherited Put(Index, Item);
end;

function BoldOCLInternalSortWrapper(Item1, Item2: Pointer): Integer;
begin
  Result := PBoldOCLSortData(Item1).SortArg.CompareTo(PBoldOCLSortData(Item2).SortArg);
end;

function BoldOCLInternalReverseSortWrapper(Item1, Item2: Pointer): Integer;
begin
  Result := - PBoldOCLSortData(Item1).SortArg.CompareTo(PBoldOCLSortData(Item2).SortArg);
end;

procedure Sortlist(Node: TBoldOclIteration; BoldList: TBoldList; SortKeyHolder: TBoldIndirectElement; Order: TBoldOclIteratorSpecifier);
var
  SortList: TBoldOCLSortArray;
  SortData: TBoldOCLSortData;
  i    : Integer;
  arglist: TBoldList;
  NewList: TBoldList;
begin
  case BoldList.Count of
    0: ;
    1: (node.Value as TBoldList).Add(BoldList[0]);
  else
    begin
      arglist := SortKeyHolder.Value as TBoldList;
      SortList := TBoldOCLSortArray.Create(BoldList.Count, []);
      try
        SortList.Count := BoldList.Count;
        for i := 0 to BoldList.Count - 1 do begin
          SortData.SortArg := ArgList[i];
          SortData.SortObj := BoldList[i];
          SortList[i] := SortData;
        end;

        case Order of
          OclOrderBy: SortList.Sort(BoldOCLInternalSortWrapper);
          OclOrderDescending: SortList.Sort(BoldOCLInternalReverseSortWrapper);
        end;

        NewList := node.Value as TBoldLIst;

        for i := 0 to BoldList.Count - 1 do begin
          SortData := SortList[i];
          NewList.Add(SortData.SortObj);
        end;
      finally
        Sortlist.Free;
      end;
    end;
  end;
end;

procedure TBoldOclEvaluatorVisitor.SubScribeToElem(N: TBoldOclNode);
begin

  if assigned(N.Value) and assigned(CurrentSubscriber) and not n.OwnsValue then
    N.Value.DefaultSubscribe(CurrentSubscriber, MapResubscribe(N.Resubscribe or fResubscribeAll))
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclListCoercion(N: TBoldOclListCoercion);
begin
  N.Child.AcceptVisitor(self);
  if assigned(n.child.value) then
    n.Child.value.GetAsList(n)
  else
    n.setreferenceValue(nil);
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclmethod(N: TBoldOclMethod);
begin
  VisitTBoldOclOperation(n);

{  n.MethodOf.AcceptVisitor(self);
  for i := 0 to N.Args.Count - 1 do begin
    N.Args[i].AcceptVisitor(self);
  end;

  case N.Symbol.NumberOfArgs of
    1: N.Symbol.Evaluate(N.MethodOf, nil, nil, nil, N);
    2: N.Symbol.Evaluate(N.MethodOf, N.Args[0], nil, nil, N);
    3: N.Symbol.Evaluate(N.MethodOf, N.Args[0], N.Args[1], nil, N);
    4: N.Symbol.Evaluate(N.MethodOf, N.Args[0], N.Args[1], N.Args[2], N)
    else
      ShowMessage('sorry, not more than 4 args implemented... blame Jonas');
  end;

  if assigned(n.value) and not assigned(n.value.Boldtype) then
    n.value.BoldType := n.ExpressionType;

  if n.OwnsValue then n.Value.MakeImmutable;
  SubScribeToElem(N);
  }
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclOperation(N: TBoldOclOperation);

  procedure ClearIfReferenced(ie: TBoldIndirectElement);
  begin
    if not ie.OwnsValue then
      ie.SetReferenceValue(nil);

  end;
var
  i                : Integer;
  backupResubscribeAll: Boolean;
  OperationParams: TBoldOclSymbolParameters;
  ArgI: TBoldOclNode;
begin
  case N.Symbol.GetShortCircuitType of
    csIf:
    begin
      backupResubscribeAll := fResubscribeAll;
      fResubscribeAll := True;
      N.Args[0].AcceptVisitor(self);
      fResubscribeAll := backupResubscribeAll;
      if assigned(n.args[0].value) and (N.Args[0].Value as TBABoolean).AsBoolean then
      begin
        N.Args[1].AcceptVisitor(self);
        ClearIfReferenced(N.Args[2]);
      end
      else
      begin
        N.Args[2].AcceptVisitor(self);
        ClearIfReferenced(N.Args[1]);
      end;
     end;

    csOr:
    begin
      fResubscribeAll := True;
      N.Args[0].AcceptVisitor(self);
      if (not assigned(n.args[0].value)) or not (N.Args[0].Value as TBABoolean).AsBoolean then
        N.Args[1].AcceptVisitor(self)
      else
        ClearIfReferenced(N.Args[1]);
    end;

    csAnd:
    begin
      fResubscribeAll := True;
      N.Args[0].AcceptVisitor(self);
      if assigned(n.args[0].value) and (N.Args[0].Value as TBABoolean).AsBoolean then
        N.Args[1].AcceptVisitor(self)
      else
        ClearIfReferenced(N.Args[1]);
    end;
    
    else
    begin
      for i := 0 to Length(N.Args) - 1 do
        N.Args[i].AcceptVisitor(self);
    end;
  end;

  try
    FillChar(OperationParams, SizeOf(OperationPArams), 0);
    try
      for i := 0 to n.Symbol.numberOfArgs-1 do
      begin
        Argi := n.args[i];
        if not assigned(Argi.value) and (Argi.BoldType is TBoldAttributeTypeInfo) and
           not (Argi.BoldType as TBoldAttributeTypeInfo).IsAbstract then
        begin
          if Argi.BoldType = fBooleanType then
            Argi.SetReferenceValue(fTrueBool)
          else
          begin
            if OclUseTemporaryDummyValue then
            begin
              Argi.SetOwnedValue(TBoldMemberFactory.CreateMemberFromBoldType(Argi.BoldType));
              Argi.HasTemporaryDummyValue := true;
            end
            else
              Argi.SetReferenceValue(nil);
          end;
        end
        else if Argi.value is TBoldObjectReference then
          OperationParams.values[i] := (Argi.value as TBoldObjectReference).BoldObject
        else
          OperationParams.values[i] := Argi.value;
        OperationParams.Nodes[i] := Argi;
      end;
      OperationParams.Result := N;
      OperationParams.SubScriber := CurrentSubscriber;
      OperationParams.System := CurrentSystem;
      OperationParams.SystemTypeInfo := CurrentSystemTypeInfo;
      n.Symbol.Evaluate(OperationParams);
      
      if OPerationParams.result.OwnsValue and assigned(OPerationParams.result.value) then
        OPerationParams.result.Value.MakeImmutable;

    finally
      for i := 0 to n.Symbol.numberOfArgs-1 do
      begin
        Argi := n.args[i];
        if Argi.HasTemporaryDummyValue then
        begin
          Argi.SetReferenceValue(nil);
          Argi.HasTemporaryDummyValue := false;
        end;
      end;
    end;
  except
    on e: EBoldOclRunTimeError do begin
      e.Position := N.Position;
      e.ErrorFixed := true;
      raise;
    end;
  end;

  if assigned(n.value) and not assigned(n.value.Boldtype) and not (n.Value is TBAValueSetValue) then // TBAValueSetValue does not have a type by design
    raise EBoldInternal.CreateFmt('Result of evaluation of operation %s has no type', [n.OperationName]);

  SubScribeToElem(N);
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclIteration(N: TBoldOclIteration);
var
  i: Integer;
  SortkeyHolder: TBoldOclNode;
  SortKeys: TBoldMEmberList;
  tempSortKey: TBoldMember;
  OperationParams: TBoldOclSymbolParameters;
  SortArgIsRTMember: Boolean;
  List: TBoldList;
{  UniqueList: TBoldList;
  ListType: TBoldListTypeInfo;
  UniqueValue: TBoldElement;
  }

begin
  OperationParams.subscriber := CurrentSubscriber;
  OperationParams.System := CurrentSystem;
  OperationParams.SystemTypeInfo := CurrentSystemTypeInfo;

  N.Args[0].AcceptVisitor(self);
  N.SetOwnedValue(CreateNewMember(n.BoldType));
  case N.IteratorSpecifier of
    OclUnique:
      (N.Value as TBABoolean).AsBoolean := true;
    OclForAll, OclExists: begin
      (N.Value as TBABoolean).AsBoolean := n.IteratorSpecifier = OclForAll;
    end;
    OclOrderBy, OclOrderDescending, OclSelect, OclReject, OclCollect: begin
      (n.Value as TBoldList).DuplicateMode := bldmAllow;
      if (n.Value is TBoldObjectList) then
        TBoldObjectList(n.Value).SubscribeToObjectsInList := false;
    end;

    else
      raise EBoldOclAbort.CreateFmt(boeUnKnownIterator,[N.Position]);
  end;

  if assigned(n.args[0].value) then begin
    OperationParams.values[0] := n.args[1].Value;
    OperationParams.nodes[0] := n.args[1];
    OperationParams.values[1] := n.LoopVar.value;
    OperationParams.nodes[1] := n.LoopVar;

    case N.IteratorSpecifier of
      OclOrderBy, OclOrderDescending:
      begin
        SortKeyHolder := TBoldOclNode.Create;
        try
          SortKeys := TBoldMemberList.Create;
          SortKeys.DuplicateMode := bldmAllow;
          SortArgIsRTMember := (N.Args[1] is TBoldOclMember) and Assigned((N.Args[1] as TBoldOclMember).RTInfo);
          SortKeys.CloneMembers := not SortArgIsRTMember;

          SortKeyHolder.SetOwnedvalue(SortKeys);
          OperationParams.Result := SortkeyHolder;
          List := N.Args[0].Value as TBoldList;
          List.EnsureRange(0, List.Count-1);
          for i := 0 to List.Count - 1 do begin
            N.LoopVar.SetReferenceValue(List[i]);
            N.Args[1].AcceptVisitor(self);
            OperationParams.values[0] := n.args[1].Value;
            OperationParams.values[1] := n.LoopVar.value;
            N.Symbol.Evaluate(OperationParams);
          end;
          Sortlist(N, List, SortKeyHolder, N.IteratorSpecifier);
          if not SortKeys.CloneMembers then
          begin
            for i := Sortkeys.Count-1 downto 0 do
            begin

              if not assigned(Sortkeys[i].OwningObject) then
              begin
                TempSortKey := SortKeys[i];
                SortKeys.RemoveByIndex(i);
                TempSortKey.Free;
              end;
            end;
          end;
        finally
          SortKeyHolder.Free;
        end
      end;
      {;
      OclUnique:
      begin
        ListType := CurrentSystemTypeInfo.ListTypeInfoByElement[n.args[1].BoldType];
        UniqueList := CreateNewMember(LIstType) as TBoldList;
        UniqueList.DuplicateMode := bldmAllow;
        OperationParams.Result := N;
        for i := 0 to (N.Args[0].Value as TBoldList).Count - 1 do begin
          n.LoopVar.BoldType := nil; // reset static boldtype
          N.LoopVar.SetReferenceValue((N.Args[0].Value as TBoldList)[i]);
          N.Args[1].AcceptVisitor(self);
          UniqueValue := n.args[1].value;
          if (UniqueValue is TBoldObjectReference) then
            UniqueValue := (UniqueValue as TBoldObjectReference).BoldObject;
          if UniqueList.Includes(UniqueValue) then
          begin
            (N.Value as TBABoolean).AsBoolean := false;
            break;
          end;
          UniqueList.Add(UniqueValue);
        end;
        UniqueList.Free;
      end
      }
      else
      begin
        OperationParams.Result := N;
        List := (N.Args[0].Value as TBoldList);
        List.EnsureRange(0, List.Count-1);

        for i := 0 to List.Count - 1 do begin
          n.LoopVar.BoldType := nil;
          N.LoopVar.SetReferenceValue(List[i]);
          N.Args[1].AcceptVisitor(self);
          OperationParams.values[0] := n.args[1].Value;
          OperationParams.values[1] := n.LoopVar.value;
          N.Symbol.Evaluate(OperationParams);
        end;
      end;
    end;
  end;
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclMember(N: TBoldOclMember);
var
  OldObjectList: TBoldObjectList;
  Obj, LoopObject: TBoldObject;
  i: integer;

  function RetrieveQualifiedSingle(Obj: TBoldObject): TBoldObject;
  var
    memberlist: TBoldMemberList;
    i: integer;
    Role: TBoldObjectList;
  begin
    MemberList := TBoldMemberList.Create;
    MemberList.CloneMembers := false;
    try
      for i := 0 to Length(n.Qualifier)-1 do
        MemberList.Add(n.Qualifier[i].value as TBoldMember);
      role := Obj.BoldMembers[n.MemberIndex] as TBoldObjectList;
      if assigned(CurrentSubscriber) then
        Role.DefaultSubscribe(CurrentSubscriber, breResubscribe);

      result := Role.GetByIndexAndSubscribe(MemberList, CurrentSubscriber);
    finally
      MemberList.Free;
    end;
  end;



  function retrieveAttribute: TBoldMemberList;
  var
    i : Integer;
    index: Integer;
  begin
    Result := CreateNewMember(n.BoldType) as TBoldMemberList;
    Result.DuplicateMode := bldmAllow;
    Result.CloneMembers := false;
    index := N.memberindex;
    Result.Capacity := OldObjectList.Count;
    for i := 0 to OldObjectList.Count - 1 do begin
      LoopObject := OldObjectList[i];
      Result.Add(LoopObject.BoldMembers[index]);
      if assigned(CurrentSubscriber) then
        LoopObject.BoldMembers[index].DefaultSubscribe(CurrentSubscriber, MapResubscribe(N.Resubscribe or fResubscribeAll));
    end;
  end;

  function retrieveClass: TBoldObjectList;
  var
    ClassType: TBoldClasstypeInfo;
  begin
    ClassType := (N.BoldType as TBoldListTypeInfo).ListElementTypeInfo as TBoldClasstypeInfo;
    Result := CurrentSystem.Classes[ClassType.TopSortedIndex];
  end;

  function retrieveMultiLink: TBoldObjectList;
  var
    i : Integer;
    roleList: TBoldObjectList;
    tempSystem: TBoldSystem;
  begin
    if (OldObjectList.count > 0) then
    begin
      if assigned(CurrentSystem) then
        tempSystem := currentSystem
      else
        TempSystem := OldObjectList[0].BoldSystem;
      tempSystem.FetchLinksWithObjects(OldObjectList, n.MemberName);
    end;
    Result := CreateNewMember(n.BoldType) as TBoldObjectList;
    Result.DuplicateMode := bldmAllow;
    if assigned(n.Qualifier) then
      for i := 0 to Length(N.qualifier) - 1 do
        N.Qualifier[i].AcceptVisitor(self);

    for i := 0 to OldObjectList.Count - 1 do
    begin
      LoopObject := OldObjectList[i];
      if assigned(n.Qualifier) then
        result.add(RetrieveQualifiedSingle(LoopObject))
      else
      begin
        roleList := LoopObject.BoldMembers[N.memberindex] as TBoldObjectList;
        if assigned(CurrentSubscriber) then
          roleList.DefaultSubscribe(CurrentSubscriber, MapResubscribe(N.Resubscribe));
        Result.AddList(roleList);
      end;
    end;
  end;

  function RetrieveSingleLink: TBoldObjectList;
  var
    i: Integer;
    Link: TBoldObjectReference;
    tempSystem: TBoldSystem;
  begin
    if (OldObjectList.count > 0) then
    begin
      if assigned(CurrentSystem) then
        tempSystem := currentSystem
      else
        TempSystem := OldObjectList[0].BoldSystem;
      tempSystem.FetchLinksWithObjects(OldObjectList, n.MemberName);
    end;
    Result := CreateNewMember(n.BoldType) as TBoldObjectList;
    Result.DuplicateMode := bldmAllow;

    for i := 0 to OldObjectList.Count - 1 do begin
      if OldObjectList.Locators[i].BoldObjectID.NonExisting then
        continue;
      LoopObject := OldObjectList[i];
      Link := LoopObject.BoldMembers[N.memberindex] as TBoldObjectReference;
      if assigned(CurrentSubscriber) then
        Link.DefaultSubscribe(CurrentSubscriber, MapResubscribe(N.Resubscribe));
      if assigned(link.BoldObject) then
        Result.Add(Link.BoldObject);
    end;
  end;

begin
  N.MemberOf.AcceptVisitor(self);

  if assigned(n.MemberOf.value) then begin // and not ((n.MemberOf.Value is TBoldObjectReference) and (n.TBoldObjectReference(MemberOf.Value).BoldObject = nil))
    case N.MemberOf.Value.BoldType.BoldValueType of
      bvtSystem: N.SetReferenceValue(retrieveClass);
      bvtClass: begin
        obj := nil;
        if N.MemberOf.Value is TBoldObjectReference then
          Obj := TBoldObjectReference(N.MemberOf.Value).BoldObject
        else if N.MemberOf.Value is TBoldObject then
          Obj := TBoldObject(N.MemberOf.Value)
        else if assigned(n.Memberof.Value) then
          raise EBold.CreateFmt(sUnknownTypeOfMemberOf,[N.MemberOf.Value.ClassName]);

        if assigned(Obj) and assigned(Obj.BoldObjectLocator) then
        begin
          if assigned(n.Qualifier) then
          begin
            for i := 0 to Length(N.qualifier) - 1 do
              N.Qualifier[i].AcceptVisitor(self);
            n.SetReferenceValue(RetrieveQualifiedSingle(obj));
          end else
            N.SetReferenceValue(Obj.BoldMembers[N.memberindex])
        end else
        begin
          if n.Boldtype.BoldValueType = bvtList then
            n.SetOwnedValue(CreateNewMember(n.Boldtype))
          else
            n.SetReferenceValue(nil);
        end;

      end;

      bvtList: begin
        OldObjectList := N.MemberOf.Value as TBoldObjectList;
        OldObjectList.EnsureObjects;
        case N.MemberType.BoldValueType of
          bvtAttr: N.SetOwnedValue(retrieveAttribute);
          bvtClass: N.SetOwnedValue(RetrieveSingleLink);
          bvtList: N.SetOwnedValue(retrieveMultiLink)
        else
          raise EBold.CreateFmt(sUnknownTypeOfMember,[N.MemberType.ClassName]);
        end;
      end;
    else
      raise EBold.CreateFmt(sUnknownTypeOfMember,[N.MemberOf.Value.BoldType.ClassName]);
    end;

    SubScribeToElem(N);
  end else
    n.SetReferenceValue(nil);
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclVariableReference(N: TBoldOclVariableReference);
var
  VariableValue: TBoldElement;
begin
  VariableValue := N.VariableBinding.Value;
  N.SetReferenceValue(VariableValue);
  if assigned(currentSubscriber) and Assigned(VariableValue) then
  begin
    if not VariableValue.Mutable then
      N.IsConstant := true;
    if not N.IsConstant then
      VariableValue.DefaultSubscribe(CurrentSubscriber, MapResubscribe(N.Resubscribe or fResubscribeAll));
  end;
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclEnumLiteral(N: TBoldOclEnumLiteral);
begin
  if not assigned(n.value) then
    n.SetReferenceValue(CurrentSystemTypeInfo.FindValueSetByName(N.Name));
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclCollectionLiteral(N: TBoldOclCollectionLiteral);
var
  i       : Integer;
  tempInteger: TBAInteger;
  BoldList: TBoldList;
begin
  if not n.OwnsValue then
    n.SetOwnedValue(CreateNewMember(n.BoldType));

  BoldList := TBoldList(n.Value);
  BoldList.Clear;

  if N.IsRange then begin
    N.RangeStart.AcceptVisitor(self);
    N.RangeStop.AcceptVisitor(self);
    if not assigned(N.RangeStart.Value) then
      raise EBoldOclRunTimeError.CreateFmt(boertRangeNotAssigned, [n.RangeStart.Position]);
    if not assigned(N.RangeStop.Value) then
      raise EBoldOclRunTimeError.CreateFmt(boertRangeNotAssigned, [n.RangeStart.Position]);

    tempInteger := MakeNewInteger;
    for i := (N.RangeStart.Value as TBAInteger).AsInteger to (N.RangeStop.Value as TBAInteger).AsInteger do begin
      TempInteger.AsInteger := i;
      BoldList.Add(tempInteger);
    end;
  end else begin
    for i := 0 to Length(N.Elements) - 1 do begin
      N.Elements[i].AcceptVisitor(self);
      BoldList.Add(N.Elements[i].Value);
    end;
  end;
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclNumericLiteral(
  N: TBoldOclNumericLiteral);
begin
  if not assigned(n.value) then
  begin
    n.SetOwnedValue(MakeNewFloat);
   (n.Value as TBAFloat).AsFloat := n.FloatValue;
    n.Value.MakeImmutable;
  end;
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclIntLiteral(
  N: TBoldOclIntLiteral);
begin
  if not assigned(n.value) then
  begin
    n.SetOwnedValue(MakeNewInteger);
   (n.Value as TBAInteger).AsInteger := n.IntValue;
    n.Value.MakeImmutable;
  end;
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclStrLiteral(
  N: TBoldOclStrLiteral);
begin
  if not assigned(n.value) then
  begin
    n.SetOwnedValue(MakeNewString);
   (n.Value as TBAString).AsString := n.StrValue;
    n.Value.MakeImmutable;
  end;
end;

function TBoldOclEvaluatorVisitor.MakeNewString: TBAString;
begin
  result := CreateNewMember(fStringType) as TBAString;
end;

function TBoldOclEvaluatorVisitor.MakeNewFloat: TBAFloat;
begin
  result := CreateNewMember(fFloatType) as TBAFloat;
end;

function TBoldOclEvaluatorVisitor.MakeNewInteger: TBAInteger;
begin
  result := CreateNewMember(fIntegerType) as TBAInteger;
end;

function TBoldOclEvaluatorVisitor.CreateNewMember(BoldType: TBoldElementTypeInfo): TBoldMember;
begin
  result := TBoldMemberFactory.CreateMemberFromBoldType(BoldType);
  if (result is TBoldObjectList) then
     TBoldObjectList(result).SubscribeToObjectsInList := false;
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclDateLiteral(
  N: TBoldOclDateLiteral);
begin
  if not assigned(n.value) then
  begin
    n.SetOwnedValue(MakeNewDate);
    (n.Value as TBADate).AsDate := n.DateValue;
    n.value.MakeImmutable
  end;
end;

procedure TBoldOclEvaluatorVisitor.VisitTBoldOclTimeLiteral(
  N: TBoldOclTimeLiteral);
begin
  if not assigned(n.value) then
  begin
    n.SetOwnedValue(MakeNewTime);
    (n.Value as TBATime).AsTime := n.TimeValue;
    n.value.MakeImmutable
  end;
end;

function TBoldOclEvaluatorVisitor.MakeNewDate: TBADate;
begin
  result := CreateNewMember(fDateType) as TBADate;
end;

function TBoldOclEvaluatorVisitor.MakeNewTime: TBATime;
begin
  result := CreateNewMember(fTimeType) as TBATime;
end;

end.
