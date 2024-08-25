{ Global compiler directives }
{$include bold.inc}
unit BoldAttributeWideString;

interface

uses
  BoldDefs,
  BoldSubscription,
  BoldDomainElement,
  BoldSystem,
  BoldElements,
  BoldValueInterfaces,
  BoldWideStringInterface;

type
  { The following class implements the WideString attribute, allowing code and }
  { GUI controls to interact with Unicode strings }
  TBAWideString = class(TBoldAttribute)
  private
    FValue: WideString;
    procedure SetContent(Newvalue: WideString);
    procedure SetDataValue(Newvalue: WideString);
    function GetContentAsWideString: WideString;
    procedure SetContentAsWideString(NewValue: WideString);
    function GetWideStringRepresentation(Representation: TBoldRepresentation): WideString;
    procedure SetWideStringRepresentation(Representation: TBoldRepresentation; const Value: WideString);
  protected
    procedure AssignContentValue(const Source: IBoldValue); override;
    procedure FreeContent; override;
    function GetStringRepresentation(Representation: TBoldRepresentation): string; override;
    procedure SetStringRepresentation(Representation: TBoldRepresentation; const Value: string); override;
    function MaySetValue(NewValue: WideString; Subscriber: TBoldSubscriber): Boolean;
    //function ProxyClass: TBoldMember_ProxyClass; override;
  public
    procedure Assign(Source: TBoldElement); override;
    procedure AssignValue(const Source: IBoldValue); override;
    function CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer; override;
    function ValidateWideString(Value: WideString; Representation: TBoldRepresentation): Boolean; 
    function ValidateString(const Value: String; Representation: TBoldRepresentation): Boolean; override;
    function CanSetValue(NewValue: WideString; Subscriber: TBoldSubscriber): Boolean;
    function ProxyInterface(const IId: TGUID; Mode: TBoldDomainElementProxyMode; out Obj): Boolean; override;
    procedure SetEmptyValue; override;
    property AsWideString: WideString index brDefault read GetWideStringRepresentation write SetWideStringRepresentation;
  end;

  { The following class implements a proxy for a WideString attribute to support
    IBoldWideStringContent, a part of the ValueSpace-interface mechanism used for
    objectspace transactions, persistence and other things }
    
  { TBAWideString_Proxy }
  TBAWideString_Proxy = class(TBoldAttribute_Proxy, IBoldWideStringContent)
  private
    function GetProxedWideString: TBAWideString;
  protected
    property ProxedWideString: TBAWideString read GetProxedWideString implements IBoldWideStringContent;
  end;

implementation

uses
  { RTL / VCL }
  Windows,
  SysUtils,
  { Bold }
  BoldMemberTypeDictionary;

{******************************************************************************}
{* TBAWideString                                                              *}
{******************************************************************************}

{* Proxy routines *************************************************************}

(*
{ The following method returns a class reference to a WideString proxy class }
{ thus allowing Bold to instantiate it }
function TBAWideString.ProxyClass: TBoldMember_ProxyClass;
begin
  result := TBAWideString_Proxy;
end;
*)

function TBAWideString.ProxyInterface(const IId: TGUID;
  Mode: TBoldDomainElementProxyMode; out Obj): Boolean;
begin
  if IsEqualGuid(IID, IBoldWideStringContent) then
    Result := RetrieveProxyInterface(IID, Mode, obj, 'IBoldWideStringContent')
  else
    result := inherited ProxyInterface(IID, Mode, Obj);
end;


{* Content management *********************************************************}

procedure TBAWideString.SetContentAsWideString(NewValue: WideString);
begin
  SetContent(NewValue);
end;

function TBAWideString.GetContentAsWideString: WideString;
begin
  result := FValue;
end;

procedure TBAWideString.SetEmptyValue;
begin
  inherited;
  FValue := '';
end;

procedure TBAWideString.FreeContent;
begin
  inherited;
  FValue := '';
end;

{* Public get/set methods *****************************************************}

procedure TBAWideString.SetStringRepresentation(Representation: TBoldRepresentation; const Value: string);
begin
  SetWideStringRepresentation(Representation, Value);
end;

function TBAWideString.GetStringRepresentation(Representation: TBoldRepresentation): string;
begin
  result := GetWideStringRepresentation(representation)
end;

{* Validation routines ********************************************************}

function TBAWideString.ValidateString(const Value: string; Representation: TBoldRepresentation): Boolean;
begin
  Result := ValidateWideString(Value, Representation);
end;

function TBAWideString.CanSetValue(NewValue: Widestring; Subscriber: TBoldSubscriber): Boolean;
begin
  Result := MaySetValue(NewValue, Subscriber) {$IFNDEF BOLD_NO_QUERIES}and
            SendQuery(bqMaySetValue, [NewValue], Subscriber){$ENDIF};
end;

{* Assignment routines ********************************************************}

procedure TBAWideString.Assign(Source: TBoldElement);
begin
  if Source is TBAWideString then
  begin
    if TBAWideString(Source).IsNull then
      SetToNull
    else
      AsWideString := TBAWideString(Source).AsWideString;
  end
  else
    inherited;
end;

procedure TBAWideString.AssignContentValue(const Source: IBoldValue);
var
  s: IBoldWideStringContent;
begin
  if not assigned(source) and CanSetToNull(nil) then
  begin
    SetContentToNull;
    FreeContent;
  end
  else if not assigned(source) then
    SetEmptyValue
  else if source.QueryInterface(IBoldWideStringContent, S) = S_OK then
  begin
    if s.IsNull then
    begin
      SetContentToNull;
      FreeContent;
    end
    else
      SetContent(s.AsWideString);
  end;
end;

procedure TBAWideString.AssignValue(const Source: IBoldValue);
var
  sw: IBoldWideStringContent;
begin
  if source.QueryInterface(IBoldWideStringContent, sw) = S_OK then
  begin
    if sw.IsNull then
      SetContentToNull
    else
      SetDataValue(sw.AsWideString);
  end
  else
    raise EBold.CreateFmt('%s.AssignValue: unknown type of source', [classname]);
end;

{* Comparison *****************************************************************}

function TBAWideString.CompareToAs(CompType: TBoldCompareType; BoldElement: TBoldElement): Integer;
var
  CompareString: TBAWideString;
begin
  if BoldElement is TBAWideString then
  begin
    CompareString := TBAWideString(BoldElement);
    if EitherIsNull(Self, CompareString) then
      Result := NullSmallest(BoldElement)
    else
      case CompType of
        ctDefault, ctCaseInsensitive:
          Result := WideCompareText(AsWideString, CompareString.AsWideString);
        ctAsString, ctCaseSensitive:
          Result := WideCompareStr(AsWideString, CompareString.AsWideString);
      else
        Result := inherited CompareToAs(CompType, BoldElement);
      end
  end
  else
    Result := inherited CompareToAs(CompType, BoldElement);
end;

{******************************************************************************}
{* TBACoordinate_Proxy                                                        *}
{******************************************************************************}

function TBAWideString_Proxy.GetProxedWideString: TBAWideString;
begin
  Result := ProxedMember as TBAWideString;
  //Result := ProxedElement as TBAWideString;
end;

function TBAWideString.MaySetValue(NewValue: WideString; Subscriber: TBoldSubscriber): Boolean;
begin
  result := true;
end;

function TBAWideString.GetWideStringRepresentation(Representation: TBoldRepresentation): WideString;
begin
  BoldClearLastFailure;
  if not CanRead(nil) then
    BoldRaiseLastFailure(self, 'GetWideStringRepresentation', '');
  case Representation of
    brDefault: 
    begin
      if IsNull then {IsNull ensures current}
        Result := ''
      else
        Result := FValue;
    end
    else
      raise EBold.CreateFmt('%s.GetWideStringRepresentation: Unsupported representation %d', [Representation]);
  end;
end;

procedure TBAWideString.SetWideStringRepresentation(Representation: TBoldRepresentation; const Value: WideString);
begin
  if not ValidateWideString(Value, Representation) then
    BoldRaiseLastFailure(self, 'SetWideStringRepresentation', 'String validation failed');

  if Representation = brDefault then
    SetDataValue(Value)
  else
    raise EBold.CreateFmt('%s.SetWideStringRepresentation: Unsupported representation %d', [Representation]);
end;

procedure TBAWideString.SetDataValue(Newvalue: WideString);
begin
  BoldClearLastFailure;
  if not CanSetValue(NewValue, nil) then
    BoldRaiseLastFailure(self, 'SetDataValue', '');

  if IsNull or (FValue <> NewValue) then
  begin
    if not StartModify then
      BoldRaiseLastFailure(self, 'SetDataValue', '');
    try
      SetContent(NewValue);
      EndModify;
    except
      FailModify;
      raise;
    end;
  end;
end;

function TBAWideString.ValidateWideString(Value: WideString; Representation: TBoldRepresentation): Boolean;
begin
  result := true;
end;

procedure TBAWideString.SetContent(Newvalue: WideString);
begin
  if (BoldPersistenceState = bvpsInvalid) or
     ContentIsNull or (FValue <> NewValue) then
  begin
    PreChange;
    FValue := NewValue;
    SetToNonNull;
    Changed(beValueChanged, [NewValue]);
  end;
end;

initialization
  BoldmemberTypes.AddMemberTypeDescriptor(TBAWideString, alConcrete);
  
finalization
  if BoldMemberTypesAssigned then
    BoldMemberTypes.RemoveDescriptorByClass(TBAWideString);
end.