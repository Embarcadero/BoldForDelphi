
{ Global compiler directives }
{$include bold.inc}
unit BoldAbstractPropertyEditors;

interface

uses
  Windows,
  Graphics,
  DesignEditors;

type
  { forward declarations }
  TBoldPropertyEditor = class;
  TBoldMethodProperty = class;
  TBoldStringProperty = class;
  TBoldComponentProperty = class;
  TBoldClassProperty = class;
  TBoldIntegerProperty = class;
  TBoldComponentPropertyIndicateMissing = class;
  TModifyingMethodProperty = class;
  TBoldOTAModifyingMethodProperty = class;
  TBoldOneLinerWithEvalMethodProperty = class;

  // NOTE: All property editors used in the products shall derive from a
  // TBold<type>Property. All TBold<type>Property classes shall have the same interface.
  // That means changes should be syncronized over all these classes


  { TBoldPropertyEditor }
  TBoldPropertyEditor = class(TPropertyEditor)
  protected
    function DrawTextInBold: boolean; virtual;
    function IsValid: boolean; virtual;
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); {$IFDEF BOLD_DELPHI5} override; {$ENDIF}
  end;

  { TBoldMethodProperty }
  TBoldMethodProperty = class(TMethodProperty)
  protected
    function DrawTextInBold: boolean; virtual;
    function IsValid: boolean; virtual;
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); {$IFDEF BOLD_DELPHI5} override; {$ENDIF}
  end;

  { TBoldStringProperty }
  TBoldStringProperty = class(TStringProperty)
  protected
    function DrawTextInBold: boolean; virtual;
    function IsValid: boolean; virtual;
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); {$IFDEF BOLD_DELPHI5} override; {$ENDIF}
  end;

  { TBoldComponentProperty }
  TBoldComponentProperty = class(TComponentProperty)
  protected
    function DrawTextInBold: boolean; virtual;
    function IsValid: boolean; virtual;
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); {$IFDEF BOLD_DELPHI5} override; {$ENDIF}
  end;

  { TBoldClassProperty }
  TBoldClassProperty = class(TClassProperty)
  protected
    function DrawTextInBold: boolean; virtual;
    function IsValid: boolean; virtual;
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); {$IFDEF BOLD_DELPHI5} override; {$ENDIF}
  end;

  { TBoldIntegerProperty }
  TBoldIntegerProperty = class(TIntegerProperty)
  protected
    function DrawTextInBold: boolean; virtual;
    function IsValid: boolean; virtual;
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean); {$IFDEF BOLD_DELPHI5} override; {$ENDIF}
  end;

  { TBoldComponentPropertyIndicateMissing }
  TBoldComponentPropertyIndicateMissing = class(TBoldComponentProperty)
  protected
    function IsValid: boolean; override;
  end;

  { Source code altering property editors }
  { TModifyingMethodProperty }
  TModifyingMethodProperty = class(TBoldMethodProperty)
  protected
    procedure InsertText(const s: string); virtual;
  public
    procedure Edit; override;
    procedure InsertVariables; virtual;
    procedure InsertImplementation; virtual;
    function ImplementationTextToInsert: string; virtual;
    function VariableDefinitionTextToInsert: string; virtual;
    procedure ReposCursor(DeltaLines, ColPos: integer);
    function GetDeltaLines: integer; virtual;
    function GetColPos: integer; virtual;
    function ConfirmAdd: boolean; virtual;
    property DeltaLines: integer read GetDeltaLines;
    property ColPos: integer read GetColPos;
  end;

  //Locates text via TIMOduleinterface
  // Finds position via line counting
  { TBoldMethodProperty }
  TBoldOTAModifyingMethodProperty = class(TModifyingMethodProperty)
  public
    procedure InsertText(const s: string); override;
  end;

  { TBoldOneLinerWithEvalMethodProperty }
  TBoldOneLinerWithEvalMethodProperty = class(TBoldOTAModifyingMethodProperty)
  public
    function GetDeltaLines: integer; override;
    function GetColPos: integer; override;
  end;

implementation

uses
  Classes,
  TypInfo,
  ToolsAPI,
  SysUtils,
  BoldDefsDT;

const
  INVALIDCOLOR = clRed;

{ TModifyingMethodProperty }
procedure TModifyingMethodProperty.Edit;
var
  NewMethod: boolean;
begin
  NewMethod := not Designer.MethodExists(GetFormMethodName);
  inherited;
  if NewMethod and ConfirmAdd then
  begin
    InsertVariables;
    InsertImplementation;
    ReposCursor(DeltaLines, ColPos);
  end;
end;

function TModifyingMethodProperty.ConfirmAdd: boolean;
begin
  Result := True;
end;

function TModifyingMethodProperty.VariableDefinitionTextToInsert: string;
begin
  result := '';
end;

function TModifyingMethodProperty.ImplementationTextToInsert: string;
begin
  result := '';
end;

procedure TModifyingMethodProperty.InsertText(const s: string);
begin
end;

procedure TModifyingMethodProperty.InsertVariables;
var
  s: string;
begin
  s := VariableDefinitionTextToInsert;
  if s = '' then
    exit;
  ReposCursor(-1, 0);
  InsertText(s);
  ReposCursor(2, 0);
end;

procedure TModifyingMethodProperty.InsertImplementation;
begin
//  ReposCursor(-1, 0);
  InsertText(ImplementationTextToInsert);
end;

procedure TModifyingMethodProperty.ReposCursor(DeltaLines, ColPos: integer);
var
  ColDelta: integer;
  EditorServices: IOTAEditorServices;
  TopView: IOTAEditView;
begin
  if Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
  begin
    TopView := EditorServices.TopBuffer.TopView;
    TopView.Position.MoveBOL;
    if ColPos > 0 then
      ColDelta := ColPos - 1
    else
      ColDelta := 0;
    TopView.Position.MoveRelative(DeltaLines, ColDelta);
  end;
end;

function TModifyingMethodProperty.GetDeltaLines: integer;
begin
  Result := 0;
end;

function TModifyingMethodProperty.GetColPos: integer;
begin
  Result := 0;
end;

{ TBoldMethodProperty }

procedure TBoldOTAModifyingMethodProperty.InsertText(const s: string);
var
  CurPos: integer;
  EditorServices: IOTAEditorServices;
  Writer: IOTAEditWriter;
  EditPos: TOTAEditpos;
  CharPos: TOTACharpos;
begin
  if (s <> '') and Supports(BorlandIDEServices, IOTAEditorServices, EditorServices) then
  begin
    EditPos := EditorServices.TopBuffer.TopView.CursorPos;
    EditorServices.TopBuffer.TopView.ConvertPos(True, EditPos, CharPos);
    CurPos := EditorServices.TopBuffer.TopView.CharPosToPos(CharPos);
    Writer := EditorServices.TopBuffer.CreateUndoableWriter;
    Writer.CopyTo(CurPos);
    Writer.Insert(PAnsiChar({$IFDEF BOLD_UNICODE}AnsiString{$ENDIF}(s)));
  end;
end;

{ TBoldMethodProperty }

function TBoldMethodProperty.IsValid: boolean;
begin
  Result := True;
end;

function TBoldMethodProperty.DrawTextInBold: boolean;
begin
  Result := False;
end;

procedure TBoldMethodProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if not isValid then
    aCanvas.Font.Color := INVALIDCOLOR;

  if aSelected and DrawTextInBold then
    aCanvas.Font.Style := aCanvas.Font.Style + [fsBold];

  inherited;
end;

{ TBoldOneLinerWithEvalMethodProperty }

function TBoldOneLinerWithEvalMethodProperty.GetDeltaLines: integer;
begin
  Result := 0;
end;

function TBoldOneLinerWithEvalMethodProperty.GetColPos: integer;
begin
  Result := Pos(BOLDSYM_QUOTECHAR, ImplementationTextToInsert)+1;
end;

{ TBoldStringProperty }

function TBoldStringProperty.DrawTextInBold: boolean;
begin
  Result := False;
end;

function TBoldStringProperty.IsValid: boolean;
begin
  Result := True;
end;

procedure TBoldStringProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if not isValid then
    aCanvas.Font.Color := INVALIDCOLOR;

  if aSelected and DrawTextInBold then
    aCanvas.Font.Style := aCanvas.Font.Style + [fsBold];

  inherited;
end;

{ TBoldComponentProperty }

function TBoldComponentProperty.DrawTextInBold: boolean;
begin
  Result := False;
end;

function TBoldComponentProperty.IsValid: boolean;
begin
  Result := True;
end;

procedure TBoldComponentProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if not isValid then
    aCanvas.Font.Color := INVALIDCOLOR;

  if aSelected and DrawTextInBold then
    aCanvas.Font.Style := aCanvas.Font.Style + [fsBold];

  inherited;
end;

{ TBoldClassProperty }

function TBoldClassProperty.DrawTextInBold: boolean;
begin
  Result := False;
end;

function TBoldClassProperty.IsValid: boolean;
begin
  Result := True;
end;

procedure TBoldClassProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if not isValid then
    aCanvas.Font.Color := INVALIDCOLOR;

  if aSelected and DrawTextInBold then
    aCanvas.Font.Style := aCanvas.Font.Style + [fsBold];

  inherited;
end;

{ TBoldIntegerProperty }

function TBoldIntegerProperty.DrawTextInBold: boolean;
begin
  Result := False;
end;

function TBoldIntegerProperty.IsValid: boolean;
begin
  Result := True;
end;

procedure TBoldIntegerProperty.PropDrawName(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if not isValid then
    aCanvas.Font.Color := INVALIDCOLOR;

  if aSelected and DrawTextInBold then
    aCanvas.Font.Style := aCanvas.Font.Style + [fsBold];

  inherited;
end;

{ TBoldPropertyEditor }

function TBoldPropertyEditor.DrawTextInBold: boolean;
begin
  Result := False;
end;

function TBoldPropertyEditor.IsValid: boolean;
begin
  Result := True;
end;

procedure TBoldPropertyEditor.PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
begin
  inherited;
  if not isValid then
    aCanvas.Font.Color := INVALIDCOLOR;

  if aSelected and DrawTextInBold then
    aCanvas.Font.Style := aCanvas.Font.Style + [fsBold];

  inherited;
end;

{ TBoldComponentPropertyIndicateMissing }

function TBoldComponentPropertyIndicateMissing.IsValid: boolean;
var
  SelectedPersistent: TPersistent;
  PropertyValue: TObject;
  i: integer;
begin
  Result := True;
  for i := 0 to PropCount - 1 do
  begin
    SelectedPersistent := GetComponent(i);
    PropertyValue := GetObjectProp(SelectedPersistent, GetName);
    Result := Result and Assigned(PropertyValue);
  end;
end;

end.
