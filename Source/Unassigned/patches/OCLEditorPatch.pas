
// (c) Boldsoft 2002 - all rights reserved
unit OCLEditorPatch;

// Fixes the problem that the colon after a class name is erronously added to the
// result text. This only happens if ShowTypes is checked.
// Code created for r3.1.2.0. Problem solved in base line.
//  -- jeho
interface

implementation

uses
  Classes,
  SysUtils,
  BoldOCLPropEditor,
  Forms;

type
  TOCLEditorPatch = class
  private
    SavedDblClick: TNotifyEvent;
    procedure ActiveFormChange(Sender: TObject);
    procedure NewDblClick(Sender: TObject);
  public
    constructor Create;
  end;

var
  _OCLEditorPatch: TOCLEditorPatch = nil;

{ TOCLEditorPatch }

procedure TOCLEditorPatch.ActiveFormChange(Sender: TObject);
var
  p: TBoldOCLPropEditForm;
begin
  // Are we activating the OCL property editor?
  if Screen.ActiveForm is TBoldOclPropEditForm then
  begin
    p := Screen.ActiveForm as TBoldOclPropEditForm;
    if p.Tag <> 2 then
    begin
      SavedDblClick := p.SelectBox.OnDblClick;
      p.SelectBox.OnDblClick := NewDblClick;
      // Set the tag to indicate we've fixed this form instance
      p.Tag := 2;
    end;
  end;
end;

constructor TOCLEditorPatch.Create;
begin
  Screen.OnActiveFormChange := ActiveFormChange;
end;

procedure TOCLEditorPatch.NewDblClick(Sender: TObject);
var
  p: TBoldOCLPropEditForm;
  s: string;
begin
  // Existing code
  SavedDblClick(Sender);
  // Remove colon from result text
  if Screen.ActiveForm is TBoldOclPropEditForm then
  begin
    p := Screen.ActiveForm as TBoldOclPropEditForm;
    s := p.EditMemo.Text;
    if Pos(':', s) = Length(s) then
      s := Copy(s, 1, Length(s) - 1);
    p.EditMemo.Text := s;
  end;
end;

initialization
  _OCLEditorPatch := TOCLEditorPatch.Create;

finalization
  FreeAndNil(_OCLEditorPatch);

end.
