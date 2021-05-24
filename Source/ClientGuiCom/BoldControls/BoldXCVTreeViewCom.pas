
{ Global compiler directives }
{$include bold.inc}
unit BoldXCVTreeViewCom;

interface

uses
  Messages,
  Classes,
  BoldTreeViewCom;

type

  TBoldXCVTreeViewCom = class(TBoldTreeViewCom)
  private
    fOnCut: TNotifyEvent;
    fOnCopy: TNotifyEvent;
    fOnPaste: TNotifyEvent;
    procedure PassToEditor(Message: TMessage);
  protected
    procedure Cut; virtual;
    procedure Copy; virtual;
    procedure Paste; virtual;
    procedure WMCut(var Message: TMessage); message WM_CUT;
    procedure WMCopy(var Message: TMessage); message WM_COPY;
    procedure WMPaste(var Message: TMessage); message WM_PASTE;
  public
  published
    property OnCut: TNotifyEvent read fOnCut write fOnCut;
    property OnCopy: TNotifyEvent read fOnCopy write fOnCopy;
    property OnPaste: TNotifyEvent read fOnPaste write fOnPaste;
  end;

implementation

uses
  Windows,
  Commctrl;

{ TBoldXCVTreeViewCom }

procedure TBoldXCVTreeViewCom.Copy;
begin
  if Assigned(fOnCopy) then
    fOnCopy(Self);
end;

procedure TBoldXCVTreeViewCom.Cut;
begin
  if Assigned(fOnCut) then
    fOnCut(Self);
end;

procedure TBoldXCVTreeViewCom.PassToEditor(Message: TMessage);
var
  ControlHand: HWnd;
begin
  ControlHand := TreeView_GetEditControl(Handle);
  PostMessage(ControlHand, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TBoldXCVTreeViewCom.Paste;
begin
  if Assigned(fOnPaste) then
    fOnPaste(Self);
end;

procedure TBoldXCVTreeViewCom.WMCopy(var Message: TMessage);
begin
  if isEditing then
    PassToEditor(Message)
  else
    Copy;
  inherited;
end;

procedure TBoldXCVTreeViewCom.WMCut(var Message: TMessage);
begin
  if isEditing then
    PassToEditor(Message)
  else
    Cut;
  inherited;
end;

procedure TBoldXCVTreeViewCom.WMPaste(var Message: TMessage);
begin
  if isEditing then
    PassToEditor(Message)
  else
    Paste;
  inherited;
end;

end.
