
{ Global compiler directives }
{$include bold.inc}
unit BoldXCVTreeView;

interface

uses
  Messages,
  Classes,
  BoldTreeView;

type

  [ComponentPlatformsAttribute (pidWin32 or pidWin64)]
  TBoldXCVTreeView = class(TBoldTreeView)
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

{ TBoldXCVTreeView }

procedure TBoldXCVTreeView.Copy;
begin
  if Assigned(fOnCopy) then
    fOnCopy(Self);
end;

procedure TBoldXCVTreeView.Cut;
begin
  if Assigned(fOnCut) then
    fOnCut(Self);
end;

procedure TBoldXCVTreeView.PassToEditor(Message: TMessage);
var
  ControlHand: HWnd;
begin
  ControlHand := TreeView_GetEditControl(Handle);
  PostMessage(ControlHand, Message.Msg, Message.wParam, Message.lParam);
end;

procedure TBoldXCVTreeView.Paste;
begin
  if Assigned(fOnPaste) then
    fOnPaste(Self);
end;

procedure TBoldXCVTreeView.WMCopy(var Message: TMessage);
begin
  if isEditing then
    PassToEditor(Message)
  else
    Copy;
  inherited;
end;

procedure TBoldXCVTreeView.WMCut(var Message: TMessage);
begin
  if isEditing then
    PassToEditor(Message)
  else
    Cut;
  inherited;
end;

procedure TBoldXCVTreeView.WMPaste(var Message: TMessage);
begin
  if isEditing then
    PassToEditor(Message)
  else
    Paste;
  inherited;
end;

end.
