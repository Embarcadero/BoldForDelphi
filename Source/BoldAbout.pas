unit BoldAbout;

interface

uses
  Windows,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  {$IFNDEF NO_OTA}
  ToolsApi,
  {$ENDIF}
  ShellAPI,
  Buttons,
  ExtCtrls,
  Menus,
  ImgList,
  Graphics;

type
  TfrmAboutBold = class(TForm)
    PageControl: TPageControl;
    BtnOK: TButton;
    TabAbout: TTabSheet;
    ImageLogoDelphi: TImage;
    LabelCopyright: TLabel;
    LabelComments: TLabel;
    Label14: TLabel;
    LabelURLBoldSoft: TLabel;
    LabelProductName: TLabel;
    LabelVersion: TLabel;
    Bevel1: TBevel;
    Label2: TLabel;
    Label12: TLabel;
    LabelURLBoldForDelphi: TLabel;
    SaveDialogRunLic: TSaveDialog;
    tsTeam: TTabSheet;
    BtnUnlock: TSpeedButton;
    Label9: TLabel;
    Label11: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Image1: TImage;
    Label8: TLabel;
    Label10: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    ImageLogoBCB: TImage;
    ImageList1: TImageList;
    Label34: TLabel;
    Label35: TLabel;
    ImageList2: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure URLHomeClick(Sender: TObject);
    procedure URLProdClick(Sender: TObject);
    procedure URLSupportClick(Sender: TObject);
    procedure ImageLogoMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ImageLogoMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ImageLogoMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BtnUnlockClick(Sender: TObject);
  private
    function GetEffectiveLogo: TImage;
  private
    DragIt: Boolean;
    DragStart: Integer;
    procedure GetVersionInfo;
    property EffectiveLogo: TImage read GetEffectiveLogo;
  public
    {public declarations}
  end;

implementation

{$R *.dfm}

uses
  ActiveX,
  Registry,
  ShlObj,
  SysUtils,

  BoldCoreConsts,
  BoldUtils,
  BoldRegistry,
  BoldDefs,
  BoldWinINet,
  BoldCursorGuard,
  BoldDefsDT;

const
  SubItemRelease = 0;
  SubItemStatus = 1;
  SubItemInfo = 2;
  SubItemProduct = 3;
  SubItemParams = 4;

function SelectDirectoryWithInitial(const Caption: string; const Root: WideString; var Directory: string): Boolean;
var
  WindowList: Pointer;
  BrowseInfo: TBrowseInfo;
  Buffer: PChar;
  RootItemIDList, ItemIDList: PItemIDList;
  ShellMalloc: IMalloc;
  IDesktopFolder: IShellFolder;
  Eaten, Flags: LongWord;

  function BrowserCallback(Wnd: HWND; uMsg: UINT; lParam, lpData: LPARAM): Integer stdcall;
  begin
    if (uMsg = BFFM_INITIALIZED) and (lpData <> 0) then
    begin
      SendMessage(Wnd, BFFM_SETSELECTION, Integer(LongBool(True)), lpData);
    end;
    Result := 0;
  end;

begin
  Result := False;
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  if (ShGetMalloc(ShellMalloc) = S_OK) and (ShellMalloc <> nil) then
  begin
    Buffer := ShellMalloc.Alloc(MAX_PATH);
    try
      RootItemIDList := nil;
      if Root <> '' then
      begin
        SHGetDesktopFolder(IDesktopFolder);
        IDesktopFolder.ParseDisplayName(Application.Handle, nil,
          POleStr(Root), Eaten, RootItemIDList, Flags);
      end;
      with BrowseInfo do
      begin
        hwndOwner := Application.Handle;
        pidlRoot := RootItemIDList;
        pszDisplayName := Buffer;
        lpszTitle := PChar(Caption);
        ulFlags := BIF_RETURNONLYFSDIRS;
        lpfn := @BrowserCallback;
        if Directory<>'' then
          lParam := integer(pchar(Directory));
      end;
      WindowList := DisableTaskWindows(0);
      try
        ItemIDList := ShBrowseForFolder(BrowseInfo);
      finally
        EnableTaskWindows(WindowList);
      end;
      Result :=  ItemIDList <> nil;
      if Result then
      begin
        ShGetPathFromIDList(ItemIDList, Buffer);
        ShellMalloc.Free(ItemIDList);
        Directory := Buffer;
      end;
    finally
      ShellMalloc.Free(Buffer);
    end;
  end;
end;

{*****************************************************************************
 * Form init and finalization
 *****************************************************************************}

procedure TfrmAboutBold.FormCreate(Sender: TObject);
begin
  PageControl.ActivePage        := TabAbout;
  GetVersionInfo;
  LabelURLBoldSoft.Caption      := sURLBoldForDelphi;
  LabelURLBoldForDelphi.Caption := sURLBoldForDelphi;

  // Hide both logos, and then redisplay the correct one.
  ImageLogoDelphi.visible := false;
  ImageLogoBCB.Visible := false;
  EffectiveLogo.Visible := true;
  EffectiveLogo.Top := btnUnlock.Top;
  tsTeam.TabVisible := false;
end;

{*****************************************************************************
 * Internal methods
 *****************************************************************************}

procedure TfrmAboutBold.GetVersionInfo;
type
  PLangCharSetInfo = ^TLangCharSetInfo;
  TLangCharSetInfo = record
    Lang: Word;
    CharSet: Word;
  end;
var
  FileName: array [0..260] of Char;
  SubBlock: array [0..255] of Char;
  VerHandle: Cardinal;
  Size: Word;
  Buffer: Pointer;
  Data: Pointer;
  DataLen: LongWord;
  LangCharSetInfo: PLangCharSetInfo;
  LangCharSetString: string;
begin
  LabelComments.Caption := sVersionInfoNotAvailable;
  {Get size and allocate buffer for VerInfo}
  if GetModuleFileName(hInstance, FileName, SizeOf(FileName)) > 0 then
  begin
    Size := GetFileVersionInfoSize(FileName, VerHandle);
    if Size > 0 then
    begin
      GetMem(Buffer, Size);
      try
        if GetFileVersionInfo(FileName, VerHandle, Size, Buffer) then
        begin
          {Query first language and that language blocks version info}
          if VerQueryValue(Buffer, '\VarFileInfo\Translation', Pointer(LangCharSetInfo), DataLen) then // do not localize
          begin
            LangCharSetString := IntToHex(LangCharSetInfo^.Lang, 4) +
                                 IntToHex(LangCharSetInfo^.CharSet, 4);
            if VerQueryValue(Buffer, StrPCopy(SubBlock, '\StringFileInfo\' + LangCharSetString + '\ProductName'), Data, DataLen) then // do not localize
            begin
              LabelProductName.Caption := StrPas(PChar(Data)); // marco
            Caption := LabelProductName.Caption;
          end;
            if VerQueryValue(Buffer, StrPCopy(SubBlock, '\StringFileInfo\' + LangCharSetString + '\FileVersion'), Data, DataLen) then // do not localize
              LabelVersion.Caption := StrPas(PChar(Data));
            if VerQueryValue(Buffer, StrPCopy(SubBlock, '\StringFileInfo\' + LangCharSetString + '\LegalCopyright'), Data, DataLen) then // do not localize
              LabelCopyright.Caption := StrPas(PChar(Data));
            if VerQueryValue(Buffer, StrPCopy(SubBlock, '\StringFileInfo\' + LangCharSetString + '\Comments'), Data, DataLen) then // do not localize
              LabelComments.Caption := StrPas(PChar(Data));
          end;
        end;
      finally
        FreeMem(Buffer, Size);
      end;
    end
  end;
end;


{*****************************************************************************
 * Easter Egg
 *****************************************************************************}

procedure TfrmAboutBold.ImageLogoMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DragIt := not btnUnlock.Visible and (Shift = [ssShift, ssCtrl, ssRight]);
  DragStart := ScreenToClient(EffectiveLogo.ClientToScreen(Point(X, Y))).Y;
end;

procedure TfrmAboutBold.ImageLogoMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: Integer;
begin
  if DragIt and (Shift = [ssShift, ssCtrl, ssRight]) then
  begin
    NewTop := 4 + ScreenToClient(EffectiveLogo.ClientToScreen(Point(X, Y))).Y - DragStart;
    if NewTop < 4 then
      NewTop := 4;
    if NewTop > 248 then
      NewTop := 248;
    EffectiveLogo.Top := NewTop;
  end
  else
  begin
    DragIt := False;
    if not btnUnLock.Visible then
      EffectiveLogo.Top := 4;
  end;
end;

procedure TfrmAboutBold.ImageLogoMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if DragIt and (EffectiveLogo.Top > 100) then
  begin
    BtnUnlock.Visible := True;
    EffectiveLogo.Top := btnUnlock.Top + btnUnlock.Height + 4;
  end
  else
  begin
    if not btnUnLock.Visible then
      EffectiveLogo.Top := 4;
  end;
  DragIt := False;
end;

procedure TfrmAboutBold.BtnUnlockClick(Sender: TObject);
begin
  tsTeam.TabVisible := True;
end;

{*****************************************************************************
 * User events
 *****************************************************************************}

{About}

procedure TfrmAboutBold.URLHomeClick(Sender: TObject);
{Open BoldSoft homepage}
begin
  ShellExecute(0, 'open', PChar(sURLBoldForDelphi), '', '', SW_SHOWMAXIMIZED); // do not localize
end;

procedure TfrmAboutBold.URLProdClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(sURLBoldForDelphi), '', '', SW_SHOWMAXIMIZED); // do not localize
end;

procedure TfrmAboutBold.URLSupportClick(Sender: TObject);
begin
  ShellExecute(0, 'open', PChar(sURLSupport), '', '', SW_SHOWNORMAL); // do not localize
end;

{Register}

function ReadURL(const URL: string): string;
var
  hInternetSession: HINTERNET;
  hURLFile: HINTERNET;
  Buffer: array [0..1024] of char;
  NumberOfBytesRead: DWORD;
begin
  Result := '';
  hInternetSession := BoldInternetOpen('BoldSoft', BOLD_INTERNET_OPEN_TYPE_PRECONFIG, '', '', 0); // do not localize
  if Assigned(hInternetSession) then
  try
    hURLFile := BoldInternetOpenUrl(hInternetSession, URL, '', BOLD_INTERNET_FLAG_RELOAD, 0);
    if Assigned(hURLFile) then
    try

      while BoldInternetReadFile(hURLFile, @Buffer, SizeOf(Buffer) - 1, NumberOfBytesRead) and
            (NumberOfBytesRead > 0) do
      begin
        Buffer[NumberOfBytesRead] := #0;
        Result := Result + Buffer;
      end
    finally
      BoldInternetCloseHandle(hURLFile);
    end;
  finally
    BoldInternetCloseHandle(hInternetSession);
  end;
  if SameText(Copy(Result, 1, 7), 'http://') then // do not localize
    Result := ReadURL(Result);
end;

function TfrmAboutBold.GetEffectiveLogo: TImage;
begin
  result := imageLogoDelphi;
end;

end.
