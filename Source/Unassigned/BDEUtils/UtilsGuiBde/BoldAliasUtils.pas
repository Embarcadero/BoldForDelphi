unit BoldAliasUtils;

interface

uses
  Classes,
  Windows,
  StdCtrls;

type
  TBoldAliasUtils = class
  public
    class procedure GetAliasNames(Strings: TStrings);
    class function ValidateAlias(AliasCombo: TComboBox): boolean;
    class procedure BrowseForPath(ComboBox: TComboBox);
  end;

implementation

uses
  SysUtils,
  ShlObj,
  ActiveX,
  Dialogs,
  Forms,
  DBTables,
  BoldCursorGuard;

{ TBoldAliasUtils }

class procedure TBoldAliasUtils.BrowseForPath(ComboBox: TComboBox);
var
  Handle: THandle;
  ShellMalloc: IMalloc;
  BrowseInfo: TBrowseInfo;
  DisplayName, Folder: array[0..MAX_PATH] of char;
  pidlBrowse, pidlDesktop: PItemIDList;
begin
  if Assigned(ComboBox.Owner) and (ComboBox.Owner is TForm) then
    Handle := TForm(ComboBox.Owner).Handle
  else
    Handle := 0;
  SHGetMalloc(ShellMalloc);
  FillChar(BrowseInfo, SizeOf(BrowseInfo), 0);
  SHGetSpecialFolderLocation(Handle, CSIDL_Desktop, pidlDesktop);
  with BrowseInfo do
  begin
    hwndOwner := Handle;
    pszDisplayName := @DisplayName;
    lpszTitle := 'Select folder for paradox database';
    ulFlags := BIF_RETURNONLYFSDIRS;
    pidlRoot := pidlDesktop;
  end;
  pidlBrowse := SHBrowseForFolder(BrowseInfo);
  if pidlBrowse <> nil then
  begin
    SHGetPathFromIDList(pidlBrowse, Folder);
    ComboBox.Text := Folder;
    if Assigned(@ComboBox.OnChange) then
      ComboBox.OnChange(nil);
    ShellMalloc.Free(pidlBrowse);
  end;
  ShellMalloc.Free(pidlDesktop);end;

class procedure TBoldAliasUtils.GetAliasNames(Strings: TStrings);
var
  CursorGuard: IBoldCursorGuard;
begin
  CursorGuard := TBoldCursorGuard.Create;
  //Load aliases
  Session.GetAliasNames(Strings);
end;

class function TBoldAliasUtils.ValidateAlias(AliasCombo: TComboBox): boolean;
var
  Alias: string;

  function PathExists(FileName: string): Boolean;
  var
    Handle: THandle;
    FindData: TWin32FindData;
  begin
    Result := False;
    Handle := FindFirstFile(PChar(FileName), FindData);
    if Handle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(Handle);
      if (FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY) <> 0 then
        Result := True;
    end;
  end;

begin
  Alias := AliasCombo.Text;
  if (Pos('\', Alias) > 0) or (Pos(':', Alias) > 0) then
  begin
    if Copy(Alias, Length(Alias), 1) = '\' then
      Alias := Copy(Alias, 1, Length(Alias) - 1);
    if not PathExists(Alias) then
    begin
      MessageDlg(Format('''%s'' is not a valid path.', [Alias]), mtError, [mbOk], 0);
      Result := False;
      Exit;
    end;
  end
  else
  begin
    if (AliasCombo.Items.IndexOf(Alias) = -1) then
    begin
      MessageDlg(Format('''%s'' is not a valid alias.', [Alias]), mtError, [mbOk], 0);
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

end.
