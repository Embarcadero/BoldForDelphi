unit FileReplacer;

interface
uses
  controls,
  dialogs,
  SysUtils,
  Classes;

procedure UpdateUsesNames( FileContents, OldUnits, NewUnits : TStrings );

procedure UpdateWord( FileContents:TStrings; OldWord, NewWord : String; WholeWord: Boolean = true );
Procedure UpdateWordInNeighbourhoodOf( FileContents:TStrings; OldWord, NewWord : String; Triggers : Array Of String; Distance : Integer );

function dfmToStrList( dfmFileName:string; FileContents : TStrings ): boolean;
procedure StrListToDfm( dfmFileName:string; FileContents : TStrings; StoreAstext: Boolean );

function ReplaceSpecial( OrgString, part1, part2, NewString: String ): String;

function ReplaceString( OrgString, OldString, NewString : String; WholeWord : Boolean ) : String;


Procedure AddUsesUnit( FileContents : TStrings; UnitName : String );

procedure RenameProperty( FileContents : TStrings; Components : array of string; OldName, NewName : String );
procedure ReTypeComponent( FileContents : TStrings; FileName, OldName, NewName:String; CantHaveProperties: array of String );

var
  replacementCount : integer;
  replacements : TStringLIst;

  const
  IdentifierChars = ['_', 'a'..'z', 'A'..'Z', '0'..'9'];

implementation


function poscis( substr, Str: String ) : integer;
begin
  result := pos( uppercase( substr) , uppercase( str ) );
end;


function WholeWordPos( substr, Str : String ) : integer;
var
  p, p2 : integer;

function StartOK : Boolean;
begin
  result := not (SubStr[1] in IdentifierChars ) or
    ( p = 1 ) or
    not (Str[p-1] in IdentifierChars );
end;

function EndOK : Boolean;
begin
  result := not (SubStr[length( SubStr) ] in IdentifierChars ) or
    (p + length( SubStr )-1 = Length( Str ) ) or
    not (Str[p+length(SubStr)] in IdentifierChars);
end;

begin
  SubStr := UpperCase( SubStr );
  Str := upperCase( Str );
  result := 0;
  p := pos( SubStr, Str );
  if (p = 0)  or
   ( startOK and EndOK ) then begin
    result := p;
  end else begin
    p2 :=  WholeWordPos( SubStr, copy( Str, p+1, length( Str )));
    if p2 <> 0 then
      result := p+p2;
  end;

end;


function dfmToStrList( dfmFileName:string; FileContents : TStrings ) : boolean;
var
  dfmBinStream : TFileStream;
  dfmTextStream: TMEmoryStream;
  Buff : Array[0..10] of char;
begin
  dfmBinStream := TFileStream.Create(dfmFileName, fmOpenRead );
  dfmTextStream := TMemoryStream.Create;
  dfmBinStream.read(buff, 6);
  Buff[6] := #0;
  dfmBinStream.Position := 0;
  if (Buff = 'object') or (buff = 'inheri') then
  begin
    dfmTextStream.LoadFromStream( dfmBinStream );
    result := true;
  end
  else
  begin
    ObjectresourceToText(dfmBinStream, dfmTextStream );
    result := false;
  end;
  FileContents.Clear;
  dfmTextStream.Position := 0;
  FileContents.LoadFromStream( dfmTextStream );
  dfmBinStream.Free;
  dfmTextStream.Free;
end;

procedure StrListToDfm( dfmFileName:string; FileContents : TStrings; StoreAsText: Boolean );
var
  dfmBinStream : TFileStream;
  dfmTextStream: TStream;
  Attr : integer;
begin
  Attr := FileGetAttr( dfmFileName );

  if (attr <> -1) and (Attr and fareadOnly = faReadOnly) then
    raise Exception.CreateFmt( '%s is read only!', [dfmFileName ]);

  if StoreAsText then
    dfmTextStream := TFileStream.Create( dfmFileName, fmCreate )
  else
    dfmTextStream := TMemoryStream.Create;

  FileContents.SaveToStream(dfmTextStream);
  dfmTextStream.Position := 0;

  if not StoreAsText then
  begin
    dfmBinStream := TFileStream.Create(dfmFileName, fmCreate );
    ObjectTextToResource(dfmTextStream, dfmBinStream );
    dfmBinStream.Free;
  end;
  
  dfmTextStream.Free;
end;

function RangeIsIdentChars( s: String; start, stop: integer ): Boolean;
var
  i: integer;
begin
  result := true;
  for i := start to stop do
    if not (s[i] in IdentifierChars) then
    begin
      result := false;
      exit;
    end;
end;


function ReplaceSpecial( OrgString, part1, part2, NewString: String ): String;
var
  p, p2 : integer;
begin
  p := poscis( part1, OrgString );
  p2 := poscis( part2, OrgString );
  if (p = 0) or (p2 = 0) then
    result := OrgString
  else if p2 < p then
    result := copy( OrgString, 1, p-1 ) + ReplaceSpecial( copy( OrgString, p, maxint ), part1, part2, newString )
  else if RangeIsIdentChars( OrgString, p+ length( part1), p2-1 ) then
  begin
    result := copy( OrgString, 1, p-1 ) + NewString + ReplaceSpecial( copy( OrgString, p2+length( part2), maxint ), part1, part2, newString );
    inc( replacementCount );
    replacements.Add( Part1+'*'+Part2 +'->'+NewString );
  end
  else
    result := copy( OrgString, 1, p ) + ReplaceSpecial( copy( OrgString, p+1, maxint ), part1, part2, newString )
end;


function ReplaceString( OrgString, OldString, NewString : String; WholeWord : Boolean ) : String;
var
  p : integer;
begin
  result := orgString;
  if WholeWord then
    p := WholeWordPos( OldString, Result )
  else
    p := Poscis( OldString, Result );
  while p <> 0 do begin
    inc( replacementCount );
    replacements.Add( OldString +'->'+NewString );
    Delete( result, p, length( OldString ) );
    Insert( NewString, result, p );
    if WholeWord then
      p := WholeWordPos( OldString, Result )
    else
      p := Poscis( OldString, Result );
  end;
end;

procedure RenameProperty( FileContents : TStrings;
  Components : array of string;
  OldName, NewName : String );
var
  State : Boolean;
  i, j, p : integer;
  Str : String;
begin
  State := false;
  for i := 0 to FileContents.count-1 do begin
    str := FileContents[i];
    for j := low(Components) to high(Components) do begin
      if ( (pos( 'object', Str ) <> 0) or (pos( 'inherited', Str ) <> 0) ) and
        ( poscis(Components[j], Str ) <> 0 ) then
        State := true;
      if trim( Str ) = 'end' then state := false;
      if state and ( poscis( OldName, Trim( Str ) ) = 1 ) then begin
        p := poscis( OldName, Str );
        inc( replacementCount );
        replacements.Add( OldName +'->'+Newname );
        Delete( Str, p, length( Oldname ));
        Insert( Newname, Str, p );
      end;

      if Str <> FileContents[i] then
        FileContents[i] := Str;
    end;
  end;
end;

procedure BackupFile( FileName : String );
var
  f, f2 : file;
begin
  assignFile( F, FileName );

  fileName[Length( FileName )] := '~';
  assignFile( f2, FileName );
  {$i-}
  reset( F2 );
  {$i+}
  if ioresult = 0 then begin
    closeFile( f2 );
    erase( f2 );
  end;
  rename( f, FileName );
end;

{
procedure replaceInFile( FileContents : TStrings; OldString, NewString : String; WholeWords : Boolean );
var
  i : integer;
begin
  for i := 0 to FileContents.count-1 do
    FileContents[i] := replaceString( FileContents[i], OldString, NewString, WholeWords );
end;
}

type
  TUnitPosState = (upsUses,upsNoUses );

procedure UpdateUsesNames( FileContents, OldUnits, NewUnits : TStrings );
var
  state : TUnitPosState;
  p, i, j : integer;
  Str : String;
  tempStr : String;
  LastUnitOnLine, OneAdded : Boolean;
  NewUnit, UsesClause : String;
begin
  state := upsNoUses;
  UsesClause := '';
  for i := 0 to FileContents.Count-1 do begin
    Str := FileContents[i];
    if WholeWordPos( 'uses', Str ) <> 0 then begin
      State := upsUses;
      j := i-1;
      repeat
        inc( j );
        usesclause := UsesClause + FileContents[j];
      until pos( ';', FileContents[j] ) <> 0;
    end;
    if state = upsUses then begin
      for j := 0 to OldUnits.count-1 do begin
        p := WholeWordPos( OldUnits[j], Str );
        if p <> 0 then begin
          inc( replacementCount );
          replacements.Add( OldUnits[j] +'->'+NewUnits[j] );
          delete( Str, p, length( OldUnits[j]) );
          LastUnitOnLine := false;
          if pos( ',', copy( str, p, length( Str ))) <> 0 then
            Delete( Str, p, pos( ',', copy( str, p, length( Str ))))
          else
            LastUnitOnLine := true;

          OneAdded := false;
          tempStr := NewUnits[j];
          while tempStr <> '' do begin
            if pos( '|', TempStr ) = 0 then begin
              NewUnit := TempStr;
              TempStr := '';
            end else begin
              NewUnit := Copy( TempStr, 1, pos( '|', tempStr )-1 );
              Delete( tempStr, 1, pos('|', tempStr ) );
            end;

            if poscis( NewUnit, UsesClause ) = 0 then begin
              if not lastunitOnLine or OneAdded then
                NewUnit := NewUnit + ', ';
              Insert( NewUnit, Str, p );
              OneAdded := true;
            end;
          end;

        end;
      end;
    end;
    if Str <> FileContents[i] then
      FileContents[i] := Str;

    if pos( ';', FileContents[i]) <> 0 then
      State := upsNoUses;
  end;
end;

procedure UpdateWord( FileContents:TStrings; OldWord, NewWord : String; WholeWord: Boolean );
var
  p, i : integer;
  part1, part2 : string;
begin
  p := pos( '*', OldWord );
  if p <> 0 then begin
    part1 := copy( oldword, 1, p-1 );
    part2 := copy( oldWord, p+1, maxint );
    for i := 0 to FileContents.Count-1 do
      FileContents[i] := ReplaceSpecial( FileContents[i], part1, part2, NewWord );
  end else begin
    for i := 0 to FileContents.Count-1 do
      FileContents[i] := ReplaceString( FileContents[i], OldWord, NewWord, WholeWord );
  end;
end;

Procedure UpdateWordInNeighbourhoodOf( FileContents:TStrings; OldWord, NewWord : String; Triggers : Array Of String; Distance : Integer );
var
  i, j, DistLeft : integer;
begin
  DistLEft := 0;
  for j := low( Triggers ) to High(triggers) do
    triggers[j] := UpperCase( Triggers[j] );

  for i := 0 to FileContents.Count-1 do begin
    for j := low( Triggers ) to High(triggers) do
      if pos( Triggers[j], UpperCase( FileContents[i] )) <> 0 then
        distLeft := Distance+1;
    if DistLeft> 0 then begin
      FileContents[i] := ReplaceString( FileContents[i], OldWord, NewWord, true );
      Dec( DistLEft );
    end else if DistLeft = -100 then
      FileContents[i] := ReplaceString( FileContents[i], OldWord, NewWord, true )
    else if (poscis( 'procedure', FileContents[i] ) <> 0 ) or
      (poscis( 'procedure', FileContents[i] ) <> 0 ) then
        distleft := 0;
  end;
end;



Procedure AddUsesUnit( FileContents : TStrings; UnitName : String );
var
  i, j : integer;
  UsesClause : String;
  Str : String;
begin
  i := 0;
  while WholeWordPos( 'uses', FileContents[i] ) = 0 do inc( i );
  j := i;
  UsesClause := '';
  while pos( ';', FIleContents[j] ) = 0 do begin
    UsesClause := UsesClause + FileContents[j];
    Inc( j );
  end;
  UsesClause := UsesClause + FileContents[j];
  if WholeWordPos( UnitName, UsesClause ) = 0 then begin
    Str := FileContents[i];
    Insert( ' '+Unitname +', ', Str, WholeWordPos( 'uses', Str ) + 4);
    FileContents[i] := Str;
    replacements.Add( format( 'added %s in uses-statement', [UnitName] ));
  end;
end;

procedure ReTypeComponent( FileContents : TStrings; FileName, OldName, NewName:String; CantHaveProperties: array of String );
var
  i, p, row, j : integer;
  s : String;
  replace : Boolean;
begin
  for i := 0 to FileContents.count-1 do
  begin
    p := poscis( ': '+OldName, FileContents[i] );
    if p > 0 then begin
      row := i;
      s := #13#10;
      replace := true;
      while trim(FileContents[Row]) <> 'end' do
      begin
        for j := low( CantHaveProperties) to high( CantHaveProperties ) do
          if pos( CantHaveProperties[j] + ' =', FileContents[Row] ) <> 0 then
            replace := false;

        s := s + #13#10 + FileContents[Row];
        inc( row );
      end;
      if replace and (MessageDlg(format( 'Unit: %s Retype the following to a %s?: %s', [FileName, NewName, S] ),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes ) then
      begin
        FileContents[i] := Copy( FileContents[i], 1, p ) + ' ' + NewName;
        inc( replacementCount );
        replacements.Add( 'Retyped '+Copy( FileContents[i], 1, p )+ ' from ' + OldName+' to '+NewName );
      end;

    end;
  end;
end;




initialization
  replacements := TStringList.Create;
finalization
  replacements.Free;
end.
