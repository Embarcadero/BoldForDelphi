
{ Global compiler directives }
{$include bold.inc}
unit BoldOTACodeGen;

interface

uses
  toolsapi;

type
  TUnitFile = class(TNotifierObject, IOTAFile)
  private
    fSource: string;
  public
    constructor Create(aSource: string);
    function GetSource: string;
    function GetAge: TDateTime;
    property Source: string read fSource write fSource;
  end;

  TUnitCreator = class(TNotifierObject, IOTACreator, IOTAModuleCreator)
  private
   fFileName, fFileExtension: string;
   fOwner: IOTAModule;
   fSource: string;
  public
   constructor Create;
   function GetCreatorType: string;
   function GetExisting: Boolean;
   function GetFileSystem: string;
   function GetOwner: IOTAModule;
   function GetUnnamed: Boolean;
   function GetAncestorName: string;
   function GetImplFileName: string;
   function GetIntfFileName: string;
   function GetFormName: string;
   function GetMainForm: Boolean;
   function GetShowForm: Boolean;
   function GetShowSource: Boolean;
   function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
   function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
   function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
   procedure FormCreated(const FormEditor: IOTAFormEditor);
   function CreateUnit(const FileName, Source: string; Owner: IOTAModule): IOTAModule;
  end;

const
  FILE_EXTENSION = 'pas';
implementation

uses
  SysUtils,
  BoldUtils;

                              { TAttrUnitCreator  }
constructor TUnitCreator.Create;
begin
  inherited Create;
  fFileName := '';
  fFileExtension := FILE_EXTENSION ;
end;

function TUnitCreator.GetCreatorType: string;
begin
 Result:= sUnit;
end;

function TUnitCreator.GetExisting: Boolean;
begin
 Result:= false;
end;

function TUnitCreator.GetFileSystem: string;
begin
 Result:= '';
end;

function TUnitCreator.GetOwner: IOTAModule;
begin
  Result := fOwner;
end;

function TUnitCreator.GetUnnamed: Boolean;
begin
 Result:= true;
end;

function TUnitCreator.GetAncestorName: string;
begin
  Result:= '';
end;

function TUnitCreator.GetImplFileName: string;
begin
 Result:= Format('%s.%s',[fFileName, fFileExtension]) ;
end;

function TUnitCreator.GetIntfFileName: string;
begin
{ Return the interface filename, or blank to have the IDE create a new
  unique one.  (C++ header) }
 Result:= '';
end;

function TUnitCreator.GetFormName: string;
begin
  Result:= '';
end;

function TUnitCreator.GetMainForm: Boolean;
begin
{ Return True to Make this module the main form of the given Owner/Project }
 Result:= false;
end;

function TUnitCreator.GetShowForm: Boolean;
begin
 Result:= true;
end;

function TUnitCreator.GetShowSource: Boolean;
begin
 Result:= true;
end;

function TUnitCreator.NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
begin
  { Create and return the Form resource for this new module if applicable }
  Result:= nil;
end;

function TUnitCreator.NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
{ Create and return the Implementation source for this module. (C++ .cpp
  file or Delphi unit) }
  Result:= TUnitFile.Create(fSource);
end;

function TUnitCreator.NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
begin
{ Create and return the Interface (C++ header) source for this module }
 Result:= nil;
end;

procedure TUnitCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TUnitCreator.CreateUnit(const FileName, Source: string; Owner: IOTAModule): IOTAModule;
begin
  fFileName := FileName;
  fSource := Source;
  fOwner := Owner;
  Result := (BorlandIDEServices as IOTAModuleServices).CreateModule(Self as IOTACreator);
end;

        {TUnitFile}
constructor TUnitFile.Create(aSource: string);
begin
  fSource := aSource;
end;

function TUnitFile.GetSource: string;
begin
  Result := Source;
end;

function TUnitFile.GetAge: TDateTime;
begin
 Result:= -1;
end;

end.
