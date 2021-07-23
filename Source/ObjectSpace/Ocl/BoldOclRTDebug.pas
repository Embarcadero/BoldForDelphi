
{ Global compiler directives }
{$include bold.inc}
unit BoldOclRTDebug;

interface

uses
  BoldBase,
  BoldElements;

type
  TBoldOclRTDebugger = class (TBoldMemoryManagedObject)
  public
    function HasFixFor(const Ocl: String; Context: TBoldElementTypeInfo): Boolean; virtual; abstract;
    function GetFixFor(const Ocl: String; Context: TBoldElementTypeInfo): String; virtual; abstract;
    function AddFixFor(const Ocl: String; Context: TBoldElementTypeInfo; const ComponentPath: String; Message: String): Boolean; virtual; abstract;
  end;

implementation


end.
