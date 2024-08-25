
{ Global compiler directives }
{$include bold.inc}
unit BoldGuard;

interface

uses
  BoldBase;

const
  MAXARRAY = 9;

type
  { forward declarations }
  TBoldGuard = class;     

  TBoldObjectVariableReference = ^TObject;

  { IBoldGuard }
  IBoldGuard = interface(IUnKnown)
  end;

  { TBoldGuard }
  TBoldGuard = class(TBoldRefCountedObject, IBoldGuard)
  private
    fUsed: integer;
    fObjectVariable: array [0..MAXARRAY] of TBoldObjectVariableReference;
  public
    constructor Create(var v0); overload;
    constructor Create(var v0, v1); overload;
    constructor Create(var v0, v1, v2); overload;
    constructor Create(var v0, v1, v2, v3); overload;
    constructor Create(var v0, v1, v2, v3, v4); overload;
    constructor Create(var v0, v1, v2, v3, v4, v5); overload;
    constructor Create(var v0, v1, v2, v3, v4, v5, v6); overload;
    constructor Create(var v0, v1, v2, v3, v4, v5, v6, v7); overload;
    constructor Create(var v0, v1, v2, v3, v4, v5, v6, v7, v8); overload;
    constructor Create(var v0, v1, v2, v3, v4, v5, v6, v7, v8, v9); overload;
    destructor Destroy; override;
  end;

implementation

constructor TBoldGuard.Create(var v0);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fUsed := 1;
end;

constructor TBoldGuard.Create(var v0, v1);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fUsed := 2;
end;

constructor TBoldGuard.Create(var v0, v1, v2);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fUsed := 3;
end;

constructor TBoldGuard.Create(var v0, v1, v2, v3);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fObjectVariable[3] := @TObject(v3);
  Tobject(v3) := nil;
  fUsed := 4;
end;

constructor TBoldGuard.Create(var v0, v1, v2, v3, v4);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fObjectVariable[3] := @TObject(v3);
  Tobject(v3) := nil;
  fObjectVariable[4] := @TObject(v4);
  Tobject(v4) := nil;
  fUsed := 5;
end;

constructor TBoldGuard.Create(var v0, v1, v2, v3, v4, v5);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fObjectVariable[3] := @TObject(v3);
  Tobject(v3) := nil;
  fObjectVariable[4] := @TObject(v4);
  Tobject(v4) := nil;
  fObjectVariable[5] := @TObject(v5);
  Tobject(v5) := nil;
  fUsed := 6;
end;

constructor TBoldGuard.Create(var v0, v1, v2, v3, v4, v5, v6);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fObjectVariable[3] := @TObject(v3);
  Tobject(v3) := nil;
  fObjectVariable[4] := @TObject(v4);
  Tobject(v4) := nil;
  fObjectVariable[5] := @TObject(v5);
  Tobject(v5) := nil;
  fObjectVariable[6] := @TObject(v6);
  Tobject(v6) := nil;
  fUsed := 7;
end;

constructor TBoldGuard.Create(var v0, v1, v2, v3, v4, v5, v6, v7);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fObjectVariable[3] := @TObject(v3);
  Tobject(v3) := nil;
  fObjectVariable[4] := @TObject(v4);
  Tobject(v4) := nil;
  fObjectVariable[5] := @TObject(v5);
  Tobject(v5) := nil;
  fObjectVariable[6] := @TObject(v6);
  Tobject(v6) := nil;
  fObjectVariable[7] := @TObject(v7);
  Tobject(v7) := nil;
  fUsed := 8;
end;

constructor TBoldGuard.Create(var v0, v1, v2, v3, v4, v5, v6, v7, v8);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fObjectVariable[3] := @TObject(v3);
  Tobject(v3) := nil;
  fObjectVariable[4] := @TObject(v4);
  Tobject(v4) := nil;
  fObjectVariable[5] := @TObject(v5);
  Tobject(v5) := nil;
  fObjectVariable[6] := @TObject(v6);
  Tobject(v6) := nil;
  fObjectVariable[7] := @TObject(v7);
  Tobject(v7) := nil;
  fObjectVariable[8] := @TObject(v8);
  Tobject(v8) := nil;
  fUsed := 9;
end;

constructor TBoldGuard.Create(var v0, v1, v2, v3, v4, v5, v6, v7, v8, v9);
begin
  fObjectVariable[0] := @TObject(v0);
  Tobject(v0) := nil;
  fObjectVariable[1] := @TObject(v1);
  Tobject(v1) := nil;
  fObjectVariable[2] := @TObject(v2);
  Tobject(v2) := nil;
  fObjectVariable[3] := @TObject(v3);
  Tobject(v3) := nil;
  fObjectVariable[4] := @TObject(v4);
  Tobject(v4) := nil;
  fObjectVariable[5] := @TObject(v5);
  Tobject(v5) := nil;
  fObjectVariable[6] := @TObject(v6);
  Tobject(v6) := nil;
  fObjectVariable[7] := @TObject(v7);
  Tobject(v7) := nil;
  fObjectVariable[8] := @TObject(v8);
  Tobject(v8) := nil;
  fObjectVariable[9] := @TObject(v9);
  Tobject(v9) := nil;
  fUsed := 10;
end;

destructor TBoldGuard.Destroy;
var
  i: integer;
begin
  for i := 0 to FUsed - 1 do
    if Assigned(fObjectVariable[i]^) then
    begin
      fObjectVariable[i]^.Free;
      fObjectVariable[i]^ := nil;
    end;
  inherited;
end;

end.
