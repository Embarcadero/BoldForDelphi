{ Global compiler directives }
{$include bold.inc}
unit BoldSystemPerf;

interface

uses
  Classes, SysUtils, PerfClasses;

type
  TBoldSystemPerfObject = class(TPerfObject)
  private
    FBoldObjectsCreatedRate: TPerfCounter;
    FBoldObjectsCreated: TPerfCounter;
    FBoldObjectsLoadedRate: TPerfCounter;
    FBoldObjectsLoaded: TPerfCounter;
    FBoldObjectsInvalidatedRate: TPerfCounter;
    FBoldObjectsInvalidated: TPerfCounter;
    FNewBoldObjectsRate: TPerfCounter;
    FNewBoldObjects: TPerfCounter;
    FNewPersistentBoldObjectsRate: TPerfCounter;
    FNewPersistentBoldObjects: TPerfCounter;
    FNewTransientBoldObjectsRate: TPerfCounter;
    FNewTransientBoldObjects: TPerfCounter;
    FNumberSQLStatementsRate: TPerfCounter;
    FNumberSQLStatements: TPerfCounter;
    FTimeSpentSQLStatementsInMsRate: TPerfCounter;
    FTimeSpentSQLStatementsInMs: TPerfCounter;
    FNumberDirtyObjectsSavedRate: TPerfCounter;
    FNumberDirtyObjectsSaved: TPerfCounter;
    FNumberDerivationsRate: TPerfCounter;
    FNumberDerivations: TPerfCounter;
    FTimeSpentDerivingMsRate: TPerfCounter;
    FTimeSpentDerivingMs: TPerfCounter;

  public
    constructor Create(AModule: TPerfModule);override;
    procedure BoldObject_InternalCreateWithClassAndLocator(ClassName : String);
    procedure BoldObject_EndFetchMembers;
    procedure BoldObject_InternalCreateNewWithClassAndSystem(ClassName: String;Persistent: Boolean);
    procedure BoldObject_Invalidate;
    procedure BoldDBXQuery_ExecSQL(ElapsedTime: TDateTime);
    procedure BoldDBXQuery_Open(ElapsedTime: TDateTime);
    procedure BoldSystem_UpdateDatabaseWithList(ObjectCount: Integer);
    procedure BoldEventPluggedDeriver_DoDeriveAndSubscribe(ElapsedTime: TDateTime);
  end;

var
  BoldSystemPerfObject : TBoldSystemPerfObject;

implementation

{ TBoldSystemPerfObject }

procedure TBoldSystemPerfObject.BoldDBXQuery_ExecSQL(ElapsedTime: TDateTime);
var
  ElapsedTimeInMs : Integer;
begin
  FNumberSQLStatements.Increment32;
  FNumberSQLStatementsRate.Increment32;

  ElapsedTimeInMs := Trunc(ElapsedTime*24*3600*1000);
  FTimeSpentSQLStatementsInMs.Increment32(ElapsedTimeInMs);
  FTimeSpentSQLStatementsInMsRate.Increment32(ElapsedTimeInMs);
end;

procedure TBoldSystemPerfObject.BoldDBXQuery_Open(ElapsedTime: TDateTime);
begin
  BoldDBXQuery_ExecSQL(ElapsedTime);
end;

procedure TBoldSystemPerfObject.BoldEventPluggedDeriver_DoDeriveAndSubscribe(ElapsedTime: TDateTime);
var
  ElapsedTimeInMs: Integer;
begin
  FNumberDerivations.Increment32;
  FNumberDerivationsRate.Increment32;
  ElapsedTimeInMs := Trunc(ElapsedTime*24*3600*1000);
  FTimeSpentDerivingMs.Increment32(ElapsedTimeInMs);
  FTimeSpentDerivingMsRate.Increment32(ElapsedTimeInMs);
end;

procedure TBoldSystemPerfObject.BoldObject_EndFetchMembers;
begin
  FBoldObjectsLoaded.Increment32;
  FBoldObjectsLoadedRate.Increment32;
end;

procedure TBoldSystemPerfObject.BoldObject_InternalCreateNewWithClassAndSystem(ClassName: String; Persistent: Boolean);
begin
  FNewBoldObjects.Increment32;
  FNewBoldObjectsRate.Increment32;
  if Persistent then
  begin
    FNewPersistentBoldObjectsRate.Increment32;
    FNewPersistentBoldObjects.Increment32;
  end else
  begin
    FNewTransientBoldObjectsRate.Increment32;
    FNewTransientBoldObjects.Increment32;
  end
end;

procedure TBoldSystemPerfObject.BoldObject_InternalCreateWithClassAndLocator(ClassName : String);
begin
  FBoldObjectsCreated.Increment32;
  FBoldObjectsCreatedRate.Increment32;
end;

procedure TBoldSystemPerfObject.BoldObject_Invalidate;
begin
  FBoldObjectsInvalidated.Increment32;
  FBoldObjectsInvalidatedRate.Increment32;
end;

procedure TBoldSystemPerfObject.BoldSystem_UpdateDatabaseWithList(ObjectCount: Integer);
begin
  FNumberDirtyObjectsSaved.Increment32(ObjectCount);
  FNumberDirtyObjectsSavedRate.Increment32(ObjectCount);
end;

constructor TBoldSystemPerfObject.Create(AModule: TPerfModule);
begin
  inherited Create(AModule);
  BoldSystemPerfObject := Self;

  MaxInstances := 10;
  NameTitle := 'Bold System';
  HelpTitle := 'Bold System Performance monitoring';

  FBoldObjectsCreated := TPerfCounter.Create(Self);
  FBoldObjectsCreated.NameTitle := 'Number of BoldObjects created';
  FBoldObjectsCreated.HelpTitle := 'Number of times TBoldObject.Create was executed';
  FBoldObjectsCreated.CounterType := ctNumberOfItems32;

  FBoldObjectsCreatedRate := TPerfCounter.Create(Self);
  FBoldObjectsCreatedRate.NameTitle := 'Number of BoldObjects created/s';
  FBoldObjectsCreatedRate.HelpTitle := 'Number of times TBoldObject.Create was executed per second';
  FBoldObjectsCreatedRate.CounterType := ctRateOfCountsPerSecond32;

  FBoldObjectsLoaded := TPerfCounter.Create(Self);
  FBoldObjectsLoaded.NameTitle := 'Number of BoldObjects Loaded';
  FBoldObjectsLoaded.HelpTitle := 'Number of times TBoldObject.EndFetchMembers was executed';
  FBoldObjectsLoaded.CounterType := ctNumberOfItems32;

  FBoldObjectsLoadedRate := TPerfCounter.Create(Self);
  FBoldObjectsLoadedRate.NameTitle := 'Number of BoldObjects Loaded/s';
  FBoldObjectsLoadedRate.HelpTitle := 'Number of times TBoldObject.EndFetchMembers was executed per second';
  FBoldObjectsLoadedRate.CounterType := ctRateOfCountsPerSecond32;

  FBoldObjectsInvalidated := TPerfCounter.Create(Self);
  FBoldObjectsInvalidated.NameTitle := 'Number of BoldObjects Invalidated';
  FBoldObjectsInvalidated.HelpTitle := 'Number of times TBoldObject.Invalidate was executed';
  FBoldObjectsInvalidated.CounterType := ctNumberOfItems32;

  FBoldObjectsInvalidatedRate := TPerfCounter.Create(Self);
  FBoldObjectsInvalidatedRate.NameTitle := 'Number of BoldObjects Invalidated/s';
  FBoldObjectsInvalidatedRate.HelpTitle := 'Number of times TBoldObject.Invalidate was executed per second';
  FBoldObjectsInvalidatedRate.CounterType := ctRateOfCountsPerSecond32;

  FNewBoldObjects := TPerfCounter.Create(Self);
  FNewBoldObjects.NameTitle := 'Number of New Objects';
  FNewBoldObjects.HelpTitle := 'Number of New Objects';
  FNewBoldObjects.CounterType := ctNumberOfItems32;

  FNewBoldObjectsRate := TPerfCounter.Create(Self);
  FNewBoldObjectsRate.NameTitle := 'Number New Objects/s';
  FNewBoldObjectsRate.HelpTitle := 'Number New Objects/s';
  FNewBoldObjectsRate.CounterType := ctRateOfCountsPerSecond32;

  FNewPersistentBoldObjects := TPerfCounter.Create(Self);
  FNewPersistentBoldObjects.NameTitle := 'Number of New Persistent Objects';
  FNewPersistentBoldObjects.HelpTitle := 'Number of New Persistent Objects';
  FNewPersistentBoldObjects.CounterType := ctNumberOfItems32;

  FNewPersistentBoldObjectsRate := TPerfCounter.Create(Self);
  FNewPersistentBoldObjectsRate.NameTitle := 'Number of New Persistent Objects/s';
  FNewPersistentBoldObjectsRate.HelpTitle := 'Number of New Persistent Objects/s';
  FNewPersistentBoldObjectsRate.CounterType := ctRateOfCountsPerSecond32;

  FNumberSQLStatements := TPerfCounter.Create(Self);
  FNumberSQLStatements.NameTitle := 'Number of SQL Statements';
  FNumberSQLStatements.HelpTitle := 'Number of SQL Statements';
  FNumberSQLStatements.CounterType := ctNumberOfItems32;

  FNumberSQLStatementsRate := TPerfCounter.Create(Self);
  FNumberSQLStatementsRate.NameTitle := 'Number of SQL Statements/s';
  FNumberSQLStatementsRate.HelpTitle := 'Number of SQL Statements/s';
  FNumberSQLStatementsRate.CounterType := ctRateOfCountsPerSecond32;

  FTimeSpentSQLStatementsInMs := TPerfCounter.Create(Self);
  FTimeSpentSQLStatementsInMs.NameTitle := 'Time spent in SQL Statements (ms)';
  FTimeSpentSQLStatementsInMs.HelpTitle := 'Time spent in SQL Statements (ms)';
  FTimeSpentSQLStatementsInMs.CounterType := ctNumberOfItems32;

  FTimeSpentSQLStatementsInMsRate := TPerfCounter.Create(Self);
  FTimeSpentSQLStatementsInMsRate.NameTitle := 'Time spent in SQL Statements ms/s';
  FTimeSpentSQLStatementsInMsRate.HelpTitle := 'Time spent in SQL Statements ms/s';
  FTimeSpentSQLStatementsInMsRate.CounterType := ctRateOfCountsPerSecond32;

  FNumberDirtyObjectsSaved := TPerfCounter.Create(Self);
  FNumberDirtyObjectsSaved.NameTitle := 'Number of Dirty objects saved';
  FNumberDirtyObjectsSaved.HelpTitle := 'Number of Dirty objects saved';
  FNumberDirtyObjectsSaved.CounterType := ctNumberOfItems32;

  FNumberDirtyObjectsSavedRate := TPerfCounter.Create(Self);
  FNumberDirtyObjectsSavedRate.NameTitle := 'Number of Dirty objects saved/s';
  FNumberDirtyObjectsSavedRate.HelpTitle := 'Number of Dirty objects saved/s';
  FNumberDirtyObjectsSavedRate.CounterType := ctRateOfCountsPerSecond32;

  FNewTransientBoldObjects := TPerfCounter.Create(Self);
  FNewTransientBoldObjects.NameTitle := 'Number of Transient New Objects';
  FNewTransientBoldObjects.HelpTitle := 'Number of Transient New Objects';
  FNewTransientBoldObjects.CounterType := ctNumberOfItems32;

  FNewTransientBoldObjectsRate := TPerfCounter.Create(Self);
  FNewTransientBoldObjectsRate.NameTitle := 'Number of New Transient Objects/s';
  FNewTransientBoldObjectsRate.HelpTitle := 'Number of New Transient Objects/s';
  FNewTransientBoldObjectsRate.CounterType := ctRateOfCountsPerSecond32;

  FNumberDerivations := TPerfCounter.Create(Self);
  FNumberDerivations.NameTitle := 'Number of derivations';
  FNumberDerivations.HelpTitle := 'Number of derivations';
  FNumberDerivations.CounterType := ctNumberOfItems32;

  FNumberDerivationsRate := TPerfCounter.Create(Self);
  FNumberDerivationsRate.NameTitle := 'Number of derivations/s';
  FNumberDerivationsRate.HelpTitle := 'Number of derivations/s';
  FNumberDerivationsRate.CounterType := ctRateOfCountsPerSecond32;

  FTimeSpentDerivingMs := TPerfCounter.Create(Self);
  FTimeSpentDerivingMs.NameTitle := 'Time spent deriving (ms)';
  FTimeSpentDerivingMs.HelpTitle := 'Time spent deriving (ms)';
  FTimeSpentDerivingMs.CounterType := ctNumberOfItems32;

  FTimeSpentDerivingMsRate := TPerfCounter.Create(Self);
  FTimeSpentDerivingMsRate.NameTitle := 'Time spent deriving ms/s';
  FTimeSpentDerivingMsRate.HelpTitle := 'Time spent deriving ms/s';
  FTimeSpentDerivingMsRate.CounterType := ctRateOfCountsPerSecond32;

end;

end.
