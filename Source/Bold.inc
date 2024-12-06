{$I BoldDelphiVer.inc}

{$WARNINGS ON}
{$HINTS ON}

{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}

{$IFDEF WIN32}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF WIN64}
  {$DEFINE MSWINDOWS}
{$ENDIF}

{$IFDEF BOLD_DELPHI_PARSER}
  {$DEFINE WIN32}
  {$DEFINE VER130}
{$ENDIF}

{$IFDEF BCB}
  {$DEFINE BOLD_BCB}
{$ENDIF}

{$IFDEF DELPHI}
  {$DEFINE BOLD_DELPHI}
{$ENDIF}

{$IFDEF BOLD_DELPHI} {$IFDEF BOLD_BCB}
Illegal symbol combination BOLD_DELPHI and BOLD_BCB
{$ENDIF} {$ENDIF}

{$IFNDEF BOLD_DELPHI} {$IFNDEF BOLD_BCB}
  {$DEFINE BOLD_DELPHI}
{$ENDIF} {$ENDIF}


{$IFDEF LINUX}
  // Our implementation depends on MSXML and is not supported on Linux
  {$UNDEF BOLD_XML_SUPPORT}
  {DEFINE BOLD_IDECLX}
  {$DEFINE BOLD_CLX}
{$ENDIF}

{$IFDEF MSWINDOWS}
  {DEFINE BOLD_IDEVCL}
{$ENDIF}

// Edition switches
{$IFNDEF BOLD_PROFESSIONAL}
  {$IFNDEF BOLD_LITE}
    {$DEFINE BOLD_ENTERPRISE}
  {$ENDIF}
{$ENDIF}


// Compiler control
{$IFDEF BOLD_DEV}
  {$ASSERTIONS ON}
  {$DEBUGINFO ON}
  {$IOCHECKS ON}
  {$LOCALSYMBOLS ON}
  {$OPTIMIZATION OFF}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
  {$REFERENCEINFO ON}
  {$STACKFRAMES ON}
{$ENDIF}

{$IFDEF BOLD_RELEASE}
  {$ASSERTIONS OFF}
  {$DEBUGINFO OFF}
  {$IOCHECKS ON}
  {$LOCALSYMBOLS OFF}
  {$OPTIMIZATION ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
  {$REFERENCEINFO OFF}
  {$STACKFRAMES OFF}
{$ENDIF}

{$IFDEF BOLD_DEBUG} {$IFDEF BOLD_RELEASE}
Illegal symbol combination
{$ENDIF} {$ENDIF}


// If building SLIB, swicth on some flags switched off by BOLD_RELEASE
{$IFDEF BOLD_SLIB}
  {$STACKFRAMES ON}
  {$OPTIMIZATION OFF}
  {$DEBUGINFO ON}
  {$LOCALSYMBOLS ON}
  {$IOCHECKS ON}
  {$OVERFLOWCHECKS ON}
  {$RANGECHECKS ON}
  {$REFERENCEINFO ON}
  {$ASSERTIONS ON}
  {$DEFINE BOLD_MEMORYDEBUG}
{$ENDIF}

{$IFDEF BOLD_MEMORYDEBUG}
  {$DEFINE BOLD_DISABLEMEMORYMANAGER}
  {$DEFINE BOLD_DISABLESHAREDSTRINGS}
{$ENDIF}

{$DEFINE BOLD_DISABLEMEMORYMANAGER}
{.$DEFINE BOLDCOMCLIENT}

{$IFNDEF DEBUG}
  {$DEFINE BOLD_INLINE}
{$ENDIF}

{$UNDEF BOLD_UNICODE}
{$IFDEF BOLD_DELPHI13_OR_LATER}
  {$DEFINE BOLD_UNICODE}
{$ENDIF}

{$IFDEF BOLD_UNICODE}
  {$STRINGCHECKS OFF} // http://www.micro-isv.asia/2008/10/needless-string-checks-with-ensureunicodestring/
{$ENDIF BOLD_UNICODE}

  // Use OXML instead of MSXML for XML operations (faster and can handle larger XML)
{.$DEFINE OXML}

  // Faster Display Queue - Simplified handling of (Most)Prioritized and less iteration of Display List
{$DEFINE BoldQueue_Optimization}

  // When fetching an object which class list is loaded then search there instead of going to db.
{$DEFINE FetchFromClassList}

  // Mechanism to automatically place subscriptions for all derivation code in .inc files
{.$DEFINE NoAutoSubscription}

  // Mechanism for efficient fetching
{.$DEFINE SpanFetch}

  // Turns off query mechanism
{$DEFINE BOLD_NO_QUERIES}

  // Do not allow creation of transient instances of a persistent class.
  // This is rarely used and if explicitly prohibited allows TBoldClassListController to not have to execute AddTransientFromSystem
{.$DEFINE NoTransientInstancesOfPersistentClass}

  // Do not allow adding of objects from different systems to same list
{.$DEFINE AllowCrossSystemLists}

  // Affects EndModify, if the new value is same as Old value mark the object Current instead of modified
{$DEFINE CompareToOldValues}

  // Debug addon to count instances per subclass of TBoldMemoryManagedObject
{.$DEFINE DebugInstanceCounter}

  // Define to use string comparison for all attributes. This may be faster, it uses IBoldStringRepresentable
  // but assumes each value has distinct string representation.
{.$DEFINE StringAttributeComparison}

  // Turns of ObjectSpace transaction mechanism
{.$DEFINE NoObjectSpaceTransactions}

  // ?
{.$DEFINE ACCESSSTATISTICS}

  // Temporarly used for debugging, remove ?
{.$DEFINE ATINDEXDEBUG}

  // ?
{.$DEFINE EXPRESSION_OVERRIDES_DOUBLE_DERIVE}

  // Turns off MayUpdate and MayCommit queries, keep them off unless used.
{$DEFINE NoMayUpdate}

  // USEGLOBALCHARBUFFER is an optimization in BoldHashIndexes.pas
{.$DEFINE USEGLOBALCHARBUFFER}

// Shared Strings brings minimal memory savings, but at cost of performance
{$DEFINE BOLD_DISABLESHAREDSTRINGS}

 // Attracs specific performance monitoring
{.$DEFINE BOLD_PERFORMANCE_COUNTERS}

// Extrenal IDServer used instead of BoldID/Timestamp tables, improves write performance and avoids deadlocks
{$DEFINE IDServer}

// Uses StringBuilder for constructing SQL statements
{$DEFINE RIL}

// All attributes will return emptyValue instead of exception reading nil.
{.$DEFINE NoNilAttributeExceptions}

// Does not allow setting or storing of negative dates (before 1899) or after (2899-12-30)
{$DEFINE NoNegativeDates}

// Converts zero date (1899) to nil when reading from db
{$DEFINE ConvertZeroDateToDateNil}

// DateTime and Date conform to each other
{.$DEFINE DateTimeConformsToDate}

// Allows usage of OCL expressions in constraint messages, the context of expression is the constrained element. Expression has to return a string.
{.$DEFINE OCLConstraintMessages}

// IndexColumn creates an extra Index column in MemberMappingTable
{.$DEFINE IndexColumn}

//Includes BoldJson and BoldObjectRepresentationJson - includes LkJSON v1.07
{$DEFINE BoldJson}

// BoldSystem broadcasts member events - allows getting events from all objects and their members by only placing one subscription on system
{$DEFINE BoldSystemBroadcastMemberEvents}

// NoEnsureEnclosure skips EnsureEnclosure call in UpdateDatabase, use NoEnsureEnclosure if objects passed to UpdateDatabase are already enclosed
{.$DEFINE NoEnsureEnclosure}

// Generates Enum classes for For..In support, available since D2007
{$IFDEF BOLD_DELPHI11_OR_LATER} // D2007+
  {$DEFINE UseBoldListEnumerator}
{$ENDIF}

// if link object is deleted (but not yet saved) and then same two objects on both ends are linked again, then reuse the deleted link object instead of creating new one.
// This effectively avoids a db delete and insert of logically same object but with new id.
// As a side effect this also avoids DetectLinkClassDuplicates in batch mode detecting false duplicate
{$DEFINE ReuseDeletedLinkObjectOnRelink}

{$IFDEF ReuseDeletedLinkObjectOnRelink}
  {$IFNDEF CompareToOldValues}
  ReuseDeletedLinkObjectOnRelink requires CompareToOldValues
  {$ENDIF}
{$ENDIF}

// settings for design package
(*
{$IFNDEF Attracs}
  {$DEFINE NoAutoSubscription}
  {$UNDEF SpanFetch}
  {$UNDEF NoTransientInstancesOfPersistentClass}
  {$UNDEF CompareToOldValues}
  {$DEFINE NoObjectSpaceTransactions}
  {$DEFINE NoMayUpdate}
  {$UNDEF IDServer}
  {$UNDEF NoNegativeDates}
  {$UNDEF ConvertZeroDateToDateNil}
  {$UNDEF BoldSystemBroadcastMemberEvents}
{$ENDIF}
*)
