unit TestIkaria;

interface

uses
  Ikaria, PluggableLogging, SyncObjs, TestFramework;

type
  TLogEntry = class(TObject)
  private
    fLogName: String;
    fDescription: String;
    fSourceRef: Cardinal;
    fSourceDesc: String;
    fSeverity: TSeverityLevel;
    fEventRef: Cardinal;
    fDebugInfo: String;
  public
    constructor Create(LogName: String;
                       Description: String;
                       SourceRef: Cardinal;
                       SourceDesc: String;
                       Severity: TSeverityLevel;
                       EventRef: Cardinal;
                       DebugInfo: String);
    property LogName: String          read fLogName;
    property Description: String      read fDescription;
    property SourceRef: Cardinal      read fSourceRef;
    property SourceDesc: String       read fSourceDesc;
    property Severity: TSeverityLevel read fSeverity;
    property EventRef: Cardinal       read fEventRef;
  end;

  TestTBooleanElement = class(TTestCase)
  private
    B: TBooleanElement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestEquals;
    procedure TestValue;
  end;

  TestTIntegerElement = class(TTestCase)
  private
    I: TIntegerElement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestEquals;
    procedure TestValue;
  end;

  TestTProcessIDElement = class(TTestCase)
  private
    PID: TProcessIDElement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestEquals;
    procedure TestValue;
  end;

  TestTStringElement = class(TTestCase)
  private
    S: TStringElement;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCopy;
    procedure TestEquals;
    procedure TestValue;
  end;

  TestTTuple = class(TTestCase)
  private
    T: TTuple;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddBoolean;
    procedure TestAddInteger;
    procedure TestAddString;
    procedure TestAsString;
    procedure TestAsStringEmptyTuple;
    procedure TestCopy;
    procedure TestEquals;
  end;

  TestTActorMailbox = class(TTestCase)
  private
    Mbox: TActorMailbox;
    M:    TActorMessage;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMessage;
    procedure TestPurge;
  end;

  TestTActor = class(TTestCase)
  private
    E: TEvent;

    procedure CheckLogForCreationMessage;
    procedure CheckLogForExitMessage;
    procedure CheckLogForFreeMessage;
    procedure CheckLogForMessage(Substring: String; Msg: String);
    procedure CheckLogForSendMessage;
    function  FindLogWithDescriptionContaining(Substring: String): TLogEntry;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDyingActorsMakesLogs;
    procedure TestSpawningActorsMakesLogs;
    procedure TestSendMakesLogs;
  end;

implementation

uses
  Contnrs, SysUtils;

type
  TEventTriggeringActor = class(TActor)
  private
    function  Ping(Msg: TActorMessage): Boolean;
    procedure Pong(Msg: TActorMessage);
  protected
    procedure Run; override;
  end;

  TSingleShotActor = class(TActor)
  protected
    procedure Run; override;
  end;

var
  Lock: TCriticalSection;
  Log:  TObjectList;

const
  QuarterSecond = 500;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('Ikaria unit tests');
  Result.AddSuite(TestTBooleanElement.Suite);
  Result.AddSuite(TestTIntegerElement.Suite);
  Result.AddSuite(TestTProcessIDElement.Suite);
  Result.AddSuite(TestTStringElement.Suite);
  Result.AddSuite(TestTTuple.Suite);
  Result.AddSuite(TestTActorMailbox.Suite);
  Result.AddSuite(TestTActor.Suite);
end;

//******************************************************************************
//* TEventTriggeringActor                                                      *
//******************************************************************************
//* TEventTriggeringActor Protected methods ************************************

procedure TEventTriggeringActor.Run;
begin
  Self.Receive(Self.Ping, Self.Pong);
end;

//* TEventTriggeringActor Private methods **************************************

function TEventTriggeringActor.Ping(Msg: TActorMessage): Boolean;
begin
  Result := true;
end;

procedure TEventTriggeringActor.Pong(Msg: TActorMessage);
begin

end;

//******************************************************************************
//* TSingleShotActor                                                           *
//******************************************************************************
//* TSingleShotActor Protected methods *****************************************

procedure TSingleShotActor.Run;
var
  M: TActorMessage;
begin
  M := TActorMessage.Create;
  try
    Self.Send(Self.ParentID, M);
  finally
    M.Free;
  end;
end;

//******************************************************************************
//* TLogEntry                                                                  *
//******************************************************************************
//* TLogEntry Public methods ***************************************************

constructor TLogEntry.Create(LogName: String;
                             Description: String;
                             SourceRef: Cardinal;
                             SourceDesc: String;
                             Severity: TSeverityLevel;
                             EventRef: Cardinal;
                             DebugInfo: String);
begin
  inherited Create;

  Self.fLogName     := LogName;
  Self.fDescription := Description;
  Self.fSourceRef   := SourceRef;
  Self.fSourceDesc  := SourceDesc;
  Self.fSeverity    := Severity;
  Self.fEventRef    := EventRef;
  Self.fDebugInfo   := DebugInfo;
end;

procedure LogEntry(LogName: String;
                   Description: String;
                   SourceRef: Cardinal;
                   SourceDesc: String;
                   Severity: TSeverityLevel;
                   EventRef: Cardinal;
                   DebugInfo: String);
begin
  Lock.Acquire;
  try
    Log.Add(TLogEntry.Create(LogName, Description, SourceRef, SourceDesc, Severity, EventRef, DebugInfo));
  finally
    Lock.Release;
  end;
end;

//******************************************************************************
//* TestTBooleanElement                                                        *
//******************************************************************************
//* TestTBooleanElement Public methods *****************************************

procedure TestTBooleanElement.SetUp;
begin
  inherited SetUp;

  Self.B := TBooleanElement.Create(true);
end;

procedure TestTBooleanElement.TearDown;
begin
  Self.B.Free;

  inherited TearDown;
end;

//* TestTBooleanElement Published methods **************************************

procedure TestTBooleanElement.TestCopy;
var
  C: TBooleanElement;
begin
  C := Self.B.Copy as TBooleanElement;
  try
    CheckEquals(Self.B.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTBooleanElement.TestEquals;
var
  Same:    TBooleanElement;
  Negated: TBooleanElement;
begin
  Same := TBooleanElement.Create(Self.B.Value);
  try
    Check(Same.Equals(Self.B), 'Same <> B');
    Check(Self.B.Equals(Same), 'B <> Same');
  finally
    Same.Free;
  end;

  Negated := TBooleanElement.Create(not Self.B.Value);
  try
    Check(not Negated.Equals(Self.B), 'Negated = B');
    Check(not Self.B.Equals(Negated), 'B = Negated');
  finally
    Negated.Free;
  end;
end;

procedure TestTBooleanElement.TestValue;
begin
  CheckEquals(true, Self.B.Value, 'Value not set in constructor');
end;

//******************************************************************************
//* TestTIntegerElement                                                        *
//******************************************************************************
//* TestTIntegerElement Public methods *****************************************

procedure TestTIntegerElement.SetUp;
begin
  inherited SetUp;

  Self.I := TIntegerElement.Create(42);
end;

procedure TestTIntegerElement.TearDown;
begin
  Self.I.Free;

  inherited TearDown;
end;

//* TestTIntegerElement Published methods **************************************

procedure TestTIntegerElement.TestCopy;
var
  C: TIntegerElement;
begin
  C := Self.I.Copy as TIntegerElement;
  try
    CheckEquals(Self.I.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTIntegerElement.TestEquals;
var
  Same: TIntegerElement;
  Succ: TIntegerElement;
begin
  Same := TIntegerElement.Create(Self.I.Value);
  try
    Check(Same.Equals(Self.I), 'Same <> I');
    Check(Self.I.Equals(Same), 'I <> Same');
  finally
    Same.Free;
  end;

  Succ := TIntegerElement.Create(Self.I.Value + 1);
  try
    Check(not Succ.Equals(Self.I), 'Succ = I');
    Check(not Self.I.Equals(Succ), 'I = Succ');
  finally
    Succ.Free;
  end;
end;

procedure TestTIntegerElement.TestValue;
begin
  CheckEquals(42, Self.I.Value, 'Value not set in constructor');
end;

//******************************************************************************
//* TestTProcessIDElement                                                      *
//******************************************************************************
//* TestTProcessIDElement Public methods ***************************************

procedure TestTProcessIDElement.SetUp;
begin
  inherited SetUp;

  Self.PID := TProcessIDElement.Create('ce39596d-ae9f-442a-8d06-33608840bb96');
end;

procedure TestTProcessIDElement.TearDown;
begin
  Self.PID.Free;

  inherited TearDown;
end;

//* TestTProcessIDElement Published methods ************************************

procedure TestTProcessIDElement.TestCopy;
var
  C: TProcessIDElement;
begin
  C := Self.PID.Copy as TProcessIDElement;
  try
    CheckEquals(Self.PID.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTProcessIDElement.TestEquals;
var
  Other: TProcessIDElement;
  Same:  TProcessIDElement;
begin
  Other := TProcessIDElement.Create(Self.PID.Value + '1');
  try
    Check(not Other.Equals(Self.PID), 'Other = PID');
    Check(not Self.PID.Equals(Other), 'PID = Other');
  finally
    Other.Free;
  end;

  Same := TProcessIDElement.Create(Self.PID.Value);
  try
    Check(Same.Equals(Self.PID), 'Same <> PID');
    Check(Self.PID.Equals(Same), 'PID <> Same');
  finally
    Same.Free;
  end;
end;

procedure TestTProcessIDElement.TestValue;
begin
  CheckEquals('ce39596d-ae9f-442a-8d06-33608840bb96', Self.PID.Value, 'Value not set in constructor');
end;

//******************************************************************************
//* TestTStringElement                                                         *
//******************************************************************************
//* TestTStringElement Public methods ******************************************

procedure TestTStringElement.SetUp;
begin
  inherited SetUp;

  Self.S := TStringElement.Create('hello');
end;

procedure TestTStringElement.TearDown;
begin
  Self.S.Free;

  inherited TearDown;
end;

//* TestTStringElement Published methods ***************************************

procedure TestTStringElement.TestCopy;
var
  C: TStringElement;
begin
  C := Self.S.Copy as TStringElement;
  try
    CheckEquals(Self.S.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTStringElement.TestEquals;
var
  Other: TStringElement;
  Same:  TStringElement;
begin
  Other := TStringElement.Create(Self.S.Value + '1');
  try
    Check(not Other.Equals(Self.S), 'Other = S');
    Check(not Self.S.Equals(Other), 'S = Other');
  finally
    Other.Free;
  end;

  Same := TStringElement.Create(Self.S.Value);
  try
    Check(Same.Equals(Self.S), 'Same <> S');
    Check(Self.S.Equals(Same), 'S <> Same');
  finally
    Same.Free;
  end;
end;

procedure TestTStringElement.TestValue;
begin
  CheckEquals('hello', Self.S.Value, 'Value not set in constructor');
end;

//******************************************************************************
//* TestTTuple                                                                 *
//******************************************************************************
//* TestTTuple Public methods **************************************************

procedure TestTTuple.SetUp;
begin
  inherited SetUp;

  Self.T := TTuple.Create;
end;

procedure TestTTuple.TearDown;
begin
  Self.T.Free;

  inherited TearDown;
end;

//* TestTTuple Published methods ***********************************************

procedure TestTTuple.TestAdd;
var
  B: TBooleanElement;
  I: TIntegerElement;
  S: TStringElement;
begin
  CheckEquals(0, Self.T.Count, 'Sanity check: T should have no elements');

  B := TBooleanElement.Create(true);
  try
    Self.T.Add(B);
    CheckEquals(1, Self.T.Count, 'Boolean not added');
    CheckEquals(B.ClassType, Self.T[0].ClassType, 'Element of wrong type added (Boolean)');
  finally
    B.Free;
  end;

  I := TIntegerElement.Create(42);
  try
    Self.T.Add(I);
    CheckEquals(2, Self.T.Count, 'Integer not added');
    CheckEquals(B.ClassType, Self.T[1].ClassType, 'Element of wrong type added (Integer)');
  finally
    I.Free;
  end;

  S := TStringElement.Create('hello');
  try
    Self.T.Add(S);
    CheckEquals(3, Self.T.Count, 'String not added');
    CheckEquals(B.ClassType, Self.T[2].ClassType, 'Element of wrong type added (String)');
  finally
    S.Free;
  end;
end;

procedure TestTTuple.TestAddBoolean;
begin
  Self.T.AddBoolean(true);
  Self.T.AddBoolean(false);

  CheckEquals(2, Self.T.Count, 'Incorrect number of elements');
  CheckEquals(TBooleanElement.ClassName, Self.T[0].ClassName, 'First element type');
  CheckEquals(TBooleanElement.ClassName, Self.T[1].ClassName, 'Second element type');

  CheckEquals(true, TBooleanElement(Self.T[0]).Value, 'First element value');
  CheckEquals(false, TBooleanElement(Self.T[1]).Value, 'Second element value');
end;

procedure TestTTuple.TestAddInteger;
begin
  Self.T.AddInteger(0);
  Self.T.AddInteger(1);

  CheckEquals(2, Self.T.Count, 'Incorrect number of elements');
  CheckEquals(TIntegerElement.ClassName, Self.T[0].ClassName, 'First element type');
  CheckEquals(TIntegerElement.ClassName, Self.T[1].ClassName, 'Second element type');

  CheckEquals(0, TIntegerElement(Self.T[0]).Value, 'First element value');
  CheckEquals(1, TIntegerElement(Self.T[1]).Value, 'Second element value');
end;

procedure TestTTuple.TestAddString;
begin
  Self.T.AddString('ping');
  Self.T.AddString('pong');

  CheckEquals(2, Self.T.Count, 'Incorrect number of elements');
  CheckEquals(TStringElement.ClassName, Self.T[0].ClassName, 'First element type');
  CheckEquals(TStringElement.ClassName, Self.T[1].ClassName, 'Second element type');

  CheckEquals('ping', TStringElement(Self.T[0]).Value, 'First element value');
  CheckEquals('pong', TStringElement(Self.T[1]).Value, 'Second element value');
end;

procedure TestTTuple.TestAsString;
begin
  Self.T.AddBoolean(true);
  Self.T.AddInteger(42);
  Self.T.AddString('hello');

  CheckEquals(Format('(%s %s %s)', [Self.T[0].AsString, Self.T[1].AsString, Self.T[2].AsString]),
              Self.T.AsString,
              'AsString');
end;

procedure TestTTuple.TestAsStringEmptyTuple;
begin
  CheckEquals('()', Self.T.AsString, 'AsString on empty tuple');
end;

procedure TestTTuple.TestCopy;
var
  C: TTuple;
  I: Integer;
begin
  C := Self.T.Copy;
  try
    CheckEquals(Self.T.Count, C.Count, 'Incorrect number of elements');

    for I := 0 to Self.T.Count - 1 do
      Check(Self.T[I].Equals(C[I]), Format('%dth element doesn''t match', [I]));
  finally
    C.Free;
  end;
end;

procedure TestTTuple.TestEquals;
var
  Different: TTuple;
  Same: TTuple;
begin
  Different := TTuple.Create;
  try
    Check(not Different.Equals(Self.T), 'Different = T');
    Check(not Self.T.Equals(Different), 'T = Different');
  finally
    Different.Free;
  end;

  Same := Self.T.Copy;
  try
    Check(Same.Equals(Self.T), 'Same <> T');
    Check(Self.T.Equals(Same), 'T <> Same');
  finally
    Same.Free;
  end;         
end;

//******************************************************************************
//* TestTActorMailbox                                                          *
//******************************************************************************
//* TestTActorMailbox Public methods *******************************************

procedure TestTActorMailbox.SetUp;
begin
  inherited SetUp;

  Self.Mbox := TActorMailbox.Create;
  Self.M    := TActorMessage.Create;
end;

procedure TestTActorMailbox.TearDown;
begin
  Self.M.Free;
  Self.Mbox.Free;

  inherited TearDown;
end;

procedure TestTActorMailbox.TestAddMessage;
begin
  Check(Self.MBox.IsEmpty, 'New mailbox isn''t empty');
  Self.MBox.AddMessage(M);
  Check(not Self.MBox.IsEmpty, 'Mailbox didn''t store the message');
end;

procedure TestTActorMailbox.TestPurge;
begin
  Self.MBox.AddMessage(M);
  Self.MBox.Purge;
  Check(Self.MBox.IsEmpty, 'Mailbox didn''t purge the message');
end;

//******************************************************************************
//* TestTActor                                                                 *
//******************************************************************************
//* TestTActor Public methods **************************************************

procedure TestTActor.SetUp;
begin
  inherited SetUp;

  Self.E := TSimpleEvent.Create;
end;

procedure TestTActor.TearDown;
begin
  Self.E.Free;

  inherited TearDown;
end;

//* TestTActor Private methods *************************************************

procedure TestTActor.CheckLogForCreationMessage;
begin
  CheckLogForMessage('created', 'No log of an Actor creation');
end;

procedure TestTActor.CheckLogForExitMessage;
begin
  CheckLogForMessage('exited', 'No log of an Actor exit');
end;

procedure TestTActor.CheckLogForFreeMessage;
begin
  CheckLogForMessage('freed', 'No log of an Actor freeing');
end;

procedure TestTActor.CheckLogForMessage(Substring: String; Msg: String);
begin
  Lock.Acquire;
  try
    Check(Assigned(FindLogWithDescriptionContaining(Substring)), Msg);
  finally
    Lock.Release;
  end;
end;

procedure TestTActor.CheckLogForSendMessage;
begin
  CheckLogForMessage('sent', 'No log of a message send');
end;

function TestTActor.FindLogWithDescriptionContaining(Substring: String): TLogEntry;
var
  E: TLogEntry;
  I: Integer;
begin
  // PRECONDITION: You've Acquired Lock.
  Result := nil;

  for I := 0 to Log.Count - 1 do begin
    E := Log[I] as TLogEntry;
    if (Pos(Substring, E.Description) > 0) then begin
      Result := E;
      Break;
    end;
  end;
end;

//* TestTActor Published methods ***********************************************

procedure TestTActor.TestDyingActorsMakesLogs;
begin
  TActor.Create('');

  CheckLogForExitMessage;
  CheckLogForFreeMessage;
end;

procedure TestTActor.TestSpawningActorsMakesLogs;
begin
  TActor.Create('');

  CheckLogForCreationMessage;
end;

procedure TestTActor.TestSendMakesLogs;
begin
  TSingleShotActor.Create(TActor.RootActor);

  CheckLogForSendMessage;
end;

initialization;
  PluggableLogging.Logger := LogEntry;
  Lock := TCriticalSection.Create;
  Log  := TObjectList.Create(true);

  RegisterTest('Ikaria', Suite);
end.
