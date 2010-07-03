{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestIkaria;

interface

uses
  Ikaria, SyncObjs, SysUtils, TestFramework;

type
  TLogEntry = class(TObject)
  private
    fLogName: String;
    fDescription: String;
    fSourceRef: Cardinal;
    fSourceDesc: String;
    fSeverity: Cardinal;
    fEventRef: Cardinal;
    fDebugInfo: String;
  public
    constructor Create(LogName: String;
                       Description: String;
                       SourceRef: Cardinal;
                       SourceDesc: String;
                       Severity: Cardinal;
                       EventRef: Cardinal;
                       DebugInfo: String);

    property LogName: String     read fLogName;
    property Description: String read fDescription;
    property SourceRef: Cardinal read fSourceRef;
    property SourceDesc: String  read fSourceDesc;
    property Severity: Cardinal  read fSeverity;
    property EventRef: Cardinal  read fEventRef;
  end;

  TestTBooleanTerm = class(TTestCase)
  private
    B: TBooleanTerm;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
    procedure TestEquals;
    procedure TestIsBoolean;
    procedure TestIsInteger;
    procedure TestIsProcessID;
    procedure TestIsString;
    procedure TestIsTuple;
    procedure TestValue;
  end;

  TestTIntegerTerm = class(TTestCase)
  private
    I: TIntegerTerm;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
    procedure TestEquals;
    procedure TestIsBoolean;
    procedure TestIsInteger;
    procedure TestIsProcessID;
    procedure TestIsString;
    procedure TestIsTuple;
    procedure TestValue;
  end;

  TestTProcessIDTerm = class(TTestCase)
  private
    PID: TProcessIDTerm;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
    procedure TestEquals;
    procedure TestIsBoolean;
    procedure TestIsInteger;
    procedure TestIsProcessID;
    procedure TestIsString;
    procedure TestIsTuple;
    procedure TestValue;
  end;

  TestTStringTerm = class(TTestCase)
  private
    S: TStringTerm;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAsString;
    procedure TestCopy;
    procedure TestEquals;
    procedure TestIsBoolean;
    procedure TestIsInteger;
    procedure TestIsProcessID;
    procedure TestIsString;
    procedure TestIsTuple;
    procedure TestValue;
  end;

  TestTTuple = class(TTestCase)
  private
    T: TTuple;

    procedure ConstructTree(T: TTuple);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddBoolean;
    procedure TestAddInteger;
    procedure TestAddProcessID;
    procedure TestAddString;
    procedure TestAsString;
    procedure TestAsStringEmptyTuple;
    procedure TestAsStringTree;
    procedure TestCopy;
    procedure TestEquals;
    procedure TestEqualsOnTree;
    procedure TestRouteTo;
    procedure TestRouteToPreservesClass;
    procedure TestIsBoolean;
    procedure TestIsInteger;
    procedure TestIsProcessID;
    procedure TestIsString;
    procedure TestIsTuple;
  end;

  TestTActorMailbox = class(TTestCase)
  private
    Actor:       TActorInterface;
    Environment: TActorEnvironment;
    Mbox:        TActorMailbox;
    M:           TActorMessage;
    NewMsgs:     Boolean;

    function  AllFinder(Msg: TTuple): Boolean;
    procedure CheckFound(ExpectedTag: String;
                         Mbox: TActorMailbox;
                         Condition: TMessageFinder;
                         MsgPrefix: String);
    function  NullFinder(Msg: TTuple): Boolean;
    procedure RegisterMessageArrival(Sender: TObject);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddMessageAndIsEmpty;
    procedure TestFindMessageEmptyMailbox;
    procedure TestFindMessageReturnsFirstMatch;
    procedure TestFindMessageStoredMessagesInSaveQueue;
    procedure TestNotifyOfNewMessages;
    procedure TestPurge;
    procedure TestTimeoutRestoresSaveQueueMessages;
  end;

  TestTActorInterface = class(TTestCase)
  private
    BarName:      String;
    Environment:  TActorEnvironment;
    FooName:      String;
    Intf:         TActorInterface;
    ReceivedABar: Boolean;
    ReceivedAFoo: Boolean;
    TimedOut:     Boolean;

    procedure ActOnBarMsg(Msg: TTuple);
    procedure ActOnFooMsg(Msg: TTuple);
    function  CreateFooMsg: TTuple;
    function  CreateMsgNamed(Name: String): TTuple;
    function  RecogniseBarMsg(Msg: TTuple): Boolean;
    function  RecogniseFooMsg(Msg: TTuple): Boolean;
    procedure Timeout;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestReceive;
    procedure TestReceiveReturnsOnlyOnAMatch;
    procedure TestReceiveTwoMessages;
    procedure TestReceiveWithTimeout;
    procedure TestReceiveWithZeroTimeout;
  end;

  TestTActorMessageTable = class(TTestCase)
  private
    FirstActionWorked:     Boolean;
    SecondActionWorked:    Boolean;
    FirstConditionWorked:  Boolean;
    SecondConditionWorked: Boolean;
    T:                     TActorMessageTable;
    TestMsg:               TTuple;

    procedure FirstAction(Msg: TTuple);
    function  FirstTestCondition(Msg: TTuple): Boolean;
    procedure SecondAction(Msg: TTuple);
    function  SecondTestCondition(Msg: TTuple): Boolean;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAddAndCount;
    procedure TestAccessors;
  end;

  TActorTestCase = class(TTestCase)
  private
    ActorExited: Boolean;
    ExitEvent:   TEvent;
    LastSentMsg: TActorMessage;
    MsgEvent:    TEvent;

    procedure NotifyOfExit(PID: String; Reason: TTuple);
    procedure StoreLastSentMessage(Sender, Target: TProcessID; Msg: TActorMessage);
  protected
    Environment: TActorEnvironment;

    procedure CheckLastMsgEquals(Expected: TTuple; Msg: String);
    function  CopyLastSentMsg: TActorMessage;
    procedure WaitFor(E: TEvent; Timeout: Cardinal; Msg: String);
    procedure WaitForExit(Timeout: Cardinal = 1000);
    procedure WaitForMsg(Timeout: Cardinal = 1000; OptionalMsg: String = '');
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TestTActor = class(TActorTestCase)
  private
    E:          TEvent;
    ExitRecvd:  Boolean;
    ExitReason: String;
    TestMsg:    TTuple;
    TimedOut:   Boolean;

    function  MatchExit(Msg: TTuple): Boolean;
    procedure RecordExit(Msg: TTuple);
    procedure Timeout;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestActorAcceptsExitMessage;
    procedure TestActorAcceptsKillMessage;
    procedure TestActorNotifiesLinkSetOfAbnormalExit;
    procedure TestActorNotifiesLinkSetOfExit;
    procedure TestActorsCanReceiveMessages;
  end;

  TestActorFunctions = class(TActorTestCase)
  private
    TestMsg:    TMessageTuple;
    ThunkEvent: TEvent;
    ThunkRan:   Boolean;

    procedure CheckMessageNameEquals(Expected, Received: TTuple; Msg: String);
    procedure TestThunk;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestRPC;
    procedure TestSpawnThunk;
  end;

const
  TenthOfASecond = 100;
  TestName       = 'test';

implementation

uses
  Classes, Contnrs;

type
  TEchoActor = class(TActor)
  private
    function  FindAllExceptKill(Msg: TTuple): Boolean;
    procedure EchoMessage(Msg: TTuple);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  end;

  TErrorActor = class(TActor)
  public
    class function ExpectedReason: String;
    class function ExceptionType: ExceptClass;
    class function ExceptionReason: String;

    procedure Step; override;
  end;

  TSingleShotActor = class(TActor)
  public
    procedure Step; override;
  end;

var
  Lock:            TCriticalSection;
  Log:             TObjectList;
  RunningTestCase: TActorTestCase;
  TestLock:        TCriticalSection;

const
  QuarterSecond = 500;

procedure LogEntry(LogName: String;
                   Description: String;
                   SourceRef: Cardinal;
                   SourceDesc: String;
                   Severity: Cardinal;
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

function CreateLogFile(Lock: TCriticalSection; Log: TObjectList): TStrings;
var
  I: Integer;
begin
  Lock.Acquire;
  try
     Result := TStringList.Create;

     for I := 0 to Log.Count - 1 do begin
       Result.Add(TLogEntry(Log[I]).Description);
     end;
  finally
    Lock.Release;
  end;
end;

procedure SaveLog(Lock: TCriticalSection; Log: TObjectList);
var
  Filename: String;
  S: TStrings;
begin
  Lock.Acquire;
  try
    if (Log.Count > 0) then
      Filename := TLogEntry(Log[0]).LogName + '.txt'
    else
      Filename := 'debug.txt';

    S := CreateLogFile(Lock, Log);
    try
      S.SaveToFile(Filename);
    finally
      S.Free;
    end;
  finally
    Lock.Release;
  end;
end;

procedure PurgeLog;
begin
  Lock.Acquire;
  try
    Log.Clear;
  finally
    Lock.Release;
  end;
end;

procedure NotifyOfActorCreation(ActorType, PID, UnusedEvent: String);
begin
  LogEntry('debug', Format(ActorCreatedMsg, [PID, ActorType]), 0, 'Ikaria', 0, 0, '');
end;

procedure NotifyOfActorExit(PID: String; ExitReason: TTuple);
begin
  LogEntry('debug', Format(ActorExitedMsg, [PID, ExitReason.AsString]), 0, 'Ikaria', 0, 0, '');

  TestLock.Acquire;
  try
    if Assigned(RunningTestCase) then
      RunningTestCase.NotifyOfExit(PID, ExitReason);
  finally
    TestLock.Release;
  end;
end;

procedure StoreLastSentMessageInTestCase(Sender, Target: TProcessID; Msg: TActorMessage);
begin
  TestLock.Acquire;
  try
    LogEntry('debug', Format(MessageSentMsg, [Sender, Target, Msg.AsString]), 0, 'Ikaria', 0, 0, '');
    if Assigned(RunningTestCase) then
      RunningTestCase.StoreLastSentMessage(Sender, Target, Msg);
  finally
    TestLock.Release;
  end;
end;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('Ikaria unit tests');
  Result.AddSuite(TestTBooleanTerm.Suite);
  Result.AddSuite(TestTIntegerTerm.Suite);
  Result.AddSuite(TestTProcessIDTerm.Suite);
  Result.AddSuite(TestTStringTerm.Suite);
  Result.AddSuite(TestTTuple.Suite);
  Result.AddSuite(TestTActorMailbox.Suite);
  Result.AddSuite(TestTActorInterface.Suite);
  Result.AddSuite(TestTActorMessageTable.Suite);
  Result.AddSuite(TestTActor.Suite);
  Result.AddSuite(TestActorFunctions.Suite);
end;

//******************************************************************************
//* TEchoActor                                                                 *
//******************************************************************************
//* TEchoActor Protected methods ***********************************************

procedure TEchoActor.RegisterActions(Table: TActorMessageTable);
begin
  inherited RegisterActions(Table);

  Table.Add(Self.FindAllExceptKill, Self.EchoMessage);
end;

//* TEchoActor Private methods *************************************************

function TEchoActor.FindAllExceptKill(Msg: TTuple): Boolean;
begin
  Result := not Self.FindKill(Msg);
end;

procedure TEchoActor.EchoMessage(Msg: TTuple);
begin
  Self.Send(TProcessIDTerm(Msg[1]).Value, Msg);
end;

//******************************************************************************
//* TErrorActor                                                                *
//******************************************************************************
//* TErrorActor Public methods *************************************************

class function TErrorActor.ExpectedReason: String;
begin
  Result := Format(ExitReasonException, [Self.ExceptionType.ClassName, Self.ExceptionReason]);
end;

class function TErrorActor.ExceptionType: ExceptClass;
begin
  Result := Exception;
end;

class function TErrorActor.ExceptionReason: String;
begin
  Result := 'Foo';
end;

procedure TErrorActor.Step;
begin
  Self.Terminate;
  raise Self.ExceptionType.Create(Self.ExceptionReason);
end;

//******************************************************************************
//* TSingleShotActor                                                           *
//******************************************************************************
//* TSingleShotActor Public methods ********************************************

procedure TSingleShotActor.Step;
begin
  Self.Send(Self.ParentID, 'ping');
  Self.Terminate;
end;

//******************************************************************************
//* TLogEntry                                                                  *
//******************************************************************************
//* TLogEntry Public methods ***************************************************

constructor TLogEntry.Create(LogName: String;
                             Description: String;
                             SourceRef: Cardinal;
                             SourceDesc: String;
                             Severity: Cardinal;
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

//******************************************************************************
//* TestTBooleanTerm                                                           *
//******************************************************************************
//* TestTBooleanTerm Public methods ********************************************

procedure TestTBooleanTerm.SetUp;
begin
  inherited SetUp;

  Self.B := TBooleanTerm.Create(true);
end;

procedure TestTBooleanTerm.TearDown;
begin
  Self.B.Free;

  inherited TearDown;
end;

//* TestTBooleanTerm Published methods *****************************************

procedure TestTBooleanTerm.TestAsString;
begin
  CheckEquals(BoolStrs[Self.B.Value], Self.B.AsString, 'BooleanElement.AsString');
end;

procedure TestTBooleanTerm.TestCopy;
var
  C: TBooleanTerm;
begin
  C := Self.B.Copy as TBooleanTerm;
  try
    CheckEquals(Self.B.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTBooleanTerm.TestEquals;
var
  Same:    TBooleanTerm;
  Negated: TBooleanTerm;
begin
  Same := TBooleanTerm.Create(Self.B.Value);
  try
    Check(Same.Equals(Self.B), 'Same <> B');
    Check(Self.B.Equals(Same), 'B <> Same');
  finally
    Same.Free;
  end;

  Negated := TBooleanTerm.Create(not Self.B.Value);
  try
    Check(not Negated.Equals(Self.B), 'Negated = B');
    Check(not Self.B.Equals(Negated), 'B = Negated');
  finally
    Negated.Free;
  end;
end;

procedure TestTBooleanTerm.TestIsBoolean;
begin
  Check(Self.B.IsBoolean, 'Element not marked as a Boolean');
end;

procedure TestTBooleanTerm.TestIsInteger;
begin
  Check(not Self.B.IsInteger, 'Element marked as an Integer');
end;

procedure TestTBooleanTerm.TestIsProcessID;
begin
  Check(not Self.B.IsProcessID, 'Element marked as a ProcessID');
end;

procedure TestTBooleanTerm.TestIsString;
begin
  Check(not Self.B.IsString, 'Element marked as a String');
end;

procedure TestTBooleanTerm.TestIsTuple;
begin
  Check(not Self.B.IsTuple, 'Element marked as a Tuple');
end;

procedure TestTBooleanTerm.TestValue;
begin
  CheckEquals(true, Self.B.Value, 'Value not set in constructor');
end;

//******************************************************************************
//* TestTIntegerTerm                                                           *
//******************************************************************************
//* TestTIntegerTerm Public methods ********************************************

procedure TestTIntegerTerm.SetUp;
begin
  inherited SetUp;

  Self.I := TIntegerTerm.Create(42);
end;

procedure TestTIntegerTerm.TearDown;
begin
  Self.I.Free;

  inherited TearDown;
end;

//* TestTIntegerTerm Published methods *****************************************

procedure TestTIntegerTerm.TestAsString;
begin
  CheckEquals(IntToStr(Self.I.Value), Self.I.AsString, 'IntegerElement.AsString');
end;

procedure TestTIntegerTerm.TestCopy;
var
  C: TIntegerTerm;
begin
  C := Self.I.Copy as TIntegerTerm;
  try
    CheckEquals(Self.I.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTIntegerTerm.TestEquals;
var
  Same: TIntegerTerm;
  Succ: TIntegerTerm;
begin
  Same := TIntegerTerm.Create(Self.I.Value);
  try
    Check(Same.Equals(Self.I), 'Same <> I');
    Check(Self.I.Equals(Same), 'I <> Same');
  finally
    Same.Free;
  end;

  Succ := TIntegerTerm.Create(Self.I.Value + 1);
  try
    Check(not Succ.Equals(Self.I), 'Succ = I');
    Check(not Self.I.Equals(Succ), 'I = Succ');
  finally
    Succ.Free;
  end;
end;

procedure TestTIntegerTerm.TestIsBoolean;
begin
  Check(not Self.I.IsBoolean, 'Element marked as a Boolean');
end;

procedure TestTIntegerTerm.TestIsInteger;
begin
  Check(Self.I.IsInteger, 'Element not marked as an Integer');
end;

procedure TestTIntegerTerm.TestIsProcessID;
begin
  Check(not Self.I.IsProcessID, 'Element marked as a ProcessID');
end;

procedure TestTIntegerTerm.TestIsString;
begin
  Check(not Self.I.IsString, 'Element marked as a String');
end;

procedure TestTIntegerTerm.TestIsTuple;
begin
  Check(not Self.I.IsTuple, 'Element marked as a Tuple');
end;

procedure TestTIntegerTerm.TestValue;
begin
  CheckEquals(42, Self.I.Value, 'Value not set in constructor');
end;

//******************************************************************************
//* TestTProcessIDTerm                                                         *
//******************************************************************************
//* TestTProcessIDTerm Public methods ******************************************

procedure TestTProcessIDTerm.SetUp;
begin
  inherited SetUp;

  Self.PID := TProcessIDTerm.Create('ce39596d-ae9f-442a-8d06-33608840bb96');
end;

procedure TestTProcessIDTerm.TearDown;
begin
  Self.PID.Free;

  inherited TearDown;
end;

//* TestTProcessIDTerm Published methods ***************************************

procedure TestTProcessIDTerm.TestAsString;
begin
  CheckEquals(Format('{%s}', [Self.PID.Value]), Self.PID.AsString, 'ProcessIDElement.AsString');
end;

procedure TestTProcessIDTerm.TestCopy;
var
  C: TProcessIDTerm;
begin
  C := Self.PID.Copy as TProcessIDTerm;
  try
    CheckEquals(Self.PID.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTProcessIDTerm.TestEquals;
var
  Other: TProcessIDTerm;
  Same:  TProcessIDTerm;
begin
  Other := TProcessIDTerm.Create(Self.PID.Value + '1');
  try
    Check(not Other.Equals(Self.PID), 'Other = PID');
    Check(not Self.PID.Equals(Other), 'PID = Other');
  finally
    Other.Free;
  end;

  Same := TProcessIDTerm.Create(Self.PID.Value);
  try
    Check(Same.Equals(Self.PID), 'Same <> PID');
    Check(Self.PID.Equals(Same), 'PID <> Same');
  finally
    Same.Free;
  end;
end;

procedure TestTProcessIDTerm.TestIsBoolean;
begin
  Check(not Self.PID.IsBoolean, 'Element marked as a Boolean');
end;

procedure TestTProcessIDTerm.TestIsInteger;
begin
  Check(not Self.PID.IsInteger, 'Element marked as an Integer');
end;

procedure TestTProcessIDTerm.TestIsProcessID;
begin
  Check(Self.PID.IsProcessID, 'Element not marked as a ProcessID');
end;

procedure TestTProcessIDTerm.TestIsString;
begin
  Check(not Self.PID.IsString, 'Element marked as a String');
end;

procedure TestTProcessIDTerm.TestIsTuple;
begin
  Check(not Self.PID.IsTuple, 'Element marked as a Tuple');
end;

procedure TestTProcessIDTerm.TestValue;
begin
  CheckEquals('ce39596d-ae9f-442a-8d06-33608840bb96', Self.PID.Value, 'Value not set in constructor');
end;

//******************************************************************************
//* TestTStringTerm                                                            *
//******************************************************************************
//* TestTStringTerm Public methods *********************************************

procedure TestTStringTerm.SetUp;
begin
  inherited SetUp;

  Self.S := TStringTerm.Create('hello');
end;

procedure TestTStringTerm.TearDown;
begin
  Self.S.Free;

  inherited TearDown;
end;

//* TestTStringTerm Published methods ******************************************

procedure TestTStringTerm.TestAsString;
begin
  CheckEquals(Format('"%s"', [Self.S.Value]), Self.S.AsString, 'StringElement.AsString');
end;

procedure TestTStringTerm.TestCopy;
var
  C: TStringTerm;
begin
  C := Self.S.Copy as TStringTerm;
  try
    CheckEquals(Self.S.Value, C.Value, 'Value not set');
  finally
    C.Free;
  end;
end;

procedure TestTStringTerm.TestEquals;
var
  Other: TStringTerm;
  Same:  TStringTerm;
begin
  Other := TStringTerm.Create(Self.S.Value + '1');
  try
    Check(not Other.Equals(Self.S), 'Other = S');
    Check(not Self.S.Equals(Other), 'S = Other');
  finally
    Other.Free;
  end;

  Same := TStringTerm.Create(Self.S.Value);
  try
    Check(Same.Equals(Self.S), 'Same <> S');
    Check(Self.S.Equals(Same), 'S <> Same');
  finally
    Same.Free;
  end;
end;

procedure TestTStringTerm.TestIsBoolean;
begin
  Check(not Self.S.IsBoolean, 'Element marked as a Boolean');
end;

procedure TestTStringTerm.TestIsInteger;
begin
  Check(not Self.S.IsInteger, 'Element marked as an Integer');
end;

procedure TestTStringTerm.TestIsProcessID;
begin
  Check(not Self.S.IsProcessID, 'Element marked as a ProcessID');
end;

procedure TestTStringTerm.TestIsString;
begin
  Check(Self.S.IsString, 'Element not marked as a String');
end;

procedure TestTStringTerm.TestIsTuple;
begin
  Check(not Self.S.IsTuple, 'Element marked as a Tuple');
end;

procedure TestTStringTerm.TestValue;
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

//* TestTTuple Private methods *************************************************

procedure TestTTuple.ConstructTree(T: TTuple);
var
  Subtree: TTuple;
begin
  // As an S-expression, this is '("tree" ("leaf"))
  Subtree := TTuple.Create;
  try
    T.AddString('tree');
    Subtree.AddString('leaf');
    T.Add(Subtree);
  finally
    Subtree.Free;
  end;
end;

//* TestTTuple Published methods ***********************************************

procedure TestTTuple.TestAdd;
var
  B: TBooleanTerm;
  I: TIntegerTerm;
  S: TStringTerm;
begin
  CheckEquals(0, Self.T.Count, 'Sanity check: T should have no elements');

  B := TBooleanTerm.Create(true);
  try
    Self.T.Add(B);
    CheckEquals(1, Self.T.Count, 'Boolean not added');
    CheckEquals(B.ClassType, Self.T[0].ClassType, 'Element of wrong type added (Boolean)');
    Check(B <> Self.T[0], 'Element not copied (Boolean)');
  finally
    B.Free;
  end;

  I := TIntegerTerm.Create(42);
  try
    Self.T.Add(I);
    CheckEquals(2, Self.T.Count, 'Integer not added');
    CheckEquals(I.ClassType, Self.T[1].ClassType, 'Element of wrong type added (Integer)');
    Check(I <> Self.T[1], 'Element not copied (Integer)');
  finally
    I.Free;
  end;

  S := TStringTerm.Create('hello');
  try
    Self.T.Add(S);
    CheckEquals(3, Self.T.Count, 'String not added');
    CheckEquals(S.ClassType, Self.T[2].ClassType, 'Element of wrong type added (String)');
    Check(S <> Self.T[2], 'Element not copied (String)');
  finally
    S.Free;
  end;
end;

procedure TestTTuple.TestAddBoolean;
begin
  Self.T.AddBoolean(true);
  Self.T.AddBoolean(false);

  CheckEquals(2, Self.T.Count, 'Incorrect number of elements');
  CheckEquals(TBooleanTerm.ClassName, Self.T[0].ClassName, 'First element type');
  CheckEquals(TBooleanTerm.ClassName, Self.T[1].ClassName, 'Second element type');

  CheckEquals(true, TBooleanTerm(Self.T[0]).Value, 'First element value');
  CheckEquals(false, TBooleanTerm(Self.T[1]).Value, 'Second element value');
end;

procedure TestTTuple.TestAddInteger;
begin
  Self.T.AddInteger(0);
  Self.T.AddInteger(1);

  CheckEquals(2, Self.T.Count, 'Incorrect number of elements');
  CheckEquals(TIntegerTerm.ClassName, Self.T[0].ClassName, 'First element type');
  CheckEquals(TIntegerTerm.ClassName, Self.T[1].ClassName, 'Second element type');

  CheckEquals(0, TIntegerTerm(Self.T[0]).Value, 'First element value');
  CheckEquals(1, TIntegerTerm(Self.T[1]).Value, 'Second element value');
end;

procedure TestTTuple.TestAddProcessID;
begin
  Self.T.AddProcessID('dd3e05ec-60a4-4f57-9c3e-f8897b66497b');
  Self.T.AddProcessID('04aa000f-edb4-4b58-9f9d-c9b1138d485f');

  CheckEquals(2, Self.T.Count, 'Incorrect number of elements');
  CheckEquals(TProcessIDTerm.ClassName, Self.T[0].ClassName, 'First element type');
  CheckEquals(TProcessIDTerm.ClassName, Self.T[1].ClassName, 'Second element type');

  CheckEquals('dd3e05ec-60a4-4f57-9c3e-f8897b66497b', TProcessIDTerm(Self.T[0]).Value, 'First element value');
  CheckEquals('04aa000f-edb4-4b58-9f9d-c9b1138d485f', TProcessIDTerm(Self.T[1]).Value, 'Second element value');
end;

procedure TestTTuple.TestAddString;
begin
  Self.T.AddString('ping');
  Self.T.AddString('pong');

  CheckEquals(2, Self.T.Count, 'Incorrect number of elements');
  CheckEquals(TStringTerm.ClassName, Self.T[0].ClassName, 'First element type');
  CheckEquals(TStringTerm.ClassName, Self.T[1].ClassName, 'Second element type');

  CheckEquals('ping', TStringTerm(Self.T[0]).Value, 'First element value');
  CheckEquals('pong', TStringTerm(Self.T[1]).Value, 'Second element value');
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

procedure TestTTuple.TestAsStringTree;
begin
  Self.ConstructTree(Self.T);

  CheckEquals('("tree" ("leaf"))', Self.T.AsString, 'AsString on tree');
end;

procedure TestTTuple.TestCopy;
var
  C: TTuple;
  I: Integer;
begin
  C := Self.T.Copy as TTuple;
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
  Same:      TTuple;
begin
  Different := TTuple.Create;
  try
    Different.AddInteger(999);

    Check(not Different.Equals(Self.T), 'Different = T');
    Check(not Self.T.Equals(Different), 'T = Different');
  finally
    Different.Free;
  end;

  Same := Self.T.Copy as TTuple;
  try
    Check(Same.Equals(Self.T), 'Same <> T');
    Check(Self.T.Equals(Same), 'T <> Same');
  finally
    Same.Free;
  end;
end;

procedure TestTTuple.TestEqualsOnTree;
var
  Different: TTuple;
  Same:      TTuple;
begin
  Self.ConstructTree(Self.T);

  Different := TTuple.Create;
  try
    Different.AddInteger(999);

    Check(not Different.Equals(Self.T), 'Different = T');
    Check(not Self.T.Equals(Different), 'T = Different');
  finally
    Different.Free;
  end;

  Same := TTuple.Create;
  try
    Self.ConstructTree(Same);

    Check(Same.Equals(Self.T), 'Same <> T');
    Check(Self.T.Equals(Same), 'T <> Same');
  finally
    Same.Free;
  end;
end;

procedure TestTTuple.TestRouteTo;
const
  OldPID: TProcessID = 'old-pid';
  NewPID: TProcessID = 'new-pid';
var
  H: TTuple;
  I: Integer;
begin
  Self.T.AddString('some-msg');
  Self.T.AddProcessID(OldPID);
  Self.T.AddInteger(42);

  H := Self.T.RouteTo(NewPID);
  try
    Check(H[1].IsProcessID, 'Second element of new msg isn''t a PID');
    CheckEquals(Self.T.Count, H.Count, 'Not all elements copied');
    for I := 1 to Self.T.Count - 1 do
      if (I <> 1) then
        CheckEquals(Self.T[I].AsString, H[I].AsString, Format('Element %d', [I]));
  finally
    H.Free;
  end;
end;

procedure TestTTuple.TestRouteToPreservesClass;
const
  OldPID: TProcessID = 'old-pid';
  NewPID: TProcessID = 'new-pid';
var
  NewMsg: TTuple;
  OldMsg: TTuple;
begin
  OldMsg := TMessageTuple.Create('some-msg', OldPID);
  try
    NewMsg := OldMsg.RouteTo(NewPID);

    CheckEquals(OldMsg.ClassType, NewMsg.ClassType, 'RouteTo didn''t preserve class type');
  finally
    OldMsg.Free;
  end;
end;

procedure TestTTuple.TestIsBoolean;
begin
  Check(not Self.T.IsBoolean, 'Element marked as a Boolean');
end;

procedure TestTTuple.TestIsInteger;
begin
  Check(not Self.T.IsInteger, 'Element marked as an Integer');
end;

procedure TestTTuple.TestIsProcessID;
begin
  Check(not Self.T.IsProcessID, 'Element marked as a ProcessID');
end;

procedure TestTTuple.TestIsString;
begin
  Check(not Self.T.IsString, 'Element marked as a String');
end;

procedure TestTTuple.TestIsTuple;
begin
  Check(Self.T.IsTuple, 'Element not marked as a Tuple');
end;

//******************************************************************************
//* TestTActorMailbox                                                          *
//******************************************************************************
//* TestTActorMailbox Public methods *******************************************

procedure TestTActorMailbox.SetUp;
begin
  inherited SetUp;

  Self.Environment := TThreadedActorEnvironment.Create;
  Self.Actor       := TActorInterface.Create(Self.Environment);

  Self.Mbox    := TActorMailbox.Create(Self.Actor);
  Self.M       := TActorMessage.Create(Self.Environment);
  Self.NewMsgs := false;
end;

procedure TestTActorMailbox.TearDown;
begin
  Self.M.Free;
  Self.Mbox.Free;
  Self.Actor.Free;
  Self.Environment.Free;

  inherited TearDown;
end;

//* TestTActorMailbox Private methods ******************************************

function TestTActorMailbox.AllFinder(Msg: TTuple): Boolean;
begin
  Result := true;
end;

procedure TestTActorMailbox.CheckFound(ExpectedTag: String;
                                       Mbox: TActorMailbox;
                                       Condition: TMessageFinder;
                                       MsgPrefix: String);
var
  Found: TActorMessage;
begin
  Found := Mbox.FindMessage(Condition);
  try
    Check(Found <> nil, MsgPrefix + ': No message found');
    CheckEquals(ExpectedTag, Found.Tag, MsgPrefix + ': Wrong message found');
  finally
    Found.Free;
  end;
end;

function TestTActorMailbox.NullFinder(Msg: TTuple): Boolean;
begin
  Result := false;
end;

procedure TestTActorMailbox.RegisterMessageArrival(Sender: TObject);
begin
  Self.NewMsgs := true;
end;

//* TestTActorMailbox Published methods ****************************************

procedure TestTActorMailbox.TestAddMessageAndIsEmpty;
begin
  Check(Self.MBox.IsEmpty, 'New mailbox isn''t empty');
  Self.MBox.AddMessage(Self.M);
  Check(not Self.MBox.IsEmpty, 'Mailbox didn''t store the message');
end;

procedure TestTActorMailbox.TestFindMessageEmptyMailbox;
var
  Found: TActorMessage;
begin
  Found := Self.MBox.FindMessage(Self.AllFinder);
  try
    Check(nil = Found, 'Message found... in an empty mailbox');
  finally
    Found.Free;
  end;
end;

procedure TestTActorMailbox.TestFindMessageReturnsFirstMatch;
var
  M2: TActorMessage;
begin
  M2 := TActorMessage.Create(Self.Environment);
  try
    Self.MBox.AddMessage(Self.M);
    Self.MBox.AddMessage(M2);

    CheckFound(Self.M.Tag, Self.Mbox, Self.AllFinder, 'First time');
    CheckFound(M2.Tag,     Self.Mbox, Self.AllFinder, 'Second time');
  finally
    M2.Free;
  end;
end;

procedure TestTActorMailbox.TestFindMessageStoredMessagesInSaveQueue;
var
  M2: TActorMessage;
  M3: TActorMessage;
begin
  // If there's a message in the mailbox that matches none of our conditions,
  // then that message is stored in the "save queue" until such time as a new
  // message arrives in the mailbox. Thus, unmatched messages will not be
  // continually rechecked by a Receive.

  M2 := TActorMessage.Create(Self.Environment);
  try
    Self.MBox.AddMessage(Self.M);
    Self.MBox.AddMessage(M2);

    Check(nil = Self.Mbox.FindMessage(Self.NullFinder), 'Message erroneously found');

    Check(nil = Self.Mbox.FindMessage(Self.AllFinder), 'Already-checked messages not moved to the save queue');

    M3 := TActorMessage.Create(Self.Environment);
    try
      Self.Mbox.AddMessage(M3);

      CheckFound(M3.Tag, Self.MBox, Self.AllFinder, 'Saved messages are added on the end of the mailbox');
      CheckFound(Self.M.Tag, Self.MBox, Self.AllFinder, 'Saved messages should be re-added to the mailbox for matching');
    finally
      M3.Free;
    end;
  finally
    M2.Free;
  end;
end;

procedure TestTActorMailbox.TestNotifyOfNewMessages;
begin
  Self.Mbox.OnMessageArrived := Self.RegisterMessageArrival;

  Self.Mbox.AddMessage(Self.M);

  Check(Self.NewMsgs, 'No notification of new messages');
end;

procedure TestTActorMailbox.TestPurge;
begin
  Self.MBox.AddMessage(Self.M);
  Self.MBox.Purge;
  Check(Self.MBox.IsEmpty, 'Mailbox didn''t purge the message');
end;

procedure TestTActorMailbox.TestTimeoutRestoresSaveQueueMessages;
var
  M2: TActorMessage;
  M3: TActorMessage;
begin
  // If an actor times out waiting for a message that matches some condition,
  // then all the messages in the save queue are re-added to the mailbox, in
  // their arrival order.

  M2 := TActorMessage.Create(Self.Environment);
  try
    Self.MBox.AddMessage(Self.M);
    Self.MBox.AddMessage(M2);

    Check(nil = Self.Mbox.FindMessage(Self.NullFinder), 'Message erroneously found');

    Check(nil = Self.Mbox.FindMessage(Self.AllFinder), 'Already-checked messages not moved to the save queue');

    M3 := TActorMessage.Create(Self.Environment);
    try
      Self.Mbox.Timeout;

      CheckFound(Self.M.Tag, Self.MBox, Self.AllFinder, 'Saved messages should be re-added to the mailbox for matching');
    finally
      M3.Free;
    end;
  finally
    M2.Free;
  end;
end;

//******************************************************************************
//* TestTActorInterface                                                        *
//******************************************************************************
//* TestTActorInterface Public methods *****************************************

procedure TestTActorInterface.SetUp;
begin
  inherited SetUp;

  Self.Environment := TThreadedActorEnvironment.Create;

  Self.BarName := 'Bar';
  Self.FooName := 'Foo';
  Self.Intf    := TActorInterface.Create(Self.Environment);

  Self.ReceivedAFoo := false;
  Self.TimedOut     := false;
end;

procedure TestTActorInterface.TearDown;
begin
  Self.Intf.Free;

  inherited TearDown;
end;

//* TestTActorInterface Private methods ****************************************

procedure TestTActorInterface.ActOnBarMsg(Msg: TTuple);
begin
  Self.ReceivedABar := true;
end;

procedure TestTActorInterface.ActOnFooMsg(Msg: TTuple);
begin
  Self.ReceivedAFoo := true;
end;

function TestTActorInterface.CreateFooMsg: TTuple;
begin
  Result := Self.CreateMsgNamed(Self.FooName);
end;

function TestTActorInterface.CreateMsgNamed(Name: String): TTuple;
var
  M: TMessageTuple;
begin
  M := TMessageTuple.Create(Name, Self.Intf.PID);
  Result := M;
end;

function TestTActorInterface.RecogniseBarMsg(Msg: TTuple): Boolean;
begin
  Result := Self.Intf.MatchMessageName(Msg, Self.BarName);
end;

function TestTActorInterface.RecogniseFooMsg(Msg: TTuple): Boolean;
begin
  Result := Self.Intf.MatchMessageName(Msg, Self.FooName);
end;

procedure TestTActorInterface.Timeout;
begin
  Self.TimedOut := true;
end;

//* TestTActorInterface Published methods **************************************

procedure TestTActorInterface.TestReceive;
var
  Foo: TTuple;
begin
  Foo := Self.CreateFooMsg;
  try
    Self.Environment.PrimitiveSend('', Self.Intf.PID, Foo);
  finally
    Foo.Free;
  end;

  Self.Intf.Receive(Self.RecogniseFooMsg, Self.ActOnFooMsg, OneSecond);

  Check(Self.ReceivedAFoo, 'Didn''t receive a message');
end;

procedure TestTActorInterface.TestReceiveReturnsOnlyOnAMatch;
begin
  Self.Intf.Send(Self.Intf.PID, Self.BarName);

  Self.Intf.Receive(Self.RecogniseFooMsg, Self.ActOnFooMsg, TenthOfASecond, Self.Timeout);

  Check(not Self.ReceivedAFoo, 'Received a message');
  Check(Self.TimedOut,         'Didn''t time out waiting for a particular message');
end;

procedure TestTActorInterface.TestReceiveTwoMessages;
var
  T: TActorMessageTable;
begin
  T := TActorMessageTable.Create;
  try
    T.Add(Self.RecogniseBarMsg, Self.ActOnBarMsg);
    T.Add(Self.RecogniseFooMsg, Self.ActOnFooMsg);

    Self.Intf.Send(Self.Intf.PID, Self.BarName);
    Self.Intf.Send(Self.Intf.PID, Self.FooName);

    Self.Intf.Receive(T, OneSecond, Self.Timeout);
    Check(Self.ReceivedABar, 'Bar message not received');

    Self.Intf.Receive(T, OneSecond, Self.Timeout);
    Check(Self.ReceivedAFoo, 'Foo message not received');
  finally
    T.Free;
  end;
end;

procedure TestTActorInterface.TestReceiveWithTimeout;
begin
  Self.Intf.Receive(Self.RecogniseFooMsg, Self.ActOnFooMsg, TenthOfASecond, Self.Timeout);

  Check(not Self.ReceivedAFoo, 'Received a message');
  Check(Self.TimedOut,         'Timeout action not invoked');
end;

procedure TestTActorInterface.TestReceiveWithZeroTimeout;
var
  Foo: TTuple;
begin
  Foo := Self.CreateFooMsg;
  try
    SendActorMessage(Self.Intf.PID, Foo);
  finally
    Foo.Free;
  end;

  Self.Intf.Receive(Self.RecogniseFooMsg, Self.ActOnFooMsg, 0, Self.Timeout);

  Check(not Self.ReceivedAFoo, 'Received a message');
  Check(Self.TimedOut,         'Timeout action not invoked');
end;

//******************************************************************************
//* TestTActorMessageTable                                                     *
//******************************************************************************
//* TestTActorMessageTable Public methods **************************************

procedure TestTActorMessageTable.SetUp;
begin
  inherited SetUp;

  Self.T := TActorMessageTable.Create;
  Self.TestMsg := TTuple.Create;

  Self.FirstActionWorked     := false;
  Self.SecondActionWorked    := false;
  Self.FirstConditionWorked  := false;
  Self.SecondConditionWorked := false;
end;

procedure TestTActorMessageTable.TearDown;
begin
  Self.TestMsg.Free;
  Self.T.Free;

  inherited TearDown;
end;

//* TestTActorMessageTable Private methods *************************************

procedure TestTActorMessageTable.FirstAction(Msg: TTuple);
begin
  Self.FirstActionWorked := true;
end;

function TestTActorMessageTable.FirstTestCondition(Msg: TTuple): Boolean;
begin
  Result := false;
  Self.FirstConditionWorked := true;
end;

procedure TestTActorMessageTable.SecondAction(Msg: TTuple);
begin
  Self.SecondActionWorked := true;
end;

function TestTActorMessageTable.SecondTestCondition(Msg: TTuple): Boolean;
begin
  Result := false;
  Self.SecondConditionWorked := true;
end;

//* TestTActorMessageTable Published methods **************************************

procedure TestTActorMessageTable.TestAddAndCount;
begin
  CheckEquals(0, Self.T.Count, 'Empty table');

  Self.T.Add(Self.FirstTestCondition, Self.FirstAction);
  CheckEquals(1, Self.T.Count, 'No condition/action pair added');

  Self.T.Add(Self.SecondTestCondition, Self.SecondAction);
  CheckEquals(2, Self.T.Count, 'No second condition/action pair added');
end;

procedure TestTActorMessageTable.TestAccessors;
begin
  Self.T.Add(Self.FirstTestCondition, Self.FirstAction);
  Self.T.Add(Self.SecondTestCondition, Self.SecondAction);

  Self.T.Actions[0](Self.TestMsg);
  Check(Self.FirstActionWorked, 'Actions[0] returned an unexpected method');

  Self.T.Actions[1](Self.TestMsg);
  Check(Self.SecondActionWorked, 'Actions[1] returned an unexpected method');

  Self.T.Conditions[0](Self.TestMsg);
  Check(Self.FirstConditionWorked, 'Conditions[0] returned an unexpected method');

  Self.T.Conditions[1](Self.TestMsg);
  Check(Self.SecondConditionWorked, 'Conditions[1] returned an unexpected method');
end;

//******************************************************************************
//* TActorTestCase                                                             *
//******************************************************************************
//* TActorTestCase Public methods **********************************************

procedure TActorTestCase.SetUp;
begin
  inherited SetUp;

  Self.Environment := TThreadedActorEnvironment.Create;
  Self.ExitEvent   := TSimpleEvent.Create;
  Self.MsgEvent    := TSimpleEvent.Create;

  OnActorCreatedHook := NotifyOfActorCreation;
  OnActorExitedHook  := NotifyOfActorExit;
  OnMessageSentHook  := StoreLastSentMessageInTestCase;


  TestLock.Acquire;
  try
    RunningTestCase := Self;
    Self.LastSentMsg  := nil;
  finally
    TestLock.Release;
  end;

  Self.ActorExited  := false;
end;

procedure TActorTestCase.TearDown;
begin
  TestLock.Acquire;
  try
    RunningTestCase := nil;
    Self.LastSentMsg.Free;
  finally
    TestLock.Release;
  end;

  Self.MsgEvent.Free;
  Self.ExitEvent.Free;
  Self.Environment.Free;

  inherited TearDown;
end;

//* TActorTestCase Protected methods *******************************************

procedure TActorTestCase.CheckLastMsgEquals(Expected: TTuple; Msg: String);
begin
  TestLock.Acquire;
  try
    CheckEquals(Expected.AsString, Self.LastSentMsg.Data.AsString, Msg);
  finally
    TestLock.Release;
  end;
end;

function TActorTestCase.CopyLastSentMsg: TActorMessage;
begin
  TestLock.Acquire;
  try
    Result := Self.LastSentMsg.Copy;
  finally
    TestLock.Release;
  end;
end;

procedure TActorTestCase.WaitFor(E: TEvent; Timeout: Cardinal; Msg: String);
begin
  if (wrSignaled <> E.WaitFor(Timeout)) then
    Fail(Msg);
end;

procedure TActorTestCase.WaitForExit(Timeout: Cardinal = 1000);
begin
  Self.WaitFor(Self.ExitEvent, Timeout, 'Timed out waiting for an exit notification');
end;

procedure TActorTestCase.WaitForMsg(Timeout: Cardinal = 1000; OptionalMsg: String = '');
var
  Msg: String;
begin
  Msg := 'Timed out waiting for a message';
  if (OptionalMsg <> '') then
    Msg := Format('%s (%s)', [Msg, OptionalMsg]);

  Self.WaitFor(Self.MsgEvent, Timeout, Msg);
end;

//* TActorTestCase Private methods *********************************************

procedure TActorTestCase.NotifyOfExit(PID: String; Reason: TTuple);
begin
  Self.ActorExited := true;
  Self.ExitEvent.SetEvent;
end;

procedure TActorTestCase.StoreLastSentMessage(Sender, Target: TProcessID; Msg: TActorMessage);
begin
  // StoreLastSentMessageInTestCase locks access to this method; nothing else
  // calls it.

  Self.LastSentMsg.Free;
  Self.LastSentMsg := Msg.Copy;
  Self.MsgEvent.SetEvent;
end;

//******************************************************************************
//* TestTActor                                                                 *
//******************************************************************************
//* TestTActor Public methods **************************************************

procedure TestTActor.SetUp;
begin
  inherited SetUp;

  PurgeLog;

  Self.E := TSimpleEvent.Create;

  Self.TestMsg := TMessageTuple.Create(TestName, '');

  Self.ExitRecvd  := false;
  Self.ExitReason := '';
  Self.TimedOut   := false;
end;

procedure TestTActor.TearDown;
begin
  Self.TestMsg.Free;
  Self.E.Free;

  inherited TearDown;
end;

//* TestTActor Private methods *************************************************

function TestTActor.MatchExit(Msg: TTuple): Boolean;
var
  O: TMessageTuple;
begin
  try
    O := TMessageTuple.Overlay(Msg);
    try
      Result := O.MessageName = ExitMsg;
    finally
      O.Free;
    end;
  except
    Result := false;
  end;
end;

procedure TestTActor.RecordExit(Msg: TTuple);
var
  O: TMessageTuple;
begin
  O := TMessageTuple.Overlay(Msg);
  try
    Self.ExitRecvd := true;
    Self.ExitReason    := (O.Parameters[0] as TStringTerm).Value;
  finally
    O.Free;
  end;
end;

procedure TestTActor.Timeout;
begin
  Self.TimedOut := true;
end;

//* TestTActor Published methods ***********************************************

procedure TestTActor.TestActorAcceptsExitMessage;
var
  PID: TProcessID;
  Reason: TTuple;
begin
  PID := Spawn(TActor);

  Reason := TTuple.Create;
  try
    Reason.AddString('Just because');
    ExitActor(PID, Reason);

    Self.WaitForExit;
    Check(Self.ActorExited, 'Actor didn''t exit, so didn''t accept the exit request');
  finally
    Reason.Free;
  end;
end;

procedure TestTActor.TestActorAcceptsKillMessage;
var
  PID: TProcessID;
begin
  PID := Spawn(TActor);
  Kill(PID);

  Self.WaitForExit;
  Check(Self.ActorExited, 'Actor didn''t exit, so didn''t accept the kill');
end;

procedure TestTActor.TestActorNotifiesLinkSetOfAbnormalExit;
var
  I: TActorInterface;
begin
  try
    I := TActorInterface.Create(Self.Environment);
    try
      I.SpawnLink(TErrorActor);

      I.Receive(Self.MatchExit, Self.RecordExit, OneSecond, Self.Timeout);
      Check(not Self.TimedOut, 'Timed out waiting for message');
      Check(Self.ExitRecvd, Format('No %s message received', [ExitMsg]));
      CheckEquals(TErrorActor.ExpectedReason, Self.ExitReason, 'Unexpected reason for exit');
    finally
      I.Free;
    end;
  finally
    I.Free;
  end;
end;

procedure TestTActor.TestActorNotifiesLinkSetOfExit;
var
  I: TActorInterface;
begin
  I := TActorInterface.Create(Self.Environment);
  try
    I.SpawnLink(TSingleShotActor);

    I.Receive(Self.MatchExit, Self.RecordExit, OneSecond, Self.Timeout);
    Check(not Self.TimedOut, 'Timed out waiting for message');
    Check(Self.ExitRecvd, Format('No %s message received', [ExitMsg]));
    CheckEquals(ExitReasonNormal, Self.ExitReason, 'Unexpected reason for exit');
  finally
    I.Free;
  end;
end;

procedure TestTActor.TestActorsCanReceiveMessages;
var
  PID:     TProcessID;
begin
  PID := Spawn(TEchoActor);
  try
    SendActorMessage(PID, Self.TestMsg);
    Self.WaitForMsg(1000);

    CheckLastMsgEquals(Self.TestMsg, 'Echo process didn''t send a message, hence didn''t receive one');
  finally
    Kill(PID);
  end;
end;

//******************************************************************************
//* TestActorFunctions                                                         *
//******************************************************************************
//* TestActorFunctions Public methods ******************************************

procedure TestActorFunctions.SetUp;
begin
  inherited SetUp;

  Self.TestMsg    := TMessageTuple.Create(TestName, TActor.RootActor);
  Self.ThunkEvent := TSimpleEvent.Create;

  Self.ThunkRan := false;
end;

procedure TestActorFunctions.TearDown;
begin
  Self.ThunkEvent.Free;
  Self.TestMsg.Free;

  inherited TearDown;
end;

//* TestActorFunctions Private methods ***************************************((

procedure TestActorFunctions.CheckMessageNameEquals(Expected, Received: TTuple; Msg: String);
var
  ExpectedMsg, ReceivedMsg: TMessageTuple;
begin
  CheckEquals(TMessageTuple, Expected.ClassType, Msg + ' (Expected tuple)');
  CheckEquals(TMessageTuple, Received.ClassType, Msg + ' (Received tuple)');
  ExpectedMsg := TMessageTuple.Overlay(Expected) as TMessageTuple;
  try
    ReceivedMsg := TMessageTuple.Overlay(Received) as TMessageTuple;
    try
      CheckEquals(ExpectedMsg.MessageName, ReceivedMsg.MessageName, Msg);
    finally
      ReceivedMsg.Free;
    end;
  finally
    ExpectedMsg.Free;
  end;
end;

procedure TestActorFunctions.TestThunk;
begin
  Self.ThunkRan := true;
  Self.ThunkEvent.SetEvent;
end;

//* TestActorFunctions Published methods ***************************************

procedure TestActorFunctions.TestRPC;
var
  AnotherTest:  TMessageTuple;
  Echo:         TProcessID;
  FirstResult:  TTuple;
  SecondResult: TTuple;
begin
  AnotherTest := TMessageTuple.Create(ConstructUUID, TRootActor.RootActor);
  try
    Echo := Spawn(TEchoActor);
    try
      FirstResult := RPC(Echo, Self.TestMsg, 1000);
      try
        CheckNotNull(FirstResult, 'RPC didn''t return any result');
        CheckMessageNameEquals(Self.TestMsg, FirstResult, 'RPC didn''t return expected result');
      finally
        FirstResult.Free;
      end;

      SecondResult := RPC(Echo, AnotherTest, 1000);
      try
        CheckNotNull(FirstResult, '2nd RPC didn''t return any result');
        // Check message names
        CheckMessageNameEquals(AnotherTest, SecondResult, '2nd RPC didn''t return expected result');
      finally
        SecondResult.Free;
      end;
    finally
      Kill(Echo);
    end;
  finally
    AnotherTest.Free;
  end;
end;

procedure TestActorFunctions.TestSpawnThunk;
begin
  Spawn(Self.TestThunk);
  Self.WaitFor(Self.ThunkEvent, OneSecond, 'Thunk didn''t run');
end;

initialization;
  Lock     := TCriticalSection.Create;
  Log      := TObjectList.Create(true);
  TestLock := TCriticalSection.Create;

  RegisterTest('Ikaria', Suite);
end.
