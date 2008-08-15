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
    procedure TestAsString;
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
    procedure TestAsString;
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
    procedure TestAsString;
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
    procedure TestAsString;
    procedure TestCopy;
    procedure TestEquals;
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
    procedure TestAddString;
    procedure TestAsString;
    procedure TestAsStringEmptyTuple;
    procedure TestAsStringTree;
    procedure TestCopy;
    procedure TestEquals;
    procedure TestEqualsOnTree;
  end;

  TestTActorMailbox = class(TTestCase)
  private
    Mbox:    TActorMailbox;
    M:       TActorMessage;
    NewMsgs: Boolean;

    function  AllFinder(Msg: TActorMessage): Boolean;
    procedure CheckFound(ExpectedTag: String;
                         Mbox: TActorMailbox;
                         Condition: TMessageFinder;
                         MsgPrefix: String);
    function  NullFinder(Msg: TActorMessage): Boolean;
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

procedure TestTBooleanElement.TestAsString;
begin
  CheckEquals(BoolStrs[Self.B.Value], Self.B.AsString, 'BooleanElement.AsString');
end;

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

procedure TestTIntegerElement.TestAsString;
begin
  CheckEquals(IntToStr(Self.I.Value), Self.I.AsString, 'IntegerElement.AsString');
end;

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

procedure TestTProcessIDElement.TestAsString;
begin
  CheckEquals(Format('{%s}', [Self.PID.Value]), Self.PID.AsString, 'ProcessIDElement.AsString');
end;

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

procedure TestTStringElement.TestAsString;
begin
  CheckEquals(Format('"%s"', [Self.S.Value]), Self.S.AsString, 'StringElement.AsString');
end;

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
    Check(B <> Self.T[0], 'Element not copied (Boolean)');
  finally
    B.Free;
  end;

  I := TIntegerElement.Create(42);
  try
    Self.T.Add(I);
    CheckEquals(2, Self.T.Count, 'Integer not added');
    CheckEquals(I.ClassType, Self.T[1].ClassType, 'Element of wrong type added (Integer)');
    Check(I <> Self.T[1], 'Element not copied (Integer)');
  finally
    I.Free;
  end;

  S := TStringElement.Create('hello');
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

//******************************************************************************
//* TestTActorMailbox                                                          *
//******************************************************************************
//* TestTActorMailbox Public methods *******************************************

procedure TestTActorMailbox.SetUp;
begin
  inherited SetUp;

  Self.Mbox    := TActorMailbox.Create;
  Self.M       := TActorMessage.Create;
  Self.NewMsgs := false;
end;

procedure TestTActorMailbox.TearDown;
begin
  Self.M.Free;
  Self.Mbox.Free;

  inherited TearDown;
end;

//* TestTActorMailbox Private methods ******************************************

function TestTActorMailbox.AllFinder(Msg: TActorMessage): Boolean;
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

function TestTActorMailbox.NullFinder(Msg: TActorMessage): Boolean;
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
  M2 := TActorMessage.Create;
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

  M2 := TActorMessage.Create;
  try
    Self.MBox.AddMessage(Self.M);
    Self.MBox.AddMessage(M2);

    Check(nil = Self.Mbox.FindMessage(Self.NullFinder), 'Message erroneously found');

    Check(nil = Self.Mbox.FindMessage(Self.AllFinder), 'Already-checked messages not moved to the save queue');

    M3 := TActorMessage.Create;
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

  M2 := TActorMessage.Create;
  try
    Self.MBox.AddMessage(Self.M);
    Self.MBox.AddMessage(M2);

    Check(nil = Self.Mbox.FindMessage(Self.NullFinder), 'Message erroneously found');

    Check(nil = Self.Mbox.FindMessage(Self.AllFinder), 'Already-checked messages not moved to the save queue');

    M3 := TActorMessage.Create;
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
