{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit Ikaria;

{ Ikaria is an attempt at implementing an Actor model in Delphi.
  Actors may
  * compute something
  * create other Actors
  * send messages (in the form of tuples) to other Actors
  * react to received messages.

  You implement Actors by subclassing TActor. Then, for each interesting
  message, you implement a finder method and a reactor method. The former takes
  a tuple and returns a Boolean indicating a match, and the latter takes a tuple
  and does something with it. Every Actor by default Steps through its
  computation, usually waiting to receive an interesting message.

  Note that you, as implementor of an Actor, need to exercise caution. You can
  easily write a malicious Actor that sits in a tight loop, starving the
  processor (and preventing a clean shutdown of the Environment). The framework
  doesn't protect you from rabbit-bombing yourself.

  Currently, the framework runs each Actor in its own, transient, thread.

  (Why "Ikaria"? It's the town of one Thespis, regarded as being the very first
  actor. "Delphi" is the name of an ancient Greek town, so it amused me to name
  this framework after another Greek town.)
}

// TODO:
// * Trapping of exits (when a flag is set)
// * Should receiving exits be pushed into the primitive layer? Right now it's
//   possible to have a badly- or maliciously-written Actor refuse to terminate
//   because it never processes an exit message.
// * Parallel map: take an array and a function to be applied to each element in
//   the array, and fork/join the processing, a la mapreduce.
// * "Futures"?

interface

uses
  Classes, Contnrs, SyncObjs, SysUtils;

type
  TActor = class;
  TActorClass = class of TActor;
  TProcessID = String;

  TTerm = class(TObject)
  public
    function AsString: String; virtual;
    function Copy: TTerm; virtual;
    function Equals(Other: TTerm): Boolean; virtual;
    function IsBoolean: Boolean; virtual;
    function IsInteger: Boolean; virtual;
    function IsProcessID: Boolean; virtual;
    function IsString: Boolean; virtual;
    function IsTuple: Boolean; virtual;
  end;

  TBooleanTerm = class(TTerm)
  private
    fValue: Boolean;
  public
    constructor Create(Value: Boolean);

    function AsString: String; override;
    function Copy: TTerm; override;
    function Equals(Other: TTerm): Boolean; override;
    function IsBoolean: Boolean; override;

    property Value: Boolean read fValue;
  end;

  TIntegerTerm = class(TTerm)
  private
    fValue: Integer;
  public
    constructor Create(Value: Integer);

    function AsString: String; override;
    function Copy: TTerm; override;
    function Equals(Other: TTerm): Boolean; override;
    function IsInteger: Boolean; override;

    property Value: Integer read fValue;
  end;

  TProcessIDTerm = class(TTerm)
  private
    fValue: TProcessID;
  public
    constructor Create(Value: TProcessID);

    function AsString: String; override;
    function Copy: TTerm; override;
    function Equals(Other: TTerm): Boolean; override;
    function IsProcessID: Boolean; override;

    property Value: TProcessID read fValue;
  end;

  TStringTerm = class(TTerm)
  private
    fValue: String;
  public
    constructor Create(Value: String);

    function AsString: String; override;
    function Copy: TTerm; override;
    function Equals(Other: TTerm): Boolean; override;
    function IsString: Boolean; override;

    property Value: String read fValue;
  end;

  TTupleClass = class of TTuple;

  TTuple = class(TTerm)
  private
    TupleElements: TObjectList;

    function GetElement(Index: Integer): TTerm;
    function ThisClassType: TTupleClass;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(E: TTerm);
    procedure AddBoolean(B: Boolean);
    procedure AddInteger(I: Integer);
    procedure AddProcessID(PID: TProcessID);
    procedure AddString(S: String);
    function  AsString: String; override;
    function  Copy: TTerm; override;
    function  Count: Integer;
    function  Equals(Other: TTerm): Boolean; override;
    function  RouteTo(NewDest: TProcessID): TTuple;
    function  IsTuple: Boolean; override;

    property Elements[Index: Integer]: TTerm read GetElement; default;
  end;

  // The base class of all standard messages in Ikaria, MessageTuples look like
  // this:
  //   ("msg-name" {reply-to-pid} (some tuple of paramters))
  TMessageTuple = class(TTuple)
  private
    function GetMessageName: String;
    function GetParameters: TTuple;
    function GetReplyTo: TProcessID;
  public
    constructor Create(MessageName: String; ReplyTo: TProcessID); overload;
    constructor Create(MessageName: String; ReplyTo: TProcessID; Parameters: TTuple); overload;
    constructor Overlay(Msg: TTuple);

    property MessageName: String     read GetMessageName;
    property Parameters:  TTuple     read GetParameters;
    property ReplyTo:     TProcessID read GetReplyTo;
  end;

  TActorEnvironment = class;

  // I represent a message sent to an Actor.
  //
  // I will free the Data you give me.
  TActorMessage = class(TObject)
  private
    Environment: TActorEnvironment;
    fData:       TTuple;
    fTag:        String;
  public
    constructor Create(E: TActorEnvironment);
    destructor  Destroy; override;

    procedure Accept(Actor: TActor); virtual;
    function  AsString: String; virtual;
    function  Copy: TActorMessage; virtual;

    property Data: TTuple read fData write fData;
    property Tag:  String read fTag;
  end;

  TMessageFinder = function(Msg: TTuple): Boolean of object;

  TActOnMessage = procedure(Msg: TTuple) of object;

  TActorMessageTable = class;
  TActorInterface = class;

  // I store messages sent to an Actor.
  TActorMailbox = class(TObject)
  private
    Environment:       TActorEnvironment;
    fOnMessageArrived: TNotifyEvent;
    fPID:              TProcessID;
    Lock:              TCriticalSection;
    LinkSet:           TStringList;
    Messages:          TObjectList;
    SaveQueue:         TObjectList;

    procedure FreeMessage(L: TObjectList; M: TActorMessage);
    procedure FreeMessages(L: TObjectList);
    function  MessageAt(Index: Integer): TActorMessage;
    procedure MoveMessages(Src, Dest: TObjectList; FromIndex, ToIndex: Integer);
    procedure NotifyOfMessageArrived;
    procedure RestoreSaveQueue;
    procedure SetOnMessageArrived(Value: TNotifyEvent);
  public
    constructor Create(Owner: TActorInterface);
    destructor  Destroy; override;

    procedure AddMessage(Msg: TActorMessage);
    function  IsEmpty: Boolean;
    function  FindAndProcessMessage(Table: TActorMessageTable;
                                    var ConditionIndex: Integer): TActorMessage;
    function  FindMessage(Condition: TMessageFinder): TActorMessage;
    procedure Link(PID: TProcessID);
    procedure Purge;
    procedure Timeout;

    property OnMessageArrived: TNotifyEvent read fOnMessageArrived write SetOnMessageArrived;
    property PID:              TProcessID   read fPID;
  end;

  TActorMessageActionPair = class(TObject)
  private
    fAction:    TActOnMessage;
    fCondition: TMessageFinder;
  public
    constructor Create(Condition: TMessageFinder; Action: TActOnMessage);

    property Action:    TActOnMessage  read fAction;
    property Condition: TMessageFinder read fCondition;
  end;

  TActorMessageTable = class(TObject)
  private
    List: TObjectList;

    function GetAction(Index: Integer): TActOnMessage;
    function GetCondition(Index: Integer): TMessageFinder;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(Condition: TMessageFinder; Action: TActOnMessage);
    function  Count: Integer;

    property Actions[Index: Integer]:    TActOnMessage  read GetAction;
    property Conditions[Index: Integer]: TMessageFinder read GetCondition;
  end;

  // There are lots of names for parameterless procedures: I've called them
  // "thunks" after the LISP community's term.
  TThunk = procedure of object;

  // I provide a way for non-Actors to both send messages to and receive
  // messages from Actors.
  TActorInterface = class(TObject)
  private
    fTerminated: Boolean;
    fTrapExits:  Boolean;
    Lock:        TCriticalSection;
    Mailbox:     TActorMailbox;
    MsgEvent:    TEvent;
    PendingMsgs: Cardinal;

    function  GetPID: TProcessID;
    procedure NewMessageArrived(Sender: TObject);
    procedure NullThunk;
    function  WaitForMessage(Timeout: Cardinal): Boolean;
  protected
    Environment: TActorEnvironment;

    procedure DoNothing(Msg: TTuple);
  public
    constructor Create(E: TActorEnvironment);
    destructor  Destroy; override;

    function  FindAny(Msg: TTuple): Boolean;

    procedure Link(PID: TProcessID);
    function  MatchMessageName(Msg: TTuple; MsgName: String): Boolean;
    procedure Receive(Matching: TMessageFinder;
                      Action: TActOnMessage); overload;
    procedure Receive(Matching: TMessageFinder;
                      Action: TActOnMessage;
                      Timeout: Cardinal); overload;
    procedure Receive(Matching: TMessageFinder;
                      Action: TActOnMessage;
                      Timeout: Cardinal;
                      TimeoutAction: TThunk); overload;
    procedure Receive(Table: TActorMessageTable); overload;
    procedure Receive(Table: TActorMessageTable;
                      Timeout: Cardinal); overload;
    procedure Receive(Table: TActorMessageTable;
                      Timeout: Cardinal;
                      TimeoutAction: TThunk); overload;
    procedure Send(Target: TProcessID; MsgName: String); overload;
    procedure Send(Target: TProcessID; MsgName: String; Parameters: TTuple); overload;
    procedure Send(Target: TProcessID; T: TTuple); overload;
    function  Spawn(ActorType: TActorClass): TProcessID;
    function  SpawnLink(ActorType: TActorClass): TProcessID;

    property PID:        TProcessID read GetPID;
    property Terminated: Boolean    read fTerminated write fTerminated;
    property TrapExits:  Boolean    read fTrapExits write fTrapExits;
  end;

  // I provide some convenience methods for the RPC function.
  TActorInterfaceForRPC = class(TActorInterface)
  private
    fResult: TTuple;
  public
    destructor Destroy; override;
    procedure NullAction(Msg: TTuple);
    procedure NullThunk;
    procedure Reset;
    procedure StoreFirstMessage(Msg: TTuple);

    property Result: TTuple read fResult;
  end;

  // I represent an execution context. I send and receive messages to and from
  // other Actors.
  //
  // Note that "execution context" doesn't map to an operating system concept:
  // right now Ikaria uses one thread to run one Actor, but later it might not
  // use threads - it could use processes, or fibres - or one thread might run several
  // cooperatively-multitasking Actors.
  //
  // Conceptually, my execution is analogous to this:
  //   SetUp;
  //   while not Self.Terminated do
  //     Step;
  //   TearDown;
  //
  // Also, I provide common matching functions - MatchAny, MatchMessageName,
  // etc. - to my subclasses.
  TActor = class(TActorInterface)
  private
    fKilled: Boolean;

    procedure ReactToExit(Msg: TTuple);
    procedure ReactToKill(Msg: TTuple);
    procedure RegisterRequiredActions(Table: TActorMessageTable);
    procedure SendExit(Reason: TTuple);
  protected
    MsgTable: TActorMessageTable;
    ParentID: TProcessID;

    procedure BeforeExit(Msg: TTuple); virtual;
    function  FindExit(Msg: TTuple): Boolean;
    function  FindKill(Msg: TTuple): Boolean;
    function  MatchAny(Msg: TTuple): Boolean;
    procedure RegisterActions(Table: TActorMessageTable); virtual;
    procedure SendExceptionalExit(E: Exception);
    procedure SendNormalExit;
  public
    class function RootActor: TProcessID;

    constructor Create(E: TActorEnvironment; Parent: TProcessID); virtual;
    destructor  Destroy; override;

    procedure SendKilledExit;
    procedure SetUp; virtual;
    procedure Step; virtual;
    procedure TearDown; virtual;
    procedure Terminate; reintroduce; overload; virtual;
    procedure Terminate(Reason: TTuple); reintroduce; overload; virtual;

    property Killed: Boolean read fKilled;
  end;

  TThunkActor = class(TActor)
  private
    fThunk: TThunk;
  public
    procedure Step; override;

    property Thunk: TThunk read fThunk write fThunk;
  end;

  TRootActor = class(TActor)
  end;

  // Given an instantiated Actor, I run the Actor in a separate thread. When I
  // terminate (or rather, when the Actor signals its completion by returning
  // from its Execute method), I signal my termination through OnExit.
  TActorRunner = class(TThread)
  private
    Actor:   TActor;
    fOnExit: TNotifyEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(A: TActor);
    destructor  Destroy; override;

    property OnExit: TNotifyEvent read fOnExit write fOnExit;
  end;

  // I represent the environment in which Actors run. Subclasses might use any
  // manner of ways of executing Actors - in threads, with thread pools, with
  // events & message queues, whatever.
  //
  // Note that I implicitly define a self-contained, isolated namespace: Actors
  // in two different ActorEnvironments cannot communicate.
  TActorEnvironment = class(TObject)
  protected
    function Whois(PID: TProcessID): TActorMailbox; virtual;
  public
    function  CreateKillMsg: TTuple;
    procedure Exit(Target: TProcessID); overload;
    procedure Exit(Target: TProcessID; Reason: TTuple); overload; virtual;
    function  IsProcessAlive(Target: TProcessID): Boolean;
    procedure Kill(Target: TProcessID);
    function  NextPID: TProcessID; virtual;
    function  NextTag: String; virtual;
    procedure PrimitiveLink(LinkingPID, LinkedPID: TProcessID); virtual;
    function  PrimitiveReceive(Mailbox: TActorMailbox; Table: TActorMessageTable): Boolean; virtual;
    function  PrimitiveRegisterActor(A: TActorMailbox): TProcessID; virtual;
    procedure PrimitiveSend(Sender, Target: TProcessID; Msg: TTuple); virtual;
    function  PrimitiveSpawn(ActorType: TActorClass; Parent: TProcessID): TProcessID; overload; virtual;
    function  PrimitiveSpawn(T: TThunk): TProcessID; overload; virtual;
    function  PrimitiveSpawnLink(ActorType: TActorClass; Parent: TProcessID): TProcessID; virtual;
    procedure PrimitiveUnregisterActor(A: TActorMailbox); virtual;
    procedure SetTrapExit(PID: TProcessID; TrapExits: Boolean); virtual;
  end;

  // I run Actors in separate threads (TActorRunners). I manage the memory
  // allocated to Actors created through PrimitiveSpawn/PrimitiveSpawnLink - I
  // free these Actors when they terminate. You may attach Actors to me
  // manually - by TActor.Create() or TActorInterface.Create() - but then YOU
  // must free the Actors. (If you don't, I will hang forever, futilely waiting
  // for all registered Actors to free.)
  TThreadedActorEnvironment = class(TActorEnvironment)
  private
    Actors:    TStringList;
    ActorLock: TCriticalSection; // Lock access to Actors.
    Runners:   TObjectList;
    UsedPIDs:  TStringList;

    // Tag generation
    NextAvailableTag: Int64;
    TagLock:          TCriticalSection;

    procedure ActivateActor(A: TActor);
    procedure Deactivate(L: TStringList);
    procedure RemoveRunner(TerminatedThread: TObject);
    procedure WaitForAllActorsExit(L: TStringList);
  protected
    procedure Lock;
    procedure Unlock;
    function  Whois(PID: TProcessID): TActorMailbox; override;
  public
    constructor Create;
    destructor  Destroy; override;

    function  NextPID: TProcessID; override;
    function  NextTag: String; override;
    procedure PrimitiveLink(LinkingPID, LinkedPID: TProcessID); override;
    function  PrimitiveReceive(Mailbox: TActorMailbox; Table: TActorMessageTable): Boolean; override;
    function  PrimitiveRegisterActor(A: TActorMailbox): TProcessID; override;
    procedure PrimitiveSend(Sender, Target: TProcessID; Msg: TTuple); override;
    function  PrimitiveSpawn(ActorType: TActorClass; Parent: TProcessID): TProcessID; override;
    function  PrimitiveSpawn(T: TThunk): TProcessID; override;
    function  PrimitiveSpawnLink(ActorType: TActorClass; Parent: TProcessID): TProcessID; override;
    procedure PrimitiveUnregisterActor(A: TActorMailbox); override;
    procedure SetTrapExit(PID: TProcessID; TrapExits: Boolean); override;
  end;

  TDebugActorEnvironment = class(TThreadedActorEnvironment)
  private
    fSentMessages: TStrings;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure PrimitiveSend(Sender, Target: TProcessID; Msg: TTuple); override;
    procedure SnapshotSentMessages(Buffer: TStrings);
  end;

  EActorException = class(Exception);
  ETimeout = class(Exception);

// Return a new PID.
function ConstructUUID: String;

// Ask an Actor to terminate for a given Reason.
procedure ExitActor(Target: TProcessID; Reason: TTuple); overload;

// Unconditionally kill an Actor. Think hard before using it!
procedure Kill(Target: TProcessID);

// Send a message to a particular actor, and wait for a response. Return the
// response, or fail after a timeout.
function RPC(Target: TProcessID; Msg: TMessageTuple; Timeout: Cardinal): TTuple;

// Send a message to a particular Actor.
procedure SendActorMessage(Target: TProcessID; Msg: TTuple);

// Spawn a new Actor of the given type as a child of the Root Actor, returning
// its PID.
function Spawn(ActorType: TActorClass): TProcessID; overload;
function Spawn(T: TThunk): TProcessID; overload;

const
  ActorCreatedMsg = 'Actor %s created (of type %s)';
  ActorExitedMsg  = 'Actor %s exited with reason %s';
  ActorLinkedMsg  = 'Actor %s linked to actor %s';
  ActorFreedMsg   = 'Actor %s freed';
  MessageSentMsg  = 'Actor %s sent message to actor %s: %s';

const
  ExitMsg             = 'exit';
  ExitReasonException = '%s: %s';
  ExitReasonNormal    = 'normal';
  ExitReasonKill      = 'kill';
  ExitReasonKilled    = 'killed';

// String conversion
const
  BoolStrs: array[false..true] of String = ('false', 'true');

// Introspection callbacks
type
  TActorEventProc  = procedure(ActorType, PID, Event: String);
  TActorLinkedProc = procedure(LinkingPID, LinkedPID: String);
  TActorMsgProc    = procedure(PID: String; Data: TTuple);
  TMessageSendProc = procedure(Sender, Target: TProcessID; Msg: TActorMessage);

var
  StandardWaitTime: Cardinal;


const
  OneMillisecond = 1/86400/1000; // One Millisecond in TDateTime format.
  OneSecond      = 1000; // milliseconds

var
  DefaultEnv:         TActorEnvironment;
  OnActorCreatedHook: TActorEventProc;
  OnActorLinkedHook:  TActorLinkedProc;
  OnActorExitedHook:  TActorMsgProc;
  OnMessageSentHook:  TMessageSendProc;

implementation

//******************************************************************************
//* Unit Private functions/procedures                                          *
//******************************************************************************

procedure RaiseAbstractError(ClassName, FunctionName: String);
begin
  raise EAbstractError.Create(Format('%s must override %s', [ClassName, FunctionName]));
end;

function WithoutFirstAndLastChars(const S: String): String;
begin
  Result := Copy(S, 2, Length(S) - 2);
end;

procedure NotifyOfActorCreation(ActorType, PID: String);
begin
  if Assigned(OnActorCreatedHook) then
    OnActorCreatedHook(ActorType, PID, 'Actor created');
end;

procedure NotifyOfActorExit(PID: String; ExitReason: TTuple);
begin
  if Assigned(OnActorExitedHook) then
    OnActorExitedHook(PID, ExitReason);
end;

procedure NotifyOfMessageLink(LinkingPID, LinkedPID: String);
begin
  if Assigned(OnActorLinkedHook) then
    OnActorLinkedHook(LinkingPID, LinkedPID);
end;

procedure NotifyOfMessageSend(Sender, Target: TProcessID; Msg: TActorMessage);
begin
  if Assigned(OnMessageSentHook) then
    OnMessageSentHook(Sender, Target, Msg);
end;

//******************************************************************************
//* Unit Public functions/procedures                                           *
//******************************************************************************

function ConstructUUID: String;
var
  NewGuid: TGUID;
begin
  CreateGUID(NewGuid);
  Result := Lowercase(WithoutFirstAndLastChars(GUIDToString(NewGuid)));
end;

procedure ExitActor(Target: TProcessID; Reason: TTuple);
begin
  DefaultEnv.Exit(Target, Reason);
end;

procedure Kill(Target: TProcessID);
begin
  DefaultEnv.Kill(Target);
end;

function RPC(Target: TProcessID; Msg: TMessageTuple; Timeout: Cardinal): TTuple;
var
  Intf:        TActorInterfaceForRPC;
  ReroutedMsg: TTuple;
begin
  Result := nil;

  Intf := TActorInterfaceForRPC.Create(DefaultEnv);
  try
    ReroutedMsg := Msg.RouteTo(Intf.PID);
    try
      Intf.Send(Target, ReroutedMsg);
    finally
      ReroutedMsg.Free;
    end;

    // Approximate Erlang version:
    // receive
    //   X -> X
    // after Timeout
    //   nil
    // end.
    Intf.Receive(Intf.FindAny, Intf.StoreFirstMessage, Timeout, Intf.NullThunk);

    if (Intf.Result <> nil) then
      Result := Intf.Result.Copy as TTuple;
  finally
    Intf.Free;
  end;
end;

procedure SendActorMessage(Target: TProcessID; Msg: TTuple);
begin
  DefaultEnv.PrimitiveSend(TActor.RootActor, Target, Msg);
end;

function Spawn(ActorType: TActorClass): TProcessID;
begin
  Result := DefaultEnv.PrimitiveSpawn(ActorType, TActor.RootActor);
end;

function Spawn(T: TThunk): TProcessID;
begin
  Result := DefaultEnv.PrimitiveSpawn(T);
end;

//******************************************************************************
//* TTerm                                                                      *
//******************************************************************************
//* TTerm Public methods *******************************************************

function TTerm.AsString: String;
begin
  // By default, do nothing:
  Result := '';
end;

function TTerm.Copy: TTerm;
begin
  Result := nil;
  
  RaiseAbstractError(Self.ClassName, 'Copy');
end;

function TTerm.Equals(Other: TTerm): Boolean;
begin
  Result := Self.ClassType = Other.ClassType;
end;

function TTerm.IsBoolean: Boolean;
begin
  Result := false;
end;

function TTerm.IsInteger: Boolean;
begin
  Result := false;
end;

function TTerm.IsProcessID: Boolean;
begin
  Result := false;
end;

function TTerm.IsString: Boolean;
begin
  Result := false;
end;

function TTerm.IsTuple: Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TBooleanTerm                                                               *
//******************************************************************************
//* TBooleanTerm Public methods ************************************************

constructor TBooleanTerm.Create(Value: Boolean);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TBooleanTerm.AsString: String;
begin
  Result := BoolStrs[Self.Value];
end;

function TBooleanTerm.Copy: TTerm;
begin
  Result := TBooleanTerm.Create(Self.Value);
end;

function TBooleanTerm.Equals(Other: TTerm): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TBooleanTerm(Other).Value);
end;

function TBooleanTerm.IsBoolean: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIntegerTerm                                                               *
//******************************************************************************
//* TIntegerTerm Public methods ************************************************

constructor TIntegerTerm.Create(Value: Integer);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TIntegerTerm.AsString: String;
begin
  Result := IntToStr(Self.Value);
end;

function TIntegerTerm.Copy: TTerm;
begin
  Result := TIntegerTerm.Create(Self.Value);
end;

function TIntegerTerm.Equals(Other: TTerm): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TIntegerTerm(Other).Value);
end;

function TIntegerTerm.IsInteger: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TProcessIDTerm                                                             *
//******************************************************************************
//* TProcessIDTerm Public methods **********************************************

constructor TProcessIDTerm.Create(Value: TProcessID);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TProcessIDTerm.AsString: String;
begin
  Result := Format('{%s}', [Self.Value]);
end;

function TProcessIDTerm.Copy: TTerm;
begin
  Result := TProcessIDTerm.Create(Self.Value);
end;

function TProcessIDTerm.Equals(Other: TTerm): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TProcessIDTerm(Other).Value);
end;

function TProcessIDTerm.IsProcessID: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TStringTerm                                                                *
//******************************************************************************
//* TStringTerm Public methods *************************************************

constructor TStringTerm.Create(Value: String);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TStringTerm.AsString: String;
begin
  Result := Format('"%s"', [Self.Value]);
end;

function TStringTerm.Copy: TTerm;
begin
  Result := TStringTerm.Create(Self.Value);
end;

function TStringTerm.Equals(Other: TTerm): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TStringTerm(Other).Value);
end;

function TStringTerm.IsString: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TTuple                                                                     *
//******************************************************************************
//* TTuple Public methods ******************************************************

constructor TTuple.Create;
begin
  inherited Create;

  Self.TupleElements := TObjectList.Create(true);
end;

destructor TTuple.Destroy;
begin
  Self.TupleElements.Free;

  inherited Destroy;
end;

procedure TTuple.Add(E: TTerm);
begin
  Self.TupleElements.Add(E.Copy);
end;

procedure TTuple.AddBoolean(B: Boolean);
begin
  Self.TupleElements.Add(TBooleanTerm.Create(B));
end;

procedure TTuple.AddInteger(I: Integer);
begin
  Self.TupleElements.Add(TIntegerTerm.Create(I));
end;

procedure TTuple.AddProcessID(PID: TProcessID);
begin
  Self.TupleElements.Add(TProcessIDTerm.Create(PID));
end;

procedure TTuple.AddString(S: String);
begin
  Self.TupleElements.Add(TStringTerm.Create(S));
end;

function TTuple.AsString: String;
var
  I: Integer;
begin
  // Return a lisp-like s-expression representing this tuple.

  Result := '(';

  for I := 0 to Self.Count - 1 do
    Result := Result + Self[I].AsString + ' ';

  Result := Trim(Result) + ')'
end;

function TTuple.Copy: TTerm;
var
  I: Integer;
  NewT: TTuple;
begin
  NewT := Self.ThisClassType.Create;

  for I := 0 to Self.Count - 1 do
    NewT.Add(Self[I]);

  Result := NewT;
end;

function TTuple.Count: Integer;
begin
  Result := Self.TupleElements.Count;
end;

function TTuple.Equals(Other: TTerm): Boolean;
var
  I:          Integer;
  OtherTuple: TTuple;
begin
  Result := inherited Equals(Other);

  if Result then begin
    OtherTuple := Other as TTuple;

    Result := Self.Count = OtherTuple.Count;

    if Result then begin
      for I := 0 to Self.Count - 1 do
        Result := Result and Self[I].Equals(OtherTuple[I]);
    end;
  end;
end;

function TTuple.RouteTo(NewDest: TProcessID): TTuple;
var
  I: Integer;
begin
  // "RouteToing" means rewriting the sender's PID (in the second element) with a
  // new PID. Thus, the recipient of the message will route the response to
  // NewDest.

  Result := Self.ThisClassType.Create;

  if (Self.Count = 0) then Exit;

  Result.Add(Self[0]);

  Result.AddProcessID(NewDest);
  for I := 2 to Self.Count - 1 do
    Result.Add(Self[I]);
end;

function TTuple.IsTuple: Boolean;
begin
  Result := true;
end;

//* TTuple Private methods *****************************************************

function TTuple.GetElement(Index: Integer): TTerm;
begin
  Result := Self.TupleElements[Index] as TTerm;
end;

function TTuple.ThisClassType: TTupleClass;
begin
  Result := TTupleClass(Self.ClassType);
end;

//******************************************************************************
//* TMessageTuple                                                              *
//******************************************************************************
//* TMessageTuple Public methods ***********************************************

constructor TMessageTuple.Create(MessageName: String; ReplyTo: TProcessID);
var
  NoParams: TTuple;
begin
  NoParams := TTuple.Create;
  try
    Create(MessageName, ReplyTo, NoParams);
  finally
    NoParams.Free;
  end;
end;

constructor TMessageTuple.Create(MessageName: String; ReplyTo: TProcessID; Parameters: TTuple);
begin
  inherited Create;

  Self.AddString(MessageName);
  Self.AddProcessID(ReplyTo);
  Self.Add(Parameters);
end;

constructor TMessageTuple.Overlay(Msg: TTuple);
var
  I: Integer;
begin
  // Ikaria's infrastructure doesn't know (or care) about how a tuple's laid
  // out. We do, however. Thus, sometimes one wants to turn a generic TTuple
  // into a TMessageTuple to avoid hard-coding indices all over the place.
  //
  // Be warned: if Msg is not the same "shape" as a TMessageTuple, you will
  // suffer agonies like EInvalidCast when calling the getters!

  inherited Create;

  for I := 0 to Msg.Count - 1 do
    Self.Add(Msg[I]);
end;

//* TMessageTuple Private methods **********************************************

function TMessageTuple.GetReplyTo: TProcessID;
begin
  Result := (Self[1] as TProcessIDTerm).Value;
end;

function TMessageTuple.GetMessageName: String;
begin
  Result := (Self[0] as TStringTerm).Value;
end;

function TMessageTuple.GetParameters: TTuple;
begin
  Result := Self[2] as TTuple;
end;

//******************************************************************************
//* TActorMessage                                                              *
//******************************************************************************
//* TActorMessage Public methods ***********************************************

constructor TActorMessage.Create(E: TActorEnvironment);
begin
  inherited Create;

  Self.Environment := E;

  Self.fData := TTuple.Create;
  Self.fTag  := Self.Environment.NextTag;
end;

destructor TActorMessage.Destroy;
begin
  Self.fData.Free;

  inherited Destroy;
end;

procedure TActorMessage.Accept(Actor: TActor);
begin
  // By default, do nothing.
end;

function TActorMessage.AsString: String;
begin
  Result := Format('(message :tag "%s" :data %s)', [Self.Tag, Self.Data.AsString]);
end;

function TActorMessage.Copy: TActorMessage;
begin
  Result := TActorMessage.Create(Self.Environment);
  Result.Data := Self.Data.Copy as TTuple;
  Result.fTag := Self.Tag;
end;

//******************************************************************************
//* TActorMailbox                                                              *
//******************************************************************************
//* TActorMailbox Public methods ***********************************************

constructor TActorMailbox.Create(Owner: TActorInterface);
begin
  inherited Create;

  Self.Environment := Owner.Environment;

  Self.LinkSet := TStringList.Create;
  Self.LinkSet.Duplicates := dupIgnore;
  Self.LinkSet.Sorted     := true;

  Self.Lock      := TCriticalSection.Create;
  Self.Messages  := TObjectList.Create(false);
  Self.SaveQueue := TObjectList.Create(false);

  Self.fPID := Self.Environment.PrimitiveRegisterActor(Self);
end;

destructor TActorMailbox.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.Environment.PrimitiveUnregisterActor(Self);
    Self.Purge;
    Self.SaveQueue.Free;
    Self.Messages.Free;
    Self.LinkSet.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TActorMailbox.AddMessage(Msg: TActorMessage);
begin
  // Store a copy of Msg in our mailbox.
  // Move any messages in the save queue back into the mailbox proper
  // (preserving arrival order).

  Self.Lock.Acquire;
  try
    Self.Messages.Add(Msg.Copy);

    Self.RestoreSaveQueue;

    Self.NotifyOfMessageArrived;
  finally
    Self.Lock.Release;
  end;
end;

function TActorMailbox.IsEmpty: Boolean;
begin
  Self.Lock.Acquire;
  try
    Result := Self.Messages.Count = 0;
  finally
    Self.Lock.Release;
  end;
end;

function  TActorMailbox.FindAndProcessMessage(Table: TActorMessageTable;
                                              var ConditionIndex: Integer): TActorMessage;
var
  FoundIndex: Integer;
  FoundMsg:   TActorMessage;
  I, J:       Integer;
  M:          TActorMessage;
begin
  ConditionIndex := -1;
  FoundMsg       := nil;
  Result         := nil;

  Self.Lock.Acquire;
  try
    FoundIndex := Self.Messages.Count;
    for I := 0 to Self.Messages.Count - 1 do begin
      M := Self.MessageAt(I);

      for J := 0 to Table.Count - 1 do begin
        if Table.Conditions[J](M.Data) then begin
          FoundIndex := I;
          FoundMsg := M;
          Break;
        end;
      end;

      if Assigned(FoundMsg) then begin
        ConditionIndex := J;
        Result := FoundMsg.Copy;
        Self.FreeMessage(Self.Messages, FoundMsg);
        Break;
      end;
    end;
    Self.MoveMessages(Self.Messages, Self.SaveQueue, 0, FoundIndex - 1);
  finally
    Self.Lock.Release;
  end;
end;

function TActorMailbox.FindMessage(Condition: TMessageFinder): TActorMessage;
var
  FoundIndex: Integer;
  I:          Integer;
  M:          TActorMessage;
begin
  // Return the first matching message, and remove it from the mailbox.
  // Move all non-matching-but-checked messages to the save queue (preserving
  // arrival order).

  Self.Lock.Acquire;
  try
    Result := nil;

    FoundIndex := Self.Messages.Count;
    for I := 0 to Self.Messages.Count - 1 do begin
      M := Self.MessageAt(I);

      if Condition(M.Data) then begin
        FoundIndex := I;
        Result := M.Copy;
        Self.FreeMessage(Self.Messages, M);
        Break;
      end;
    end;
    Self.MoveMessages(Self.Messages, Self.SaveQueue, 0, FoundIndex - 1);
  finally
    Self.Lock.Release;
  end;
end;

procedure TActorMailbox.Link(PID: TProcessID);
begin
  Self.Environment.PrimitiveLink(Self.PID, PID);
end;

procedure TActorMailbox.Purge;
begin
  // Summarily delete all stored messages.

  Self.Lock.Acquire;
  try
    Self.FreeMessages(Self.Messages);
    Self.FreeMessages(Self.SaveQueue);
  finally
    Self.Lock.Release;
  end;
end;

procedure TActorMailbox.Timeout;
begin
  // Call this method when you've timed out waiting for a message.
  // Move any messages in the save queue back into the mailbox proper
  // (preserving arrival order).

  Self.Lock.Acquire;
  try
    Self.RestoreSaveQueue;
  finally
    Self.Lock.Release;
  end;
end;

//* TActorMailbox Private methods **********************************************

procedure TActorMailbox.FreeMessage(L: TObjectList; M: TActorMessage);
begin
  L.Remove(M);
  M.Free;
end;

procedure TActorMailbox.FreeMessages(L: TObjectList);
begin
  while L.Count > 0 do begin
    L[0].Free;
    L.Delete(0);
  end;
end;

function TActorMailbox.MessageAt(Index: Integer): TActorMessage;
begin
  Result := Self.Messages[Index] as TActorMessage;
end;

procedure TActorMailbox.MoveMessages(Src, Dest: TObjectList; FromIndex, ToIndex: Integer);
var
  I: Integer;
begin
  for I := FromIndex to ToIndex do
    Dest.Add(Src[I]);

  for I := FromIndex to ToIndex do
    Src.Delete(FromIndex);
end;

procedure TActorMailbox.NotifyOfMessageArrived;
begin
  if Assigned(Self.fOnMessageArrived) then
    Self.fOnMessageArrived(Self);
end;

procedure TActorMailbox.RestoreSaveQueue;
begin
  Self.MoveMessages(Self.SaveQueue, Self.Messages, 0, Self.SaveQueue.Count - 1);
end;

procedure TActorMailbox.SetOnMessageArrived(Value: TNotifyEvent);
begin
  Self.Lock.Acquire;
  try
    Self.fOnMessageArrived := Value;
  finally
    Self.Lock.Release;
  end;
end;

//******************************************************************************
//* TActorMessageActionPair                                                    *
//******************************************************************************
//* TActorMessageActionPair Public methods *************************************

constructor TActorMessageActionPair.Create(Condition: TMessageFinder; Action: TActOnMessage);
begin
  inherited Create;

  Self.fAction    := Action;
  Self.fCondition := Condition;
end;

//******************************************************************************
//* TActorMessageTable                                                         *
//******************************************************************************
//* TActorMessageTable Public methods ******************************************

constructor TActorMessageTable.Create;
begin
  inherited Create;

  Self.List := TObjectList.Create(true);
end;

destructor TActorMessageTable.Destroy;
begin
  Self.List.Free;

  inherited Destroy;
end;

procedure TActorMessageTable.Add(Condition: TMessageFinder; Action: TActOnMessage);
begin
  Self.List.Add(TActorMessageActionPair.Create(Condition, Action));
end;

function TActorMessageTable.Count: Integer;
begin
  Result := Self.List.Count;
end;

//* TActorMessageTable Private methods *****************************************

function TActorMessageTable.GetAction(Index: Integer): TActOnMessage;
begin
  Result := TActorMessageActionPair(Self.List[Index]).Action;
end;

function TActorMessageTable.GetCondition(Index: Integer): TMessageFinder;
begin
  Result := TActorMessageActionPair(Self.List[Index]).Condition;
end;

//******************************************************************************
//* TActorInterface                                                            *
//******************************************************************************
//* TActorInterface Public methods *********************************************

constructor TActorInterface.Create(E: TActorEnvironment);
begin
  inherited Create;

  Self.Environment := E;

  Self.fTerminated := false;
  Self.Lock        := TCriticalSection.Create;
  Self.Mailbox     := TActorMailbox.Create(Self);
  Self.MsgEvent    := TSimpleEvent.Create;
  Self.PendingMsgs := 0;

  Self.Mailbox.OnMessageArrived := Self.NewMessageArrived;

  NotifyOfActorCreation(Self.ClassName, Self.PID);
end;

destructor TActorInterface.Destroy;
begin
  Self.fTerminated := true;
  Self.MsgEvent.Free;
  Self.Mailbox.Free;
  Self.Lock.Free;

  inherited Destroy;
end;

function TActorInterface.FindAny(Msg: TTuple): Boolean;
begin
  Result := true;
end;

procedure TActorInterface.Link(PID: TProcessID);
begin
  Self.Mailbox.Link(PID);
end;

function TActorInterface.MatchMessageName(Msg: TTuple; MsgName: String): Boolean;
var
  O: TMessageTuple;
begin
  try
    O := TMessageTuple.Overlay(Msg);
    try
      Result := O.MessageName = MsgName;
    finally
      O.Free;
    end;
  except
    Result := false;
  end;
end;

procedure TActorInterface.Receive(Matching: TMessageFinder;
                                  Action: TActOnMessage);
var
  T: TActorMessageTable;
begin
  T := TActorMessageTable.Create;
  try
    T.Add(Matching, Action);
    Self.Receive(T);
  finally
    T.Free;
  end;
end;

procedure TActorInterface.Receive(Matching: TMessageFinder;
                                  Action: TActOnMessage;
                                  Timeout: Cardinal);
begin
  Self.Receive(Matching, Action, Timeout, Self.NullThunk);
end;

procedure TActorInterface.Receive(Matching: TMessageFinder;
                                  Action: TActOnMessage;
                                  Timeout: Cardinal;
                                  TimeoutAction: TThunk);
var
  T: TActorMessageTable;
begin
  T := TActorMessageTable.Create;
  try
    T.Add(Matching, Action);
    Self.Receive(T, Timeout, TimeoutAction);
  finally
    T.Free;
  end;
end;

procedure TActorInterface.Receive(Table: TActorMessageTable);
begin
  // Block execution (for an unbounded length of time) until we receive a
  // message matching a condition in Table.

  while not Self.Terminated do begin
    if Self.WaitForMessage(StandardWaitTime) then begin
      Self.Environment.PrimitiveReceive(Self.Mailbox, Table);
      Break;
    end;
  end;
end;

procedure TActorInterface.Receive(Table: TActorMessageTable;
                                  Timeout: Cardinal);
begin
  Self.Receive(Table, Timeout, Self.NullThunk);
end;

procedure TActorInterface.Receive(Table: TActorMessageTable;
                                  Timeout: Cardinal;
                                  TimeoutAction: TThunk);
var
  EndTime:  TDateTime;
  Finished: Boolean;
begin
  // Block execution until we receive a message matching condition in Table. If
  // we wait more than Timeout milliseconds, run TimeoutAction. If something bad
  // happens, raise an exception.

  EndTime  := Now + Timeout*OneMillisecond;
  Finished := false;

  while not Self.Terminated and (Now < EndTime) do begin
    if Self.WaitForMessage(StandardWaitTime) then begin
      Finished := Self.Environment.PrimitiveReceive(Self.Mailbox, Table);
      if Finished then Break;
    end;
  end;

  if not Self.Terminated and not Finished then TimeoutAction;
end;

procedure TActorInterface.Send(Target: TProcessID; MsgName: String);
var
  NoParams: TTuple;
begin
  NoParams := TTuple.Create;
  try
    Self.Send(Target, MsgName, NoParams);
  finally
    NoParams.Free;
  end;
end;

procedure TActorInterface.Send(Target: TProcessID; MsgName: String; Parameters: TTuple);
var
  Msg: TMessageTuple;
begin
  Msg := TMessageTuple.Create(MsgName, Self.Mailbox.PID, Parameters);
  try
    Self.Send(Target, Msg);
  finally
    Msg.Free;
  end;
end;

procedure TActorInterface.Send(Target: TProcessID; T: TTuple);
begin
  Self.Environment.PrimitiveSend(Self.Mailbox.PID, Target, T);
end;

function TActorInterface.Spawn(ActorType: TActorClass): TProcessID;
begin
  Result := Self.Environment.PrimitiveSpawn(ActorType, Self.PID);
end;

function TActorInterface.SpawnLink(ActorType: TActorClass): TProcessID;
begin
  Result := Self.Environment.PrimitiveSpawnLink(ActorType, Self.PID);
end;

//* TActorInterface Protected methods ******************************************

procedure TActorInterface.DoNothing(Msg: TTuple);
begin
  // Often, this method is used to turn an asynchronous communication into a
  // synchronous _procedure_ call. 
end;

//* TActorInterface Private methods ********************************************

function TActorInterface.GetPID: TProcessID;
begin
  Result := Self.Mailbox.PID;
end;

procedure TActorInterface.NewMessageArrived(Sender: TObject);
begin
  // This executes in the context of whatever thread's currently controlling the
  // mailbox.
  // Sender points to the mailbox.

  Self.Lock.Acquire;
  try
    Inc(Self.PendingMsgs);
    Self.MsgEvent.SetEvent;
  finally
    Self.Lock.Release;
  end;
end;

procedure TActorInterface.NullThunk;
begin
end;

function TActorInterface.WaitForMessage(Timeout: Cardinal): Boolean;
  procedure Fail(Reason: String);
  const
    ErrorMsg = 'Unexpected result waiting for response: %s';
  begin
    raise ETimeout.Create(Format(ErrorMsg, [Reason]));
  end;
begin
  // Return true if we received a message, false otherwise.
  // If something goes badly wrong, raise an exception.

  Result := false;

  case Self.MsgEvent.WaitFor(Timeout) of
    wrSignaled:  begin
      Result := true;

      Self.Lock.Acquire;
      try
        Dec(Self.PendingMsgs);
        if (Self.PendingMsgs = 0) then
          Self.MsgEvent.ResetEvent;
      finally
        Self.Lock.Release;
      end;
    end;
    wrTimeout:   ; // Just keep waiting
    wrError:     Fail(SysErrorMessage(Self.MsgEvent.LastError));
    wrAbandoned: Fail('Event abandoned');
  else
    Fail('Unknown error');
  end;
end;

//******************************************************************************
//* TActorInterfaceForRPC                                                      *
//******************************************************************************
//* TActorInterfaceForRPC Public methods ***************************************

destructor TActorInterfaceForRPC.Destroy;
begin
  Self.fResult.Free;

  inherited Destroy;
end;

procedure TActorInterfaceForRPC.NullAction(Msg: TTuple);
begin
  // Do nothing.
end;

procedure TActorInterfaceForRPC.NullThunk;
begin
  // Do nothing.
end;

procedure TActorInterfaceForRPC.Reset;
begin
  if Assigned(Self.fResult) then begin
    Self.fResult.Free;
    Self.fResult := nil;
  end;
end;

procedure TActorInterfaceForRPC.StoreFirstMessage(Msg: TTuple);
begin
  Self.Reset;
    
  Self.fResult := Msg.Copy as TTuple;
end;


//******************************************************************************
//* TActor                                                                     *
//******************************************************************************
//* TActor Public methods ******************************************************

class function TActor.RootActor: TProcessID;
begin
  // The "root actor" is not used at all, at the moment. We thus don't waste
  // resources creating one.

  Result := '';
end;

constructor TActor.Create(E: TActorEnvironment; Parent: TProcessID);
begin
  inherited Create(E);

  Self.fKilled  := false;
  Self.ParentID := Parent;

  Self.MsgTable := TActorMessageTable.Create;
  Self.RegisterRequiredActions(Self.MsgTable);
  Self.RegisterActions(Self.MsgTable);
end;

destructor TActor.Destroy;
begin
  Self.MsgTable.Free;

  inherited Destroy;
end;

procedure TActor.SendKilledExit;
var
  K: TTuple;
begin
  K := TTuple.Create;
  try
    K.AddString(ExitReasonKilled);
    Self.SendExit(K);
  finally
    K.Free;
  end;
end;

procedure TActor.SetUp;
begin
  // Put any initial message-sending stuff here.
end;

procedure TActor.Step;
begin
  // Perform one step of a computation. By default, that means waiting for a
  // message.

  Self.Receive(Self.MsgTable);
end;

procedure TActor.TearDown;
begin
  // Put any cleanup stuff here.
end;

procedure TActor.Terminate;
begin
  Self.Terminated := true;

  Self.SendNormalExit;
end;

procedure TActor.Terminate(Reason: TTuple);
begin
  Self.Terminated := true;

  Self.SendExit(Reason);
end;

//* TActor Protected methods ***************************************************

procedure TActor.BeforeExit(Msg: TTuple);
begin
  // By default do nothing.
end;

function TActor.FindExit(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ExitMsg);
end;

function TActor.FindKill(Msg: TTuple): Boolean;
begin
  Result := Msg.Count > 1;

  if Result then begin
    Result := Msg[0].IsString
           and (TStringTerm(Msg[0]).Value = ExitMsg)
           and Msg[1].IsString
           and (TStringTerm(Msg[1]).Value = ExitReasonKill);
  end;
end;

function TActor.MatchAny(Msg: TTuple): Boolean;
begin
  Result := true;
end;

procedure TActor.RegisterActions(Table: TActorMessageTable);
begin
  // Register actions against conditions
end;

procedure TActor.SendExceptionalExit(E: Exception);
var
  Reason: TTuple;
begin
  Reason := TTuple.Create;
  try
    Reason.AddString(Format(ExitReasonException, [E.ClassName, E.Message]));
    Self.SendExit(Reason);
  finally
    Reason.Free;
  end;
end;

procedure TActor.SendNormalExit;
var
  Reason: TTuple;
begin
  Reason := TTuple.Create;
  try
    Reason.AddString(ExitReasonNormal);
    Self.SendExit(Reason);
  finally
    Reason.Free;
  end;
end;

//* TActor Private methods *****************************************************

procedure TActor.ReactToExit(Msg: TTuple);
begin
  Self.BeforeExit(Msg);
  Self.Terminate;
end;

procedure TActor.ReactToKill(Msg: TTuple);
begin
  Self.fKilled := true;
  Self.Terminate;
end;

procedure TActor.RegisterRequiredActions(Table: TActorMessageTable);
begin
  // The Kill message - ("exit" {some-pid} ("kill")) - has a tighter definition
  // than a normal exit.
  //
  // Hence we try match it first, because FindExit will find all the OTHER
  // possible exit messages.
  Table.Add(Self.FindKill, Self.ReactToKill);
  Table.Add(Self.FindExit, Self.ReactToExit);  
end;

procedure TActor.SendExit(Reason: TTuple);
var
  Exit: TTuple;
  I:    Integer;
begin
  Exit := TMessageTuple.Create(ExitMsg, Self.PID, Reason);
  try
    for I := 0 to Self.Mailbox.LinkSet.Count - 1 do
      Self.Send(Self.Mailbox.LinkSet[I], ExitMsg, Reason);

    NotifyOfActorExit(Self.PID, Exit);
  finally
    Exit.Free;
  end;
end;

//******************************************************************************
//* TThunkActor                                                                *
//******************************************************************************
//* TThunkActor Public methods *************************************************

procedure TThunkActor.Step;
begin
  Self.Thunk;
  Self.Terminate;
end;

//******************************************************************************
//* TActorRunner                                                               *
//******************************************************************************
//* TActorRunner Public methods ************************************************

constructor TActorRunner.Create(A: TActor);
begin
  inherited Create(true);

  Self.Actor           := A;
  Self.FreeOnTerminate := true;
end;

destructor TActorRunner.Destroy;
begin
  Self.Actor.Free;

  inherited Destroy;
end;

//* TActorRunner Protected methods *********************************************

procedure TActorRunner.Execute;
begin
  try
    try
      Self.Actor.SetUp;
      try
        while not Self.Terminated and not Self.Actor.Terminated do
          Self.Actor.Step;
      finally
        Self.Actor.TearDown;
      end;

      if Self.Actor.Killed then
        Self.Actor.SendKilledExit
      else
        Self.Actor.SendNormalExit;
    except
      on E: Exception do
        Self.Actor.SendExceptionalExit(E);
    end;
  finally
      if Assigned(Self.fOnExit) then
      Self.fOnExit(Self);
  end;
end;

//******************************************************************************
//* TActorEnvironment                                                          *
//******************************************************************************
//* TActorEnvironment Public methods *******************************************

function TActorEnvironment.CreateKillMsg: TTuple;
begin
  Result := TTuple.Create;
  Result.AddString(ExitMsg);
  Result.AddString(ExitReasonKill);
end;

procedure TActorEnvironment.Exit(Target: TProcessID);
var
  E: TMessageTuple;
begin
  E := TMessageTuple.Create(ExitMsg, TActor.RootActor);
  try
    Self.PrimitiveSend('', Target, E);
  finally
    E.Free;
  end;
end;

procedure TActorEnvironment.Exit(Target: TProcessID; Reason: TTuple);
var
  E: TMessageTuple;
begin
  E := TMessageTuple.Create(ExitMsg, TActor.RootActor, Reason);
  try
    Self.PrimitiveSend('', Target, E);
  finally
    E.Free;
  end;
end;

function TActorEnvironment.IsProcessAlive(Target: TProcessID): Boolean;
begin
  Result := Assigned(Self.Whois(Target));
end;

procedure TActorEnvironment.Kill(Target: TProcessID);
var
  Kill: TTuple;
begin
  Kill := Self.CreateKillMsg;
  try
    Self.Exit(Target, Kill);
  finally
    Kill.Free;
  end;
end;

function TActorEnvironment.NextPID: TProcessID;
begin
  Result := '';

  RaiseAbstractError(Self.ClassName, ' NextPID');
end;

function TActorEnvironment.NextTag: String;
begin
  Result := '';

  RaiseAbstractError(Self.ClassName, 'NextTag');
end;

procedure TActorEnvironment.PrimitiveLink(LinkingPID, LinkedPID: TProcessID);
begin
  RaiseAbstractError(Self.ClassName, 'PrimitiveLink');
end;

function TActorEnvironment.PrimitiveReceive(Mailbox: TActorMailbox; Table: TActorMessageTable): Boolean;
begin
  Result := false;

  RaiseAbstractError(Self.ClassName, 'PrimitiveReceive');
end;

function TActorEnvironment.PrimitiveRegisterActor(A: TActorMailbox): TProcessID;
begin
  Result := '';

  RaiseAbstractError(Self.ClassName, 'PrimitiveRegisterActor');
end;

procedure TActorEnvironment.PrimitiveSend(Sender, Target: TProcessID; Msg: TTuple);
begin
  RaiseAbstractError(Self.ClassName, 'PrimitiveSend');
end;

function TActorEnvironment.PrimitiveSpawn(ActorType: TActorClass; Parent: TProcessID): TProcessID;
begin
  Result := '';

  RaiseAbstractError(Self.ClassName, 'PrimitiveSpawn(ActorType, PID)');
end;

function TActorEnvironment.PrimitiveSpawn(T: TThunk): TProcessID;
begin
  Result := '';

  RaiseAbstractError(Self.ClassName, 'PrimitiveSpawn(Thunk)');
end;

function TActorEnvironment.PrimitiveSpawnLink(ActorType: TActorClass; Parent: TProcessID): TProcessID;
begin
  Result := '';

  RaiseAbstractError(Self.ClassName, 'PrimitiveSpawnLink');
end;

procedure TActorEnvironment.PrimitiveUnregisterActor(A: TActorMailbox);
begin
  RaiseAbstractError(Self.ClassName, 'PrimitiveUnregisterActor');
end;

procedure TActorEnvironment.SetTrapExit(PID: TProcessID; TrapExits: Boolean);
begin
  RaiseAbstractError(Self.ClassName, 'SetTrapExit');
end;

//* TActorEnvironment Protected methods ****************************************

function TActorEnvironment.Whois(PID: TProcessID): TActorMailbox;
begin
  Result := nil;

  RaiseAbstractError(Self.ClassName, 'Whois');
end;

//******************************************************************************
//* TThreadedActorEnvironment                                                  *
//******************************************************************************
//* TThreadedActorEnvironment Public methods ***********************************

constructor TThreadedActorEnvironment.Create;
begin
  inherited Create;

  Self.ActorLock := TCriticalSection.Create;
  Self.Actors    := TStringList.Create;
  Self.Runners   := TObjectList.Create(false);
  Self.UsedPIDs  := TStringList.Create;

  // Tag generation
  Self.NextAvailableTag := 0;
  Self.TagLock          := TCriticalSection.Create;
end;

destructor TThreadedActorEnvironment.Destroy;
begin
  Self.Deactivate(Self.Actors);
  Self.WaitForAllActorsExit(Self.Actors);

  Self.Lock;
  try
    Self.Runners.Free;
    Self.UsedPIDs.Free;
    Self.Actors.Free;
  finally
    Self.Unlock;
  end;
  Self.ActorLock.Free;
  Self.TagLock.Free;

  inherited Destroy;
end;

function TThreadedActorEnvironment.NextPID: TProcessID;
begin
  Self.Lock;
  try
    repeat
      Result := ConstructUUID;
    until (UsedPIDS.IndexOf(Result) = -1);

    UsedPIDs.Add(Result);
  finally
    Self.Unlock;
  end;
end;

function TThreadedActorEnvironment.NextTag: String;
begin
  // For now, tags start at 0 and increment up.
  TagLock.Acquire;
  try
    Result := IntToStr(NextAvailableTag);
    Inc(NextAvailableTag);
  finally
    TagLock.Release;
  end;
end;

procedure TThreadedActorEnvironment.PrimitiveLink(LinkingPID, LinkedPID: TProcessID);
var
  A, B: TActorMailbox;
begin
  Self.Lock;
  try
    A := Self.Whois(LinkingPID);
    B := Self.Whois(LinkedPID);

    if Assigned(A) and Assigned(B) then begin
      A.LinkSet.Add(B.PID);
      B.LinkSet.Add(A.PID);
      NotifyOfMessageLink(LinkingPID, LinkedPID);
    end;
  finally
    Self.Unlock;
  end;
end;

function TThreadedActorEnvironment.PrimitiveReceive(Mailbox: TActorMailbox; Table: TActorMessageTable): Boolean; 
var
  I:   Integer;
  Msg: TActorMessage;
begin
  // Return true iff we receive a message matching one of Table's conditions.
  Result := false;

  Msg := Mailbox.FindAndProcessMessage(Table, I);
  try
    if Assigned(Msg) then begin
      Result := true;
      Table.Actions[I](Msg.Data);
    end;
  finally
    Msg.Free;
  end;
end;

function TThreadedActorEnvironment.PrimitiveRegisterActor(A: TActorMailbox): TProcessID;
begin
  Self.Lock;
  try
    Result := NextPID;

    Self.Actors.AddObject(Result, A);
  finally
    Self.Unlock;
  end;
end;

procedure TThreadedActorEnvironment.PrimitiveSend(Sender, Target: TProcessID; Msg: TTuple);
  procedure DeliverMsg(Target: TProcessID; Msg: TActorMessage);
  var
    TargetMbox: TActorMailbox;
  begin
    Self.Lock;
    try
      TargetMbox := Self.Whois(Target);

      if Assigned(TargetMbox) then
        TargetMbox.AddMessage(Msg);
    finally
      Self.Unlock;
    end;
  end;
var
  AMsg: TActorMessage;
begin
  // Send a message Msg from Sender to Target.
  //
  // It doesn't matter whether there is an actor with mail address Target.

  AMsg := TActorMessage.Create(Self);
  try
    AMsg.Data := Msg.Copy as TTuple;

    // We log before the actual delivery because it's possible for the receiving
    // Actor to process the message and deliver another message before we reach
    // the next statement after DeliverMsg. In other words, logging first means
    // that we preserve the time ordering of messages any log you might store.
    NotifyOfMessageSend(Sender, Target, AMsg);

    DeliverMsg(Target, AMsg);
  finally
    AMsg.Free;
  end;
end;

function TThreadedActorEnvironment.PrimitiveSpawn(ActorType: TActorClass; Parent: TProcessID): TProcessID;
var
  A: TActor;
begin
  A := ActorType.Create(Self, Parent);

  Result := A.PID;
  Self.ActivateActor(A);
end;

function TThreadedActorEnvironment.PrimitiveSpawn(T: TThunk): TProcessID;
var
  Thunker: TThunkActor;
begin
  Thunker := TThunkActor.Create(Self, TActor.RootActor);
  Thunker.Thunk := T;

  Result := Thunker.PID;
  Self.ActivateActor(Thunker);
end;

function TThreadedActorEnvironment.PrimitiveSpawnLink(ActorType: TActorClass; Parent: TProcessID): TProcessID;
var
  Child: TActor;
begin
  Child := ActorType.Create(Self, Parent);
  PrimitiveLink(Parent, Child.PID);

  Result := Child.PID;

  Self.ActivateActor(Child);
end;

procedure TThreadedActorEnvironment.PrimitiveUnregisterActor(A: TActorMailbox);
var
  Index: Integer;
begin
  Self.Lock;
  try
    Index := Self.Actors.IndexOf(A.PID);

    if (Index <> -1) then
      Self.Actors.Delete(Index);
  finally
    Self.Unlock;
  end;
end;

procedure TThreadedActorEnvironment.SetTrapExit(PID: TProcessID; TrapExits: Boolean);
begin
  Self.Lock;
  try
  finally
    Self.Unlock;
  end;
end;

//* TThreadedActorEnvironment Protected methods ********************************

procedure TThreadedActorEnvironment.Lock;
begin
  Self.ActorLock.Acquire;
end;

procedure TThreadedActorEnvironment.Unlock;
begin
  Self.ActorLock.Release;
end;

function TThreadedActorEnvironment.Whois(PID: TProcessID): TActorMailbox;
var
  Index: Integer;
begin
  // Return a pointer to the actor known by PID, or return nil if no such actor
  // exists.
  //
  // PRECONDITION: You've acquired ActorLock!
  Result := nil;

  Index := Self.Actors.IndexOf(PID);

  if (Index <> -1) then
    Result := Self.Actors.Objects[Index] as TActorMailbox;
end;

//* TThreadedActorEnvironment Private methods **********************************

procedure TThreadedActorEnvironment.ActivateActor(A: TActor);
var
  R: TActorRunner;
begin
  R := TActorRunner.Create(A);
  R.OnExit := Self.RemoveRunner;

  Self.Lock;
  try
    Self.Runners.Add(R);
  finally
    Self.Unlock;
  end;
  R.Resume;
end;

procedure TThreadedActorEnvironment.Deactivate(L: TStringList);
var
  I: Integer;
begin
  Self.Lock;
  try
    for I := 0 to L.Count - 1 do
      Self.Kill(L[I]);
  finally
    Self.Unlock;
  end;
end;

procedure TThreadedActorEnvironment.RemoveRunner(TerminatedThread: TObject);
begin
  Self.Lock;
  try
    Self.Runners.Remove(TerminatedThread);
  finally
     Self.Unlock;
  end;
end;

procedure TThreadedActorEnvironment.WaitForAllActorsExit(L: TStringList);
begin
  // Poll every 500 milliseconds to see whether all actors have terminated.
  while true do begin
    Self.Lock;
    try
      if (L.Count = 0) then Break;
    finally
      Self.Unlock;
    end;
    Sleep(OneSecond div 2);
  end;
end;

//******************************************************************************
//* TDebugActorEnvironment                                                     *
//******************************************************************************
//* TDebugActorEnvironment Public methods **************************************

constructor TDebugActorEnvironment.Create;
begin
  inherited Create;

  Self.fSentMessages := TStringList.Create;
end;

destructor TDebugActorEnvironment.Destroy;
begin
  Self.fSentMessages.Free;

  inherited Destroy;
end;

procedure TDebugActorEnvironment.PrimitiveSend(Sender, Target: TProcessID; Msg: TTuple);
begin
  Self.fSentMessages.Add(Format('%s -> %s: %s', [Sender, Target, Msg.AsString]));

  inherited PrimitiveSend(Sender, Target, Msg);
end;

procedure TDebugActorEnvironment.SnapshotSentMessages(Buffer: TStrings);
begin
  Buffer.Clear;

  Self.Lock;
  try
    Buffer.AddStrings(Self.fSentMessages);
  finally
    Self.Unlock;
  end;
end;

initialization
  DefaultEnv       := TThreadedActorEnvironment.Create;
  StandardWaitTime := OneSecond;
end.
