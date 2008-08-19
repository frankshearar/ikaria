unit Ikaria;

interface

uses
  Classes, Contnrs, SyncObjs, SysUtils;

type
  TActor = class;
  TActorClass = class of TActor;
  TProcessID = String;

  TTupleElement = class(TObject)
  public
    function AsString: String; virtual;
    function Copy: TTupleElement; virtual;
    function Equals(Other: TTupleElement): Boolean; virtual;
    function IsBoolean: Boolean; virtual;
    function IsInteger: Boolean; virtual;
    function IsProcessID: Boolean; virtual;
    function IsString: Boolean; virtual;
    function IsTuple: Boolean; virtual;
  end;

  TBooleanElement = class(TTupleElement)
  private
    fValue: Boolean;
  public
    constructor Create(Value: Boolean);

    function AsString: String; override;
    function Copy: TTupleElement; override;
    function Equals(Other: TTupleElement): Boolean; override;
    function IsBoolean: Boolean; override;

    property Value: Boolean read fValue;
  end;

  TIntegerElement = class(TTupleElement)
  private
    fValue: Integer;
  public
    constructor Create(Value: Integer);

    function AsString: String; override;
    function Copy: TTupleElement; override;
    function Equals(Other: TTupleElement): Boolean; override;
    function IsInteger: Boolean; override;

    property Value: Integer read fValue;
  end;

  TProcessIDElement = class(TTupleElement)
  private
    fValue: TProcessID;
  public
    constructor Create(Value: TProcessID);

    function AsString: String; override;
    function Copy: TTupleElement; override;
    function Equals(Other: TTupleElement): Boolean; override;
    function IsProcessID: Boolean; override;

    property Value: TProcessID read fValue;
  end;

  TStringElement = class(TTupleElement)
  private
    fValue: String;
  public
    constructor Create(Value: String);

    function AsString: String; override;
    function Copy: TTupleElement; override;
    function Equals(Other: TTupleElement): Boolean; override;
    function IsString: Boolean; override;

    property Value: String read fValue;
  end;

  TTuple = class(TTupleElement)
  private
    TupleElements: TObjectList;

    function GetElement(Index: Integer): TTupleElement;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Add(E: TTupleElement);
    procedure AddBoolean(B: Boolean);
    procedure AddInteger(I: Integer);
    procedure AddProcessID(PID: TProcessID);
    procedure AddString(S: String);
    function  AsString: String; override;
    function  Copy: TTupleElement; override;
    function  Count: Integer;
    function  Equals(Other: TTupleElement): Boolean; override;
    function  IsTuple: Boolean; override;

    property Elements[Index: Integer]: TTupleElement read GetElement; default;
  end;

  // I represent a message sent to an Actor.
  //
  // I will free the Data you give me.
  TActorMessage = class(TObject)
  private
    fData: TTuple;
    fTag:  String;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Accept(Actor: TActor); virtual;
    function  AsString: String; virtual;
    function  Copy: TActorMessage; virtual;

    property Data: TTuple read fData write fData;
    property Tag:  String read fTag;
  end;

  TMessageFinder = function(Msg: TActorMessage): Boolean of object;

  TActOnMessage = procedure(Msg: TActorMessage) of object;

  TActorMessageTable = class;

  // I store messages sent to an Actor.
  TActorMailbox = class(TObject)
  private
    fOnMessageArrived: TNotifyEvent;
    Lock:              TCriticalSection;
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
    constructor Create;
    destructor  Destroy; override;

    procedure AddMessage(Msg: TActorMessage);
    function  IsEmpty: Boolean;
    function  FindAndProcessMessage(Table: TActorMessageTable;
                                    var ConditionIndex: Integer): TActorMessage;
    function  FindMessage(Condition: TMessageFinder): TActorMessage;
    procedure Purge;
    procedure Timeout;

    property OnMessageArrived: TNotifyEvent read fOnMessageArrived write SetOnMessageArrived;
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

  // I represent an execution context. I send and receive messages to and from
  // other Actors.
  TActor = class(TThread)
  private
    fPID:     TProcessID;
    Mailbox:  TActorMailbox;
    MsgEvent: TEvent;

    function  FindKill(Msg: TActorMessage): Boolean;
    procedure NewMessageArrived(Sender: TObject);
    procedure NullMethod;
    procedure ReactToKill(Msg: TActorMessage);
    procedure RegisterRequiredActions(Table: TActorMessageTable);
    procedure WaitForMessage(MillisecondTimeout: Cardinal);
  protected
    MsgTable: TActorMessageTable;
    ParentID: TProcessID;

    procedure Execute; override;
    procedure Receive(Matching: TMessageFinder;
                      Action: TActOnMessage); overload;
    procedure Receive(Matching: TMessageFinder;
                      Action: TActOnMessage;
                      Timeout: Cardinal;
                      TimeoutAction: TThunk); overload;
    procedure Receive(Table: TActorMessageTable); overload;
    procedure Receive(Table: TActorMessageTable;
                      Timeout: Cardinal;
                      TimeoutAction: TThunk); overload;
    procedure RegisterActions(Table: TActorMessageTable); virtual;
    procedure Run; virtual;
    procedure Send(Target: TProcessID; Msg: TTuple);
    procedure SendExceptionalExit(E: Exception);
    procedure SendNormalExit;
    function  Spawn(ActorType: TActorClass): TProcessID;
  public
    class function RootActor: TProcessID;

    constructor Create(Parent: TProcessID); virtual;
    destructor  Destroy; override;

    property PID: TProcessID read fPID;
  end;

  TEventMapping = class(TObject)
  private
    fEvent:  TEvent;
    fPID:    TProcessID;
    fResult: TTuple;
  public
    constructor Create(Event: TEvent; PID: TProcessID);

    function  HasResult: Boolean;
    procedure SetResult(Value: TTuple);

    property Event:  TEvent     read fEvent;
    property PID:    TProcessID read fPID;
    property Result: TTuple     read fResult;
  end;

  TEventDictionary = class(TObject)
  end;

  TRootActor = class(TActor)
  private
    fEvents: TEventDictionary;

    function  FindProxy(Msg: TActorMessage): Boolean;
    function  FindResponse(Msg: TActorMessage): Boolean;
    procedure ProxyMsg(Msg: TActorMessage);
    procedure RelayResponse(Msg: TActorMessage);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  public
    constructor Create(Parent: TProcessID); override;
    destructor  Destroy; override;

    property Events: TEventDictionary read fEvents;
  end;

  // I translate messages sent to me into PostMessages to a Windows message
  // queue.
  TWindowsMessageForwarder = class(TActor)
  private
    function  MatchAll(Msg: TActorMessage): Boolean;
    procedure ForwardToMessageQueue(Msg: TActorMessage);
  protected
    procedure Run; override;
  end;

  EActorException = class(Exception);
  ETimeout = class(Exception);

// Unconditionally kill an Actor. Think hard before using it!
procedure Kill(Target: TProcessID);

// Send a message to a particular actor, and wait for a response. Return the
// response, or fail after a timeout.
function RPC(Target: TProcessID; Msg: TTuple; Timeout: Cardinal): TTuple;

// Send a message to a particular Actor.
procedure SendActorMessage(Target: TProcessID; Msg: TTuple);

// Spawn a new Actor of the given type as a child of the Root Actor, returning
// its PID.
function Spawn(ActorType: TActorClass): TProcessID;

// Wait for a particular Actor to send you a message matching some Condition.
function WaitForMessageFrom(Target: TProcessID; Condition: TMessageFinder): TActorMessage;

const
  ActorCreatedMsg = 'Actor %s created (of type %s)';
  ActorExitedMsg  = 'Actor %s exited with reason %s';
  ActorFreedMsg   = 'Actor %s freed';
  MessageSentMsg  = 'Actor %s sent message to actor %s: %s';

const
  ExitMsg             = 'exit';
  ExitReasonException = '%s: %s';
  ExitReasonNormal    = 'normal';
  ExitReasonKill       = 'kill';
  RpcProxyMsg          = 'rpc-proxy';

// String conversion
const
  BoolStrs: array[false..true] of String = ('false', 'true');

// Introspection callbacks
type
  TActorEventProc  = procedure(ActorType, PID, Event: String);
  TActorMsgProc    = procedure(PID: String; Data: TTuple);
  TMessageSendProc = procedure(Sender, Target: TProcessID; Msg: TActorMessage);

var
  OnActorCreatedHook: TActorEventProc;
  OnActorExitedHook:  TActorMsgProc;
  OnMessageSentHook:  TMessageSendProc;

implementation

var
  Actors:    TStringList;
  Root:      TActor;
  ActorLock: TCriticalSection; // Used to lock access to Actors.
  UsedPIDs:  TStringList;
  TempData:  TTuple;
  TempEvent: TEvent;

const
  TempEventPID = '86657b58-6e06-4b02-8469-b7d6f1ee652f';

// Tag generation
var
  NextAvailableTag: Int64;
  TagLock: TCriticalSection;

//******************************************************************************
//* Unit Private functions/procedures                                          *
//******************************************************************************

function WithoutFirstAndLastChars(const S: String): String;
begin
  Result := Copy(S, 2, Length(S) - 2);
end;

function ConstructUUID: String;
var
  NewGuid: TGUID;
begin
  CreateGUID(NewGuid);
  Result := Lowercase(WithoutFirstAndLastChars(GUIDToString(NewGuid)));
end;

function DataFor(E: TEvent): TTuple;
begin
  Result := TempData;
end;

function NextPID: TProcessID;
begin
  ActorLock.Acquire;
  try
    repeat
      Result := ConstructUUID;
    until (UsedPIDS.IndexOf(Result) = -1);

    UsedPIDs.Add(Result);
  finally
    ActorLock.Release;
  end;
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

procedure NotifyOfMessageSend(Sender, Target: TProcessID; Msg: TActorMessage);
begin
  if Assigned(OnMessageSentHook) then
    OnMessageSentHook(Sender, Target, Msg);
end;

function NextTag: String;
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

function PIDFor(E: Tevent): TProcessID;
begin
  Result := TempEventPID;
end;

procedure PrimitiveReceive(Target: TProcessID; Msg: TActorMessage);
begin
end;

function PrimitiveRegisterActor(A: TActor): TProcessID;
begin
  ActorLock.Acquire;
  try
    Result := NextPID;

    Actors.AddObject(Result, A);
  finally
    ActorLock.Release;
  end;
end;

procedure PrimitiveSend(Sender, Target: TProcessID; Msg: TTuple);
var
  Index: Integer;
  AMsg:   TActorMessage;
begin
  // Send a message Msg from Sender to Target.
  //
  // It doesn't matter whether there is an actor with mail address Target.

  ActorLock.Acquire;
  try
    Index := Actors.IndexOf(Target);

    if (Index <> -1) then begin
      AMsg := TActorMessage.Create;
      try
        AMsg.Data := Msg.Copy as TTuple;
        TActor(Actors.Objects[Index]).Mailbox.AddMessage(AMsg);

        NotifyOfMessageSend(Sender, Target, AMsg);
      finally
        AMsg.Free;
      end;
    end;
  finally
    ActorLock.Release;
  end;
end;

procedure PrimitiveUnregisterActor(A: TActor);
var
  Index: Integer;
begin
  ActorLock.Acquire;
  try
    Index := Actors.IndexOf(A.PID);

    if (Index <> -1) then
      Actors.Delete(Index);
  finally
    ActorLock.Release;
  end;
end;

function ReserveEvent: TEvent;
begin
  Result := TempEvent;
end;

procedure UnreserveEvent(E: TEvent);
begin

end;

//******************************************************************************
//* Unit Public functions/procedures                                           *
//******************************************************************************

procedure Kill(Target: TProcessID);
var
  Kill: TTuple;
begin
  Kill := TTuple.Create;
  try
    Kill.AddString(ExitMsg);
    Kill.AddString(ExitReasonKill);

    SendActorMessage(Target, Kill);
  finally
    Kill.Free;
  end;
end;

function RPC(Target: TProcessID; Msg: TTuple; Timeout: Cardinal): TTuple;
const
  OneMillisecond = 1/86400/1000; // One Millisecond in TDateTime format.
var
  E:        TEvent;
  EndTime:  TDateTime;
  ProxyMsg: TTuple;
  Wait:     TWaitResult;
begin
  // Ikaria keeps a pool of mutexes. When you call RPC, it reserves one of these
  // mutexes (call it E). It then asks the RootActor to forward Msg on to the
  // Target actor. RPC then busy-waits. If RPC waits for longer than Timeout
  // milliseconds, it raises an ETimeout exception. When RootActor receives a
  // response from Target, it sets E

  Result := nil;

  EndTime := Now + Timeout*OneMillisecond;
  E := ReserveEvent;
  try
    ProxyMsg := TTuple.Create;
    try
      ProxyMsg.AddProcessID(PIDFor(E));
      ProxyMsg.AddString(RpcProxyMsg);
      ProxyMsg.Add(Msg);

      PrimitiveSend(TActor.RootActor, TActor.RootActor, ProxyMsg);
    finally
      ProxyMsg.Free;
    end;

    while (Now < EndTime) do begin
      Wait := E.WaitFor(1000);

      case Wait of
        wrSignaled: begin
          // We received a response: return it.
          Result := DataFor(E).Copy as TTuple;
          Break;
        end;
        wrTimeout:;
      else
        raise ETimeout.Create('Something meaningful');
      end;
    end;

    if not Assigned(Result) then
      raise ETimeout.Create('Something meaningful');
  finally
    UnreserveEvent(E);
  end;
end;

procedure SendActorMessage(Target: TProcessID; Msg: TTuple);
begin
  PrimitiveSend(TActor.RootActor, Target, Msg);
end;

function Spawn(ActorType: TActorClass): TProcessID;
var
  A: TActor;
begin
  A := ActorType.Create(TActor.RootActor);
  Result := A.PID;
  A.Resume;
end;

function WaitForMessageFrom(Target: TProcessID; Condition: TMessageFinder): TActorMessage;
begin
end;

//******************************************************************************
//* TTupleElement                                                              *
//******************************************************************************
//* TTupleElement Public methods ***********************************************

function TTupleElement.AsString: String;
begin
  // By default, do nothing:
  Result := '';
end;

function TTupleElement.Copy: TTupleElement;
begin
  raise EAbstractError.Create(Self.ClassName + ' must override Copy');
end;

function TTupleElement.Equals(Other: TTupleElement): Boolean;
begin
  Result := Self.ClassType = Other.ClassType;
end;

function TTupleElement.IsBoolean: Boolean;
begin
  Result := false;
end;

function TTupleElement.IsInteger: Boolean;
begin
  Result := false;
end;

function TTupleElement.IsProcessID: Boolean;
begin
  Result := false;
end;

function TTupleElement.IsString: Boolean;
begin
  Result := false;
end;

function TTupleElement.IsTuple: Boolean;
begin
  Result := false;
end;

//******************************************************************************
//* TBooleanElement                                                            *
//******************************************************************************
//* TBooleanElement Public methods *********************************************

constructor TBooleanElement.Create(Value: Boolean);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TBooleanElement.AsString: String;
begin
  Result := BoolStrs[Self.Value];
end;

function TBooleanElement.Copy: TTupleElement;
begin
  Result := TBooleanElement.Create(Self.Value);
end;

function TBooleanElement.Equals(Other: TTupleElement): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TBooleanElement(Other).Value);
end;

function TBooleanElement.IsBoolean: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TIntegerElement                                                            *
//******************************************************************************
//* TIntegerElement Public methods *********************************************

constructor TIntegerElement.Create(Value: Integer);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TIntegerElement.AsString: String;
begin
  Result := IntToStr(Self.Value);
end;

function TIntegerElement.Copy: TTupleElement;
begin
  Result := TIntegerElement.Create(Self.Value);
end;

function TIntegerElement.Equals(Other: TTupleElement): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TIntegerElement(Other).Value);
end;

function TIntegerElement.IsInteger: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TProcessIDElement                                                          *
//******************************************************************************
//* TProcessIDElement Public methods *******************************************

constructor TProcessIDElement.Create(Value: TProcessID);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TProcessIDElement.AsString: String;
begin
  Result := Format('{%s}', [Self.Value]);
end;

function TProcessIDElement.Copy: TTupleElement;
begin
  Result := TProcessIDElement.Create(Self.Value);
end;

function TProcessIDElement.Equals(Other: TTupleElement): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TProcessIDElement(Other).Value);
end;

function TProcessIDElement.IsProcessID: Boolean;
begin
  Result := true;
end;

//******************************************************************************
//* TStringElement                                                             *
//******************************************************************************
//* TStringElement Public methods **********************************************

constructor TStringElement.Create(Value: String);
begin
  inherited Create;

  Self.fValue := Value;
end;

function TStringElement.AsString: String;
begin
  Result := Format('"%s"', [Self.Value]);
end;

function TStringElement.Copy: TTupleElement;
begin
  Result := TStringElement.Create(Self.Value);
end;

function TStringElement.Equals(Other: TTupleElement): Boolean;
begin
  Result := inherited Equals(Other) and
            (Self.Value = TStringElement(Other).Value);
end;

function TStringElement.IsString: Boolean;
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

procedure TTuple.Add(E: TTupleElement);
begin
  Self.TupleElements.Add(E.Copy);
end;

procedure TTuple.AddBoolean(B: Boolean);
begin
  Self.TupleElements.Add(TBooleanElement.Create(B));
end;

procedure TTuple.AddInteger(I: Integer);
begin
  Self.TupleElements.Add(TIntegerElement.Create(I));
end;

procedure TTuple.AddProcessID(PID: TProcessID);
begin
  Self.TupleElements.Add(TProcessIDElement.Create(PID));
end;

procedure TTuple.AddString(S: String);
begin
  Self.TupleElements.Add(TStringElement.Create(S));
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

function TTuple.Copy: TTupleElement;
var
  I: Integer;
  NewT: TTuple;
begin
  NewT := TTuple.Create;

  for I := 0 to Self.Count - 1 do
    NewT.Add(Self[I]);

  Result := NewT;
end;

function TTuple.Count: Integer;
begin
  Result := Self.TupleElements.Count;
end;

function TTuple.Equals(Other: TTupleElement): Boolean;
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

function TTuple.IsTuple: Boolean;
begin
  Result := true;
end;

//* TTuple Private methods *****************************************************

function TTuple.GetElement(Index: Integer): TTupleElement;
begin
  Result := Self.TupleElements[Index] as TTupleElement;
end;

//******************************************************************************
//* TActorMessage                                                              *
//******************************************************************************
//* TActorMessage Public methods ***********************************************

constructor TActorMessage.Create;
begin
  inherited Create;

  Self.fData := TTuple.Create;
  Self.fTag  := NextTag;
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
  Result := TActorMessage.Create;
  Result.Data := Self.Data.Copy as TTuple;
  Result.fTag := Self.Tag;
end;

//******************************************************************************
//* TActorMailbox                                                              *
//******************************************************************************
//* TActorMailbox Public methods ***********************************************

constructor TActorMailbox.Create;
begin
  inherited Create;

  Self.Lock      := TCriticalSection.Create;
  Self.Messages  := TObjectList.Create(false);
  Self.SaveQueue := TObjectList.Create(false);
end;

destructor TActorMailbox.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.Purge;
    Self.SaveQueue.Free;
    Self.Messages.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;

  inherited Destroy;
end;

procedure TActorMailbox.AddMessage(Msg: TActorMessage);
begin
  // Store a copy of Msg in our mailbox.
  // Move any messages in the save queue back into the mailbox proper (preserving arrival order).

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
        if Table.Conditions[J](M) then begin
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

      if Condition(M) then begin
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
//* TActor                                                                     *
//******************************************************************************
//* TActor Public methods ******************************************************

class function TActor.RootActor: TProcessID;
begin
  ActorLock.Acquire;
  try
    if not Assigned(Root) then begin
      Root := TRootActor.Create('');
      Root.Resume;
    end;

    Result := Root.PID;
  finally
    ActorLock.Release;
  end;
end;

constructor TActor.Create(Parent: TProcessID);
begin
  inherited Create(true);

  Self.FreeOnTerminate := true;

  Self.ParentID := Parent;

  Self.Mailbox  := TActorMailbox.Create;
  Self.Mailbox.OnMessageArrived := Self.NewMessageArrived;

  Self.MsgEvent := TSimpleEvent.Create;
  Self.MsgTable := TActorMessageTable.Create;
  Self.RegisterRequiredActions(Self.MsgTable);
  Self.RegisterActions(Self.MsgTable);

  Self.fPID := PrimitiveRegisterActor(Self);

  NotifyOfActorCreation(Self.ClassName, Self.PID);
end;

destructor TActor.Destroy;
begin
  PrimitiveUnregisterActor(Self);
  // Notify all known actors of freeing!

  Self.Mailbox.OnMessageArrived := nil;
  Self.MsgTable.Free;
  Self.MsgEvent.Free;
  Self.Mailbox.Free;

  inherited Destroy;
end;

//* TActor Protected methods ***************************************************

procedure TActor.Execute;
begin
  try
    Self.Run;
    Self.SendNormalExit;
  except
    on E: Exception do
      Self.SendExceptionalExit(E);
  end;
end;

procedure TActor.Receive(Matching: TMessageFinder;
                         Action: TActOnMessage);
begin
  Self.Receive(Matching, Action, 0, Self.NullMethod);
end;

procedure TActor.Receive(Matching: TMessageFinder;
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

procedure TActor.Receive(Table: TActorMessageTable);
begin
  Self.Receive(Table, 0, Self.NullMethod);
end;

procedure TActor.Receive(Table: TActorMessageTable;
                         Timeout: Cardinal;
                         TimeoutAction: TThunk);
var
  I:   Integer;
  Msg: TActorMessage;
begin
  // Block execution until we receive a message matching the condition/s defined
  // by the function Matching.

  while not Self.Terminated do begin
    Msg := Self.Mailbox.FindAndProcessMessage(Table, I);
    try
      if Assigned(Msg) then
        Table.Actions[I](Msg);
    finally
      Msg.Free;
    end;

    // If we're terminated, there's no sense waiting.
    if not Self.Terminated then
      Self.WaitForMessage(1000);
  end;
end;

procedure TActor.RegisterActions(Table: TActorMessageTable);
begin
  // Register actions against conditions
end;

procedure TActor.Run;
begin
  while not Self.Terminated do
    Self.Receive(Self.MsgTable);
end;

procedure TActor.SendExceptionalExit(E: Exception);
var
  BadExit: TTuple;
begin
  BadExit := TTuple.Create;
  try
    BadExit.AddProcessID(Self.PID);
    BadExit.AddString(ExitMsg);
    BadExit.AddString(Format(ExitReasonException, [E.ClassName, E.Message]));

    // Send to everyone in the link set!


    NotifyOfActorExit(Self.PID, BadExit);
  finally
    BadExit.Free;
  end;
end;

procedure TActor.SendNormalExit;
var
  NormalExit: TTuple;
begin
  NormalExit := TTuple.Create;
  try
    NormalExit.AddProcessID(Self.PID);
    NormalExit.AddString(ExitMsg);
    NormalExit.AddString(ExitReasonNormal);

    // Send to everyone in the link set!


    NotifyOfActorExit(Self.PID, NormalExit);
  finally
    NormalExit.Free;
  end;
end;

procedure TActor.Send(Target: TProcessID; Msg: TTuple);
begin
//  if (ActorReferences.IndexOf(Target) = -1) then
//    raise EActorException.Create(Format('Actor %s sent a message of type %s to an unknown target: %s', [Self.PID, Msg.ClassName, Target]));

  PrimitiveSend(Self.PID, Target, Msg);
end;

function TActor.Spawn(ActorType: TActorClass): TProcessID;
var
  A: TActor;
begin
  A := ActorType.Create(Self.PID);

  Result := A.PID;

  A.Resume;
end;

//* TActor Private methods *****************************************************

function TActor.FindKill(Msg: TActorMessage): Boolean;
begin
  Result := Msg.Data.Count > 1;

  if Result then begin
    Result := Msg.Data[0].IsString
           and (TStringElement(Msg.Data[0]).Value = ExitMsg)
           and Msg.Data[1].IsString
           and (TStringElement(Msg.Data[1]).Value = ExitReasonKill);
  end;
end;

procedure TActor.NewMessageArrived(Sender: TObject);
begin
  // This executes in the context of whatever thread's currently controlling the
  // mailbox.
  // Sender points to the mailbox.

  Self.MsgEvent.SetEvent;
end;

procedure TActor.NullMethod;
begin
  // Do nothing.
end;

procedure TActor.ReactToKill(Msg: TActorMessage);
begin
  Self.Terminate;
end;

procedure TActor.RegisterRequiredActions(Table: TActorMessageTable);
begin
  Table.Add(Self.FindKill, Self.ReactToKill);
end;

procedure TActor.WaitForMessage(MillisecondTimeout: Cardinal);
begin
  Self.MsgEvent.WaitFor(MillisecondTimeout);
  Self.MsgEvent.ResetEvent;
end;

//******************************************************************************
//* TEventMapping                                                              *
//******************************************************************************
//* TEventMapping Public methods ***********************************************

constructor TEventMapping.Create(Event: TEvent; PID: TProcessID);
begin
  inherited Create;

  Self.fEvent := Event;
  Self.fPID   := PID;
end;

function TEventMapping.HasResult: Boolean;
begin
  Result := Assigned(Self.Result);
end;

procedure TEventMapping.SetResult(Value: TTuple);
begin
  if not Assigned(Self.fResult) then
    Self.fResult := Value.Copy as TTuple;
end;

//******************************************************************************
//* TRootActor                                                                 *
//******************************************************************************
//* TRootActor Public methods **************************************************

constructor TRootActor.Create(Parent: TProcessID);
begin
  inherited Create(Parent);

  Self.fEvents := TEventDictionary.Create;
end;

destructor TRootActor.Destroy;
begin
  Self.fEvents.Free;

  inherited Destroy;
end;

//* TRootActor Protected methods ***********************************************

function TRootActor.FindProxy(Msg: TActorMessage): Boolean;
begin
  Result := (Msg.Data.Count > 1)
        and (TStringElement(Msg.Data[1]).Value = RpcProxyMsg);
end;

function TRootActor.FindResponse(Msg: TActorMessage): Boolean;
begin
  Result := not Self.FindProxy(Msg);
end;

procedure TRootActor.ProxyMsg(Msg: TActorMessage);
begin
  Self.Send(TStringElement(Msg.Data[0]).Value, TTuple(Msg.Data[2]));
end;

procedure TRootActor.RelayResponse(Msg: TActorMessage);
begin
  // * Store the response in the event dictionary.
  // * Set the associated event.

  if Assigned(TempData) then
    TempData.Free;
  TempData := Msg.Data.Copy as TTuple;
  TempEvent.SetEvent;
end;

//* TRootActor Private methods *************************************************

procedure TRootActor.RegisterActions(Table: TActorMessageTable);
begin
  Table.Add(Self.FindProxy, Self.ProxyMsg);
  Table.Add(Self.FindResponse, Self.RelayResponse);
end;

//******************************************************************************
//* TWindowsMessageForwarder                                                   *
//******************************************************************************
//* TWindowsMessageForwarder Protected methods *********************************

procedure TWindowsMessageForwarder.Run;
begin
  while not Self.Terminated do begin
    Self.Receive(Self.MatchAll, Self.ForwardToMessageQueue);
  end;
end;

//* TWindowsMessageForwarder Private methods ***********************************

function TWindowsMessageForwarder.MatchAll(Msg: TActorMessage): Boolean;
begin
  Result := true;
end;

procedure TWindowsMessageForwarder.ForwardToMessageQueue(Msg: TActorMessage);
begin
  // * Map the Msg to a (Windows) Message;
  // * dump the tuple into the WParam;
  // * send to a Windows message queue.
end;

initialization
  ActorLock := TCriticalSection.Create;
  Actors    := TStringList.Create;
  UsedPIDs  := TStringList.Create;

  TempEvent := TSimpleEvent.Create;

  // Tag generation
  NextAvailableTag := 0;
  TagLock          := TCriticalSection.Create;
end.
