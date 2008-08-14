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
  end;

  TBooleanElement = class(TTupleElement)
  private
    fValue: Boolean;
  public
    constructor Create(Value: Boolean);

    function AsString: String; override;
    function Copy: TTupleElement; override;
    function Equals(Other: TTupleElement): Boolean; override;

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
    procedure AddString(S: String);
    function  AsString: String; override;
    function  Copy: TTupleElement; override;
    function  Count: Integer;
    function  Equals(Other: TTupleElement): Boolean; override;

    property Elements[Index: Integer]: TTupleElement read GetElement; default;
  end;

  // I represent a message sent to an Actor.
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

    property Data: TTuple read fData;
    property Tag:  String read fTag;
  end;

  TMessageFinder = function(Msg: TActorMessage): Boolean of object;

  TActOnMessage = procedure(Msg: TActorMessage) of object;

  // I store messages sent to an Actor.
  TActorMailbox = class(TObject)
  private
    Lock:      TCriticalSection;
    Messages:  TObjectList;
    SaveQueue: TObjectList;

    procedure FreeMessage(L: TObjectList; M: TActorMessage);
    procedure FreeMessages(L: TObjectList);
    function  MessageAt(Index: Integer): TActorMessage;
    procedure MoveMessages(Src, Dest: TObjectList; FromIndex, ToIndex: Integer);
    procedure RestoreSaveQueue;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure AddMessage(Msg: TActorMessage);
    function  IsEmpty: Boolean;
    function  FindMessage(Condition: TMessageFinder): TActorMessage; overload;
    procedure Purge;
    procedure Timeout;
  end;

  // I represent an execution context. I send and receive messages to and from
  // other Actors.
  TActor = class(TThread)
  private
    ActorReferences: TStringList;
    fPID:            TProcessID;
    Mailbox:         TActorMailbox;
  protected
    ParentID: TProcessID;

    procedure Execute; override;
    procedure Receive(Matching: TMessageFinder; Action: TActOnMessage; Timeout: Cardinal = 0); overload;
    procedure Run; virtual;
    procedure Send(Target: TProcessID; Msg: TActorMessage);
    function  Spawn(ActorType: TActorClass): TProcessID;
  public
    class function RootActor: TProcessID;

    constructor Create(Parent: TProcessID); virtual;
    destructor  Destroy; override;

    property PID: TProcessID read fPID;
  end;

  TRootActor = class(TActor)
  protected
    procedure Run; override;
  end;

  // I translate messages sent to me into PostMessages to a Windows message
  // queue.
  TWindowsMessageForwarder = class(TActor)
  end;

  EActorException = class(Exception);

// Interface to the outside world:
// Send a message to a particular Actor.
procedure SendActorMessage(Target: TProcessID; Msg: TActorMessage);
// Wait for a particular Actor to send you a message matching some Condition.
function  WaitForMessageFrom(Target: TProcessID; Condition: TMessageFinder): TActorMessage;

const
  ActorCreatedMsg = 'Actor %s created';
  ActorExitedMsg  = 'Actor %s exited with reason %s';
  ActorFreedMsg   = 'Actor %s freed';
  MessageSentMsg  = 'Actor %s sent message to actor %s: %s';

const
  ExitReasonException = '%s: %s';
  ExitReasonNormal    = 'normal';

implementation

uses
  PluggableLogging;

var
  Actors:    TStringList;
  Root:      TActor;
  ActorLock: TCriticalSection; // Used to lock access to Actors.
  SendLock:  TCriticalSection; // Used to synchronise the sending of messages to mailboxes.
  UsedPIDs:  TStringList;

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

procedure PrimitiveSend(Sender, Target: TProcessID; Msg: TActorMessage);
var
  Index: Integer;
begin
  // Send a message Msg from Sender to Target.
  //
  // It doesn't matter whether there is an actor with mail address Target.

  SendLock.Acquire;
  try
    ActorLock.Acquire;
    try
      Index := Actors.IndexOf(Target);

      if (Index <> -1) then
        TActor(Actors.Objects[Index]).Mailbox.AddMessage(Msg);
    finally
      ActorLock.Release;
    end;

    LogEntry('', Format(MessageSentMsg, [Sender, Target, Msg.AsString]), 0, 'Ikaria', slDebug, 0, '');
  finally
    SendLock.Release;
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

//******************************************************************************
//* Unit Public functions/procedures                                           *
//******************************************************************************

procedure SendActorMessage(Target: TProcessID; Msg: TActorMessage);
begin
  PrimitiveSend(TActor.RootActor, Target, Msg);
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
const
  BoolStrs: array[false..true] of String = ('false', 'true');
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
  Result := Self.Value;
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
  Result := Self.Value;
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

procedure TTuple.AddString(S: String);
begin
  Self.TupleElements.Add(TStringElement.Create(S));
end;

function TTuple.AsString: String;
var
  I: Integer;
begin
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
  Result := Format('(message :tag "%s")', [Self.Tag]);
end;

function TActorMessage.Copy: TActorMessage;
begin
  Result := TActorMessage.Create;
  Result.fData := Self.Data.Copy as TTuple;
  Result.fTag  := Self.Tag;
end;

//******************************************************************************
//* TActorMailbox                                                              *
//******************************************************************************
//* TActorMailbox Public methods ***********************************************

constructor TActorMailbox.Create;
begin
  inherited Create;

  Self.Lock     := TCriticalSection.Create;
  Self.Messages := TObjectList.Create(false);
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

function TActorMailbox.FindMessage(Condition: TMessageFinder): TActorMessage;
var
  FoundIndex: Integer;
  I:          Integer;
  M:          TActorMessage;
begin
  // Return the first matching message, and remove it from the mailbox.
  // Move all non-matching-but-checked messages to the save queue (preserving arrival order).

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
  // Move any messages in the save queue back into the mailbox proper (preserving arrival order).

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

procedure TActorMailbox.RestoreSaveQueue;
begin
  Self.MoveMessages(Self.SaveQueue, Self.Messages, 0, Self.SaveQueue.Count - 1);
end;

//******************************************************************************
//* TActor                                                                     *
//******************************************************************************
//* TActor Public methods ******************************************************

class function TActor.RootActor: TProcessID;
begin
  ActorLock.Acquire;
  try
    if not Assigned(Root) then
      Root := TRootActor.Create('');

    Result := Root.PID;
  finally
    ActorLock.Release;
  end;
end;

constructor TActor.Create(Parent: TProcessID);
begin
  inherited Create(true);

  Self.FreeOnTerminate := true;

  Self.ActorReferences := TStringList.Create;
  Self.ActorReferences.Duplicates := dupIgnore;
  Self.ActorReferences.Sorted     := true;

  // Special-case the Root Actor: by definition, it has no parent.
  if (Parent <> '') then
    Self.ActorReferences.Add(Parent);

  Self.ParentID := Parent;

  Self.Mailbox := TActorMailbox.Create;

  Self.fPID := PrimitiveRegisterActor(Self);

  LogEntry('', Format(ActorCreatedMsg, [Self.PID]), 0, 'Ikaria', slDebug, 0, '');

  Self.Resume;
end;

destructor TActor.Destroy;
begin
  PrimitiveUnregisterActor(Self);
  // Notify all known actors of freeing!

  Self.Mailbox.Free;
  Self.ActorReferences.Free;

  LogEntry('', Format(ActorFreedMsg, [Self.PID]), 0, 'Ikaria', slDebug, 0, '');

  inherited Destroy;
end;

//* TActor Protected methods ***************************************************

procedure TActor.Execute;
begin
  try
    Self.Run;
    LogEntry('', Format(ActorExitedMsg, [Self.PID, ExitReasonNormal]), 0, 'Ikaria', slDebug, 0, '');
  except
    on E: Exception do
      LogEntry('', Format(ActorExitedMsg, [Self.PID, Format(ExitReasonException, [E.ClassName, E.Message])]), 0, 'Ikaria', slDebug, 0, '');
  end;
end;

procedure TActor.Receive(Matching: TMessageFinder; Action: TActOnMessage; Timeout: Cardinal = 0);
var
  Msg: TActorMessage;
begin
  // Block execution until we receive a message matching the condition/s defined
  // by the function Matching.

  while not Self.Terminated do begin
    Msg := Self.Mailbox.FindMessage(Matching);
    try
      if Assigned(Msg) then begin
        Action(Msg);
        Break;
      end;
    finally
      Msg.Free;
    end;

    // Wait a bit.
    Sleep(1000);
  end;
end;

procedure TActor.Run;
begin
  // For now, do nothing.
end;

procedure TActor.Send(Target: TProcessID; Msg: TActorMessage);
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
end;

//******************************************************************************
//* TRootActor                                                                 *
//******************************************************************************
//* TRootActor Public methods **************************************************

procedure TRootActor.Run;
begin
  while not Terminated do
    Sleep(1000);
end;

initialization
  ActorLock := TCriticalSection.Create;
  Actors    := TStringList.Create;
  SendLock  := TCriticalSection.Create;
  UsedPIDs  := TStringList.Create;

  // Tag generation
  NextAvailableTag := 0;
  TagLock          := TCriticalSection.Create;
end.
