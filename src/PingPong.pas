{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit PingPong;

interface

uses
  Classes, Controls, ExtCtrls, Forms, Ikaria, StdCtrls;

type
  TFibonacciGenerator = class;

  TPingPongDemo = class(TForm)
    Panel1: TPanel;
    StartPing: TButton;
    Log: TMemo;
    StopPing: TButton;
    NextFib: TButton;
    NumProcs: TEdit;
    NumLoops: TEdit;
    RingBenchmark: TButton;
    procedure StartPingClick(Sender: TObject);
    procedure StopPingClick(Sender: TObject);
    procedure NextFibClick(Sender: TObject);
    procedure RingBenchmarkClick(Sender: TObject);
  private
    NextFibber: TProcessID;
    Pinger:     TProcessID;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPingActor = class(TActor)
  private
    Ponger: TProcessID;

    function  FindPong(Msg: TTuple): Boolean;
    procedure Ping(PID: TProcessID);
    procedure ReactToPong(Msg: TTuple);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
    procedure Run; override;
  end;

  TPongActor = class(TActor)
  private
    function  FindPing(Msg: TTuple): Boolean;
    procedure Pong(PID: TProcessID);
    procedure ReactToPing(Msg: TTuple);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  end;

  TFibonacciGenerator = class(TObject)
  private
    CurrentNumber: Integer;
    LastFib:       Integer;
    SecondLastFib: Integer;
  public
    constructor Create;

    function Next: Integer;
  end;

  TFibonacciActor = class(TActor)
  private
    FibGen: TFibonacciGenerator;

    function  FindNext(Msg: TTuple): Boolean;
    procedure ReturnNextFibonacci(Msg: TTuple);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  public
    constructor Create(Parent: TProcessID); override;
    destructor  Destroy; override;
  end;

  TRingBenchmark = class(TActor)
  private
    Caller:   TProcessID;
    First:    TProcessID;
    NumProcs: Integer;
    NumLoops: Integer;

    function  FindFinished(Msg: TTuple): Boolean;
    function  FindGo(Msg: TTuple): Boolean;
    function  FindSetup(Msg: TTuple): Boolean;
    function  FindTick(Msg: TTuple): Boolean;
    procedure RunBenchmark(Msg: TTuple);
    procedure Setup(Msg: TTuple);
    procedure SignalSetupFinished(Msg: TTuple);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  end;

  TForwarder = class(TActor)
  private
    Next: TProcessID;

    function  FindSetup(Msg: TTuple): Boolean;
    function  FindTick(Msg: TTuple): Boolean;
    procedure ForwardTick(Msg: TTuple);
    procedure Setup(Msg: TTuple);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  end;

var
  PingPongDemo: TPingPongDemo;

implementation

{$R *.dfm}

uses
  SyncObjs, SysUtils;

const
  LevelDebug = 0;
  LevelInfo  = 1;
  NextName   = 'next';
  PingName   = 'ping';
  PongName   = 'pong';

var
  Lock: TCriticalSection;

//******************************************************************************
//* Unit private functions/procedures                                          *
//******************************************************************************

procedure LogToDemo(LogName: String;
                    Description: String;
                    SourceRef: Cardinal;
                    SourceDesc: String;
                    Severity: Cardinal;
                    EventRef: Cardinal;
                    DebugInfo: String);
begin
  Lock.Acquire;
  try
    if (Severity > LevelDebug) then
      PingPongDemo.Log.Text := PingPongDemo.Log.Text + #13#10 + Description;
  finally
    Lock.Release;
  end;
end;

procedure NotifyOfActorCreation(ActorType, PID: String; Event: String);
begin
  LogToDemo('', Format(ActorCreatedMsg, [PID, ActorType]), 0, 'Ikaria', LevelDebug, 0, '');
end;

procedure NotifyOfActorExit(PID: String; ExitReason: TTuple);
begin
  LogToDemo('', Format(ActorExitedMsg, [PID, ExitReason.AsString]), 0, 'Ikaria', LevelDebug, 0, '');
end;

procedure NotifyOfMessageSend(Sender, Target: TProcessID; Msg: TActorMessage);
begin
  LogToDemo('', Format(MessageSentMsg, [Sender, Target, Msg.AsString]), 0, 'Ikaria', LevelDebug, 0, '');
end;

//******************************************************************************
//* TPingPongDemo                                                              *
//******************************************************************************
//* TPingPongDemo Public methods ***********************************************

constructor TPingPongDemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Ikaria.OnActorCreatedHook := NotifyOfActorCreation;
  Ikaria.OnActorExitedHook  := NotifyOfActorExit;
  Ikaria.OnMessageSentHook  := NotifyOfMessageSend;
end;

//* TPingPongDemo Published methods ********************************************

procedure TPingPongDemo.RingBenchmarkClick(Sender: TObject);
var
  Intf:   TActorInterfaceForRPC;
  Params: TTuple;
  RB:     TProcessID;
  Start,
  Stop:   TDateTime;
begin
  // PRECONDITION: NumProcs.Text, NumLoops.Text contain positive integers.

  RB := Spawn(TRingBenchmark);
  try
    Intf := TActorInterfaceForRPC.Create;
    Params := TTuple.Create;
    try
      Params.AddInteger(StrToInt(Self.NumProcs.Text));
      Params.AddInteger(StrToInt(Self.NumLoops.Text));

      Start := Now;

      Intf.Send(RB, 'setup', Params);
      Intf.Receive(Intf.AllMatcher, Intf.StoreFirstMessage, 10*OneSecond);
      if Assigned(Intf.Result) then begin
        Stop := Now;
        LogToDemo('', Format('Ring benchmark: Setup time: %s', [FormatDateTime('hh:mm:ss.zzz', Stop - Start)]), 0, '', LevelInfo, 0, '');
      end
      else
        LogToDemo('', 'Ring benchmark: Timed out waiting for setup', 0, '', LevelInfo, 0, '');
    finally
      Params.Free;
    end;

    Start := Now;

    Intf.Reset;
    Intf.Send(RB, 'go');
    Intf.Receive(Intf.AllMatcher, Intf.StoreFirstMessage, 10*OneSecond);
    if Assigned(Intf.Result) then begin
      Stop := Now;
      LogToDemo('', Format('Ring benchmark: Run time: %s', [FormatDateTime('hh:mm:ss.zzz', Stop - Start)]), 0, '', LevelInfo, 0, '');
    end
    else
      LogToDemo('', 'Ring benchmark: Timed out waiting for run', 0, '', LevelInfo, 0, '');
  finally
    Kill(RB);
  end;
end;

procedure TPingPongDemo.NextFibClick(Sender: TObject);
var
  AskForNext: TTuple;
  Next:       TTuple;
begin
  if (Self.NextFibber = '') then
    Self.NextFibber := Spawn(TFibonacciActor);

  AskForNext := TMessageTuple.Create(NextName, TActor.RootActor);
  try
    Next := RPC(Self.NextFibber, AskForNext, 1000);
    if Assigned(Next) then
      try
        LogToDemo('', Format('Next Fibonacci: %d', [TIntegerTerm(Next[0]).Value]), 0, '', LevelInfo, 0, '');
      finally
        Next.Free;
      end
    else
      LogToDemo('', 'Timeout waiting for the next Fibonacci', 0, '', LevelInfo, 0, '')
  finally
    AskForNext.Free;
  end;
end;

procedure TPingPongDemo.StartPingClick(Sender: TObject);
begin
  Self.Pinger := Spawn(TPingActor);
end;

procedure TPingPongDemo.StopPingClick(Sender: TObject);
begin
  Kill(Self.Pinger);
end;

//******************************************************************************
//* TPingActor                                                                 *
//******************************************************************************
//* TPingActor Protected methods ***********************************************

procedure TPingActor.RegisterActions(Table: TActorMessageTable);
begin
  inherited RegisterActions(Table);

  Table.Add(Self.FindPong, Self.ReactToPong);
end;

//* TPingActor Private methods *************************************************

function TPingActor.FindPong(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, PongName);
end;

procedure TPingActor.Ping(PID: TProcessID);
begin
  Self.Send(PID, PingName);
end;

procedure TPingActor.ReactToPong(Msg: TTuple);
begin
  LogToDemo('', PongName, 0, 'PingPongDemo', LevelInfo, 0, Self.PID);
  Sleep(1000);
  Self.Ping(Self.Ponger);
end;

procedure TPingActor.Run;
begin
  Self.Ponger := Self.SpawnLink(TPongActor);
  Self.Ping(Self.Ponger);

  inherited Run;
end;

//******************************************************************************
//* TPongActor                                                                 *
//******************************************************************************
//* TPongActor Protected methods ***********************************************

procedure TPongActor.RegisterActions(Table: TActorMessageTable);
begin
  inherited RegisterActions(Table);

  Table.Add(Self.FindPing, Self.ReactToPing);
end;

//* TPongActor Private methods *************************************************

function TPongActor.FindPing(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, PingName);
end;

procedure TPongActor.Pong(PID: TProcessID);
begin
  Self.Send(PID, PongName);
end;

procedure TPongActor.ReactToPing(Msg: TTuple);
begin
  LogToDemo('', PingName, 0, 'PingPongDemo', LevelInfo, 0, Self.PID);
  Sleep(1000);
  Self.Pong(Self.ParentID);
end;

//******************************************************************************
//* TFibonacciGenerator                                                        *
//******************************************************************************
//* TFibonacciGenerator Public methods *****************************************

constructor TFibonacciGenerator.Create;
begin
  inherited Create;

  Self.CurrentNumber := 0;
  Self.SecondLastFib := 0;
  Self.LastFib       := 0;  
end;

function TFibonacciGenerator.Next: Integer;
begin
  Self.SecondLastFib := Self.LastFib;
  Self.LastFib       := Self.CurrentNumber;

  if (Self.SecondLastFib = 0) and (Self.LastFib = 0) then
    Self.CurrentNumber := 1
  else if (Self.SecondLastFib = 0) and (Self.LastFib = 1) then
    Self.CurrentNumber := 1
  else
    Self.CurrentNumber := Self.LastFib + Self.SecondLastFib;

  Result := Self.CurrentNumber;
end;

//******************************************************************************
//* TFibonacciActor                                                            *
//******************************************************************************
//* TFibonacciActor Public methods *********************************************

constructor TFibonacciActor.Create(Parent: TProcessID);
begin
  inherited Create(Parent);

  Self.FibGen := TFibonacciGenerator.Create;
end;

destructor TFibonacciActor.Destroy;
begin
  Self.FibGen.Free;

  inherited Destroy;
end;

//* TFibonacciActor Protected methods ******************************************

procedure TFibonacciActor.RegisterActions(Table: TActorMessageTable);
begin
  Table.Add(Self.FindNext, Self.ReturnNextFibonacci);
end;

//* TFibonacciActor Private methods ********************************************

function TFibonacciActor.FindNext(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, NextName);
end;

procedure TFibonacciActor.ReturnNextFibonacci(Msg: TTuple);
var
  Answer: TTuple;
  Request: TMessageTuple;
begin
  Request := TMessageTuple.Overlay(Msg);
  try
    Answer := TTuple.Create;
    try
      Answer.AddInteger(Self.FibGen.Next);
      Self.Send(Request.ReplyTo, Answer);
    finally
      Answer.Free;
    end;
  finally
    Request.Free;
  end;
end;

//******************************************************************************
//* TRingBenchmark                                                             *
//******************************************************************************
//* TRingBenchmark Protected methods *******************************************

procedure TRingBenchmark.RegisterActions(Table: TActorMessageTable);
begin
  Table.Add(Self.FindGo, Self.RunBenchmark);
  Table.Add(Self.FindSetup, Self.Setup);
  Table.Add(Self.FindFinished, Self.SignalSetupFinished);
end;

//* TRingBenchmark Private methods *********************************************

function TRingBenchmark.FindFinished(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, 'finished');
end;

function TRingBenchmark.FindGo(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, 'go');
end;

function TRingBenchmark.FindSetup(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, 'setup')
end;

function TRingBenchmark.FindTick(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, 'tick')
end;

procedure TRingBenchmark.RunBenchmark(Msg: TTuple);
var
  I: Integer;
begin
  for I := 1 to Self.NumLoops do begin
    Self.Send(Self.First, 'tick');
    Self.Receive(Self.FindTick, Self.DoNothing);
  end;

  Self.Send(Self.Caller, 'gone');
end;

procedure TRingBenchmark.Setup(Msg: TTuple);
var
  Params:      TMessageTuple;
  SetupParams: TTuple;
begin
  Params := TMessageTuple.Overlay(Msg);
  try
    Self.Caller   := Params.ReplyTo;
    Self.NumProcs := (Params.Parameters[0] as TIntegerTerm).Value;
    Self.NumLoops := (Params.Parameters[1] as TIntegerTerm).Value;
  finally
    Params.Free;
  end;

  Self.First := Self.SpawnLink(TForwarder);

  // ("setup" {sender} ({benchmark_pid} N))
  SetupParams := TTuple.Create;
  try
    SetupParams.AddProcessID(Self.PID);
    SetupParams.AddInteger(NumProcs - 1);
    Self.Send(Self.First, 'setup', SetupParams);
  finally
    SetupParams.Free;
  end;
end;

procedure TRingBenchmark.SignalSetupFinished(Msg: TTuple);
begin
  Self.Send(Self.Caller, 'finished-setup');
end;

//******************************************************************************
//* TForwarder                                                                 *
//******************************************************************************
//* TForwarder Protected methods ***********************************************

procedure TForwarder.RegisterActions(Table: TActorMessageTable);
begin
  Table.Add(Self.FindSetup, Self.Setup);
  Table.Add(Self.FindTick, Self.ForwardTick);
end;

//* TForwarder Private methods *************************************************

function TForwarder.FindSetup(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, 'setup');
end;

function TForwarder.FindTick(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, 'tick');
end;

procedure TForwarder.ForwardTick(Msg: TTuple);
begin
  Self.Send(Self.Next, 'tick');
end;

procedure TForwarder.Setup(Msg: TTuple);
var
  N:           Integer;
  Params:      TMessageTuple;
  SetupParams: TTuple;
begin
  Params := TMessageTuple.Overlay(Msg);
  try
    N := (Params.Parameters[1] as TIntegerTerm).Value;

    if (N = 0) then begin
      // Close the ring.
      Self.Next := (Params.Parameters[0] as TProcessIDTerm).Value;
      Self.Send(Self.Next, 'finished');
    end
    else begin
      Self.Next := Self.SpawnLink(TForwarder);

      // ("setup" {self} ({ring_benchmark_pid} N))
      SetupParams := TTuple.Create;
      try
        SetupParams.Add(Params.Parameters[0]);
        SetupParams.AddInteger(N - 1);
        Self.Send(Self.Next, 'setup', SetupParams);
      finally
        SetupParams.Free;
      end;
    end;
  finally
    Params.Free;
  end;
end;

initialization
  Lock := TCriticalSection.Create;
end.
