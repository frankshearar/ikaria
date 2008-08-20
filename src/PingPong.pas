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
    procedure StartPingClick(Sender: TObject);
    procedure StopPingClick(Sender: TObject);
    procedure NextFibClick(Sender: TObject);
  private
    NextFibber: TProcessID;
    Pinger:     TProcessID;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TPingActor = class(TActor)
  private
    Ponger: TProcessID;

    function  FindPong(Msg: TActorMessage): Boolean;
    procedure Ping(PID: TProcessID);
    procedure ReactToPong(Msg: TActorMessage);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
    procedure Run; override;
  end;

  TPongActor = class(TActor)
  private
    function  FindPing(Msg: TActorMessage): Boolean;
    procedure Pong(PID: TProcessID);
    procedure ReactToPing(Msg: TActorMessage);
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

    function  FindNext(Msg: TActorMessage): Boolean;
    procedure ReturnNextFibonacci(Msg: TActorMessage);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  public
    constructor Create(Parent: TProcessID); override;
    destructor  Destroy; override;
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
//    if (Severity > LevelDebug) then
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

procedure TPingPongDemo.NextFibClick(Sender: TObject);
var
  AskForNext: TTuple;
  Next:       TTuple;
begin
  if (Self.NextFibber = '') then
    Self.NextFibber := Spawn(TFibonacciActor);

  AskForNext := TTuple.Create;
  try
     AskForNext.AddProcessID(TActor.RootActor);
     AskForNext.AddString(NextName);

    Next := RPC(Self.NextFibber, AskForNext, 100000);
    try
      LogToDemo('', Format('Next Fibonacci: %d', [TIntegerElement(Next[0]).Value]), 0, '', LevelInfo, 0, '');
    finally
      Next.Free;
    end;
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

function TPingActor.FindPong(Msg: TActorMessage): Boolean;
begin
  Result := (Msg.Data.Count > 1)
        and Msg.Data[0].IsProcessID
        and Msg.Data[1].IsString
        and (TStringElement(Msg.Data[1]).Value = PongName);
end;

procedure TPingActor.Ping(PID: TProcessID);
var
  P: TTuple;
begin
  P := TTuple.Create;
  try
    P.AddProcessID(Self.PID);
    P.AddString(PingName);
    Self.Send(PID, P);
  finally
    P.Free;
  end;
end;

procedure TPingActor.ReactToPong(Msg: TActorMessage);
begin
  LogToDemo('', PongName, 0, 'PingPongDemo', LevelInfo, 0, Self.PID);
  Sleep(1000);
  Self.Ping(Self.Ponger);
end;

procedure TPingActor.Run;
begin
  Self.Ponger := Self.Spawn(TPongActor);
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

function TPongActor.FindPing(Msg: TActorMessage): Boolean;
begin
  Result := (Msg.Data.Count > 1)
        and (Msg.Data[0] is TProcessIDElement)
        and (Msg.Data[1] is TStringElement)
        and (TStringElement(Msg.Data[1]).Value = PingName);
end;

procedure TPongActor.Pong(PID: TProcessID);
var
  P: TTuple;
begin
  P := TTuple.Create;
  try
    P.AddProcessID(Self.PID);
    P.AddString(PongName);
    Self.Send(PID, P);
  finally
    P.Free;
  end;
end;

procedure TPongActor.ReactToPing(Msg: TActorMessage);
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

function TFibonacciActor.FindNext(Msg: TActorMessage): Boolean;
begin
  Result := (Msg.Data.Count > 1)
         and Msg.Data[1].IsString
         and (TStringElement(Msg.Data[1]).Value = NextName)
end;

procedure TFibonacciActor.ReturnNextFibonacci(Msg: TActorMessage);
var
  Answer: TTuple;
begin
  Answer := TTuple.Create;
  try
    Answer.AddInteger(Self.FibGen.Next);
    Self.Send(TProcessIDElement(Msg.Data[0]).Value, Answer);
  finally
    Answer.Free;
  end;
end;

initialization
  Lock := TCriticalSection.Create;
end.
