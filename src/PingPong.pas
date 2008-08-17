unit PingPong;

interface

uses
  Classes, Controls, ExtCtrls, Forms, Ikaria, StdCtrls;

type
  TPingPongDemo = class(TForm)
    Panel1: TPanel;
    Go: TButton;
    Log: TMemo;
    procedure GoClick(Sender: TObject);
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

var
  PingPongDemo: TPingPongDemo;

implementation

{$R *.dfm}

uses
  SyncObjs, SysUtils;

const
  LevelDebug = 0;
  LevelInfo  = 1;
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
    try
      if (Severity > LevelDebug) then
        PingPongDemo.Log.Lines.Add(Description);
    except
      on E: Exception do begin
        PingPongDemo.Log.Lines.Add(Format('+++++ %s: %s +++++', [E.ClassName, E.Message]));
        raise;
      end;
    end;
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

procedure TPingPongDemo.GoClick(Sender: TObject);
var
  P: TActor;
begin
  P := TPingActor.Create('');
  P.Resume;
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

initialization
  Lock := TCriticalSection.Create;
end.
