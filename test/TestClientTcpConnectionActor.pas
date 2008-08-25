unit TestClientTcpConnectionActor;

interface

uses
  ClientTcpConnectionActor, IdTcpServer, Ikaria, SyncObjs, TestIkaria,
  TestFramework;

type
  TestTClientTcpConnectionActor = class(TActorTestCase)
  private
    Connected:       Boolean;
    Connection:      TProcessID;
    ConnectionCount: Integer;
    ConnectTo:       TConnectMsg;
    ConnEvent:       TEvent;
    DisconEvent:     TEvent;
    Location:        TLocationTuple;
    Server:          TIdTcpServer;

    procedure CountConnections(Thread: TIdPeerThread);
    procedure DoNothing(Thread: TIdPeerThread);
    procedure OnDisconnect(Thread: TIdPeerThread);
    procedure WaitForTimeout(E: TEvent; Timeout: Cardinal; Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClose;
    procedure TestConnect;
    procedure TestDoubleConnect;
  end;

const
  OneSecond      = 1000;
  DefaultTimeout = OneSecond;

implementation

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('ClientTcpConnectionActor unit tests');
  Result.AddSuite(TestTClientTcpConnectionActor.Suite);
end;

//******************************************************************************
//* TestTClientTcpConnectionActor                                              *
//******************************************************************************
//* TestTClientTcpConnectionActor Public methods *******************************

procedure TestTClientTcpConnectionActor.SetUp;
begin
  inherited SetUp;

  Self.ConnEvent   := TSimpleEvent.Create;
  Self.DisconEvent := TSimpleEvent.Create;
  Self.Connected       := false;
  Self.ConnectionCount := 0;

  Self.Server    := TIdTcpServer.Create(nil);
  Self.Server.OnConnect    := Self.CountConnections;
  Self.Server.OnDisconnect := Self.OnDisconnect;
  Self.Server.OnExecute    := Self.DoNothing;
  Self.Server.Bindings.Add;
  Self.Server.Bindings[0].IP   := '127.0.0.1';
  Self.Server.Bindings[0].Port := 8000;
  Self.Server.Active := true;

  Self.Location := TLocationTuple.Create(Self.Server.Bindings[0].IP, Self.Server.Bindings[0].Port, 'TCP');
  Self.ConnectTo := TConnectMsg.Create('', Self.Location);

  Self.Connection := Spawn(TClientTcpConnectionActor);
end;

procedure TestTClientTcpConnectionActor.TearDown;
begin
  Kill(Self.Connection);
  Self.ConnectTo.Free;
  Self.Location.Free;
  Self.Server.Free;
  Self.DisconEvent.Free;
  Self.ConnEvent.Free;

  inherited TearDown;
end;

//* TestTClientTcpConnectionActor Private methods ******************************

procedure TestTClientTcpConnectionActor.CountConnections(Thread: TIdPeerThread);
begin
  Self.Connected := true;
  Inc(Self.ConnectionCount);
  Self.ConnEvent.SetEvent;
end;

procedure TestTClientTcpConnectionActor.DoNothing(Thread: TIdPeerThread);
begin
end;

procedure TestTClientTcpConnectionActor.OnDisconnect(Thread: TIdPeerThread);
begin
  Self.DisconEvent.SetEvent;
end;

procedure TestTClientTcpConnectionActor.WaitForTimeout(E: TEvent; Timeout: Cardinal; Msg: String);
begin
  if (wrTimeout <> E.WaitFor(Timeout)) then
    Fail(Msg);
end;

//* TestTClientTcpConnectionActor Published methods ****************************

procedure TestTClientTcpConnectionActor.TestClose;
var
  CloseConnection: TMessageTuple;
begin
  SendActorMessage(Self.Connection, Self.ConnectTo);
  Self.WaitFor(Self.ConnEvent, DefaultTimeout, 'No connection made');

  CloseConnection := TMessageTuple.Create(CloseConnectionMsg, '');
  try
    SendActorMessage(Self.Connection, CloseConnection);
  finally
    CloseConnection.Free;
  end;

  Self.WaitFor(Self.DisconEvent, DefaultTimeout, 'Connection not closed');
end;

procedure TestTClientTcpConnectionActor.TestConnect;
begin
  SendActorMessage(Self.Connection, Self.ConnectTo);
  Self.WaitFor(Self.ConnEvent, DefaultTimeout, 'No connection attempt');
end;

procedure TestTClientTcpConnectionActor.TestDoubleConnect;
begin
  SendActorMessage(Self.Connection, Self.ConnectTo);
  Self.WaitFor(Self.ConnEvent, DefaultTimeout, 'No connection attempt');
  Self.ConnEvent.ResetEvent;

  // "Reconnecting" should do nothing.
  SendActorMessage(Self.Connection, Self.ConnectTo);
  Self.WaitForTimeout(Self.ConnEvent, DefaultTimeout, 'Reconnection attempt');
  CheckEquals(1, Self.ConnectionCount, 'Actor tried to reconnect');
end;

initialization;
  RegisterTest('ClientTcpConnectionActor', Suite);
end.
