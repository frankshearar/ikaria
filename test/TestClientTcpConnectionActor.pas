{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit TestClientTcpConnectionActor;

interface

uses
  ClientTcpConnectionActor, IdTcpServer, Ikaria, SyncObjs, TestIkaria,
  TestFramework;

type
  TestTClientTcpConnectionActor = class(TActorTestCase)
  private
    CloseConnection: TMessageTuple;
    Closed:          Boolean;
    Connected:       Boolean;
    Connection:      TProcessID;
    ConnectionCount: Integer;
    ConnectTo:       TConnectMsg;
    ConnEvent:       TEvent;
    DisconEvent:     TEvent;
    Location:        TLocationTuple;
    RPC:             TActorInterface;
    Server:          TIdTcpServer;
    TimedOut:        Boolean;

    function  ClientCountOf(S: TIdTcpServer): Integer;
    procedure CountConnections(Thread: TIdPeerThread);
    procedure DoNothing(Thread: TIdPeerThread);
    function  FindClosed(Msg: TTuple): Boolean;
    function  FindConnected(Msg: TTuple): Boolean;
    procedure MarkClosed(Msg: TTuple);
    procedure MarkConnected(Msg: TTuple);
    function  MatchMessageName(Msg: TTuple; Name: String): Boolean;
    procedure OnDisconnect(Thread: TIdPeerThread);
    procedure Timeout;
    procedure WaitForTimeout(E: TEvent; Timeout: Cardinal; Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClose;
    procedure TestCloseSendsClosedMessage;
    procedure TestConnect;
    procedure TestConnectSendsConnectedMessage;
    procedure TestDoubleConnect;
  end;

const
  OneSecond      = 1000;
  DefaultTimeout = OneSecond;

implementation

uses
  Classes, SysUtils;

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

  Self.Location        := TLocationTuple.Create(Self.Server.Bindings[0].IP, Self.Server.Bindings[0].Port, 'TCP');
  Self.RPC             := TActorInterface.Create;
  Self.ConnectTo       := TConnectMsg.Create(Self.RPC.PID, Self.Location);
  Self.CloseConnection := TMessageTuple.Create(CloseConnectionMsg, Self.RPC.PID);

  Self.Connection := Spawn(TClientTcpConnectionActor);
  Self.TimedOut   := false;
end;

procedure TestTClientTcpConnectionActor.TearDown;
begin
  Kill(Self.Connection);
  Self.CloseConnection.Free;
  Self.ConnectTo.Free;
  Self.RPC.Free;
  Self.Location.Free;
  Self.Server.Free;
  Self.DisconEvent.Free;
  Self.ConnEvent.Free;

  inherited TearDown;
end;

//* TestTClientTcpConnectionActor Private methods ******************************

function TestTClientTcpConnectionActor.ClientCountOf(S: TIdTcpServer): Integer;
var
  L: TList;
begin
  L := S.Threads.LockList;
  try
    Result := L.Count;
  finally
    S.Threads.UnlockList;
  end;
end;

procedure TestTClientTcpConnectionActor.CountConnections(Thread: TIdPeerThread);
begin
//  Self.Connected := true;
  Inc(Self.ConnectionCount);
  Self.ConnEvent.SetEvent;
end;

procedure TestTClientTcpConnectionActor.DoNothing(Thread: TIdPeerThread);
begin
end;

function TestTClientTcpConnectionActor.FindClosed(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ClosedConnectionMsg);
end;

function TestTClientTcpConnectionActor.FindConnected(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ConnectedMsg);
end;

procedure TestTClientTcpConnectionActor.MarkClosed(Msg: TTuple);
begin
  Self.Closed := true;
end;

procedure TestTClientTcpConnectionActor.MarkConnected(Msg: TTuple);
begin
  Self.Connected := true;
end;

function TestTClientTcpConnectionActor.MatchMessageName(Msg: TTuple; Name: String): Boolean;
var
  O: TMessageTuple;
begin
  try
    O := TMessageTuple.Overlay(Msg);
    try
      Result := O.MessageName = Name;
    finally
      O.Free;
    end;
  except
    Result := false;
  end;
end;

procedure TestTClientTcpConnectionActor.OnDisconnect(Thread: TIdPeerThread);
begin
  Self.DisconEvent.SetEvent;
end;

procedure TestTClientTcpConnectionActor.Timeout;
begin
  Self.TimedOut := true;
end;

procedure TestTClientTcpConnectionActor.WaitForTimeout(E: TEvent; Timeout: Cardinal; Msg: String);
begin
  if (wrTimeout <> E.WaitFor(Timeout)) then
    Fail(Msg);
end;

//* TestTClientTcpConnectionActor Published methods ****************************

procedure TestTClientTcpConnectionActor.TestClose;
var
  ClientCount: Integer;
begin
  Self.RPC.Send(Self.Connection, Self.ConnectTo);

  Self.RPC.Receive(Self.FindConnected, Self.MarkConnected, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, Format('Timeout waiting for %s message', [ConnectedMsg]));

  ClientCount := Self.ClientCountOf(Self.Server);

  Self.TimedOut := false;
  Self.RPC.Send(Self.Connection, Self.CloseConnection);

  Self.RPC.Receive(Self.FindClosed, Self.MarkClosed, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, Format('Timeout waiting for %s message', [ClosedConnectionMsg]));

  Check(ClientCount > Self.ClientCountOf(Self.Server), 'Connection not closed');
end;

procedure TestTClientTcpConnectionActor.TestCloseSendsClosedMessage;
begin
  Self.RPC.Send(Self.Connection, Self.ConnectTo);

  Self.RPC.Receive(Self.FindConnected, Self.MarkConnected, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, Format('Timeout waiting for %s message', [ConnectedMsg]));
  Check(Self.Connected, Format('No %s message received', [ConnectedMsg]));

  Self.RPC.Send(Self.Connection, Self.CloseConnection);

  Self.TimedOut := false;
  Self.RPC.Receive(Self.FindClosed, Self.MarkClosed, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, Format('Timeout waiting for %s message', [ClosedConnectionMsg]));
  Check(Self.Closed, Format('No %s message received', [ClosedConnectionMsg]));
end;

procedure TestTClientTcpConnectionActor.TestConnect;
begin
  SendActorMessage(Self.Connection, Self.ConnectTo);
  Self.WaitFor(Self.ConnEvent, DefaultTimeout, 'No connection attempt');
end;

procedure TestTClientTcpConnectionActor.TestConnectSendsConnectedMessage;
begin
  Self.RPC.Send(Self.Connection, Self.ConnectTo);

  Self.RPC.Receive(Self.FindConnected, Self.MarkConnected, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, Format('Timeout waiting for %s message', [ConnectedMsg]));
  Check(Self.Connected, Format('No %s message received', [ConnectedMsg]));
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
