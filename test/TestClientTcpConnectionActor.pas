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
  TestFramework, Windows;

type
  TestTClientTcpConnectionActor = class(TActorTestCase)
  private
    CloseConnection: TMessageTuple;
    Closed:          Boolean;
    Connected:       Boolean;
    Connection:      TClientTcpConnectionActor;
    ConnectionCount: Integer;
    ConnectTo:       TOpenMsg;
    ConnEvent:       TEvent;
    DisconEvent:     TEvent;
    Disconnected:    Boolean;
    Environment:     TActorEnvironment;
    Error:           String;
    LastClientPort:  Integer;
    Location:        TLocationTuple;
    ReceivedData:    String;
    RPC:             TActorInterface;
    SendData:        TSendDataMsg;
    SendEvent:       TEvent;
    Server:          TIdTcpServer;
    TestData:        String;
    TestTable:       TActorMessageTable;
    TimedOut:        Boolean;

    procedure AckConnection(Thread: TIdPeerThread);
    procedure CollectTestData(Thread: TIdPeerThread);
    procedure Connect;
    procedure Disconnect;
    function  FindClosed(Msg: TTuple): Boolean;
    function  FindConnected(Msg: TTuple): Boolean;
    function  FindReceivedData(Msg: TTuple): Boolean;
    procedure MarkConnected(Msg: TTuple);
    procedure MarkClosed(Msg: TTuple);
    function  MatchMessageName(Msg: TTuple; Name: String): Boolean;
    procedure ServerSend(S: String);
    procedure StoreReceivedData(Msg: TTuple);
    procedure Timeout;
    procedure WaitFor(E: TEvent; Timeout: Cardinal; Msg: String);
    procedure WaitForTimeout(E: TEvent; Timeout: Cardinal; Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClose;
    procedure TestCloseSendsClosedMessage;
    procedure TestConnect;
    procedure TestDoubleClose;
    procedure TestDoubleConnect;
    procedure TestReceivedData;
    procedure TestSendData;
  end;

const
  OneSecond      = 1000;
  DefaultTimeout = OneSecond;

implementation

uses
  Classes, Forms, Messages, SysUtils, IdTCPConnection;

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
  Self.Environment := TThreadedActorEnvironment.Create;
  Self.SendEvent   := TSimpleEvent.Create;
  Self.TestData    := 'test data';

  Self.Server := TIdTcpServer.Create(nil);
  Self.Server.OnExecute := Self.AckConnection;

  Self.Server.DefaultPort := 8000;
  Self.Server.Active := true;

  Self.TestTable := TActorMessageTable.Create;
  Self.TestTable.Add(Self.FindClosed, Self.MarkClosed);
  Self.TestTable.Add(Self.FindConnected, Self.MarkConnected);
  Self.TestTable.Add(Self.FindReceivedData, Self.StoreReceivedData);

  Self.Location        := TLocationTuple.Create('localhost', Self.Server.DefaultPort, 'TCP');
  Self.RPC             := TActorInterface.Create(Self.Environment);
  Self.ConnectTo       := TOpenMsg.Create(Self.RPC.PID, Self.Location);
  Self.CloseConnection := TMessageTuple.Create(CloseConnectionMsg, Self.RPC.PID);
  Self.SendData        := TSendDataMsg.Create(Self.RPC.PID, Self.TestData);

  Self.Connection := TClientTcpConnectionActor.Create(Self.Environment, TActor.RootActor);

  Self.Closed          := false;
  Self.Connected       := false;
  Self.ConnectionCount := 0;
  Self.Disconnected    := false;
  Self.LastClientPort  := 0;
  Self.TimedOut        := false;
end;

procedure TestTClientTcpConnectionActor.TearDown;
begin
  Self.Connection.Free;
  Self.SendData.Free;
  Self.CloseConnection.Free;
  Self.ConnectTo.Free;
  Self.RPC.Free;
  Self.Location.Free;
  Self.TestTable.Free;
  Self.Server.Free;
  Self.SendEvent.Free;
  Self.DisconEvent.Free;
  Self.ConnEvent.Free;

  inherited TearDown;
end;

//* TestTClientTcpConnectionActor Private methods ******************************

procedure TestTClientTcpConnectionActor.AckConnection(Thread: TIdPeerThread);
begin
  // Here I thought this callback ran just once per connection, but it's running
  // a metric kajillion times per connection. Thus, LastClientPort allows us to
  // set ConnEvent ONCE per unique connection (almost).

  if (Thread.Connection.Socket.Binding.PeerPort <> Self.LastClientPort) then begin
    Self.LastClientPort := Thread.Connection.Socket.Binding.PeerPort;
    Self.ConnEvent.SetEvent;
  end;
end;

procedure TestTClientTcpConnectionActor.CollectTestData(Thread: TIdPeerThread);
var
  S: TStringStream;
begin
  Self.ConnEvent.SetEvent;

  S := TStringStream.Create('');
  try
    while (Self.ReceivedData = '') do begin
      Thread.Connection.ReadFromStack(true, OneSecond, false);
      S.CopyFrom(Thread.Connection.InputBuffer, 0);
      Thread.Connection.InputBuffer.Remove(Thread.Connection.InputBuffer.Size);

      Self.ReceivedData := Self.ReceivedData + S.DataString;
    end;
  finally
    S.Free;
  end;

  Self.SendEvent.SetEvent;
end;

procedure TestTClientTcpConnectionActor.Connect;
begin
  Self.Connection.Connect(Self.ConnectTo);
  Self.WaitFor(Self.ConnEvent, OneSecond, 'Timed out waiting for server to accept connection');
  Self.RPC.Receive(Self.TestTable, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, 'Timed out waiting to open connection');
  Check(not Self.Closed, 'Connection closed');
  CheckEquals('', Self.Error, 'Error connecting');

  Self.WaitFor(Self.ConnEvent, OneSecond, 'Timed out waiting for connection');
end;

procedure TestTClientTcpConnectionActor.Disconnect;
begin
  Self.Connection.Close(Self.CloseConnection);

  Self.TimedOut := false;
  Self.RPC.Receive(Self.FindClosed, Self.MarkClosed, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, Format('Timeout waiting for %s message', [ClosedConnectionMsg]));
end;

function TestTClientTcpConnectionActor.FindClosed(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ClosedConnectionMsg);
end;

function TestTClientTcpConnectionActor.FindConnected(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, OpenedMsg);
end;

function TestTClientTcpConnectionActor.FindReceivedData(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ReceivedDataMsg);
end;

procedure TestTClientTcpConnectionActor.MarkClosed(Msg: TTuple);
begin
  Self.Closed := true;
end;

procedure TestTClientTcpConnectionActor.MarkConnected(Msg: TTuple);
var
  O: TMessageTuple;
begin
  Self.Connected := true;

  O := TMessageTuple.Overlay(Msg);
  try
    if (O.Parameters.Count > 0) then
      Self.Error := (O.Parameters[0] as TStringTerm).Value;
  finally
    O.Free;
  end;
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

procedure TestTClientTcpConnectionActor.ServerSend(S: String);
var
  I: Integer;
  L: TList;
begin
  L := Self.Server.Threads.LockList;
  try
    for I := 0 to L.Count - 1 do
      TIdPeerThread(L[I]).Connection.Write(S);
  finally
    Self.Server.Threads.UnlockList;
  end;
end;

procedure TestTClientTcpConnectionActor.StoreReceivedData(Msg: TTuple);
var
  O: TMessageTuple;
begin
  O := TMessageTuple.Overlay(Msg);
  try
    Self.ReceivedData := (O.Parameters[0] as TStringTerm).Value;
  finally
    O.Free;
  end;
end;

procedure TestTClientTcpConnectionActor.Timeout;
begin
  Self.TimedOut := true;
end;

procedure TestTClientTcpConnectionActor.WaitFor(E: TEvent; Timeout: Cardinal; Msg: String);
begin
  if (wrSignaled <> E.WaitFor(Timeout)) then
    Fail(Msg);
end;

procedure TestTClientTcpConnectionActor.WaitForTimeout(E: TEvent; Timeout: Cardinal; Msg: String);
begin
  if (wrTimeout <> E.WaitFor(Timeout)) then
    Fail(Msg);
end;

//* TestTClientTcpConnectionActor Published methods ****************************

procedure TestTClientTcpConnectionActor.TestClose;
begin
  Self.Connect;

  Self.Disconnect;

  Check(not Self.Connection.Connected, 'Connection not closed');
end;

procedure TestTClientTcpConnectionActor.TestCloseSendsClosedMessage;
begin
  Self.Connect;

  Self.Disconnect;

  Check(Self.Closed, Format('No %s message received', [ClosedConnectionMsg]));
end;

procedure TestTClientTcpConnectionActor.TestConnect;
begin
  Self.Connect;
end;

procedure TestTClientTcpConnectionActor.TestDoubleClose;
begin
  // Check that calling disconnect twice does nothing.

  Self.Connect;
  Self.Disconnect;

  Self.Connection.Close(Self.CloseConnection);
  Self.TimedOut := false;
  Self.RPC.Receive(Self.FindClosed, Self.MarkClosed, OneSecond, Self.Timeout);
  Check(Self.TimedOut, Format('Received a %s message', [ClosedConnectionMsg]));
end;

procedure TestTClientTcpConnectionActor.TestDoubleConnect;
begin
  Self.Connect;

  Self.ConnEvent.ResetEvent;
  Self.Connection.Connect(Self.ConnectTo);
  Self.WaitFor(Self.ConnEvent, OneSecond, 'Client didn''t reconnect');
end;

procedure TestTClientTcpConnectionActor.TestReceivedData;
begin
  Self.Connect;

  Self.ServerSend(Self.TestData);

  Self.Connection.ReceiveData(OneSecond);

  Self.RPC.Receive(Self.FindReceivedData, Self.StoreReceivedData, OneSecond, Self.Timeout);
  Check(not Self.TimedOut, 'Timed out waiting for data');
  CheckEquals(Self.TestData, Self.ReceivedData, 'Incorrect data received');
end;

procedure TestTClientTcpConnectionActor.TestSendData;
begin
  Self.Server.OnExecute := Self.CollectTestData;

  Self.Connect;

  Self.Connection.SendData(Self.SendData);

  Self.WaitFor(Self.SendEvent, OneSecond, 'Waiting for sent data to arrive');

  CheckEquals(Self.TestData, Self.ReceivedData, 'Data not sent');
end;

initialization;
  RegisterTest('ClientTcpConnectionActor', Suite);
end.
