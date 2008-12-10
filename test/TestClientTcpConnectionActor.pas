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
  TestFunctions = class(TTestCase)
  published
    procedure TestStreamToStrEmptyString;
    procedure TestStreamToStr;
  end;

  TestTClientTcpConnectionActor = class(TActorTestCase)
  private
    CloseConnection: TMessageTuple;
    Closed:          Boolean;
    Connected:       Boolean;
    ConnectionCount: Integer;
    ConnectTo:       TOpenMsg;
    ConnEvent:       TEvent;
    DisconEvent:     TEvent;
    Disconnected:    Boolean;
    Error:           String;
    LastClientPort:  Integer;
    Location:        TLocationTuple;
    TestTable:       TActorMessageTable;

    procedure AckConnection(Thread: TIdPeerThread);
    function  FindClosed(Msg: TTuple): Boolean;
    function  FindConnected(Msg: TTuple): Boolean;
    procedure MarkConnected(Msg: TTuple);
    procedure MarkClosed(Msg: TTuple);
    function  MatchMessageName(Msg: TTuple; Name: String): Boolean;
  protected
    Connection:   TClientTcpConnectionActor;
    Environment:  TActorEnvironment;
    ReceivedData: String;
    RPC:          TActorInterface;
    SendData:     TSendDataMsg;
    SendEvent:    TEvent;
    Server:       TIdTcpServer;
    TestData:     String;
    TimedOut:     Boolean;

    procedure CollectTestData(Thread: TIdPeerThread);
    procedure Connect;
    procedure Disconnect;
    function  FindReceivedData(Msg: TTuple): Boolean;
    procedure ServerSend(S: String);
    function  SpawnConnection: TClientTcpConnectionActor; virtual;
    procedure StoreReceivedData(Msg: TTuple);
    procedure Timeout;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestClose;
    procedure TestCloseSendsClosedMessage;
    procedure TestConnect;
    procedure TestDoubleClose;
    procedure TestDoubleConnect;
    procedure TestReceivedData; virtual;
    procedure TestSendData; virtual;
  end;

  TestTClientTcpConnectionActorInterface = class(TTestCase)
  private
    Client:        TProcessID;
    ClientAddress: String;
    ClientPort:    Integer;
    ConnectEvent:  TEvent;
    DataEvent:     TEvent;
    Iface:         TClientTcpConnectionActorInterface;
    ReceivedData:  String;
    Server:        TIdTcpServer;
    TestData:      String;

    procedure AckConnection(Peer: TIdPeerThread);
    procedure RecordData(Peer: TIdPeerThread);
    procedure CheckPortFree(Address: String; Port: Cardinal);
    procedure WaitFor(E: TEvent; Timeout: Cardinal; Msg: String);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestConnect;
    procedure TestConnectNoServer;
    procedure TestSendData;
  end;

const
  OneSecond      = 1000;
  DefaultTimeout = OneSecond;

implementation

uses
  Classes, Forms, IdException, Messages, SysUtils, IdTCPConnection;

function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('ClientTcpConnectionActor unit tests');
  Result.AddSuite(TestFunctions.Suite);
  Result.AddSuite(TestTClientTcpConnectionActor.Suite);
  Result.AddSuite(TestTClientTcpConnectionActorInterface.Suite);
end;

//******************************************************************************
//* TestFunctions                                                              *
//******************************************************************************
//* TestFunctions Published methods ********************************************

procedure TestFunctions.TestStreamToStrEmptyString;
var
  S: TStringStream;
begin
  S := TStringStream.Create('');
  try
    CheckEquals('', StreamToStr(S), 'Empty string');
  finally
    S.Free;
  end;
end;

procedure TestFunctions.TestStreamToStr;
const
  Data = 'foobar';
var
  S: TStringStream;
begin
  S := TStringStream.Create(Data);
  try
    CheckEquals(Data, StreamToStr(S), 'Non-empty string');
  finally
    S.Free;
  end;
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

  Self.Connection := Self.SpawnConnection;

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

//* TestTClientTcpConnectionActor Protected methods ****************************

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
  Self.Connection.Open(Self.ConnectTo);
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

function TestTClientTcpConnectionActor.FindReceivedData(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ReceivedDataMsg);
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

function TestTClientTcpConnectionActor.SpawnConnection: TClientTcpConnectionActor;
begin
  Result := TClientTcpConnectionActor.Create(Self.Environment, TActor.RootActor);
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

function TestTClientTcpConnectionActor.FindClosed(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ClosedConnectionMsg);
end;

function TestTClientTcpConnectionActor.FindConnected(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, OpenedMsg);
end;

procedure TestTClientTcpConnectionActor.MarkClosed(Msg: TTuple);
var
  O: TMessageTuple;
begin
  Self.Closed := true;

  O := TMessageTuple.Overlay(Msg);
  try
    if (O.Parameters.Count > 0) then
      Self.Error := (O.Parameters[0] as TStringTerm).Value;
  finally
    O.Free;
  end;
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
  Self.Connection.Open(Self.ConnectTo);
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

//******************************************************************************
//* TestTClientTcpConnectionActorInterface                                     *
//******************************************************************************
//* TestTClientTcpConnectionActorInterface Public methods **********************

procedure TestTClientTcpConnectionActorInterface.SetUp;
begin
  inherited SetUp;

  Self.Client       := Spawn(TClientTcpConnectionActor);
  Self.ConnectEvent := TSimpleEvent.Create;
  Self.DataEvent    := TSimpleEvent.Create;
  Self.Iface        := TClientTcpConnectionActorInterface.Create(DefaultEnv, Self.Client);
  Self.Iface.Timeout := OneSecond;

  Self.Server := TIdTCPServer.Create(nil);
  Self.Server.OnExecute := Self.AckConnection;
  Self.Server.Bindings.Add;
  Self.Server.Bindings[0].IP   := '127.0.0.1';
  Self.Server.Bindings[0].Port := 54321;

  CheckPortFree(Self.Server.Bindings[0].IP, Self.Server.Bindings[0].Port);

  Self.Server.Active := true;

  Self.ReceivedData := '';
  Self.TestData     := 'test';
end;

procedure TestTClientTcpConnectionActorInterface.TearDown;
begin
  Self.Server.Free;
  Self.Iface.Terminate;
  Self.Iface.Free;
  Self.DataEvent.Free;
  Self.ConnectEvent.Free;

  inherited TearDown;
end;

//* TestTClientTcpConnectionActorInterface Private methods *********************

procedure TestTClientTcpConnectionActorInterface.AckConnection(Peer: TIdPeerThread);
begin
  Self.ClientAddress := Peer.Connection.Socket.Binding.PeerIP;
  Self.ClientPort    := Peer.Connection.Socket.Binding.PeerPort;

  Self.ConnectEvent.SetEvent;
end;

procedure TestTClientTcpConnectionActorInterface.RecordData(Peer: TIdPeerThread);
const
  MoreThanTestDataLength = 1024;
begin
  Self.ReceivedData := Peer.Connection.ReadString(Length(Self.TestData));

  Self.DataEvent.SetEvent;
end;

procedure TestTClientTcpConnectionActorInterface.CheckPortFree(Address: String; Port: Cardinal);
var
  S: TIdTcpServer;
begin
  S := TIdTCPServer.Create(nil);
  try
    S.Bindings.Add;
    S.Bindings[0].IP   := Address;
    S.Bindings[0].Port := Port;

    try
      S.Active := true;
    except
      on E: Exception do
        Fail(Format('Test requires a used port: Could not bind to %s:%d (%s: %s)', [Address, Port, E.ClassName, E.Message]));
    end;

  finally
    S.Free;
  end;
end;

procedure TestTClientTcpConnectionActorInterface.WaitFor(E: TEvent; Timeout: Cardinal; Msg: String);
begin
  if (wrSignaled <> E.WaitFor(Timeout)) then
    Fail(Msg);
end;

//* TestTClientTcpConnectionActorInterface Published methods *******************

procedure TestTClientTcpConnectionActorInterface.TestConnect;
begin
  Self.Iface.Connect(Self.Server.Bindings[0].IP, Self.Server.Bindings[0].Port);

  CheckEquals(Self.ClientAddress, Self.Iface.LocalBinding.Address,   'LocalBinding Address');
  CheckEquals(Self.ClientPort,    Self.Iface.LocalBinding.Port,      'LocalBinding Port');
  CheckEquals(TcpTransport,       Self.Iface.LocalBinding.Transport, 'LocalBinding Transport');

  CheckEquals(Self.Server.Bindings[0].IP,   Self.Iface.PeerBinding.Address,   'PeerBinding Address');
  CheckEquals(Self.Server.Bindings[0].Port, Self.Iface.PeerBinding.Port,      'PeerBinding Port');
  CheckEquals(TcpTransport,                 Self.Iface.PeerBinding.Transport, 'PeerBinding Transport');

  Self.WaitFor(Self.ConnectEvent, OneSecond, 'Timed out waiting for a connection');
end;

procedure TestTClientTcpConnectionActorInterface.TestConnectNoServer;
var
  Address: String;
  Port:    Cardinal;
begin
  Address := Self.Server.Bindings[0].IP;
  Port    := Self.Server.Bindings[0].Port + 1;

  CheckPortFree(Address, Port);

  Self.ExpectedException := EIdConnectException;
  Self.Iface.Connect(Address, Port);
end;

procedure TestTClientTcpConnectionActorInterface.TestSendData;
begin
  Self.Server.OnExecute := Self.RecordData;

  Self.Iface.Connect(Self.Server.Bindings[0].IP, Self.Server.Bindings[0].Port);

  Self.Iface.SendData(TestData);

  Self.WaitFor(Self.DataEvent, OneSecond, 'Failed to receive data');

  CheckEquals(Self.TestData, Self.ReceivedData, 'Data corrupted');
end;

initialization;
  RegisterTest('ClientTcpConnectionActor', Suite);
end.
