unit ClientTcpConnectionActor;

interface

uses
  Classes, Contnrs, IdTcpClient, Ikaria, Messages, SyncObjs;

type
  // In Erlang: {"127.0.0.1", 8080, "TCP"}
  // In LISP: '("127.0.0.1" 8080 "TCP")
  TLocationTuple = class(TTuple)
  private
    function  GetAddress: String;
    function  GetPort: Integer;
    function  GetTransport: String;
  public
    constructor Create(Address: String; Port: Cardinal; Transport: String);

    property Address:   String  read GetAddress;
    property Port:      Integer read GetPort;
    property Transport: String  read GetTransport;
  end;

  // ("connect" {reply-to} ("127.0.0.1" 8000 "TCP"))
  TConnectMsg = class(TMessageTuple)
  private
    function GetLocation: TLocationTuple;
  public
    constructor Create(ReplyTo: TProcessID; Location: TLocationTuple);

    property Location: TLocationTuple read GetLocation;
  end;

  // An Actor that sends the following messages:
  // * ("closed" {own-pid})
  // * ConnectionOpened
  // * Error
  // * Exit
  // * ReceivedData(String)
  // and receives the following messages:
  // * ("close" {src-pid})
  // * ("connect" {src-pid} ("address" port "transport))
  // * SendData(String)
  TClientTcpConnectionActor = class(TActor)
  private
    Connection: TIdTcpClient;
    Controller: TProcessID;

    procedure Close(Msg: TActorMessage);
    procedure Connect(Msg: TActorMessage);
    function  FindClose(Msg: TActorMessage): Boolean;
    function  FindConnect(Msg: TActorMessage): Boolean;
    procedure SignalClosureTo(Target: TProcessID);
    procedure SignalOpeningTo(Target: TProcessID);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  public
    constructor Create(Parent: TProcessID); override;
    destructor  Destroy; override;
  end;

const
  CloseConnectionMsg  = 'close';
  ClosedConnectionMsg = 'closed';
  ConnectMsg          = 'connect';
  ConnectedMsg        = 'connected';

implementation

//******************************************************************************
//* TLocationTuple                                                             *
//******************************************************************************
//* TLocationTuple Public methods **********************************************

constructor TLocationTuple.Create(Address: String; Port: Cardinal; Transport: String);
begin
  inherited Create;

  Self.AddString(Address);
  Self.AddInteger(Port);
  Self.AddString(Transport);
end;

//* TLocationTuple Private methods *********************************************

function TLocationTuple.GetAddress: String;
begin
  Result := TStringElement(Self[0]).Value;
end;

function TLocationTuple.GetPort: Integer;
begin
  Result := TIntegerElement(Self[1]).Value;
end;

function TLocationTuple.GetTransport: String;
begin
  Result := TStringElement(Self[2]).Value;
end;

//******************************************************************************
//* TConnectMsg                                                                *
//******************************************************************************
//* TConnectMsg Public methods *************************************************

constructor TConnectMsg.Create(ReplyTo: TProcessID; Location: TLocationTuple);
begin
  inherited Create(ConnectMsg, ReplyTo, Location);
end;

//* TConnectMsg Private methods ************************************************

function TConnectMsg.GetLocation: TLocationTuple;
begin
  Result := Self.Parameters as TLocationTuple;
end;

//******************************************************************************
//* TClientTcpConnectionActor                                                  *
//******************************************************************************
//* TClientTcpConnectionActor Public methods ***********************************

constructor TClientTcpConnectionActor.Create(Parent: TProcessID);
begin
  inherited Create(Parent);

  Self.Connection := TIdTcpClient.Create(nil);
end;

destructor TClientTcpConnectionActor.Destroy;
begin
  Self.Connection.Free;

  inherited Destroy;
end;

//* TClientTcpConnectionActor Protected methods ********************************

procedure TClientTcpConnectionActor.RegisterActions(Table: TActorMessageTable);
begin
  inherited RegisterActions(Table);

  Table.Add(Self.FindConnect, Self.Connect);
  Table.Add(Self.FindClose, Self.Close);
end;

//* TClientTcpConnectionActor Private methods **********************************

procedure TClientTcpConnectionActor.Close(Msg: TActorMessage);
begin
  Self.Connection.Disconnect;

  Self.SignalClosureTo(Self.Controller);
end;

procedure TClientTcpConnectionActor.Connect(Msg: TActorMessage);
var
  Conn: TConnectMsg;
begin
  if Self.Connection.Connected then Exit;

  Conn := TConnectMsg.Overlay(Msg.Data);
  try
    Self.Controller := Conn.ReplyTo;

    Self.Connection.Host := Conn.Location.Address;
    Self.Connection.Port := Conn.Location.Port;
    Self.Connection.Connect;
  finally
    Conn.Free;
  end;

  Self.SignalOpeningTo(Self.Controller);
end;

function TClientTcpConnectionActor.FindClose(Msg: TActorMessage): Boolean;
begin
  Result := (Msg.Data.Count > 0)
         and ((Msg.Data[0] as TStringElement).Value = CloseConnectionMsg);
end;

function TClientTcpConnectionActor.FindConnect(Msg: TActorMessage): Boolean;
begin
  Result := (Msg.Data.Count > 0)
         and ((Msg.Data[0] as TStringElement).Value = ConnectMsg);
end;

procedure TClientTcpConnectionActor.SignalClosureTo(Target: TProcessID);
var
  Closed: TMessageTuple;
begin
  Closed := TMessageTuple.Create(ClosedConnectionMsg, Self.PID);
  try
    Self.Send(Target, Closed);
  finally
    Closed.Free;
  end;
end;

procedure TClientTcpConnectionActor.SignalOpeningTo(Target: TProcessID);
begin
end;

end.
