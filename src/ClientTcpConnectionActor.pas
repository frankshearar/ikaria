{
  (c) 2008 Directorate of New Technologies, Royal National Institute for Deaf people (RNID)

  The RNID licence covers this unit. Read the licence at:
      http://www.ictrnid.org.uk/docs/gw/rnid_license.txt

  This unit contains code written by:
    * Frank Shearar
}
unit ClientTcpConnectionActor;

interface

uses
  Classes, Contnrs, IdTcpClient, IdTcpConnection, Ikaria, Messages, SyncObjs,
  Windows;

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

  // ("open" {reply-to} ("127.0.0.1" 8000 "TCP"))
  TOpenMsg = class(TMessageTuple)
  private
    function GetLocation: TLocationTuple;
  public
    constructor Create(ReplyTo: TProcessID; Location: TLocationTuple); overload;
    constructor Create(ReplyTo: TProcessID; Address: String; Port: Cardinal; Transport: String); overload;

    property Location: TLocationTuple read GetLocation;
  end;

  // I report the opening of a connection, returning the binding information of
  // that connection:
  //
  // ("opened" <own pid> (<local binding> <remote binding>)):
  // ("opened" {reply-to} (("127.0.0.01" 3017 "TCP") ("10.0.0.1" 80 "TCP")))
  TOpenedMsg = class(TMessageTuple)
  private
    function GetLocalBinding: TLocationTuple;
    function GetPeerBinding: TLocationTuple;
  public
    constructor Create(ReplyTo: TProcessID; LocalBinding, PeerBinding: TLocationTuple); overload;
    constructor Create(ReplyTo: TProcessID;
                       LocalAddress: String; LocalPort: Cardinal; LocalTransport: String;
                       PeerAddress: String; PeerPort: Cardinal; PeerTransport: String); overload;

    property LocalBinding: TLocationTuple read GetLocalBinding;
    property PeerBinding:  TLocationTuple read GetPeerBinding;
  end;

  TDataMsg = class(TMessageTuple)
  private
    function GetData: String;
  protected
    function MessageName: String; virtual;
  public
    constructor Create(ReplyTo: TProcessID; Data: String);

    property Data: String read GetData;
  end;

  // ("received-data" {reply-to} ("data"))
  TReceivedDataMsg = class(TDataMsg)
    function MessageName: String; override;
  end;

  // ("send-data" {reply-to} ("data"))
  TSendDataMsg = class(TDataMsg)
    function MessageName: String; override;
  end;

  // An Actor that sends the following messages:
  // * ("closed" {own-pid})
  // * ("closed" {own-pid} ("error reason"))
  // * ("opened" <own pid> (<local binding> <remote binding>))
  // * ("received-data {own-pid} ("data"))
  // and receives the following messages:
  // * ("close" {src-pid})
  // * ("connect" {src-pid} ("address" port "transport))
  // * ("send-data" {src-pid} ("data"))
  TClientTcpConnectionActor = class(TActor)
  private
    Connection: TIdTcpClient;
    Controller: TProcessID;
    Timeout:    Cardinal;

    procedure CloseConnection(Connection: TIdTCPConnection; Controller: TProcessID);
    procedure Disconnected(Sender: TObject);
    procedure ReportReceivedData(C: TIdTcpConnection);
    procedure SignalClosureTo(Target: TProcessID; Reason: String = '');
    procedure SignalDataTo(Target: TProcessID; Data: String);
    procedure SignalOpeningTo(Target: TProcessID);
    procedure TryReceiveData(Timeout: Cardinal);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
    procedure Run; override;
  public
    constructor Create(E: TActorEnvironment; Parent: TProcessID); override;
    destructor  Destroy; override;

    procedure Close(Msg: TTuple);
    procedure Connect(Msg: TTuple);
    function  Connected: Boolean;
    function  FindClose(Msg: TTuple): Boolean;
    function  FindConnect(Msg: TTuple): Boolean;
    function  FindSendData(Msg: TTuple): Boolean;
    procedure ReceiveData(Timeout: Cardinal);
    procedure SendData(Msg: TTuple);
  end;

  // I provide a nice "normal" interface to a ClientTcpConnectionActor.
  TClientTcpConnectionActorInterface = class(TActorInterface)
  private
    Client:        TProcessID;
    fLocalBinding: TLocationTuple;
    fPeerBinding:  TLocationTuple;
    fTimeout:      Cardinal;

    function  FindClosed(Msg: TTuple): Boolean;
    function  FindOpened(Msg: TTuple): Boolean;
    procedure RaiseConnectTimeoutException;
    procedure ReactToOpened(Msg: TTuple);
    procedure Replace(var Field: TLocationTuple; NewValue: TLocationTuple);
  public
    constructor Create(E: TActorEnvironment; ClientPID: TProcessID);
    destructor  Destroy; override;

    procedure Close;
    procedure Connect(Address: String; Port: Cardinal; Transport: String = 'TCP');
    procedure SendData(S: String);

    property LocalBinding: TLocationTuple read fLocalBinding;
    property PeerBinding:  TLocationTuple read fPeerBinding;
    property Timeout:      Cardinal       read fTimeout write fTimeout;
  end;

const
  CloseConnectionMsg  = 'close';
  ClosedConnectionMsg = 'closed';
  OpenMsg             = 'open';
  OpenedMsg           = 'opened';
  ReceivedDataMsg     = 'received-data';
  SendDataMsg         = 'send-data';

const
  FiftyMilliseconds = 50;

implementation

uses
  IdException, IdSocketHandle, SysUtils, TypInfo, WinSock;

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
  Result := TStringTerm(Self[0]).Value;
end;

function TLocationTuple.GetPort: Integer;
begin
  Result := TIntegerTerm(Self[1]).Value;
end;

function TLocationTuple.GetTransport: String;
begin
  Result := TStringTerm(Self[2]).Value;
end;

//******************************************************************************
//* TOpenMsg                                                                   *
//******************************************************************************
//* TOpenMsg Public methods ****************************************************

constructor TOpenMsg.Create(ReplyTo: TProcessID; Location: TLocationTuple);
begin
  inherited Create(OpenMsg, ReplyTo, Location);
end;

constructor TOpenMsg.Create(ReplyTo: TProcessID; Address: String; Port: Cardinal; Transport: String);
var
  Target: TLocationTuple;
begin
  Target := TLocationTuple.Create(Address, Port, Transport);
  try
    Self.Create(ReplyTo, Target);
  finally
    Target.Free;
  end;
end;

//* TOpenMsg Private methods ***************************************************

function TOpenMsg.GetLocation: TLocationTuple;
begin
  Result := Self.Parameters as TLocationTuple;
end;

//******************************************************************************
//* TOpenedMsg                                                                 *
//******************************************************************************
//* TOpenedMsg Public methods **************************************************

constructor TOpenedMsg.Create(ReplyTo: TProcessID; LocalBinding, PeerBinding: TLocationTuple);
var
  Params: TTuple;
begin
  Params := TTuple.Create;
  try
    Params.Add(LocalBinding);
    Params.Add(PeerBinding);

    inherited Create(OpenedMsg, ReplyTo, Params);
  finally
    Params.Free;
  end;
end;

constructor TOpenedMsg.Create(ReplyTo: TProcessID;
                              LocalAddress: String; LocalPort: Cardinal; LocalTransport: String;
                              PeerAddress: String; PeerPort: Cardinal; PeerTransport: String);
var
  LocalBinding: TLocationTuple;
  PeerBinding:  TLocationTuple;
begin
  LocalBinding := TLocationTuple.Create(LocalAddress, LocalPort, LocalTransport);
  try
    PeerBinding := TLocationTuple.Create(PeerAddress, PeerPort, PeerTransport);
    try
      Self.Create(ReplyTo, LocalBinding, PeerBinding);
    finally
      PeerBinding.Free;
    end;
  finally
    LocalBinding.Free;
  end;
end;

//* TOpenedMsg Private methods **************************************************

function TOpenedMsg.GetLocalBinding: TLocationTuple;
begin
  Result := Self.Parameters[0] as TLocationTuple;
end;

function TOpenedMsg.GetPeerBinding: TLocationTuple;
begin
  Result := Self.Parameters[1] as TLocationTuple;
end;

//******************************************************************************
//* TDataMsg                                                                   *
//******************************************************************************
//* TDataMsg Public methods ****************************************************

constructor TDataMsg.Create(ReplyTo: TProcessID; Data: String);
var
  Params: TTuple;
begin
  Params := TTuple.Create;
  try
    Params.AddString(Data);

    inherited Create(Self.MessageName, ReplyTo, Params);
  finally
    Params.Free;
  end;
end;

//* TDataMsg Protected methods *************************************************

function TDataMsg.MessageName: String;
begin
  Result := 'someone-didnt-override-tdatamsg-messagename';
end;

//* TDataMsg Private methods ***************************************************

function TDataMsg.GetData: String;
begin
  Result := (Self.Parameters[0] as TStringTerm).Value;
end;

//******************************************************************************
//* TReceivedDataMsg                                                           *
//******************************************************************************
//* TReceivedDataMsg Protected methods *****************************************

function TReceivedDataMsg.MessageName: String;
begin
  Result := ReceivedDataMsg;
end;

//******************************************************************************
//* TSendDataMsg                                                               *
//******************************************************************************
//* TSendDataMsg Protected methods *********************************************

function TSendDataMsg.MessageName: String;
begin
  Result := SendDataMsg;
end;

//******************************************************************************
//* TClientTcpConnectionActor                                                  *
//******************************************************************************
//* TClientTcpConnectionActor Public methods ***********************************

constructor TClientTcpConnectionActor.Create(E: TActorEnvironment; Parent: TProcessID);
begin
  inherited Create(E, Parent);

  Self.Connection := TIdTcpClient.Create(nil);
  Self.Timeout := 20*OneSecond;

  Self.Connection.OnDisconnected := Self.Disconnected;
end;

destructor TClientTcpConnectionActor.Destroy;
begin
  Self.Connection.Free;

  inherited Destroy;
end;

procedure TClientTcpConnectionActor.Close(Msg: TTuple);
begin
  Self.CloseConnection(Self.Connection, Self.Controller);
end;

procedure TClientTcpConnectionActor.Connect(Msg: TTuple);
var
  Conn: TOpenMsg;
begin
  if Self.Connection.Connected then
    Self.CloseConnection(Self.Connection, Self.Controller);

  Conn := TOpenMsg.Overlay(Msg);
  try
    Self.Controller := Conn.ReplyTo;

    Self.Connection.Host := Conn.Location.Address;
    Self.Connection.Port := Conn.Location.Port;
    Self.Connection.Connect(Self.Timeout);
  finally
    Conn.Free;
  end;

  Self.SignalOpeningTo(Self.Controller);
end;

function TClientTcpConnectionActor.Connected: Boolean;
begin
  Result := Self.Connection.Connected;
end;

function TClientTcpConnectionActor.FindClose(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, CloseConnectionMsg);
end;

function TClientTcpConnectionActor.FindConnect(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, OpenMsg);
end;

function TClientTcpConnectionActor.FindSendData(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, SendDataMsg);
end;

procedure TClientTcpConnectionActor.ReceiveData(Timeout: Cardinal);
begin
  Self.Connection.ReadFromStack(true, Timeout, false);

  if (Self.Connection.InputBuffer.Size > 0) then begin
    Self.ReportReceivedData(Self.Connection);
    Self.Connection.InputBuffer.Remove(Self.Connection.InputBuffer.Size);
  end;
end;

procedure TClientTcpConnectionActor.SendData(Msg: TTuple);
var
  O: TSendDataMsg;
begin
  O := TSendDataMsg.Overlay(Msg);
  try
    Self.Connection.Write(O.Data);
  finally
    O.Free;
  end;
end;

//* TClientTcpConnectionActor Protected methods ********************************

procedure TClientTcpConnectionActor.RegisterActions(Table: TActorMessageTable);
begin
  inherited RegisterActions(Table);

  Table.Add(Self.FindConnect, Self.Connect);
  Table.Add(Self.FindClose, Self.Close);
  Table.Add(Self.FindSendData, Self.SendData);
end;

procedure TClientTcpConnectionActor.Run;
begin
  while not Self.Terminated do begin
    Self.Receive(Self.MsgTable, FiftyMilliseconds);

    Self.TryReceiveData(FiftyMilliseconds);
  end;
end;

//* TClientTcpConnectionActor Private methods **********************************

procedure TClientTcpConnectionActor.CloseConnection(Connection: TIdTCPConnection; Controller: TProcessID);
begin
  if Connection.Connected then
    Connection.Disconnect;
end;

procedure TClientTcpConnectionActor.Disconnected(Sender: TObject);
begin
  Self.SignalClosureTo(Controller);
end;

procedure TClientTcpConnectionActor.ReportReceivedData(C: TIdTcpConnection);
var
  Result: TStringStream;
begin
  Result := TStringStream.Create('');
  try
    Result.CopyFrom(C.InputBuffer, 0);

    Self.SignalDataTo(Self.Controller, Result.DataString);
  finally
    Result.Free;
  end;
end;

procedure TClientTcpConnectionActor.SignalClosureTo(Target: TProcessID; Reason: String = '');
var
  Params: TTuple;
begin
  Params := TTuple.Create;
  try
    Params.AddString(Reason);

    Self.Send(Target, ClosedConnectionMsg, Params);
  finally
    Params.Free;
  end;
end;

procedure TClientTcpConnectionActor.SignalDataTo(Target: TProcessID; Data: String);
var
  Params: TTuple;
begin
  Params := TTuple.Create;
  try
    Params.AddString(Data);

    Self.Send(Target, ReceivedDataMsg, Params);
  finally
    Params.Free;
  end;
end;

procedure TClientTcpConnectionActor.SignalOpeningTo(Target: TProcessID);
var
  B:        TIdSocketHandle;
  Bindings: TOpenedMsg;
begin
  B := Self.Connection.Socket.Binding;
  Bindings := TOpenedMsg.Create(Self.PID, B.IP, B.Port, 'TCP', B.PeerIP, B.PeerPort, 'TCP');
  try
    Self.Send(Self.Controller, Bindings);
  finally
    Bindings.Free;
  end;
end;

procedure TClientTcpConnectionActor.TryReceiveData(Timeout: Cardinal);
begin
  if Self.Connection.Connected then begin
    try
      Self.ReceiveData(FiftyMilliseconds);
    except
      on EIdConnClosedGracefully do
        Self.SignalClosureTo(Self.Controller);
    end;
  end;
end;

//******************************************************************************
//* TClientTcpConnectionActorInterface                                         *
//******************************************************************************
//* TClientTcpConnectionActorInterface Public methods **************************

constructor TClientTcpConnectionActorInterface.Create(E: TActorEnvironment; ClientPID: TProcessID);
begin
  inherited Create(E);

  Self.Client := ClientPID;

  Self.fLocalBinding := TLocationTuple.Create('', 0, '');
  Self.fPeerBinding  := Self.LocalBinding.Copy as TLocationTuple;
  Self.Timeout := 5*OneSecond;
end;

destructor TClientTcpConnectionActorInterface.Destroy;
begin
  Self.fPeerBinding.Free;
  Self.fLocalBinding.Free;

  inherited Destroy;
end;

procedure TClientTcpConnectionActorInterface.Close;
begin
  Self.Send(Self.Client, CloseConnectionMsg);

//  Self.Receive(Self.FindClosed, Self.DoNothing, Self.Timeout, Self.RaiseConnectTimeoutException);
end;

procedure TClientTcpConnectionActorInterface.Connect(Address: String; Port: Cardinal; Transport: String = 'TCP');
var
  M:    TOpenMsg;
  Peer: TLocationTuple;
begin
  // In the case of reconnections, this is wrong, for a time at least. In
  // particular, PeerBinding will contain the binding of the yet-to-be-connected
  // connection.
  Peer := TLocationTuple.Create(Address, Port, Transport);
  try
    Self.Replace(Self.fPeerBinding, Peer);
  finally
    Peer.Free;
  end;

  M := TOpenMsg.Create(Self.PID, Address, Port, Transport);
  try
    Self.Send(Self.Client, M);
  finally
    M.Free;
  end;

  Self.Receive(Self.FindOpened, Self.ReactToOpened, Self.Timeout, Self.RaiseConnectTimeoutException);
end;

procedure TClientTcpConnectionActorInterface.SendData(S: String);
var
  M: TSendDataMsg;
begin
  M := TSendDataMsg.Create(Self.PID, S);
  try
    SendActorMessage(Self.Client, M);
  finally
    M.Free;
  end;
end;

//* TClientTcpConnectionActorInterface Private methods *************************

function TClientTcpConnectionActorInterface.FindClosed(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ClosedConnectionMsg)
end;

function TClientTcpConnectionActorInterface.FindOpened(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, OpenedMsg)
end;

procedure TClientTcpConnectionActorInterface.RaiseConnectTimeoutException;
var
  B: TLocationTuple;
begin
  B := Self.PeerBinding;

  raise EIdConnectException.Create(Format('Failed to connect to %s:%d/%s', [B.Address, B.Port, B.Transport]));
end;

procedure TClientTcpConnectionActorInterface.ReactToOpened(Msg: TTuple);
var
  O: TOpenedMsg;
begin
  O := TOpenedMsg.Overlay(Msg);
  try
    Self.Replace(Self.fLocalBinding, O.LocalBinding);
    Self.Replace(Self.fPeerBinding, O.PeerBinding);
  finally
    O.Free;
  end;
end;

procedure TClientTcpConnectionActorInterface.Replace(var Field: TLocationTuple; NewValue: TLocationTuple);
begin
  Field := NewValue.Copy as TLocationTuple;
end;

end.
