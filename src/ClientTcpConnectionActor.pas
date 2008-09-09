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
    constructor Create(ReplyTo: TProcessID; Location: TLocationTuple);

    property Location: TLocationTuple read GetLocation;
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
  // * ("opened" {own-pid})
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
    procedure ReportReceivedData(C: TIdTcpConnection);
    procedure SignalClosureTo(Target: TProcessID; Reason: String = '');
    procedure SignalDataTo(Target: TProcessID; Data: String);
    procedure SignalOpeningTo(Target: TProcessID);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
    procedure Run; override;
  public
    constructor Create(Parent: TProcessID); override;
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
  SysUtils, TypInfo, WinSock;

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

//* TOpenMsg Private methods ***************************************************

function TOpenMsg.GetLocation: TLocationTuple;
begin
  Result := Self.Parameters as TLocationTuple;
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

constructor TClientTcpConnectionActor.Create(Parent: TProcessID);
begin
  inherited Create(Parent);

  Self.Connection := TIdTcpClient.Create(nil);
  Self.Timeout := 20*OneSecond;
end;

destructor TClientTcpConnectionActor.Destroy;
begin
  Self.Connection.Free;

  inherited Destroy;
end;

procedure TClientTcpConnectionActor.Close(Msg: TTuple);
begin
  if Self.Connection.Connected then begin
    Self.Connection.Disconnect;

    Self.SignalClosureTo(Self.Controller);
  end;
end;

procedure TClientTcpConnectionActor.Connect(Msg: TTuple);
var
  Conn: TOpenMsg;
begin
  if Self.Connection.Connected then Exit;

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
  Self.ReportReceivedData(Self.Connection);
  Self.Connection.InputBuffer.Remove(Self.Connection.InputBuffer.Size);
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
    Self.ReceiveData(FiftyMilliseconds);
  end;
end;

//* TClientTcpConnectionActor Private methods **********************************

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
begin
  Self.Send(Target, OpenedMsg);
end;

end.
