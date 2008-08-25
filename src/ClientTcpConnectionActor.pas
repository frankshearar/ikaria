unit ClientTcpConnectionActor;

interface

uses
  Classes, Contnrs, Ikaria, Messages, SyncObjs;

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

  TConnectMsg = class(TTuple)
  private
    function GetLocation: TLocationTuple;
  public
    constructor Create(Location: TLocationTuple);

    property Location: TLocationTuple read GetLocation;
  end;

  // An Actor that sends the following messages:
  // * ConnectionClosed
  // * ConnectionOpened
  // * Error
  // * Exit
  // * ReceivedData(String)
  // and receives the following messages:
  // * CloseConnection
  // * ("connect" {src-pid} ("address" port "transport))
  // * SendData(String)
  TClientTcpConnectionActor = class(TActor)
  private
//    Connection: TIdTcpClient;

//    function  FindOpenConnection(Msg: TActorMessage): Boolean;
//    procedure ReactToOpenConnection(Msg: TActorMessage);
  public
    constructor Create(Parent: TProcessID); override;
    destructor  Destroy; override;
  end;

const
  ConnectMsg = 'connect';

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

constructor TConnectMsg.Create(Location: TLocationTuple);
begin
  inherited Create;

  Self.AddString(ConnectMsg);
  Self.Add(Location);
end;

//* TConnectMsg Private methods ************************************************

function TConnectMsg.GetLocation: TLocationTuple;
begin
  Result := Self[1] as TLocationTuple;
end;

//******************************************************************************
//* TClientTcpConnectionActor                                                  *
//******************************************************************************
//* TClientTcpConnectionActor  Public methods **********************************

constructor TClientTcpConnectionActor.Create(Parent: TProcessID);
begin
  inherited Create(Parent);

//  Self.Connection := TIdTcpClient.Create;
end;

destructor TClientTcpConnectionActor.Destroy;
begin
//  Self.Connection.Free;

  inherited Destroy;
end;

end.
