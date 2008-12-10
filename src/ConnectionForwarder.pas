unit ConnectionForwarder;

interface

uses
  ClientTcpConnectionActor, Ikaria, IkariaForWindows;

type
  // I act as a receptionist for a connection actor and a message-forwarding
  // actor. First, I route any command messages to either Connection or
  // Forwarder. Second, _most_ notification messages from Connection come to me
  // to be passed on to the Forwarder. Notably, "opened" is passed back to the
  // actor that sent the "open" message.
  //
  // Forwarder doesn't send any actor messages, only Windows messages.
  TConnectionForwarder = class(TActor)
  private
    Caller:     TProcessID;
    Connection: TProcessID;
    Forwarder:  TProcessID;

    procedure Middleman(Target: TProcessID; Msg: TTuple);
  protected
    procedure RegisterActions(Table: TActorMessageTable); override;
  public
    procedure Close(Msg: TTuple);
    procedure Closed(Msg: TTuple);
    function  FindClose(Msg: TTuple): Boolean;
    function  FindClosed(Msg: TTuple): Boolean;
    function  FindMessageQueueHandle(Msg: TTuple): Boolean;
    function  FindOpen(Msg: TTuple): Boolean;
    function  FindOpened(Msg: TTuple): Boolean;
    function  FindReceivedData(Msg: TTuple): Boolean;
    function  FindSendData(Msg: TTuple): Boolean;
    procedure Open(Msg: TTuple);
    procedure Opened(Msg: TTuple);
    procedure ReceivedData(Msg: TTuple);
    procedure SendData(Msg: TTuple);
    procedure SetMessageQueueHandle(Msg: TTuple);
    procedure SetUp; override;
  end;

implementation

//******************************************************************************
//* TConnectionForwarder                                                       *
//******************************************************************************
//* TConnectionForwarder Public methods ****************************************

procedure TConnectionForwarder.Close(Msg: TTuple);
begin
  Self.Middleman(Self.Connection, Msg);
end;

procedure TConnectionForwarder.Closed(Msg: TTuple);
begin
  Self.Middleman(Self.Forwarder, Msg);
end;

function TConnectionForwarder.FindClose(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, CloseConnectionMsg)
end;

function TConnectionForwarder.FindClosed(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ClosedConnectionMsg)
end;

function TConnectionForwarder.FindMessageQueueHandle(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, MessageQueueHandleName);
end;

function TConnectionForwarder.FindOpen(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, OpenMsg);
end;

function TConnectionForwarder.FindOpened(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, OpenedMsg);
end;

function TConnectionForwarder.FindReceivedData(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, ReceivedDataMsg);
end;

function TConnectionForwarder.FindSendData(Msg: TTuple): Boolean;
begin
  Result := Self.MatchMessageName(Msg, SendDataMsg);
end;

procedure TConnectionForwarder.Open(Msg: TTuple);
var
  O: TMessageTuple;
begin
  O := TMessageTuple.Overlay(Msg);
  try
    Self.Caller := O.ReplyTo;
  finally
    O.Free;
  end;

  Self.Middleman(Self.Connection, Msg);
end;

procedure TConnectionForwarder.Opened(Msg: TTuple);
begin
  // Allow a synchronous open-connection call.
  Self.Middleman(Self.Caller, Msg);
end;

procedure TConnectionForwarder.ReceivedData(Msg: TTuple);
begin
  Self.Middleman(Self.Forwarder, Msg);
end;

procedure TConnectionForwarder.SendData(Msg: TTuple);
begin
  Self.Middleman(Self.Connection, Msg);
end;

procedure TConnectionForwarder.SetMessageQueueHandle(Msg: TTuple);
begin
  Self.Middleman(Self.Forwarder, Msg);
end;

procedure TConnectionForwarder.SetUp;
begin
  inherited SetUp;

  Self.Connection := Self.SpawnLink(TClientTcpConnectionActor);
  Self.Forwarder  := Self.SpawnLink(TWindowsMessageForwarder);
end;

//* TConnectionForwarder Protected methods *************************************

procedure TConnectionForwarder.RegisterActions(Table: TActorMessageTable);
begin
  Table.Add(Self.FindClose, Self.Close);
  Table.Add(Self.FindClosed, Self.Closed);
  Table.Add(Self.FindMessageQueueHandle, Self.SetMessageQueueHandle);
  Table.Add(Self.FindOpen, Self.Open);
  Table.Add(Self.FindOpened, Self.Opened);
  Table.Add(Self.FindReceivedData, Self.ReceivedData);
  Table.Add(Self.FindSendData, Self.SendData);
end;

//* TConnectionForwarder Private methods ***************************************

procedure TConnectionForwarder.Middleman(Target: TProcessID; Msg: TTuple);
var
  M: TTuple;
begin
  M := Msg.RouteTo(Self.PID);
  try
    Self.Send(Target, M);
  finally
    M.Free;
  end;
end;

end.
