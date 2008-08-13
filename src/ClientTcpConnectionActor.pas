unit ClientTcpConnectionActor;

interface

uses
  Classes, Contnrs, Ikaria, Messages, SyncObjs;

type
  // An Actor that sends the following messages:
  // * ConnectionClosed
  // * ConnectionOpened
  // * Error
  // * Exit
  // * ReceivedData(String)
  // and receives the following messages:
  // * CloseConnection
  // * Kill
  // * OpenConnection(Address, PortNumber)
  // * SendData(String)

  TClientTcpConnection = class;

  TClientTcpConnectionActor = class(TActor)
  private
    Connection: TClientTcpConnection;
    Messages:   TObjectList;
    Lock:       TCriticalSection;
    WaitEvent:  TEvent;

    procedure AddMessage(Msg: TActorMessage);
    procedure ProcessMessages(List: TObjectList);
  protected
    procedure Execute; override;
  public
    constructor Create(Parent: TProcessID); override;
    destructor  Destroy; override;

    procedure CloseConnection;
    procedure Kill;
    procedure OpenConnection(Address: String; Port: Cardinal);
    procedure SendData(Data: String);
  end;

  TClientTcpConnection = class(TObject)
  public
    procedure CloseConnection;
    procedure Kill;
    procedure OpenConnection(Address: String; Port: Cardinal);
    procedure SendData(Data: String);
  end;

  TCloseConnectionRequest = class(TActorMessage)
  public
    procedure Accept(Actor: TActor); override;
  end;

  TKillRequest = class(TActorMessage)
  public
    procedure Accept(Actor: TActor); override;
  end;

  TOpenConnectionRequest = class(TActorMessage)
  private
    fAddress: String;
    fPort:    Cardinal;
  public
    procedure Accept(Actor: TActor); override;

    property Address: String   read fAddress write fAddress;
    property Port:    Cardinal read fPort write fPort;
  end;

  TSendDataRequest = class(TActorMessage)
  private
    fData: String;
  public
    procedure Accept(Actor: TActor); override;

    property Data: String read fData write fData;
  end;

implementation

//******************************************************************************
//* TClientTcpConnectionActor                                                  *
//******************************************************************************
//* TClientTcpConnectionActor  Public methods **********************************

constructor TClientTcpConnectionActor.Create(Parent: TProcessID);
begin
  inherited Create(Parent);

  Self.Connection := TClientTcpConnection.Create;
  Self.Lock       := TCriticalSection.Create;
  Self.Messages   := TObjectList.Create(true);
  Self.WaitEvent  := TSimpleEvent.Create;
end;

destructor TClientTcpConnectionActor.Destroy;
begin
  Self.Lock.Acquire;
  try
    Self.Messages.Free;
  finally
    Self.Lock.Release;
  end;
  Self.Lock.Free;
  Self.WaitEvent.Free;

  Self.Connection.Free;

  inherited Destroy;
end;

procedure TClientTcpConnectionActor.CloseConnection;
begin

end;

procedure TClientTcpConnectionActor.Kill;
begin

end;

procedure TClientTcpConnectionActor.OpenConnection(Address: String; Port: Cardinal);
begin

end;

procedure TClientTcpConnectionActor.SendData(Data: String);
begin

end;

//* TClientTcpConnectionActor Protected methods ********************************

procedure TClientTcpConnectionActor.Execute;
begin
  while not Self.Terminated do begin
    Self.WaitEvent.WaitFor(1000);

    if not Self.Terminated then begin
      Self.Lock.Acquire;
      try
        Self.ProcessMessages(Self.Messages);
      finally
        Self.Lock.Release;
      end;
    end;
  end;

  // Self.SendToLinkSet(EXIT, 'normal');
end;

//* TClientTcpConnectionActor Private methods **********************************

procedure TClientTcpConnectionActor.AddMessage(Msg: TActorMessage);
begin
  Self.Lock.Acquire;
  try
    Self.Messages.Add(Msg);
  finally
    Self.Lock.Release;
  end;

  Self.WaitEvent.SetEvent;
end;

procedure TClientTcpConnectionActor.ProcessMessages(List: TObjectList);
var
  NextMsg: TActorMessage;
begin
  while (List.Count > 0) do begin
    NextMsg := List[0] as TActorMessage;
    NextMsg.Accept(Self);
    List.Delete(0);
  end;

  Self.WaitEvent.ResetEvent;
end;

//******************************************************************************
//* TClientTcpConnection                                                       *
//******************************************************************************
//* TClientTcpConnection Public methods ****************************************

procedure TClientTcpConnection.CloseConnection;
begin

end;

procedure TClientTcpConnection.Kill;
begin

end;

procedure TClientTcpConnection.OpenConnection(Address: String; Port: Cardinal);
begin

end;

procedure TClientTcpConnection.SendData(Data: String);
begin

end;

//******************************************************************************
//* TCloseConnectionRequest                                                    *
//******************************************************************************
//* TCloseConnectionRequest Public methods *************************************

procedure TCloseConnectionRequest.Accept(Actor: TActor);
begin
  (Actor as TClientTcpConnectionActor).Connection.CloseConnection;
end;

//******************************************************************************
//* TKillRequest                                                               *
//******************************************************************************
//* TKillRequest Public methods ************************************************

procedure TKillRequest.Accept(Actor: TActor);
begin
  Actor.Terminate;
end;

//******************************************************************************
//* TOpenConnectionRequest                                                     *
//******************************************************************************
//* TOpenConnectionRequest Public methods **************************************

procedure TOpenConnectionRequest.Accept(Actor: TActor);
begin

end;

//******************************************************************************
//* TSendDataRequest                                                           *
//******************************************************************************
//* TSendDataRequest Public methods ********************************************

procedure TSendDataRequest.Accept(Actor: TActor);
begin
end;

end.
