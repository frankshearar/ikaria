program ActorFramework;

uses
  Forms,
  GuiTestRunner,
  ClientTcpConnectionActor in 'src\ClientTcpConnectionActor.pas',
  Ikaria in 'src\Ikaria.pas',
  PingPong in 'src\PingPong.pas' {PingPongDemo},
  TestIkaria in 'test\TestIkaria.pas',
  TestClientTcpConnectionActor in 'test\TestClientTcpConnectionActor.pas';

{$R *.res}

//{$DEFINE DEMO}

begin
  {$IFNDEF DEMO}
  GuiTestRunner.RunRegisteredTests;
  {$ELSE}
  Application.Initialize;
  Application.CreateForm(TPingPongDemo, PingPongDemo);
  Application.Run;
  {$ENDIF}
end.
