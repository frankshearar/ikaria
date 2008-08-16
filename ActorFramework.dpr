program ActorFramework;

uses
  Forms,
  GuiTestRunner,
  ClientTcpConnectionActor in 'src\ClientTcpConnectionActor.pas',
  Ikaria in 'src\Ikaria.pas',
  TestIkaria in 'test\TestIkaria.pas',
  PingPong in 'src\PingPong.pas' {PingPongDemo};

{$R *.res}

{$DEFINE DEMO}

begin
  {$IFNDEF DEMO}
  GuiTestRunner.RunRegisteredTests;
  {$ELSE}
  Application.Initialize;
  Application.CreateForm(TPingPongDemo, PingPongDemo);
  Application.Run;
  {$ENDIF}
end.
