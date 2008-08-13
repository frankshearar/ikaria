program ActorFramework;

uses
  GuiTestRunner,
  ClientTcpConnectionActor in 'src\ClientTcpConnectionActor.pas',
  Ikaria in 'src\Ikaria.pas',
  PluggableLogging in 'src\PluggableLogging.pas',
  TestIkaria in 'test\TestIkaria.pas';

{$R *.res}

begin
  GuiTestRunner.RunRegisteredTests;
end.
