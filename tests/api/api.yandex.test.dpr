program api.yandex.test;

{$APPTYPE CONSOLE}
{$R *.res}

uses DUnitTestRunner,
  System.SysUtils,
  test in 'test.pas',
  ymApi in '..\..\source\api\ymApi.pas',
  ymFace in '..\..\source\api\ymFace.pas',
  test.obj in 'test.obj.pas';

begin
  ReportMemoryLeaksOnShutdown := true;
  try RunRegisteredTests;
  except
    on E: Exception do Writeln(E.ClassName, ': ', E.Message);
  end;

  ReadLn;

end.
