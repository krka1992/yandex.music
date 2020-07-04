unit test.async.client.http;

interface

uses sysutils, TestFramework, ymFace, ymHTTPClient, Classes, System.Net.HttpClient;

type

  THttpClientTest = class(TTestCase)
  private
    FClient: TymHTTPClient;
  protected
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure TestQuerySync;
    procedure TestQueryAsync;
  end;

implementation

uses windows;

procedure THttpClientTest.SetUp;
begin
  inherited;
  FClient := TymHTTPClient.Create;
end;

procedure THttpClientTest.TearDown;
begin
  _ReleaseNil(FClient);
  inherited;
end;

procedure THttpClientTest.TestQueryAsync;
var
  Context: TymHTTPAsyncContext;
  Stream: TMemoryStream;
  Request: IHTTPRequest;
begin
  WriteLn;
  WriteLn(Format('THREAD: %d', [GetCurrentThreadId]));
  Stream := TMemoryStream.Create;
  try
    Context := FClient.GetAsync(
      {}'https://speedtest.selectel.ru/10MB',
      {}Stream,
      {}nil,
      {}nil,
      {}nil);

    if not Context.Wait() then
      {}raise Exception.Create('Превышен таймаут запроса');

  finally
    Stream.Free;
    _ReleaseNil(Context);
  end;
end;

procedure THttpClientTest.TestQuerySync;
var
  Stream: TMemoryStream;
  Response: IHTTPResponse;
  Res: String;
begin
  Stream := TMemoryStream.Create;
  try Response := FClient.Get('https://qna.habr.com/tags/firsttime', Stream);
  finally Stream.Free;
  end;

  Res := FClient.Get('https://qna.habr.com/tags/firsttime');
end;

initialization

RegisterTest(THttpClientTest.Suite);

finalization

end.
