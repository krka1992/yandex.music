unit ymApiCommon;

interface

uses IdHTTP, ymFace, System.Classes, System.JSON, SysUtils, ymHTTPClient, System.Net.HTTPClient,
  System.Net.URLClient;

type
  TymConnection = class(TymObject)
  private
    FHTTP: TidHTTP;
  public
    constructor Create;
    destructor Destroy; override;
    property HTTP: TidHTTP read FHTTP;
  end;

type
  TymToken = record
    TokenType: String;
    AccesToken: String;
    Expires: String;
    Uid: String;
  end;

type
  TymObjectApi = class(TymObject)
  protected
    FAccessToken: TymToken;
    FAuthorized: Boolean;
    FHeaders: TNetHeaders;
    procedure PrepareHeaders;
  public
    function GET(const Uri: String): String;
    function POST(const Uri: String; Params: TStringList): String;
    function CreateConnection: TymConnection;
    function SendGet(const Uri: String): String;
    function SendGetAsync(const Uri: String; AContent: TStream): TymHTTPAsyncContext;
    function SendMethod(const Uri: String): TJSONObject;
    function GetHTTPClient: TymHTTPClient; virtual;
    function GetAsyncMode: Boolean; virtual;
  end;

implementation

constructor TymConnection.Create;
begin
  inherited Create;
  FHTTP := TidHTTP.Create();
  FHTTP.HTTPOptions := FHTTP.HTTPOptions + [hoKeepOrigProtocol];
  FHTTP.Request.Accept := 'application/json';
  FHTTP.Request.Connection := 'Keep-Alive';
  FHTTP.Request.AcceptLanguage := 'ru';
  FHTTP.Request.CacheControl := 'no-cache';
  //FHTTP.Request.AcceptEncoding := 'gzip, deflate';
  {TODO: Сделать сжатие запроса и ответа}
  FHTTP.Request.UserAgent := 'Windows 10';
end;

destructor TymConnection.Destroy;
begin
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

function TymObjectApi.CreateConnection: TymConnection;
begin
  Result := TymConnection.Create;
  if FAuthorized then
    {}with Result.HTTP.Request.CustomHeaders do
      {}AddValue('Authorization', Format('OAuth %s', [FAccessToken.AccesToken]));
end;

function TymObjectApi.GET(const Uri: String): String;
begin
  with CreateConnection do
  begin
    try Result := HTTP.GET(Uri);
    finally _Release;
    end;
  end;
end;

function TymObjectApi.GetAsyncMode: Boolean;
begin
  raise Exception.Create('У данного экземпляра отсутствует свойство AsyncMode');
end;

function TymObjectApi.GetHTTPClient: TymHTTPClient;
begin
  raise Exception.Create('У данного экземпляра отсутствует объект HTTP клиента');
end;

function TymObjectApi.POST(const Uri: String; Params: TStringList): String;
begin
  with CreateConnection do
  begin
    try
      HTTP.Request.ContentType := 'application/x-www-form-urlencoded';
      Result := HTTP.POST(Uri, Params);
    finally _Release;
    end;
  end;
end;

procedure TymObjectApi.PrepareHeaders;
begin
  if not FAuthorized then exit;
  if Length(FHeaders) = 1 then exit;

  SetLength(FHeaders, 1);
  FHeaders[0].Name := 'Authorization';
  FHeaders[0].Value := Format('OAuth %s', [FAccessToken.AccesToken]);
end;

function TymObjectApi.SendGet(const Uri: String): String;
var
  Client: TymHTTPClient;
begin
  Client := GetHTTPClient;
  PrepareHeaders;
  Result := Client.GET(Uri, FHeaders);
end;

function TymObjectApi.SendGetAsync(const Uri: String; AContent: TStream): TymHTTPAsyncContext;
var
  Client: TymHTTPClient;
begin
  Client := GetHTTPClient;
  PrepareHeaders;
  Result := Client.GetAsync(Uri, AContent, FHeaders);
end;

function TymObjectApi.SendMethod(const Uri: String): TJSONObject;
begin
  //Result := TJSONObject.ParseJSONValue(GET(Uri)) as TJSONObject;
  Result := TJSONObject.ParseJSONValue(SendGet(Uri)) as TJSONObject;
end;

end.
