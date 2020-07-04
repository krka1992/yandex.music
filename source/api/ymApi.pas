unit ymApi;

interface

uses SysUtils, idHTTP, System.classes, System.JSON, oxmlcdom,
  {}ymFace, ymPlaylist, ymTrack, ymUser, ymFeed, ymHTTPClient, System.Net.HTTPClient, ymApiCommon;

type
  TymApi = class(TymObjectApi)
  private
    FHTTPClient: TymHTTPClient;
    FAsyncMode: Boolean;
    function GetAuthParams(const Username, Password: String): TStringList;
    function InternalAuth(const Username, Password: String): Boolean;
  protected
  public
    constructor Create;
    destructor Destroy; override;
    function CheckTokenExpires: Boolean;
    function Auth(const Username, Password: String): Boolean;
    function GetTrack(const TrackId: String): TymTrack;
    function GetUserPlaylist(const PlaylistId: String): TymPlaylist;
    function GetUser: TymUser;
    function GetFeed: TymFeed;
    function GetHTTPClient: TymHTTPClient; override;
    function GetAsyncMode: Boolean; override;
    property AsyncMode: Boolean write FAsyncMode;
  end;

implementation

const
  _CLIENT_ID = '23cabbbdc6cd418abb4b39c32c41195d';
  _CLIENT_SECRET = '53bc75238f0c4d08a118e51fe9203300';

constructor TymApi.Create;
begin
  inherited Create;
  FHTTPClient := TymHTTPClient.Create();
end;

destructor TymApi.Destroy;
begin
  _ReleaseNil(FHTTPClient);
  inherited Destroy;
end;

function TymApi.Auth(const Username, Password: String): Boolean;
begin
  try Result := InternalAuth(Username, Password);
  except
    on E: Exception do
    begin
      Result := false;
    end;
  end;
end;

function TymApi.CheckTokenExpires: Boolean;
begin
  _RAISE_STUB('Сделать проверку истечения времени действия токена');
  {TODO: Сделать проверку истечения времени действия токена}
end;

function TymApi.GetAsyncMode: Boolean;
begin
  Result := FAsyncMode;
end;

function TymApi.GetAuthParams(const Username, Password: String): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('grant_type=password');
  Result.Add(Format('client_id=%s', [_CLIENT_ID]));
  Result.Add(Format('client_secret=%s', [_CLIENT_SECRET]));
  Result.Add(Format('username=%s', [Username]));
  Result.Add(Format('password=%s', [Password]));
end;

function TymApi.GetFeed: TymFeed;
begin
  Result := TymFeed.Create(self);
end;

function TymApi.GetHTTPClient: TymHTTPClient;
begin
  Result := FHTTPClient;
end;

function TymApi.GetUserPlaylist(const PlaylistId: String): TymPlaylist;
begin
  Result := TymPlaylist.Create(self, PlaylistId);
end;

function TymApi.GetTrack(const TrackId: String): TymTrack;
begin
  Result := TymTrack.Create(self, TrackId);
end;

function TymApi.GetUser: TymUser;
begin
  Result := TymUser.Create(self);
end;

function TymApi.InternalAuth(const Username, Password: String): Boolean;
var
  Response: IHTTPResponse;
  params: TStringList;
  Res: String;
  JSON: TJSONObject;
begin
  params := GetAuthParams(Username, Password);
  try
    Response := FHTTPClient.POST('https://oauth.yandex.ru/token', params);
    if Response.StatusCode <> 200 then
      {}raise Exception.CreateFmt('Неожиданный ответ от сервера [Код: %d] [Ответ: %s]', [Response.StatusCode, Response.StatusText]);

    Res := Response.ContentAsString;
    if Res = '' then
      {}raise Exception.Create('Ошибка аутентификации. Сервер вернул пустое тело ответа');

    JSON := TJSONObject.ParseJSONValue(Res, false, true) as TJSONObject;

    FAccessToken.TokenType := JSON.Values['token_type'].Value;
    FAccessToken.AccesToken := JSON.Values['access_token'].Value;
    FAccessToken.Expires := JSON.Values['expires_in'].Value;
    FAccessToken.Uid := JSON.Values['uid'].Value;

    FAuthorized := true;
    Result := true;
  finally
    Response := nil;
    if Assigned(JSON) then JSON.Free;
    params.Free;
  end;
end;

end.
