unit ymFace;

interface

uses Sysutils, idHTTP, System.JSON, System.Classes;

type
  TymObject = class(TObject)
  private
    FRefCount: Integer;
    FTerminated: nativeInt;
    function GetTerminated: Boolean;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Terminate;
    function _CopyRef: TymObject;
    procedure _Release;
    property RefCount: Integer read FRefCount;
    property Terminated: Boolean read GetTerminated;
  end;

  TymConnection = class(TymObject)
  private
    FHTTP: TidHTTP;
  public
    constructor Create;
    destructor Destroy; override;
    property HTTP: TidHTTP read FHTTP;
  end;

  TymToken = record
    TokenType: String;
    AccesToken: String;
    Expires: String;
    Uid: String;
  end;

  TymObjectApi = class(TymObject)
  protected
    FAccessToken: TymToken;
    FAuthorized: Boolean;
  public
    function GET(const Uri: String): String;
    function POST(const Uri: String; Params: TStringList): String;
    function CreateConnection: TymConnection;
    function SendMethod(const Uri: String): TJSONObject;
  end;

procedure _ReleaseNil(var Obj: TymObject);
procedure _TerminateReleaseNil(var Obj: TymObject);

function _JSONString(Node: TJSONObject; const Name: String): String;
function _JSONBool(Node: TJSONObject; const Name: String): Boolean;
function _JSONInt32(Node: TJSONObject; const Name: String): Integer;

{$IFDEF DEBUG}
procedure _RAISE_STUB(Message: String = ''); inline;
{$ENDIF}

implementation

{$IFDEF DEBUG}

procedure _RAISE_STUB(Message: String = ''); inline;
begin
  if Message = '' then Message := 'STUB!';
  raise Exception.Create(Message);
end;
{$ENDIF}

function _JSONString(Node: TJSONObject; const Name: String): String;
var
  ErrMsg: String;
begin
  if not Assigned(Node) then raise Exception.Create('Error Message');
  if Node.Values[Name].TryGetValue(Result) then exit;
  ErrMsg := Format('Не удалось получить строковое значение пары с ключом: "%s"', [Name]);
  raise Exception.Create(ErrMsg);
end;

function _JSONBool(Node: TJSONObject; const Name: String): Boolean;
var
  ErrMsg: String;
begin
  if not Assigned(Node) then raise Exception.Create('Error Message');
  if Node.Values[Name].TryGetValue(Result) then exit;
  ErrMsg := Format('Не удалось получить булево значение пары с ключом: "%s"', [Name]);
  raise Exception.Create(ErrMsg);
end;

function _JSONInt32(Node: TJSONObject; const Name: String): Integer;
var
  ErrMsg: String;
begin
  if not Assigned(Node) then raise Exception.Create('Error Message');
  if Node.Values[Name].TryGetValue(Result) then exit;
  ErrMsg := Format('Не удалось получить числовое значение пары с ключом: "%s"', [Name]);
  raise Exception.Create(ErrMsg);
end;

procedure _RAISE_YM_OBJECT(Message: String = ''); inline;
begin
  if message = '' then message := 'Error';
  raise Exception.CreateFmt('TymObject error: %s', [message]);
end;

{TymObject}

procedure _ReleaseNil(var Obj: TymObject);
begin
  Obj._Release;
  Obj := nil;
end;

procedure _TerminateReleaseNil(var Obj: TymObject);
begin
  Obj.Terminate;
  Obj._Release;
  Obj := nil;
end;

procedure TymObject.AfterConstruction;
begin
  FRefCount := 1;
end;

procedure TymObject.BeforeDestruction;
begin
  if FRefCount <> 0 then System.Error(reInvalidPtr);
  if not Terminated then Terminate;
end;

function TymObject.GetTerminated: Boolean;
begin
  Result := FTerminated <> 0;
end;

procedure TymObject.Terminate;
begin
  AtomicExchange(FTerminated, 1);
end;

function TymObject._CopyRef: TymObject;
begin
  if FRefCount <= 0 then _RAISE_YM_OBJECT('Invalid Reference count');
  Result := self;
  AtomicIncrement(FRefCount);
end;

procedure TymObject._Release;
begin
  if self = nil then _RAISE_YM_OBJECT('Invalid Reference count');
  if FRefCount <= 0 then _RAISE_YM_OBJECT('Invalid Reference count');
  AtomicDecrement(FRefCount);
  if FRefCount > 0 then exit;
  Destroy;
end;

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

function TymObjectApi.SendMethod(const Uri: String): TJSONObject;
begin
  Result := TJSONObject.ParseJSONValue(GET(Uri)) as TJSONObject;
end;

end.
