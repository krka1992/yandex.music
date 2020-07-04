unit ymFace;

interface

uses Sysutils, idHTTP, System.JSON, System.Classes, Windows, sblRefObject;

type
  TymObject = class(TObject)
  private
    FRefCount: Integer;
    FTerminated: nativeInt;
    function GetTerminated: Boolean;
  protected
    procedure DoTerminate; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    procedure Terminate;
    function _CopyRef: Pointer;
    procedure _Release;
    property RefCount: Integer read FRefCount;
    property Terminated: Boolean read GetTerminated;
  end;

  TymObjectLocked = class(TymObject)
  private
    FRTLCS: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock;
    procedure Unlock;
  end;

  TymObjectList = class(TymObject)
  protected
    FList: TList;
    function GetCount: Integer;
    function GetItems(Index: Integer): Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure AddDirect(Item: TymObject);
    procedure Add(Item: TymObject);
    function Extract(var Item: TymObject): Boolean;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Pointer read GetItems; default;
  end;

  TymObjectList<T: TymObject> = class(TymObjectList)
  public
    destructor Destroy; override;
    procedure Clear; override;
    function Extract(var Item: T): Boolean;
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

procedure _ReleaseNil(var Obj);
procedure _TerminateReleaseNil(var Obj);

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

procedure _ReleaseNil(var Obj);
var
  ObjTmp: TymObject;
begin
  Pointer(ObjTmp) := Pointer(Obj);
  if not Assigned(ObjTmp) then exit;
  Pointer(Obj) := nil;
  ObjTmp._Release;
end;

procedure _TerminateReleaseNil(var Obj);
var
  ObjTmp: TymObject;
begin
  Pointer(ObjTmp) := Pointer(Obj);
  if not Assigned(ObjTmp) then exit;
  Pointer(Obj) := nil;
  ObjTmp.Terminate;
  ObjTmp._Release;
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

procedure TymObject.DoTerminate;
begin

end;

function TymObject.GetTerminated: Boolean;
begin
  Result := FTerminated <> 0;
end;

procedure TymObject.Terminate;
begin
  AtomicExchange(FTerminated, 1);
  DoTerminate;
end;

function TymObject._CopyRef: Pointer;
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

constructor TymObjectList.Create;
begin
  inherited Create;
  FList := TList.Create();
end;

destructor TymObjectList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TymObjectList.Add(Item: TymObject);
begin
  FList.Add(Item);
  Item._CopyRef;
end;

procedure TymObjectList.AddDirect(Item: TymObject);
begin
  FList.Add(Item);
end;

procedure TymObjectList.Clear;
begin
  FList.Clear;
end;

function TymObjectList.Extract(var Item: TymObject): Boolean;
begin
  Result := FList.Count > 0;
  if not Result then exit;
  Item := FList[0];
  FList.Delete(0);
end;

function TymObjectList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TymObjectList.GetItems(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= GetCount) then exit(nil);
  Result := FList[Index];
end;

destructor TymObjectList<T>.Destroy;
var
  p: T;
begin
  //while FList.Extract(Pointer(p)) <> nil do
  //{}p._Release;
  inherited Destroy;
end;

procedure TymObjectList<T>.Clear;
var
  i: Integer;
  Item: TymObject;
begin
  for i := 0 to FList.Count - 1 do
  begin
    Item := TymObject(FList[i]);
    _ReleaseNil(Item);
  end;
end;

function TymObjectList<T>.Extract(var Item: T): Boolean;
begin
  Result := inherited Extract(TymObject(Item));
  //Result := Assigned(FList.Extract(Item));
end;

constructor TymObjectLocked.Create;
begin
  inherited Create;
  InitializeCriticalSection(FRTLCS);
end;

destructor TymObjectLocked.Destroy;
begin
  DeleteCriticalSection(FRTLCS);
  inherited Destroy;
end;

{TymObjectLocked}

procedure TymObjectLocked.Lock;
begin
  EnterCriticalSection(FRTLCS);
end;

procedure TymObjectLocked.Unlock;
begin
  LeaveCriticalSection(FRTLCS);
end;

end.
