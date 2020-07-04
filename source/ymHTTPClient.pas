unit ymHTTPClient;

interface

uses Sysutils, System.Net.HTTPClient, ymFace, System.classes,
  System.Net.URLClient, System.Types, Windows, Messages, ymHTTPClientNotifies;

const
  WM_YM_BASE = WM_USER + $100;
  WM_YM_ON_RECEIVE = WM_YM_BASE + 1;

type
  TymHTTPClient = class;
  TymHTTPClientAgent = class;
  TymHTTPClientNotifyList = class;

  TymOnContextFinalize = procedure(Status: Integer; const Error: String; Response: IHTTPResponse) of Object;
  TymOnDataReceive = procedure(ALen: Int64; ACount: Int64) of Object;

  TymHTTPClientNotifyProc = procedure of object;

  TymNotifyType = (ntReceive, ntComplete, ntError);

  TymHTTPAsyncContext = class(TymObject)
  private
    FClient: TymHTTPClientAgent;
    FRequest: IHTTPRequest;
    FAsyncRes: IAsyncResult;
    FOnDataReceive: TymOnDataReceive;
    FOnContextFinalize: TymOnContextFinalize;
    FAborted: Boolean;
    FNotifyList: TymHTTPClientNotifyList;
    FNotifyHandle: HWND;
    FFail: Boolean;
    FErrorMessage: String;
    procedure DataCompleteCallback(const ARes: IAsyncResult);
    procedure DataReceive(ALen: Int64; ACount: Int64);
    procedure ContextFinalize(Status: Integer; const Error: String; Response: IHTTPResponse);
  protected
    procedure DoTerminate; override;
  public
    constructor Create(AClient: TymHTTPClientAgent; ARequest: IHTTPRequest; ANotifyList: TymHTTPClientNotifyList; AHandle: HWND);
    destructor Destroy; override;
    procedure Abort;
    function Wait(Timeout: Cardinal = INFINITE): Boolean;
    function IsCompleted: Boolean;
    function IsFail: Boolean;
    function IsDone: Boolean;
    property OnDataReceive: TymOnDataReceive read FOnDataReceive write FOnDataReceive;
    property OnContextFinalize: TymOnContextFinalize read FOnContextFinalize write FOnContextFinalize;
    property ErrorMessage: String read FErrorMessage;
  end;

  TymHTTPClientAgent = class(TymObject)
  private
    FOwner: TymHTTPClient;
  public
    function GetOwner: TymHTTPClient;
  end;

  TymAsyncContextList = class(TymObjectList<TymHTTPAsyncContext>)
  public
    function FindByRequest(ARequest: IHTTPRequest): TymHTTPAsyncContext;
  end;

  TymHTTPClientNotify = class(TymObject)
  private
    FNotifyData: TymObject;
    FRequest: IHTTPRequest;
    FNotifyType: TymNotifyType;
  public
    constructor Create(ARequest: IHTTPRequest; ANotifyType: TymNotifyType; AData: TymObject);
    destructor Destroy; override;
    property NotifyData: TymObject read FNotifyData write FNotifyData;
    property Request: IHTTPRequest read FRequest;
    property NotifyType: TymNotifyType read FNotifyType;
  end;

  TymHTTPClientNotifyList = class(TymObjectLocked)
  private
    FList: TymObjectList<TymHTTPClientNotify>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(ARequest: IHTTPRequest; ANotifyType: TymNotifyType; AData: TymObject = nil);
    function Extract(var ANotifyObj: TymHTTPClientNotify): Boolean;
  end;

  TymHTTPClient = class(TymObject)
  private
    FAgent: TymHTTPClientAgent;
    FHTTPClient: THTTPClient;
    FContextList: TymAsyncContextList;
    FNotifyList: TymHTTPClientNotifyList;
    FNotifyHandle: HWND;
    function FindContext(ARequest: IHTTPRequest): TymHTTPAsyncContext;
    function PrepareGet(const AURI: String): IHTTPRequest;
    function PreparePost(const AURI: String; ASource: TStream; const MimeType: String): IHTTPRequest;
    function GetContext(Request: IHTTPRequest; CallbackRecv: TymOnDataReceive = nil; CallbackFin: TymOnContextFinalize = nil)
      : TymHTTPAsyncContext;
    procedure OnReceive(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
    procedure DispatchNotifyReceive(AContext: TymHTTPAsyncContext; AData: TymObject);
    procedure DispatchNotifyComplete(AContext: TymHTTPAsyncContext; AData: TymObject);
    procedure DispatchNotifyError(AContext: TymHTTPAsyncContext; AData: TymObject);
  public
    constructor Create(NotifyHandle: HWND = 0);
    destructor Destroy; override;
    procedure CheckNotifies;
    procedure OnTime;
    function GetAsync(const AURI: String; AContent: TStream; AHeaders: TNetHeaders = nil; CallbackRecv: TymOnDataReceive = nil;
      CallbackFin: TymOnContextFinalize = nil): TymHTTPAsyncContext;
    function PostAsync(const AURI: String; AContent: TStream; ASource: TStream; const MimeType: String; AHeaders: TNetHeaders = nil;
      CallbackRecv: TymOnDataReceive = nil; CallbackFin: TymOnContextFinalize = nil): TymHTTPAsyncContext;
    function Get(const AURI: String; AContent: TStream; AHeaders: TNetHeaders = nil): IHTTPResponse; overload;
    function Get(const AURI: String; AHeaders: TNetHeaders = nil): string; overload;
    function Post(const AURI, ASource: String; AContent: TStream; AHeaders: TNetHeaders = nil): IHTTPResponse; overload;
    function Post(const AURI: String; const ASource: Tstringlist; AHeaders: TNetHeaders = nil): IHTTPResponse; overload;
    property HTTPClient: THTTPClient read FHTTPClient;
  end;

implementation

constructor TymHTTPClient.Create(NotifyHandle: HWND = 0);
begin
  inherited Create;
  FHTTPClient := THTTPClient.Create();
  FHTTPClient.OnReceiveData := OnReceive;
  FAgent := TymHTTPClientAgent.Create;
  FAgent.FOwner := self;
  FContextList := TymAsyncContextList.Create();
  FNotifyList := TymHTTPClientNotifyList.Create();
  FNotifyHandle := NotifyHandle;
end;

destructor TymHTTPClient.Destroy;
begin
  _ReleaseNil(FNotifyList);
  _ReleaseNil(FContextList);
  FreeAndNil(FHTTPClient);
  _TerminateReleaseNil(FAgent);
  inherited Destroy;
end;

procedure TymHTTPClient.CheckNotifies;
var
  Notify: TymHTTPClientNotify;
  Context: TymHTTPAsyncContext;
begin
  while FNotifyList.Extract(Notify) do
  begin
    Context := FContextList.FindByRequest(Notify.Request);
    try
      if not Assigned(Context) then continue;

      case Notify.NotifyType of
        ntReceive: DispatchNotifyReceive(Context, Notify.NotifyData);
        ntComplete: DispatchNotifyComplete(Context, Notify.NotifyData);
        ntError: DispatchNotifyError(Context, Notify.NotifyData);
      end;
    finally _ReleaseNil(Notify);
    end;
  end;
end;

procedure TymHTTPClient.DispatchNotifyComplete(AContext: TymHTTPAsyncContext; AData: TymObject);
var
  DataComplete: THTTPClientNotifyComplete;
begin
  DataComplete := AData as THTTPClientNotifyComplete;
  AContext.ContextFinalize(0, '', DataComplete.Response);
end;

procedure TymHTTPClient.DispatchNotifyError(AContext: TymHTTPAsyncContext; AData: TymObject);
var
  DataError: THTTPClientNotifyError;
begin
  DataError := AData as THTTPClientNotifyError;
  AContext.ContextFinalize(-1, DataError.Message, nil);
end;

procedure TymHTTPClient.DispatchNotifyReceive(AContext: TymHTTPAsyncContext; AData: TymObject);
var
  DataReceive: THTTPClientNotifyReceive;
begin
  DataReceive := AData as THTTPClientNotifyReceive;
  AContext.DataReceive(DataReceive.ContentLength, DataReceive.ReadCount);
end;

function TymHTTPClient.FindContext(ARequest: IHTTPRequest): TymHTTPAsyncContext;
var
  i: Integer;
begin
  for i := 0 to FContextList.Count - 1 do
  begin
    Result := FContextList[i];
    if Result.FRequest = ARequest then exit;
  end;
  Result := nil;
end;

function TymHTTPClient.Get(const AURI: String; AContent: TStream; AHeaders: TNetHeaders = nil): IHTTPResponse;
begin
  Result := FHTTPClient.Get(AURI, AContent, AHeaders);
end;

function TymHTTPClient.Get(const AURI: String; AHeaders: TNetHeaders = nil): string;
begin
  Result := FHTTPClient.Get(AURI, nil, AHeaders).ContentAsString;
end;

function TymHTTPClient.GetAsync(const AURI: String; AContent: TStream; AHeaders: TNetHeaders = nil; CallbackRecv: TymOnDataReceive = nil;
  CallbackFin: TymOnContextFinalize = nil): TymHTTPAsyncContext;
var
  Request: IHTTPRequest;
begin
  Request := PrepareGet(AURI);
  Result := GetContext(Request, CallbackRecv, CallbackFin);
  Result.FAsyncRes := FHTTPClient.BeginExecute(Result.DataCompleteCallback, Request, AContent, AHeaders);
end;

function TymHTTPClient.GetContext(Request: IHTTPRequest; CallbackRecv: TymOnDataReceive = nil; CallbackFin: TymOnContextFinalize = nil)
  : TymHTTPAsyncContext;
begin
  Result := TymHTTPAsyncContext.Create(FAgent, Request, FNotifyList, FNotifyHandle);
  Result.OnDataReceive := CallbackRecv;
  Result.OnContextFinalize := CallbackFin;
  FContextList.Add(Result);
end;

procedure TymHTTPClient.OnReceive(const Sender: TObject; AContentLength: Int64; AReadCount: Int64; var Abort: Boolean);
var
  Request: IHTTPRequest;
begin
  if Terminated then
  begin
    Abort := true;
    exit;
  end;
  if FNotifyHandle = 0 then exit;

  Request := IHTTPRequest(THTTPRequest(Sender));

  FNotifyList.Push(Request, ntReceive, THTTPClientNotifyReceive.Create(AContentLength, AReadCount));
  PostMessage(FNotifyHandle, WM_YM_ON_RECEIVE, 0, 0);
end;

procedure TymHTTPClient.OnTime;
begin
  CheckNotifies;
end;

function TymHTTPClient.Post(const AURI: String; const ASource: Tstringlist; AHeaders: TNetHeaders = nil): IHTTPResponse;
begin
  Result := FHTTPClient.Post(AURI, ASource);
end;

function TymHTTPClient.Post(const AURI, ASource: String; AContent: TStream; AHeaders: TNetHeaders = nil): IHTTPResponse;
begin
  Result := FHTTPClient.Post(AURI, ASource, AContent);
end;

function TymHTTPClient.PostAsync(const AURI: String; AContent: TStream; ASource: TStream; const MimeType: String;
  AHeaders: TNetHeaders = nil; CallbackRecv: TymOnDataReceive = nil; CallbackFin: TymOnContextFinalize = nil): TymHTTPAsyncContext;
var
  Request: IHTTPRequest;
begin
  Request := PreparePost(AURI, ASource, MimeType);
  Result := GetContext(Request, CallbackRecv, CallbackFin);
  Result.FAsyncRes := FHTTPClient.BeginExecute(Result.DataCompleteCallback, Request, AContent, AHeaders)
end;

function TymHTTPClient.PrepareGet(const AURI: String): IHTTPRequest;
begin
  Result := FHTTPClient.GetRequest(sHTTPMethodGet, AURI);
end;

function TymHTTPClient.PreparePost(const AURI: String; ASource: TStream; const MimeType: String): IHTTPRequest;
begin
  Result := FHTTPClient.GetRequest(sHTTPMethodPost, AURI);
  Result.SourceStream := ASource;
  Result.SourceStream.Position := 0;
  Result.AddHeader('Content-Type', MimeType);
end;

function TymHTTPClientAgent.GetOwner: TymHTTPClient;
begin
  if not Assigned(FOwner) then exit(nil);
  if FOwner.Terminated then exit(nil);
  Result := FOwner;
end;

constructor TymHTTPAsyncContext.Create(AClient: TymHTTPClientAgent; ARequest: IHTTPRequest; ANotifyList: TymHTTPClientNotifyList;
  AHandle: HWND);
begin
  inherited Create;
  FRequest := ARequest;
  FClient := AClient._CopyRef;
  FNotifyList := ANotifyList._CopyRef;
  FNotifyHandle := AHandle;
end;

destructor TymHTTPAsyncContext.Destroy;
begin
  _ReleaseNil(FNotifyList);
  _ReleaseNil(FClient);
  FRequest := nil;
  inherited Destroy;
end;

procedure TymHTTPAsyncContext.Abort;
var
  Client: TymHTTPClient;
begin
  if FAsyncRes = nil then exit;
  if IsDone then exit;
  if FAborted then exit;

  FAborted := true;
  if Assigned(FAsyncRes) then FAsyncRes.Cancel;

  THTTPClient.EndAsyncHTTP(FAsyncRes);
end;

procedure TymHTTPAsyncContext.ContextFinalize(Status: Integer; const Error: String; Response: IHTTPResponse);
begin
  if Assigned(FOnContextFinalize) then
    {}FOnContextFinalize(Status, Error, Response);
end;

procedure TymHTTPAsyncContext.DataCompleteCallback(const ARes: IAsyncResult);
var
  Obj: TObject;
  Response: IHTTPResponse;
begin
  if FNotifyHandle = 0 then exit;

  try
    Response := THTTPClient.EndAsyncHTTP(ARes);
    FNotifyList.Push(FRequest, ntComplete, THTTPClientNotifyComplete.Create(Response));
  except
    on E: ENetHTTPClientException do
    begin
      FNotifyList.Push(FRequest, ntError, THTTPClientNotifyError.Create(E.ToString));
    end;
  end;

  PostMessage(FNotifyHandle, WM_YM_ON_RECEIVE, 0, 0);
end;

procedure TymHTTPAsyncContext.DataReceive(ALen: Int64; ACount: Int64);
begin
  if Assigned(FOnDataReceive) then
    {}FOnDataReceive(ALen, ACount);
end;

procedure TymHTTPAsyncContext.DoTerminate;
begin
  inherited;
  Abort;
end;

function TymHTTPAsyncContext.IsCompleted: Boolean;
begin
  Result := FAsyncRes.IsCompleted;
end;

function TymHTTPAsyncContext.IsDone: Boolean;
begin
  Result := FAsyncRes.IsCompleted or FAsyncRes.IsCancelled;
end;

function TymHTTPAsyncContext.IsFail: Boolean;
begin
  Result := FFail;
end;

function TymHTTPAsyncContext.Wait(Timeout: Cardinal = INFINITE): Boolean;
var
  Time: Cardinal;
begin
  Time := 0;
  while not IsDone do
  begin
    if Timeout <> INFINITE then
    begin
      Inc(Time);
      if Time >= Timeout then
      begin
        Abort;
        exit(false);
      end;
    end;
    sleep(1);
  end;
  Result := true;
end;

constructor TymHTTPClientNotify.Create(ARequest: IHTTPRequest; ANotifyType: TymNotifyType; AData: TymObject);
begin
  inherited Create;
  FNotifyType := ANotifyType;
  FRequest := ARequest;
  FNotifyData := AData;
end;

destructor TymHTTPClientNotify.Destroy;
begin
  FRequest := nil;
  _ReleaseNil(FNotifyData);
  inherited Destroy;
end;

constructor TymHTTPClientNotifyList.Create;
begin
  inherited Create;
  FList := TymObjectList<TymHTTPClientNotify>.Create();
end;

destructor TymHTTPClientNotifyList.Destroy;
begin
  _ReleaseNil(FList);
  inherited Destroy;
end;

function TymHTTPClientNotifyList.Extract(var ANotifyObj: TymHTTPClientNotify): Boolean;
begin
  lock;
  try Result := FList.Extract(ANotifyObj);
  finally unlock;
  end;
end;

procedure TymHTTPClientNotifyList.Push(ARequest: IHTTPRequest; ANotifyType: TymNotifyType; AData: TymObject = nil);
var
  Notify: TymHTTPClientNotify;
begin
  lock;
  try
    Notify := TymHTTPClientNotify.Create(ARequest, ANotifyType, AData);

    FList.AddDirect(Notify);
  finally unlock;
  end;
end;

function TymAsyncContextList.FindByRequest(ARequest: IHTTPRequest): TymHTTPAsyncContext;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    Result := Items[i];
    if Result.FRequest = ARequest then exit;
  end;
  Result := nil;
end;

end.
