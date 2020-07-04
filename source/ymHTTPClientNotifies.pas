unit ymHTTPClientNotifies;

interface

uses SysUtils, ymFace, System.Net.HttpClient;

type
  THTTPClientNotifyReceive = class(TymObject)
  private
    FContentLength: Int64;
    FReadCount: Int64;
  public
    constructor Create(AContentLength, AReadCount: Int64);
    property ContentLength: Int64 read FContentLength write FContentLength;
    property ReadCount: Int64 read FReadCount write FReadCount;
  end;

  THTTPClientNotifyComplete = class(TymObject)
  private
    FResponse: IHTTPResponse;
  public
    constructor Create(AResponse: IHTTPResponse);
    property Response: IHTTPResponse read FResponse write FResponse;
  end;

  THTTPClientNotifyError = class(TymObject)
  private
    FMessage: String;
  public
    constructor Create(AMessage: String);
    property Message: String read FMessage write FMessage;
  end;

implementation

constructor THTTPClientNotifyReceive.Create(AContentLength, AReadCount: Int64);
begin
  inherited Create;
  FReadCount := AReadCount;
  FContentLength := AContentLength;
end;

constructor THTTPClientNotifyError.Create(AMessage: String);
begin
  inherited Create;
  FMessage := AMessage;
end;

constructor THTTPClientNotifyComplete.Create(AResponse: IHTTPResponse);
begin
  inherited Create;
  FResponse := AResponse;
end;

end.
