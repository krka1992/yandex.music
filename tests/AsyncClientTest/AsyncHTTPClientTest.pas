unit AsyncHTTPClientTest;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, ymHTTPClient, ymFace,
  Vcl.AppEvnts, Vcl.ExtCtrls, Vcl.ComCtrls, System.Net.HttpClient;

type
  TForm1 = class(TForm)
    EditURL: TEdit;
    ButtonSend: TButton;
    ApplicationEvents1: TApplicationEvents;
    ProgressBar1: TProgressBar;
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure ButtonSendClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FHTTPClient: TymHTTPClient;
    FContext: TymHTTPAsyncContext;
    FStream: TMemoryStream;
    procedure OnFinalizeData(Code: Integer; const Error: string; Response: IHTTPResponse);
    procedure OnReceiveData(ALen: Int64; ACount: Int64);
  public
    {Public declarations}
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
begin
  //
  case Msg.message of
    WM_YM_ON_RECEIVE: FHTTPClient.CheckNotifies;
  end;
end;

procedure TForm1.ButtonSendClick(Sender: TObject);
begin
  FContext := FHTTPClient.GetAsync(EditURL.Text, FStream, nil, OnReceiveData, OnFinalizeData);
  ButtonSend.Enabled := false;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  _ReleaseNil(FContext);
  _ReleaseNil(FHTTPClient);
  FStream.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FHTTPClient := TymHTTPClient.Create(Handle);
  FStream := TMemoryStream.Create;
end;

procedure TForm1.OnFinalizeData(Code: Integer; const Error: string; Response: IHTTPResponse);
begin
  case Code of
    - 1: MessageDlg(Format('Запрос завершился с ошибкой, [%s]', [Error]), mtError, [mbOK], 0);
    0: MessageDlg(Format('Запрос успешно завершен [CODE: %d] [STATUS: %s]', [Response.StatusCode, Response.StatusText]), mtInformation,
        [mbOK], 0);
  end;
  _ReleaseNil(FContext);
  ButtonSend.Enabled := true;
end;

procedure TForm1.OnReceiveData(ALen: Int64; ACount: Int64);
begin
  ProgressBar1.Max := ALen;
  ProgressBar1.Position := ACount;
end;

end.
