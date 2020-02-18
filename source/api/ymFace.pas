unit ymFace;

interface

uses Sysutils;

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

procedure _ReleaseNil(var Obj: TymObject);
procedure _TerminateReleaseNil(var Obj: TymObject);

implementation

procedure _RAISE_YM_OBJECT(message: String = ''); inline;
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
  if FRefCount <> 0 then system.Error(reInvalidPtr);
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

end.
