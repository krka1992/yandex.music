unit test.obj;

interface

uses sysutils, TestFramework, ymFace;

type

  TObjTest = class(TTestCase)
  published
    procedure TestObject;
  end;

implementation

procedure TObjTest.TestObject;
var
  ymObject, ymObjectCopy: TymObject;
begin
  ymObject := TymObject.Create;
  try
    ymObjectCopy := ymObject._CopyRef;
    CheckEquals(2, ymObject.RefCount);
    CheckEquals(2, ymObjectCopy.RefCount);
  finally
    _ReleaseNil(ymObjectCopy);
    CheckEquals(1, ymObject.RefCount);
    _ReleaseNil(ymObject);
    CheckTrue(ymObject = nil);
    CheckTrue(ymObjectCopy = nil);
  end;
end;

begin
  RegisterTest(TObjTest.Suite);

end.
