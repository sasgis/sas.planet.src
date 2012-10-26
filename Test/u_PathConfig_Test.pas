unit u_PathConfig_Test;

{

  Delphi DUnit Test Case
  ----------------------
}

interface

uses
  TestFramework,
  i_PathConfig;

type
  TestIPathConfigWithoutBase = class(TTestCase)
  private
    FPath: IPathConfig;
  protected
    procedure SetUp; override;
  published
    procedure TestSimpleFullPath;
    procedure TestSimpleRelativePath;
    procedure TestAdvRelativePath;
    procedure TestEmptyPath;
  end;

  TestIPathConfigWithBase = class(TTestCase)
  private
    FPath: IPathConfig;
  protected
    procedure SetUp; override;
  published
    procedure TestSimpleFullPath;
    procedure TestSimpleRelativePath;
    procedure TestEmptyPath;
    procedure TestAdvRelativePath;
    procedure TestChangeBasePathForFull;
    procedure TestChangeBasePathForRelative;
  end;

implementation

uses
  ShLwApi,
  SysUtils,
  u_PathConfig;

{ TestIPathConfigWithoutBase }

procedure TestIPathConfigWithoutBase.SetUp;
begin
  inherited;
  FPath := TPathConfig.Create('', '', nil);
end;

procedure TestIPathConfigWithoutBase.TestAdvRelativePath;
var
  VFullPath: string;
  VDir: string;
begin
  VFullPath := ExpandFileName(ParamStr(0));
  VDir := IncludeTrailingPathDelimiter(ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(VFullPath))));
  if PathIsRoot(PChar(VDir)) then begin
    Fail('Put exe to folder');
  end;
  VFullPath := VDir + 'Test.txt';
  FPath.Path := '..\Test.txt';
  CheckEqualsString(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigWithoutBase.TestEmptyPath;
var
  VFullPath: string;
begin
  VFullPath := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  FPath.Path := '';
  CheckEqualsString(VFullPath, FPath.FullPath);

  FPath.Path := '.';
  CheckEqualsString(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigWithoutBase.TestSimpleFullPath;
begin
  FPath.Path := 'C:\Windows';
  CheckEqualsString('C:\Windows', FPath.FullPath);
end;

procedure TestIPathConfigWithoutBase.TestSimpleRelativePath;
var
  VFullPath: string;
begin
  VFullPath := ExpandFileName(ParamStr(0));
  FPath.Path := ExtractFileName(VFullPath);
  CheckEqualsString(VFullPath, FPath.FullPath);

  FPath.Path := '.\' + ExtractFileName(VFullPath);
  CheckEqualsString(VFullPath, FPath.FullPath);
end;

{ TestIPathConfigWithBase }

procedure TestIPathConfigWithBase.SetUp;
var
  VBase: IPathConfig;
begin
  inherited;
  VBase := TPathConfig.Create('', 'C:\Windows\System32', nil);
  FPath := TPathConfig.Create('', '', VBase);
end;

procedure TestIPathConfigWithBase.TestAdvRelativePath;
var
  VFullPath: string;
begin
  VFullPath := 'C:\Windows\Test.txt';
  FPath.Path := '..\Test.txt';
  CheckEqualsString(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigWithBase.TestChangeBasePathForFull;
begin
  FPath.Path := 'C:\Wind';
  FPath.BasePathConfig.Path := 'D:\';
  CheckEqualsString('C:\Wind', FPath.FullPath);

  FPath.Path := 'D:\Windows';
  FPath.BasePathConfig.Path := 'C:\';
  CheckEqualsString('D:\Windows', FPath.FullPath);
end;

procedure TestIPathConfigWithBase.TestChangeBasePathForRelative;
var
  VFullPath: string;
begin
  VFullPath := 'D:\Test.txt';
  FPath.Path := '.\Test.txt';
  FPath.BasePathConfig.Path := 'D:\';
  CheckEqualsString(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigWithBase.TestEmptyPath;
var
  VFullPath: string;
begin
  VFullPath := FPath.BasePathConfig.FullPath;
  FPath.Path := '.';
  CheckEqualsString(VFullPath, FPath.FullPath);
  FPath.Path := '';
  CheckEqualsString(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigWithBase.TestSimpleFullPath;
begin
  FPath.Path := 'C:\Windows';
  CheckEqualsString('C:\Windows', FPath.FullPath);

  FPath.Path := 'D:\Windows';
  CheckEqualsString('D:\Windows', FPath.FullPath);
end;

procedure TestIPathConfigWithBase.TestSimpleRelativePath;
var
  VFullPath: string;
begin
  VFullPath := IncludeTrailingPathDelimiter(FPath.BasePathConfig.FullPath) + 'Test.txt';
  FPath.Path := 'Test.txt';
  CheckEqualsString(VFullPath, FPath.FullPath);

  FPath.Path := '.\Test.txt';
  CheckEqualsString(VFullPath, FPath.FullPath);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestIPathConfigWithoutBase.Suite);
  RegisterTest(TestIPathConfigWithBase.Suite);
end.
