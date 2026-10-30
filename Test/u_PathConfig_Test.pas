unit u_PathConfig_Test;

interface

uses
  TestFramework,
  i_PathConfig;

type
  TestIPathConfigWithoutBase = class(TTestCase)
  private
    FPath: IPathConfig;
    procedure CheckSamePath(const A, B: string);
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

  TestIPathConfigEnvVars = class(TTestCase)
  private
    FPath: IPathConfig;
    procedure CheckSamePath(const A, B: string);
  protected
    procedure SetUp; override;
  published
    procedure TestEnvVarWithoutBase;
    procedure TestEnvVarWithBase;
    procedure TestUndefinedEnvVarWithBase;
    procedure TestRawPathIsKept;
    procedure TestEnvVarInBasePath;
  end;

implementation

uses
  ShLwApi,
  SysUtils,
  u_PathConfig;

{ TestIPathConfigWithoutBase }

procedure TestIPathConfigWithoutBase.CheckSamePath(const A, B: string);
begin
  CheckEqualsString(AnsiLowerCase(A), AnsiLowerCase(B));
end;

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
  CheckSamePath(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigWithoutBase.TestEmptyPath;
var
  VFullPath: string;
begin
  VFullPath := ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  FPath.Path := '';
  CheckSamePath(VFullPath, FPath.FullPath);

  FPath.Path := '.';
  CheckSamePath(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigWithoutBase.TestSimpleFullPath;
begin
  FPath.Path := 'C:\Windows';
  CheckSamePath('C:\Windows', FPath.FullPath);
end;

procedure TestIPathConfigWithoutBase.TestSimpleRelativePath;
var
  VFullPath: string;
begin
  VFullPath := ExpandFileName(ParamStr(0));
  FPath.Path := ExtractFileName(VFullPath);
  CheckSamePath(VFullPath, FPath.FullPath);

  FPath.Path := '.\' + ExtractFileName(VFullPath);
  CheckSamePath(VFullPath, FPath.FullPath);
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

{ TestIPathConfigEnvVars }

procedure TestIPathConfigEnvVars.CheckSamePath(const A, B: string);
begin
  CheckEqualsString(AnsiLowerCase(A), AnsiLowerCase(B));
end;

procedure TestIPathConfigEnvVars.SetUp;
var
  VBase: IPathConfig;
begin
  inherited;
  VBase := TPathConfig.Create('', 'C:\Windows\System32', nil);
  FPath := TPathConfig.Create('', '', VBase);
end;

procedure TestIPathConfigEnvVars.TestEnvVarWithoutBase;
var
  VPath: IPathConfig;
  VFullPath: string;
begin
  VFullPath := SysUtils.GetEnvironmentVariable('SystemDrive') + '\SAS.Planet';
  VPath := TPathConfig.Create('', '', nil);
  VPath.Path := '%SystemDrive%\SAS.Planet';
  CheckFalse(VPath.IsRelative);
  CheckSamePath(VFullPath, VPath.FullPath);
end;

procedure TestIPathConfigEnvVars.TestEnvVarWithBase;
var
  VFullPath: string;
begin
  // base path must not be prepended: expanded path is absolute
  VFullPath := SysUtils.GetEnvironmentVariable('SystemDrive') + '\SAS.Planet';
  FPath.Path := '%SystemDrive%\SAS.Planet';
  CheckFalse(FPath.IsRelative);
  CheckSamePath(VFullPath, FPath.FullPath);
end;

procedure TestIPathConfigEnvVars.TestUndefinedEnvVarWithBase;
begin
  // undefined var is left as is -> treated as a relative path under the base
  FPath.Path := '%SAS_PLANET_UNDEFINED_VAR%\Test';
  CheckTrue(FPath.IsRelative);
  CheckEqualsString('C:\Windows\System32\%SAS_PLANET_UNDEFINED_VAR%\Test', FPath.FullPath);
end;

procedure TestIPathConfigEnvVars.TestRawPathIsKept;
begin
  // raw value with env variable is exposed as is (for ini and gui)
  FPath.Path := '%SystemDrive%\SAS.Planet';
  CheckEqualsString('%SystemDrive%\SAS.Planet', FPath.Path);
end;

procedure TestIPathConfigEnvVars.TestEnvVarInBasePath;
var
  VBase: IPathConfig;
  VPath: IPathConfig;
begin
  VBase := TPathConfig.Create('', '%SystemDrive%\Base', nil);
  VPath := TPathConfig.Create('', '.\Sub', VBase);
  CheckSamePath(SysUtils.GetEnvironmentVariable('SystemDrive') + '\Base\Sub', VPath.FullPath);

  // child must be re-resolved when the base raw value changes
  VBase.Path := '%SystemRoot%';
  CheckSamePath(SysUtils.GetEnvironmentVariable('SystemRoot') + '\Sub', VPath.FullPath);
end;

initialization
  RegisterTest(TestIPathConfigWithoutBase.Suite);
  RegisterTest(TestIPathConfigWithBase.Suite);
  RegisterTest(TestIPathConfigEnvVars.Suite);

end.
