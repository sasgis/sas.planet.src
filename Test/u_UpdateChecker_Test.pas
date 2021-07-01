unit u_UpdateChecker_Test;

interface

uses
  TestFramework,
  t_UpdateChecker,
  i_DownloadResult;

type
  TestUpdateChecker = class(TTestCase)
  private
    FResult: TUpdateCheckerResult;
    FUpdateSource: TUpdateSource;
    FUpdateChannel: TUpdateChannel;
    function GetFakeResponse: IDownloadResultOk;
    procedure ParseResponse;
  published
    procedure ParseHomeResponse;
    procedure ParseBitBucketResponse;
    procedure ParseGitHubResponse;
  end;

implementation

uses
  SysUtils,
  i_BinaryData,
  u_BinaryData,
  u_DownloadResult,
  u_UpdateCheckerFunc;

{ TestUpdateCheckParser }

procedure TestUpdateChecker.ParseResponse;
var
  VDownloadResult: IDownloadResultOk;
begin
  VDownloadResult := GetFakeResponse;

  FResult :=
    TUpdateCheckerFunc.ParseDownloadResult(
      FUpdateChannel,
      FUpdateSource,
      VDownloadResult
    );
end;

procedure TestUpdateChecker.ParseHomeResponse;
begin
  FUpdateSource := usHome;

  FUpdateChannel := ucNightly;
  ParseResponse;
  Check(FResult.IsFound = True);
  Check(FResult.OutFileName = 'SAS.Planet.Nightly.210616.10132.7z');

  FUpdateChannel := ucRelease;
  ParseResponse;
  Check(FResult.IsFound = True);
  Check(FResult.OutFileName = 'SAS.Planet.Release.201212.zip');
end;

procedure TestUpdateChecker.ParseBitBucketResponse;
begin
  FUpdateSource := usBitBucket;

  FUpdateChannel := ucNightly;
  ParseResponse;
  Check(FResult.IsFound = True);
  Check(FResult.OutFileName = 'SAS.Planet.Nightly.210616.10132.7z');

  FUpdateChannel := ucRelease;
  ParseResponse;
  Check(FResult.IsFound = True);
  Check(FResult.OutFileName = 'SAS.Planet.Release.201212.zip');
end;

procedure TestUpdateChecker.ParseGitHubResponse;
begin
  FUpdateSource := usGitHub;

  FUpdateChannel := ucNightly;
  ParseResponse;
  Check(FResult.IsFound = False);

  FUpdateChannel := ucRelease;
  ParseResponse;
  Check(FResult.IsFound = True);
  Check(FResult.OutFileName = 'SAS.Planet.Release.201212.zip');
end;

function TestUpdateChecker.GetFakeResponse: IDownloadResultOk;
const
  CHomeFakeHtml: array [TUpdateChannel] of AnsiString = (
    '<a href="https://bitbucket.org/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.210616.10132.7z">SAS.Planet.Nightly.210616.10132.7z</a>',
    '<a href="https://bitbucket.org/sas_team/sas.planet.bin/downloads/SAS.Planet.Release.201212.zip">SAS.Planet.Release.201212.zip</a>'
  );

  CGitHubFakeHtml: AnsiString =
    '<a href="/sasgis/sas.planet.src/releases/download/v.200606/SAS.Planet.Release.200606.zip" rel="nofollow" class="d-flex flex-items-center min-width-0">' + #10 +
    '<a href="/sasgis/sas.planet.src/releases/download/v.201212/SAS.Planet.Release.201212.zip" rel="nofollow" class="d-flex flex-items-center min-width-0">';

  CBitBucketFakeHtml: AnsiString =
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.200504.128.7z">SAS.Planet.Nightly.210504.10128.7z</a>' + #10 +
    '</td>' + #10 +
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.210616.10132.7z">SAS.Planet.Nightly.210616.10132.7z</a>' + #10 +
    '</td>' + #10 +
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.210504.10128.7z">SAS.Planet.Nightly.210504.10128.7z</a>' + #10 +
    '</td>' + #10 +
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Release.201212.zip">SAS.Planet.Release.201212.zip</a>' + #10 +
    '</td>' + #10 +
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Release.200606.zip">SAS.Planet.Release.200606.zip</a>' + #10 +
    '</td>' + #10;
var
  VData: IBinaryData;
begin
  case FUpdateSource of
    usHome:      VData := TBinaryData.CreateByAnsiString(CHomeFakeHtml[FUpdateChannel]);
    usBitBucket: VData := TBinaryData.CreateByAnsiString(CBitBucketFakeHtml);
    usGitHub:    VData := TBinaryData.CreateByAnsiString(CGitHubFakeHtml);
  else
    Assert(False);
  end;

  Result := TDownloadResultOk.Create(nil, 200, '', '', VData);
end;

initialization
  RegisterTest(TestUpdateChecker.Suite);

end.
