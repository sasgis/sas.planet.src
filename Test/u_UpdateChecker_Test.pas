unit u_UpdateChecker_Test;

interface

uses
  TestFramework,
  t_UpdateChecker,
  i_DownloadResult;

type
  TestUpdateChecker = class(TTestCase)
  private
    function GetFakeResponse(
      const AUpdateSource: TUpdateSource;
      const AUpdateChannel: TUpdateChannel
    ): IDownloadResultOk;

    function ParseResponse(
      const AUpdateSource: TUpdateSource;
      const AUpdateChannel: TUpdateChannel
    ): TUpdateCheckerResult;
  published
    procedure ParseSasGisResponse;
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

const
  CReleaseFileName = 'SAS.Planet.Release.201212' + {$IFDEF WIN64} '.x64' + {$ENDIF} '.zip';
  CNightlyFileName = 'SAS.Planet.Nightly.210616.10132' + {$IFDEF WIN64} '.x64' + {$ENDIF} '.7z';

{ TestUpdateCheckParser }

function TestUpdateChecker.ParseResponse(
  const AUpdateSource: TUpdateSource;
  const AUpdateChannel: TUpdateChannel
): TUpdateCheckerResult;
begin
  Result :=
    TUpdateCheckerFunc.ParseDownloadResult(
      AUpdateChannel,
      AUpdateSource,
      Self.GetFakeResponse(AUpdateSource, AUpdateChannel)
    );
end;

procedure TestUpdateChecker.ParseSasGisResponse;
var
  VResult: TUpdateCheckerResult;
begin
  VResult := ParseResponse(usSasGis, ucNightly);
  CheckTrue(VResult.IsFound, 'Nightly');
  CheckEquals(VResult.OutFileName, CNightlyFileName, 'Nightly');

  VResult := ParseResponse(usSasGis, ucRelease);
  CheckTrue(VResult.IsFound, 'Release');
  CheckEquals(VResult.OutFileName, CReleaseFileName, 'Release');
end;

procedure TestUpdateChecker.ParseBitBucketResponse;
var
  VResult: TUpdateCheckerResult;
begin
  VResult := ParseResponse(usBitBucket, ucNightly);
  CheckTrue(VResult.IsFound);
  CheckEquals(VResult.OutFileName, CNightlyFileName);

  VResult := ParseResponse(usBitBucket, ucRelease);
  CheckTrue(VResult.IsFound);
  CheckEquals(VResult.OutFileName, CReleaseFileName);
end;

procedure TestUpdateChecker.ParseGitHubResponse;
var
  VResult: TUpdateCheckerResult;
begin
  VResult := ParseResponse(usGitHub, ucNightly);
  CheckTrue(VResult.IsFound);
  CheckEquals(VResult.OutFileName, CNightlyFileName);

  VResult := ParseResponse(usGitHub, ucRelease);
  CheckTrue(VResult.IsFound);
  CheckEquals(VResult.OutFileName, CReleaseFileName);
end;

function TestUpdateChecker.GetFakeResponse(
  const AUpdateSource: TUpdateSource;
  const AUpdateChannel: TUpdateChannel
): IDownloadResultOk;
const
  CSasGisFakeHtml: array [TUpdateChannel] of AnsiString = (
    '<a href="https://bitbucket.org/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.210616.10132.x64.7z">SAS.Planet.Nightly.210616.10132.x64.7z</a>' +
    '<a href="https://bitbucket.org/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.210616.10132.7z">SAS.Planet.Nightly.210616.10132.7z</a>',

    '<a href="https://bitbucket.org/sas_team/sas.planet.bin/downloads/SAS.Planet.Release.201212.x64.zip">SAS.Planet.Release.201212.x64.zip</a>' +
    '<a href="https://bitbucket.org/sas_team/sas.planet.bin/downloads/SAS.Planet.Release.201212.zip">SAS.Planet.Release.201212.zip</a>'
  );

  CGitHubFakeHtml: array [TUpdateChannel] of AnsiString = (
    '"browser_download_url": "https://github.com/sasgis/sas.planet.src/releases/download/nightly/SAS.Planet.Nightly.210616.10132.7z"' + #10 +
    '"browser_download_url": "https://github.com/sasgis/sas.planet.src/releases/download/nightly/SAS.Planet.Nightly.210616.10132.x64.7z"' + #10,

    '"browser_download_url": "https://github.com/sasgis/sas.planet.src/releases/download/v.201212/SAS.Planet.Release.201212.zip"' + #10 +
    '"browser_download_url": "https://github.com/sasgis/sas.planet.src/releases/download/v.201212/SAS.Planet.Release.201212.x64.zip"' + #10
  );

  CBitBucketFakeHtml: AnsiString =
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.200504.128.7z">SAS.Planet.Nightly.210504.10128.7z</a>' + #10 +
    '</td>' + #10 +
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Nightly.210616.10132.x64.7z">SAS.Planet.Nightly.210616.10132.x64.7z</a>' + #10 +
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
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Release.201212.x64.zip">SAS.Planet.Release.201212.x64.zip</a>' + #10 +
    '</td>' + #10 +
    '<td class="name">' + #10 +
    '  <a class="execute" rel="nofollow"' + #10 +
    '    href="/sas_team/sas.planet.bin/downloads/SAS.Planet.Release.200606.zip">SAS.Planet.Release.200606.zip</a>' + #10 +
    '</td>' + #10;
var
  VStr: AnsiString;
  VData: IBinaryData;
begin
  case AUpdateSource of
    usSasGis:    VStr := CSasGisFakeHtml[AUpdateChannel];
    usBitBucket: VStr := CBitBucketFakeHtml;
    usGitHub:    VStr := CGitHubFakeHtml[AUpdateChannel];
  else
    raise Exception.CreateFmt(
      'Unexpected UpdateSource value: %d', [Integer(AUpdateSource)]
    );
  end;

  VData := TBinaryData.CreateByAnsiString(VStr);
  Result := TDownloadResultOk.Create(nil, 200, '', '', VData);
end;

initialization
  RegisterTest(TestUpdateChecker.Suite);

end.
