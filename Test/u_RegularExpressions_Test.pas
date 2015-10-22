unit u_RegularExpressions_Test;

interface

uses
  TestFramework,
  RegExpr;

type
  TestRegularExpressions = class(TTestCase)
  private
    FRegExpr: TRegExpr;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestTileFileNameTMS;
    procedure TestTileFileNameSAS;
    procedure TestGeocoderWikimapia;
    procedure TestSasVersionCheck;
    procedure TestRegExprReplaceMatchSubStrSimple;
  end;


implementation

uses
  SysUtils,
  RegExprUtils;

{ TestRegularExpressions }

procedure TestRegularExpressions.SetUp;
begin
  inherited;
  FRegExpr := TRegExpr.Create;
end;

procedure TestRegularExpressions.TearDown;
begin
  inherited;
  FreeAndNil(FRegExpr);
end;

procedure TestRegularExpressions.TestGeocoderWikimapia;
const
  cFloatRegEx = '[-+]?[0-9]*\.?[0-9]+';
  cSearchResultItemRegEx =
    '<li class="search-result-item".*?' +
    'data-latitude="(' + cFloatRegEx + ')".*?' +
    'data-longitude="(' + cFloatRegEx + ')".*?' +
    '<strong>(.*?)</strong>.*?' +
    '<small>(.*?)?(<.*?)*?</small>.*?' +
    '</li>';

const
  cExample: AnsiString =
'<li class="search-result-item"'#13#10 +
'    data-zoom="11"'#13#10 +
'    data-latitude="53.91"'#13#10 +
'    data-longitude="27.55">'#13#10 +
'    <div>'#13#10 +
'        <span style="color: #9BBDDE;">&bull;</span> <strong>Minsk (Минск, Мінск)</strong>'#13#10 +
'        <span class="label label-info">6508&nbsp;km</span>'#13#10 +
'    </div>'#13#10 +
'    <small>'#13#10 +
'        Minsk, Belarus <span class="small">(city)</span>'#13#10 +
'    </small>'#13#10 +
'</li>';
begin
  FRegExpr.Expression := cSearchResultItemRegEx;

  FRegExpr.ModifierI := True;
  FRegExpr.ModifierM := True;
  CheckTrue(FRegExpr.Exec(cExample));
  CheckEquals('53.91', FRegExpr.Match[1]);
  CheckEquals('27.55', FRegExpr.Match[2]);
  CheckEquals('Minsk (Минск, Мінск)', FRegExpr.Match[3]);
  CheckEquals(#13#10'        Minsk, Belarus ', FRegExpr.Match[4]);
end;

procedure TestRegularExpressions.TestRegExprReplaceMatchSubStrSimple;
const
  cExample1: AnsiString =
    'http://mt.google.com/vt/lyrs=h@169000000&hl=ru';
  cExample2: AnsiString =
    '<test1=100>'#13#10 +
    '<test2=asdf>'#13#10 +
    '<testKey=asdfad>'#13#10 +
    '<testKey=1578>'#13#10;
  cExpr = '<testKey=(\d+)>';
  cRepl = '<testKey=8751>';
var
  VResult: AnsiString;
begin
  VResult := RegExprReplaceMatchSubStr(cExample1, 'mt\.google', 'mt'+inttostr(3)+'.google');
  CheckEquals('http://mt3.google.com/vt/lyrs=h@169000000&hl=ru', VResult);

  VResult := RegExprReplaceMatchSubStr(cExample2, cExpr, cRepl);
  CheckEquals('<test1=100>'#13#10'<test2=asdf>'#13#10'<testKey=asdfad>'#13#10'<testKey=8751>'#13#10, VResult);
end;

procedure TestRegularExpressions.TestSasVersionCheck;
const
  cSearchVersionInfoRegExpr = 'SAS\.Planet\.(Stable|Release|Nightly)\.(\d\d)(\d\d)(\d\d)(\.(\d+))?\.(zip|rar|7z)';
  cSearchAvailableVersionUrlRegExpr = '<a href="(.*?)">' + cSearchVersionInfoRegExpr + '</a>';
const
  cExample: AnsiString =
    'If the download does not start automatically, use the direct link: <a href="http://dl.bintray.com/zed/SASPlanet/SAS.Planet.Nightly.151021.9142.7z">SAS.Planet.Nightly.151021.9142.7z</a>';
begin
  FRegExpr.Expression := cSearchAvailableVersionUrlRegExpr;
  CheckTrue(FRegExpr.Exec(cExample));
  CheckEquals('http://dl.bintray.com/zed/SASPlanet/SAS.Planet.Nightly.151021.9142.7z', FRegExpr.Match[1]);
  CheckEquals('Nightly', FRegExpr.Match[2]);
  CheckEquals('15', FRegExpr.Match[3]);
  CheckEquals('10', FRegExpr.Match[4]);
  CheckEquals('21', FRegExpr.Match[5]);
  CheckEquals('.9142', FRegExpr.Match[6]);
  CheckEquals('9142', FRegExpr.Match[7]);
  CheckEquals('7z', FRegExpr.Match[8]);
end;

procedure TestRegularExpressions.TestTileFileNameTMS;
const
  c_TMS_Expr = '^(.+\\)?(\d\d?)\\(\d+)\\(\d+)(\..+)?$';
const
  cExample: AnsiString = 'cache_tms\sat\10\603\680.jpg';
begin
  FRegExpr.Expression := c_TMS_Expr;
  CheckTrue(FRegExpr.Exec(cExample));
  CheckEquals('cache_tms\sat\', FRegExpr.Match[1]);
  CheckEquals('10', FRegExpr.Match[2]);
  CheckEquals('603', FRegExpr.Match[3]);
  CheckEquals('680', FRegExpr.Match[4]);
  CheckEquals('.jpg', FRegExpr.Match[5]);
end;

procedure TestRegularExpressions.TestTileFileNameSAS;
const
  c_SAS_Expr = '^(.+\\)?[zZ](\d\d?)\\\d+\\[xX](\d+)\\\d+\\[yY](\d+)(\..+)?$';
const
  cExample: AnsiString = 'cache\sat\z11\0\x604\0\y342.jpg';
begin
  FRegExpr.Expression := c_SAS_Expr;
  CheckTrue(FRegExpr.Exec(cExample));
  CheckEquals('cache\sat\', FRegExpr.Match[1]);
  CheckEquals('11', FRegExpr.Match[2]);
  CheckEquals('604', FRegExpr.Match[3]);
  CheckEquals('342', FRegExpr.Match[4]);
  CheckEquals('.jpg', FRegExpr.Match[5]);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestRegularExpressions.Suite);
end.
