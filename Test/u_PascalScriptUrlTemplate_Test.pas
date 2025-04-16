unit u_PascalScriptUrlTemplate_Test;

interface

uses
  TestFramework,
  i_LanguageManager,
  i_ProjectionSet,
  i_TileDownloadRequestBuilderConfig;

type
  TestPascalScriptUrlTemplate = class(TTestCase)
  private
    FLanguageManager: ILanguageManager;
    FProjectionSet: IProjectionSet;
    FRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
  protected
    procedure SetUp; override;
  published
    procedure TestUrlTemplate;
  end;

implementation

uses
  Types,
  RegularExpressions,
  c_CoordConverter,
  i_SimpleFlag,
  i_TileRequest,
  i_HashFunction,
  i_HashFunctionImpl,
  i_DatumFactory,
  i_ProjectionSetFactory,
  u_TileRequest,
  u_LanguageManager,
  u_DatumFactory,
  u_HashFunctionByImpl,
  u_HashFunctionCRC64,
  u_ProjectionSetFactorySimple,
  u_TileDownloadRequestBuilderConfig,
  u_PascalScriptUrlTemplate;

{ TestPascalScriptUrlTemplate }

procedure TestPascalScriptUrlTemplate.SetUp;
var
  VHashFunction: IHashFunction;
  VDatumFactory: IDatumFactory;
  VProjectionSetFactory: IProjectionSetFactory;
begin
  inherited;

  if FLanguageManager = nil then begin
    FLanguageManager := TLanguageManager.Create('.\lang\');
  end;

  VHashFunction := THashFunctionByImpl.Create(THashFunctionCRC64.Create as IHashFunctionImpl);
  VDatumFactory := TDatumFactory.Create(VHashFunction);
  VProjectionSetFactory := TProjectionSetFactorySimple.Create(VHashFunction, VDatumFactory);

  FProjectionSet := VProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);

  FRequestBuilderConfig :=
    TTileDownloadRequestBuilderConfig.Create(
      TTileDownloadRequestBuilderConfigStatic.Create(
        '', '', '', False, ''
      ) as ITileDownloadRequestBuilderConfigStatic
    );
end;

procedure TestPascalScriptUrlTemplate.TestUrlTemplate;
const
  CZoom = 18;
  CTile: TPoint = (X: 200; Y: 300);
var
  VUrl: string;
  VRequest: ITileRequest;
  VUrlTemplate: TPascalScriptUrlTemplate;
begin
  VRequest := TTileRequest.Create(CTile, CZoom, nil);

  VUrlTemplate :=
    TPascalScriptUrlTemplate.Create(
      FLanguageManager,
      FProjectionSet,
      FRequestBuilderConfig
    );
  try
    FRequestBuilderConfig.UrlBase := 'http://{s}.example.com/{z}/{x}/{y}.png';
    FRequestBuilderConfig.ServerNames := 'n1, n2 ';
    VUrl := VUrlTemplate.Render(VRequest);
    Check(TRegEx.IsMatch(VUrl, 'http://(n1|n2)\.example\.com/18/200/300\.png'), VUrl);

    FRequestBuilderConfig.UrlBase := 'http://{a,b,c,d}.example.com/{z}/{x}/{y}.png';
    FRequestBuilderConfig.ServerNames := '';
    VUrl := VUrlTemplate.Render(VRequest);
    Check(TRegEx.IsMatch(VUrl, 'http://(a|b|c|d)\.example\.com/18/200/300\.png'), VUrl);

    FRequestBuilderConfig.UrlBase := 'http://example.com/{z-1}/{x*2}/{y/2}.png';
    FRequestBuilderConfig.ServerNames := '';
    VUrl := VUrlTemplate.Render(VRequest);
    Check(VUrl = 'http://example.com/17/400/150.png', VUrl);

    FRequestBuilderConfig.UrlBase := 'http://example.com/{z-1}/{x*2}/{y/2}.png';
    FRequestBuilderConfig.ServerNames := '';
    VUrl := VUrlTemplate.Render(TTileRequest.Create(CTile, CZoom+1, nil));
    Check(VUrl = 'http://example.com/18/400/150.png', VUrl);

    FRequestBuilderConfig.UrlBase := 'http://example.com/{q}';
    VUrl := VUrlTemplate.Render(VRequest);
    Check(VUrl = 'http://example.com/000000000211203200', VUrl);

    FRequestBuilderConfig.UrlBase := 'http://example.com/p?bbox={bbox}';
    FRequestBuilderConfig.ServerNames := '';
    VUrl := VUrlTemplate.Render(VRequest);
    Check(VUrl = 'http://example.com/p?bbox=-20006933.53147520,19991493.25176160,-20006780.65741860,19991646.12581810', VUrl);

    FRequestBuilderConfig.UrlBase := 'http://example.com/{sas_path}.png';
    VUrl := VUrlTemplate.Render(VRequest);
    Check(VUrl = 'http://example.com/z19/0/x200/0/y300.png', VUrl);

    FRequestBuilderConfig.UrlBase := 'http://example.com/z{z+1}/{x/1024}/x{x}/{y/1024}/y{y}.png';
    VUrl := VUrlTemplate.Render(VRequest);
    Check(VUrl = 'http://example.com/z19/0/x200/0/y300.png', VUrl);
  finally
    VUrlTemplate.Free;
  end;
end;

initialization
  RegisterTest(TestPascalScriptUrlTemplate.Suite);

end.
