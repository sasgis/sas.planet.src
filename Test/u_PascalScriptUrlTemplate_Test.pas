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

  FLanguageManager := TLanguageManager.Create('.\lang\');

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
  CUrlTemplate = 'https://{s}.example.com/{z}/{x}/{y}.png';
  CUrlRendered = 'https://n1.example.com/18/200/300.png';
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
    FRequestBuilderConfig.UrlBase := CUrlTemplate;
    FRequestBuilderConfig.ServerNames := 'n1, n1 ';

    VUrl := VUrlTemplate.Render(VRequest);

    Check(VUrl = CUrlRendered, 'Render failed: ' + VUrl);

    //
    FRequestBuilderConfig.UrlBase := 'https://{s}.example.com/p?bbox={bbox}/{q}';
    FRequestBuilderConfig.ServerNames := 'a,b,c,d';
    VUrl := VUrlTemplate.Render(VRequest);
  finally
    VUrlTemplate.Free;
  end;
end;

initialization
  RegisterTest(TestPascalScriptUrlTemplate.Suite);

end.
