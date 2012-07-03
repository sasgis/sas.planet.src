unit u_ImportSLS;

interface

uses
  i_VectorItmesFactory,
  i_ImportFile,
  i_ImportConfig;

type
  TImportSLS = class(TInterfacedObject, IImportFile)
  private
    FFactory: IVectorItmesFactory;
  private
    function ProcessImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): Boolean;
  public
    constructor Create(
      const AFactory: IVectorItmesFactory
    );
  end;

implementation

uses
  IniFiles,
  SysUtils,
  t_GeoTypes,
  i_MarksSimple,
  i_ConfigDataProvider,
  u_ConfigDataProviderByIniFile,
  i_DoublePointsAggregator,
  u_DoublePointsAggregator;

{ TImportHLG }

constructor TImportSLS.Create(const AFactory: IVectorItmesFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TImportSLS.ProcessImport(
  const AFileName: string;
  const AConfig: IImportConfig
): Boolean;
var
  VIni: TMemIniFile;
  i: integer;
  VPointsAggregator: IDoublePointsAggregator;
  VMark: IMark;
  VPoint: TDoublePoint;
  VSLSData: IConfigDataProvider;
  VSessionSection: IConfigDataProvider;
  VValidPoint: Boolean;
begin
  Result := False;
  VPointsAggregator := TDoublePointsAggregator.Create;
  if AConfig.TemplateNewPoly <> nil then begin
    VIni := TMemIniFile.Create(AFileName);
    try
      VSLSData := TConfigDataProviderByIniFile.Create(VIni);
      VIni := nil;
    finally
      FreeAndNil(VIni);
    end;
    VSessionSection := VSLSData.GetSubItem('Session');
    if VSessionSection <> nil then begin
      i := 1;
      repeat
        VPoint.X := VSessionSection.ReadFloat('LLPointX_' + inttostr(i), -10000);
        VPoint.Y := VSessionSection.ReadFloat('LLPointY_' + inttostr(i), -10000);
        VValidPoint := (Abs(VPoint.X) < 360) and (Abs(VPoint.Y) < 360);
        if VValidPoint then begin
          VPointsAggregator.Add(VPoint);
          inc(i);
        end;
      until not VValidPoint;
    end;
    if VPointsAggregator.Count > 2 then begin
      VMark := AConfig.MarkDB.Factory.CreateNewPoly(
        FFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count),
        ExtractFileName(AFileName),
        '',
        AConfig.TemplateNewPoly
      );
      if VMark <> nil then begin
        AConfig.MarkDB.UpdateMark(nil, VMark);
        Result := True;
      end;
    end;
  end;
end;

end.
