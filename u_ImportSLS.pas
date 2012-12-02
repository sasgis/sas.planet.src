unit u_ImportSLS;

interface

uses
  i_VectorItmesFactory,
  i_ImportFile,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TImportSLS = class(TBaseInterfacedObject, IImportFile)
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
  i_MarksSimple,
  i_ConfigDataProvider,
  i_VectorItemLonLat,
  u_ConfigDataProviderByIniFile,
  u_ConfigProviderHelpers;

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
  VMark: IMark;
  VSLSData: IConfigDataProvider;
  VPolygonSection: IConfigDataProvider;
  VPolygon: ILonLatPolygon;
begin
  Result := False;
  if AConfig.TemplateNewPoly <> nil then begin
    VIni := TMemIniFile.Create(AFileName);
    try
      VSLSData := TConfigDataProviderByIniFile.Create(VIni);
      VIni := nil;
    finally
      FreeAndNil(VIni);
    end;
    VPolygonSection := VSLSData.GetSubItem('Session');
    if VPolygonSection <> nil then begin
      VPolygon := ReadPolygon(VPolygonSection, FFactory);
    end;
    if (VPolygon <> nil) and (VPolygon.Count > 0) then begin
      VMark :=
        AConfig.MarkDB.Factory.CreateNewPoly(
          VPolygon,
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
