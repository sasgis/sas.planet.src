unit u_ImportSLS;

interface

uses
  Classes,
  i_VectorItemsFactory,
  i_ImportFile,
  i_ImportConfig,
  u_BaseInterfacedObject;

type
  TImportSLS = class(TBaseInterfacedObject, IImportFile)
  private
    FFactory: IVectorItemsFactory;
  private
    function ProcessImport(
      const AFileName: string;
      const AConfig: IImportConfig
    ): IInterfaceList;
  public
    constructor Create(
      const AFactory: IVectorItemsFactory
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

constructor TImportSLS.Create(const AFactory: IVectorItemsFactory);
begin
  inherited Create;
  FFactory := AFactory;
end;

function TImportSLS.ProcessImport(
  const AFileName: string;
  const AConfig: IImportConfig
): IInterfaceList;
var
  VIni: TMemIniFile;
  VMark: IMark;
  VSLSData: IConfigDataProvider;
  VPolygonSection: IConfigDataProvider;
  VPolygon: ILonLatPolygon;
begin
  Result := nil;
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
        VMark := AConfig.MarkDB.UpdateMark(nil, VMark);
        if VMark <> nil then begin
          Result := TInterfaceList.Create;
          Result.Add(VMark);
        end;
      end;
    end;
  end;
end;

end.
