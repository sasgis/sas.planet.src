unit u_ImportByFileExt;

interface

uses
  i_ImportFile,
  i_KmlInfoSimpleLoader,
  i_ImportConfig;

type
  TImportByFileExt = class(TInterfacedObject, IImportFile)
  private
    FImportPLT: IImportFile;
    FImportKML: IImportFile;
    FImportKMZ: IImportFile;
    FImportHLG: IImportFile;
    FImportMP: IImportFile;
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  public
    constructor Create(
      APltLoader: IKmlInfoSimpleLoader;
      AKmlLoader: IKmlInfoSimpleLoader;
      AKmzLoader: IKmlInfoSimpleLoader
    );
  end;

implementation

uses
  SysUtils,
  u_ImportKML,
  u_ImportHLG,
  u_ImportMpSimple;

{ TImportByFileExt }

constructor TImportByFileExt.Create(
  APltLoader: IKmlInfoSimpleLoader;
  AKmlLoader: IKmlInfoSimpleLoader;
  AKmzLoader: IKmlInfoSimpleLoader
);
begin
  FImportPLT := TImportKML.Create(APltLoader);
  FImportHLG := TImportHLG.Create;
  FImportMP := TImportMpSimple.Create;
  FImportKML := TImportKML.Create(AKmlLoader);
  FImportKMZ := TImportKML.Create(AKmzLoader);
end;

function TImportByFileExt.ProcessImport(AFileName: string;
  AConfig: IImportConfig): Boolean;
begin
  Result := False;
  if (LowerCase(ExtractFileExt(AFileName))='.kml') then begin
    Result := FImportKML.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.kmz') then begin
    Result := FImportKMZ.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.plt') then begin
    Result := FImportPLT.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.hlg') then begin
    Result := FImportHLG.ProcessImport(AFileName, AConfig);
  end else if (LowerCase(ExtractFileExt(AFileName))='.mp') then begin
    Result := FImportMP.ProcessImport(AFileName, AConfig);
  end;
end;

end.
