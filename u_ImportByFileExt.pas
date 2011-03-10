unit u_ImportByFileExt;

interface

uses
  i_IImportFile,
  i_IKmlInfoSimpleLoader,
  i_IImportConfig;

type
  TImportByFileExt = class(TInterfacedObject, IImportFile)
  private
    FImportPLT: IImportFile;
    FImportKML: IImportFile;
    FImportKMZ: IImportFile;
    FImportHLG: IImportFile;
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  public
    constructor Create(AKmlLoader: IKmlInfoSimpleLoader; AKmzLoader: IKmlInfoSimpleLoader);
  end;

implementation

uses
  SysUtils,
  u_ImportKML,
  u_ImportHLG,
  u_ImportPLT;

{ TImportByFileExt }

constructor TImportByFileExt.Create(AKmlLoader: IKmlInfoSimpleLoader; AKmzLoader: IKmlInfoSimpleLoader);
begin
  FImportPLT := TImportPLT.Create;
  FImportHLG := TImportHLG.Create;
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
  end;
end;

end.
