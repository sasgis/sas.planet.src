unit u_ImportByFileExt;

interface

uses
  i_IImportFile,
  i_IImportConfig;

type
  TImportByFileExt = class(TInterfacedObject, IImportFile)
  private
    FImportPLT: IImportFile;
    FImportKML: IImportFile;
    FImportKMZ: IImportFile;
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  u_ImportKML,
  u_ImportPLT,
  u_GlobalState;

{ TImportByFileExt }

constructor TImportByFileExt.Create;
begin
  FImportPLT := TImportPLT.Create;
  FImportKML := TImportKML.Create(GState.KmlLoader);
  FImportKMZ := TImportKML.Create(GState.KmzLoader);
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
  end;
end;

end.
