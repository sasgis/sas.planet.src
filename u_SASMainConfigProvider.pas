unit u_SASMainConfigProvider;

interface

uses
  IniFiles,
  u_ConfigDataWriteProviderWithGlobal;

type
  TSASMainConfigProvider = class(TConfigDataWriteProviderWithGlobal)
  private
    FMainIni: TMemIniFile;
    function GetMainConfigFileName(ABasePath, AExeFileName: string): string;
  public
    constructor Create(ABasePath, AExeFileName: string; AHandle: THandle);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigDataProviderVirtualWithSubItem,
  u_ConfigDataProviderByResources;

{ TSASMainConfigProvider }

constructor TSASMainConfigProvider.Create(ABasePath, AExeFileName: string; AHandle: THandle);
var
  VResourceProvider: IConfigDataProvider;
  VGlobalProvider: IConfigDataProvider;
  VMainProvider: IConfigDataWriteProvider;
begin
  VResourceProvider := TConfigDataProviderByResources.Create(AHandle);
  VGlobalProvider := TConfigDataProviderVirtualWithSubItem.Create('Resource', VResourceProvider);
  FMainIni := TMeminifile.Create(GetMainConfigFileName(ABasePath, AExeFileName));
  VMainProvider := TConfigDataWriteProviderByIniFile.Create(FMainIni);
  inherited Create(VMainProvider, 'sas:\', VGlobalProvider);
end;

destructor TSASMainConfigProvider.Destroy;
begin
  try
    FMainIni.UpdateFile;
  except
  end;
  inherited;
end;

function TSASMainConfigProvider.GetMainConfigFileName(ABasePath, AExeFileName: string): string;
var
  VPos: Integer;
begin
  VPos := Pos('.', AExeFileName);
  Result := ABasePath + LeftStr(AExeFileName, VPos - 1) + '.ini';
end;

end.
