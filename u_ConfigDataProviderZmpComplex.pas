unit u_ConfigDataProviderZmpComplex;

interface

uses
  i_IConfigDataProvider,
  u_ConfigDataProviderWithLocal;

type
  TConfigDataProviderZmpComplex = class(TConfigDataProviderWithLocal)
  public
    constructor Create(
      AZmpMapConfig: IConfigDataProvider;
      ALocalMapConfig: IConfigDataProvider
    );
  end;

implementation

uses
  Classes,
  u_ConfigDataProviderWithUseDepreciated,
  u_ConfigDataProviderVirtualWithSubItem,
  u_ConfigDataProviderWithReplacedSubItem;

{ TConfigDataProviderZmpComplex }

constructor TConfigDataProviderZmpComplex.Create(AZmpMapConfig,
  ALocalMapConfig: IConfigDataProvider);
var
  VConfig: IConfigDataProvider;
  VParamsTXT: IConfigDataProvider;
  VParams: IConfigDataProvider;
  VRenamesList: TStringList;
  VLocalMapConfig: IConfigDataProvider;
begin
  VConfig := AZmpMapConfig;
  VParamsTXT := VConfig.GetSubItem('params.txt');
  VParams := VParamsTXT.GetSubItem('PARAMS');

  VRenamesList := TStringList.Create;
  try
    VRenamesList.Values['DefURLBase'] := 'URLBase';
    VRenamesList.Values['DefHotKey'] := 'HotKey';
    VParams := TConfigDataProviderWithUseDepreciated.Create(VParams, VRenamesList);
  finally
    VRenamesList.Free;
  end;
  VParamsTXT := TConfigDataProviderWithReplacedSubItem.Create(VParamsTXT, 'PARAMS', VParams);
  VConfig := TConfigDataProviderWithReplacedSubItem.Create(VConfig, 'params.txt', VParamsTXT);

  VLocalMapConfig :=
    TConfigDataProviderVirtualWithSubItem.Create(
      'params.txt',
      TConfigDataProviderVirtualWithSubItem.Create(
        'PARAMS',
        ALocalMapConfig
      )
    );

  inherited Create(VConfig, VLocalMapConfig);
end;

end.
