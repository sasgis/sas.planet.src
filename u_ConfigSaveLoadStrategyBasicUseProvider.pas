unit u_ConfigSaveLoadStrategyBasicUseProvider;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement,
  i_ConfigSaveLoadStrategy;

type
  TConfigSaveLoadStrategyBasicUseProvider = class(TInterfacedObject, IConfigSaveLoadStrategy)
  protected
    procedure WriteConfig(
      AProvider: IConfigDataWriteProvider;
      AElement: IConfigDataElement
    );
    procedure ReadConfig(
      AProvider: IConfigDataProvider;
      AElement: IConfigDataElement
    );
  end;

implementation

{ TConfigSaveLoadStrategyBasicUseProvider }

procedure TConfigSaveLoadStrategyBasicUseProvider.ReadConfig(
  AProvider: IConfigDataProvider; AElement: IConfigDataElement);
begin
  AElement.ReadConfig(AProvider);
end;

procedure TConfigSaveLoadStrategyBasicUseProvider.WriteConfig(
  AProvider: IConfigDataWriteProvider; AElement: IConfigDataElement);
begin
  AElement.WriteConfig(AProvider);
end;

end.
