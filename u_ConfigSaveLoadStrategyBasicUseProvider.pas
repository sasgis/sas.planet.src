unit u_ConfigSaveLoadStrategyBasicUseProvider;

interface

uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IConfigDataElement,
  i_IConfigSaveLoadStrategy;

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
