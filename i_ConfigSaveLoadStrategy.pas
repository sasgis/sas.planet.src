unit i_ConfigSaveLoadStrategy;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement;

type
  IConfigSaveLoadStrategy = interface
    ['{B56F4565-5D2D-44BE-966C-F7036C02D538}']
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

end.
