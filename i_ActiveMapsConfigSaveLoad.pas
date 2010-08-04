unit i_ActiveMapsConfigSaveLoad;

interface

uses
  i_IActiveMapsConfig;

type
  IActiveMapsConfigSaver = interface
    ['{85DA186A-8D0F-4C82-A64D-B73F51507274}']
    procedure Save(AConfig: IActiveMapWithHybrConfig);
  end;

  IActiveMapsConfigLoader = interface
    ['{84DDCC2C-DDA5-4E42-BA33-E6AAEB6C94F8}']
    procedure Load(AConfig: IActiveMapWithHybrConfig);
  end;

implementation

end.
 