unit i_GlobalDownloadConfig;

interface

uses
  i_ConfigDataElement;

type
  IGlobalDownloadConfig = interface(IConfigDataElement)
    ['{66442801-51C5-43DF-AB59-747E058A3567}']
    // Переходить к следующему тайлу если произошла ошибка закачки
    function GetIsGoNextTileIfDownloadError: Boolean;
    procedure SetIsGoNextTileIfDownloadError(AValue: Boolean);
    property IsGoNextTileIfDownloadError: Boolean read GetIsGoNextTileIfDownloadError write SetIsGoNextTileIfDownloadError;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    function GetIsUseSessionLastSuccess: Boolean;
    procedure SetIsUseSessionLastSuccess(AValue: Boolean);
    property IsUseSessionLastSuccess: Boolean read GetIsUseSessionLastSuccess write SetIsUseSessionLastSuccess;

    //Записывать информацию о тайлах отсутствующих на сервере
    function GetIsSaveTileNotExists: Boolean;
    procedure SetIsSaveTileNotExists(AValue: Boolean);
    property IsSaveTileNotExists: Boolean read GetIsSaveTileNotExists write SetIsSaveTileNotExists;
  end;

implementation

end.
