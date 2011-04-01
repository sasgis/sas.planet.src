unit u_ResStrings;

interface

ResourceString
  SAS_MSG_need_reload_application_curln = 'Для того чтобы изменения вступили в силу'#13#10 +
    'необходимо перезапустить программу.';
  SAS_MSG_coution = 'Внимание!';
  SAS_MSG_youasure = 'Подтвердите факт удаления';
  SAS_MSG_youasurerefrsensor = 'Вы действительно хотите обнулить датчик?';
  SAS_MSG_SelectArea = 'Часть выделенной области находится вне карты.'#13#10 +
    'Область будет ограничена по размерам карты.';
  SAS_MSG_FileExists = 'Файл %0:s уже есть у вас в кэше.'#13#10 +
    'Заменить этот файл вновь скачанным?';
  SAS_MSG_NeedHL = 'Сначала необходимо выделить хотябы один раз область!';
  SAS_MSG_FunExForPoly = 'В данной версии функция доступна только для полигонов';
  SAS_MSG_FileBeCreateTime = 'Данный файл создан позже указаного срока, пропускаем загрузку.';
  SAS_MSG_FileBeCreateLen = 'Данный файл равен по размеру существующему, пропускаем загрузку.';
  SAS_MSG_ProcessFilesComplete = 'Обработка файлов завершена!';
  SAS_MSG_LoadComplete = 'Загрузка завершена';
  SAS_MSG_NoGPSdetected = 'Не найден GPS приемник';
  SAS_MSG_GarminMax1Mp = 'Количество полученных jpeg-файлов превысит 100 штук, если ваш'#13#10 +
    'навигатор не отображает карту разбейте сохраняемое изображение на части в '#13#10 +
    'диалоге "Операции с выделенной областью\склеить"'#13#10 +
    'и используйте полученные kmz по отдельности';
  SAS_MSG_NotDelWhereHasChildren = 'Категория не пуста! Удалите все дочерние категории.';
  SAS_MSG_HotKeyExists = 'Горячая клавиша уже используется, пожалуйста, выберите другую';
  SAS_MSG_CantGetTileFileName = 'Это не тайловый кэш, невозможно получить имя файла с тайлом.';

  SAS_ERR_Nopoints = 'Необходимый набор точек отсутствует!';
  SAS_ERR_ProxyStrFormat = 'Неверный формат записи прокси-сервера';
  SAS_ERR_Write = 'Ошибка записи на диск. Операция будет прервана!';
  SAS_ERR_Save = 'Ошибка при сохранении!';
  SAS_ERR_code = 'Код ошибки:';
  SAS_ERR_Read = 'Ошибка чтения файла!';
  SAS_ERR_SelectArea = 'Выделенная область находится целиком вне карты!';
  SAS_ERR_NoMaps = 'Без карт программа не будет работать!';
  SAS_ERR_BadFile = 'Файл испорчен!';
  SAS_ERR_FileNotFound = 'Файл не найден';
  SAS_ERR_PortOpen = 'Ошибка открытия порта!';
  SAS_ERR_Communication = 'Ошибка коммуникации';
  SAS_ERR_UnablePposition = 'Невозможно определить позицию';
  SAS_ERR_ParamsInput = 'Ошибка ввода параметров!';
  SAS_ERR_LonLat1 = 'Долгота в левом верхнем углу должна быть меньше '#13#10 +
    'долготы в правом нижнем!';
  SAS_ERR_LonLat2 = 'Широта в левом верхнем углу должна быть меньше '#13#10 +
    'широты в правом нижнем!';
  SAS_ERR_CreateArh = 'Ошибка создания архива!';
  SAS_ERR_NotLoads = 'Карта не позволяет загружать';
  SAS_ERR_Authorization = 'Ошибка авторизации на прокси!';
  SAS_ERR_WaitTime = 'Ждем %0:d секунд...';
  SAS_ERR_Ban = 'Высока вероятность того, что вас забанили!';
  SAS_ERR_TileNotExists = 'Такого изображения нет на сервере!';
  SAS_ERR_Noconnectionstointernet = 'Отсутствует подключение к интернет!';
  SAS_ERR_RepeatProcess = 'Пытаемся повторить обработку';
  SAS_ERR_FileExistsShort = 'Данный файл уже имеется в кэше';
  SAS_ERR_Memory = 'Невозможно выделить память для данной операции';
  SAS_ERR_UseADifferentFormat = 'Для склейки больших размеров используйте другой формат (ecw,bmp,jp2)';
  SAS_ERR_BadMIMEForDownloadRastr  =  'Сервис вернул тип "%0:s", это не растровое изображение';
  SAS_ERR_BadMIME  =  'Сервис вернул тип "%0:s", а не один из ожидаемых';
  SAS_ERR_MapGUIDEmpty = 'Пустой GUID';
  SAS_ERR_MapGUIDBad = 'GUID %0:s не соответствует формату';
  SAS_ERR_MapGUIDError = 'В карте %0:s ошибка: %1:s';
  SAS_ERR_MapGUIDDuplicate = 'В файлах %0:s и %1:s одинаковые GUID';
  SAS_ERR_MainMapNotExists = 'Среди ZMP должна быть хотя бы одна карта';
  SAS_ERR_CategoryNameDoubling = 'Такое имя категории уже существует';
  SAS_ERR_UrlScriptError = 'Ошибка скрипта карты %0:s (%2:s):'#13#10'%1:s';
  SAS_ERR_UrlScriptUnexpectedError = 'Ошибка скрипта карты %0:s (%2:s):'#13#10'Неожиданная ошибка';
  SAS_ERR_UrlScriptCompileError = 'Ошибка в скрипте при компиляции'#13#10'%0:s';
  SAS_ERR_UrlScriptByteCodeLoad = 'Ошибка при загрузке байткода';
  SAS_ERR_MapProjectionUnexpectedType = 'Ошибочный тип проэкции карты %0:s';
  SAS_ERR_MapDownloadByError = 'Для карты %0:s отключена загрузка тайлов из-за ошибки: %1:s';
  SAS_ERR_TileDownloadContentTypeUnexpcted = 'Ошибочный тип данных';
  SAS_ERR_TileDownloadUnexpectedError = 'Неизвестная ошибка при скачивании';
  SAS_ERR_CoordinatesInput='Ошибка ввода координат';

  SAS_STR_MarshLen = 'Длина маршрута: ';
  SAS_STR_Marshtime = 'Время в пути: ';
  SAS_STR_coordinates = 'Координаты';
  SAS_STR_time = 'Время';
  SAS_STR_load = 'Скачано';
  SAS_STR_Scale = 'Масштаб';
  SAS_STR_Speed = 'Скорость';
  SAS_STR_LenPath = 'Длина пути';
  SAS_STR_LenToMark = 'Расстояние до метки';
  SAS_STR_filesnum = 'Количество файлов';
  SAS_STR_activescale = 'Активный масштаб';
  SAS_STR_for = 'Для';
  SAS_STR_savetreck = 'Сохранение трека...';
  SAS_STR_loadhl = 'Загрузка выделения...';
  SAS_STR_notfound = 'Искомая комбинация на карте не встречается.';
  SAS_STR_foundplace = 'Место найденное по запросу';
  SAS_STR_Process = 'Идет обработка...';
  SAS_STR_WiteLoad = 'Подождите, идет загрузка...';
  SAS_STR_Processed = 'Обработано';
  SAS_STR_Saves = 'Сохранено';
  SAS_STR_AllProcessed = 'Всего обработано:';
  SAS_STR_AllLoad = 'Всего загружено:';
  SAS_STR_TimeRemained = 'Осталось времени:';
  SAS_STR_LoadRemained = 'Примерно еще загрузить:';
  SAS_STR_ProcessedNoMore = 'Обработать не более';
  SAS_STR_AllDelete = 'Всего удалено:';
  SAS_STR_AllSaves = 'Всего сохранить:';
  SAS_STR_files = 'файлов';
  SAS_STR_file = 'файл';
  SAS_STR_No = 'Нет';
  SAS_STR_Yes = 'Да';
  SAS_STR_Deleted = 'Удаляет:';
  SAS_STR_Gamma = 'Цветовая гамма';
  SAS_STR_Contrast = 'Контраст';
  SAS_STR_NewPath = 'Путь %0:d';
  SAS_STR_NewMark = 'Метка %0:d';
  SAS_STR_NewPoly = 'Полигон %0:d';
  SAS_STR_NewCategory = 'Новая категория';
  SAS_STR_AddNewPath = 'Добавить новый путь';
  SAS_STR_AddNewMark = 'Добавить новую метку';
  SAS_STR_AddNewPoly = 'Добавить новый полигон';
  SAS_STR_AddNewCategory = 'Добавить новую категорию';
  SAS_STR_EditPath = 'Изменить путь';
  SAS_STR_EditMark = 'Изменить метку';
  SAS_STR_EditPoly = 'Изменить полигон';
  SAS_STR_EditCategory = 'Изменить категорию';
  SAS_STR_Add = 'Добавить';
  SAS_STR_Edit = 'Изменить';
  SAS_STR_EditMap = 'Редактировать описание карты:';
  SAS_STR_Stop = 'Стоп';
  SAS_STR_Stop1 = 'Приостановлено';
  SAS_STR_Continue = 'Продолжить';
  SAS_STR_ExportTiles = 'Экспорт тайлов';
  SAS_STR_DivideInto = 'Разбить на';
  SAS_STR_Resolution = 'Разрешение';
  SAS_STR_UserStop = 'Приостановлено пользователем...';
  SAS_STR_LoadProcess = 'Загрузка';
  SAS_STR_LoadProcessRepl = 'Загрузка с заменой ...';
  SAS_STR_ProcessedFile = 'Обработка файла: %0:s ...';
  SAS_STR_Wite = 'Ждем';
  SAS_STR_S = 'Площадь';
  SAS_STR_L = 'Длина';
  SAS_STR_Lat = 'Широта';
  SAS_STR_Lon = 'Долгота';
  SAS_STR_OnVertical = 'По вертикали';
  SAS_STR_OnHorizontal = 'По горизонтали';
  SAS_STR_P = 'Периметр';
  SAS_STR_Whole = 'Всего';
  SAS_STR_Maps = 'Карты';
  SAS_STR_Layers = 'Слои';
  SAS_STR_InputLacitp = 'Введите через запятую mnc, mcc, LAC, CellID (Например: 02,250,17023,13023)';
  SAS_STR_InputLacitpCaption = 'Ввод параметров';
  SAS_STR_ExportYaMapsCaption = 'Мобильные Яндекс.Карты (версия 3)';
  SAS_STR_ExportGEKmlExportCaption = 'KML (Для просмотра в GE)';
  SAS_STR_ExportIPhone128Caption = 'iPhone (2.2 и выше 128х128)';
  SAS_STR_ExportIPhone64Caption = 'iPhone (Ниже v2.2 64х64)';
  SAS_STR_ExportAUXGeoServerCaption = 'AUX для GeoExpress Server';
  SAS_STR_ExportZipPackCaption = 'Упаковка в Zip';
  SAS_STR_OperationDeleteCaption = 'Удаление';
  SAS_STR_OperationGenPrevCaption = 'Генерация верхних уровней';
  SAS_STR_OperationTilesCopyCaption = 'Копирование тайлов';
  SAS_STR_OperationMapCombineCaption = 'Склейка карты';
  SAS_STR_OperationDownloadCaption = 'Скачивание';
  SAS_STR_ApplicationTitle = 'SAS.Планета';
  SAS_STR_BattaryStateOnLine = 'От сети';
  SAS_STR_BattaryStateCharge = 'Заряжается';
  SAS_STR_BattaryStateUnknown = 'Неизвестно';
  SAS_STR_MapCombineProgressLine0 = 'Склеить: %0:dx%1:d (%2:d) файлов';
  SAS_STR_MapCombineProgressCaption = 'Разрешение: %0:dx%1:d Разбить на %2:d файлов';
  SAS_STR_MiniMapAsMainMap = 'Как на главной карте';
  SAS_STR_SensorReset = 'Сбросить';

  SAS_STR_SensorGPSRecorderLastSpeedCaption = 'Скорость, км/ч:';
  SAS_STR_SensorGPSRecorderLastSpeedDescription = 'Отображает текущую скорость движения';
  SAS_STR_SensorGPSRecorderLastSpeedMenuItemName = 'Скорость';

  SAS_STR_SensorGPSRecorderAvgSpeedCaption = 'Скорость сред., км/ч:';
  SAS_STR_SensorGPSRecorderAvgSpeedDescription = 'Отображает среднюю скорость движения';
  SAS_STR_SensorGPSRecorderAvgSpeedMenuItemName = 'Скорость средняя';

  SAS_STR_SensorGPSRecorderMaxSpeedCaption = 'Скорость макс., км/ч:';
  SAS_STR_SensorGPSRecorderMaxSpeedDescription = 'Отображает максимальную скорость движения';
  SAS_STR_SensorGPSRecorderMaxSpeedMenuItemName = 'Скорость максимальная';

  SAS_STR_SensorGPSRecorderDistCaption = 'Пройденный путь:';
  SAS_STR_SensorGPSRecorderDistDescription = 'Отображает пройденный путь считаемый от подключения к GPS-приемнику';
  SAS_STR_SensorGPSRecorderDistMenuItemName = 'Пройденный путь';

  SAS_STR_SensorGPSRecorderOdometer1Caption = 'Одометр, км:';
  SAS_STR_SensorGPSRecorderOdometer1Description = 'Отображает весь пройденный путь';
  SAS_STR_SensorGPSRecorderOdometer1MenuItemName = 'Одометр';

  SAS_STR_SensorGPSRecorderOdometer2Caption = 'Одометр №2, км:';
  SAS_STR_SensorGPSRecorderOdometer2Description = 'Отображает весь пройденный путь';
  SAS_STR_SensorGPSRecorderOdometer2MenuItemName = 'Одометр №2';

  SAS_STR_SensorGPSRecorderAltitudeCaption = 'Высота, м:';
  SAS_STR_SensorGPSRecorderAltitudeDescription = 'Отображает высоту над уровнем моря по данным GPS-приемника';
  SAS_STR_SensorGPSRecorderAltitudeMenuItemName = 'Высота';

  SAS_STR_SensorGPSRecorderHeadingCaption = 'Азимут:';
  SAS_STR_SensorGPSRecorderHeadingDescription = 'Отображает азимут направления';
  SAS_STR_SensorGPSRecorderHeadingMenuItemName = 'Азимут';

  SAS_STR_SensorNavToPointCaption = 'Расстояние до метки:';
  SAS_STR_SensorNavToPointDescription = 'Отображает расстояние до выбранной метки';
  SAS_STR_SensorNavToPointMenuItemName = 'Расстояние до метки';

  SAS_STR_SensorBatteryStatusCaption = 'Батарея:';
  SAS_STR_SensorBatteryStatusDescription = 'Отображает состояние питания';
  SAS_STR_SensorBatteryStatusMenuItemName = 'Батарея';

  SAS_UNITS_kb = 'Кб';
  SAS_UNITS_mb = 'Мб';
  SAS_UNITS_gb = 'Гб';
  SAS_UNITS_kmperh = 'км/час';
  SAS_UNITS_mperp = '/пикс.';
  SAS_UNITS_km = 'км';
  SAS_UNITS_sm = 'см';
  SAS_UNITS_m = 'м';
  SAS_UNITS_m2 = 'м2';
  SAS_UNITS_km2 = 'км2';
  SAS_UNITS_Secund = 'секунд';
  SAS_UNITS_Min = 'мин.';
implementation

end.
