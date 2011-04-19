unit u_ResStrings;

interface

ResourceString
  SAS_MSG_need_reload_application_curln = 'Для того чтобы изменения вступили в силу'#13#10 +
    'необходимо перезапустить программу.';
  SAS_MSG_coution = 'Attention!';
  SAS_MSG_youasure = 'Are you sure';
  SAS_MSG_youasurerefrsensor = 'Are you sure?';
  SAS_MSG_SelectArea = 'Часть выделенной области находится вне карты.'#13#10 +
    'Область будет ограничена по размерам карты.';
  SAS_MSG_FileExists = 'Файл %0:s уже есть у вас в кэше.'#13#10 +
    'Заменить этот файл вновь скачанным?';
  SAS_MSG_NeedHL = 'Please select the region at least once!';
  SAS_MSG_FunExForPoly = 'The feature could be used on polygons only';
  SAS_MSG_FileBeCreateTime = 'The tile is newer than the entered age, skipping.';
  SAS_MSG_FileBeCreateLen = 'The tile size is equal to the existing one, skipping.';
  SAS_MSG_ProcessFilesComplete = 'The task is completed!';
  SAS_MSG_LoadComplete = 'Finished downloading';
  SAS_MSG_NoGPSdetected = 'No GPS devices were found';
  SAS_MSG_GarminMax1Mp = 'Количество полученных jpeg-файлов превысит 100 штук, если ваш'#13#10 +
    'навигатор не отображает карту разбейте сохраняемое изображение на части в '#13#10 +
    'диалоге "Операции с выделенной областью\склеить"'#13#10 +
    'и используйте полученные kmz по отдельности';
  SAS_MSG_NotDelWhereHasChildren = 'Category is not empty. Please delete child categories first';
  SAS_MSG_HotKeyExists = 'The hotkey is already in use. Please select another one';
  SAS_MSG_CantGetTileFileName = 'This is not simple file storage. Tile''s file name not exists.';

  SAS_ERR_Nopoints = 'Required pointset is not found!';
  SAS_ERR_ProxyStrFormat = 'Proxy server string is not correct';
  SAS_ERR_Write = 'Error in disk operations. The task will be aborted!';
  SAS_ERR_Save = 'Error while saving!';
  SAS_ERR_code = 'Error code:';
  SAS_ERR_Read = 'Error reading file!';
  SAS_ERR_SelectArea = 'Selection is out of the map!';
  SAS_ERR_NoMaps = 'Unable to load map(s)! Aborting...';
  SAS_ERR_BadFile = 'The tile is corrupt!';
  SAS_ERR_FileNotFound = 'File not found!';
  SAS_ERR_PortOpen = 'Error opening port!';
  SAS_ERR_Communication = 'Error communicating with the device';
  SAS_ERR_UnablePposition = 'Unable to fix position';
  SAS_ERR_ParamsInput = 'Error in parameters!';
  SAS_ERR_LonLat1 = 'Долгота в левом верхнем углу должна быть меньше '#13#10 +
    'долготы в правом нижнем!';
  SAS_ERR_LonLat2 = 'Широта в левом верхнем углу должна быть меньше '#13#10 +
    'широты в правом нижнем!';
  SAS_ERR_CreateArh = 'Error creating archive!';
  SAS_ERR_NotLoads = 'The map is unable to load';
  SAS_ERR_Authorization = 'Error proxy authorizing';
  SAS_ERR_WaitTime = 'Wait %0:d secons...';
  SAS_ERR_Ban = 'Most likely you''ve got banned by the server!';
  SAS_ERR_TileNotExists = 'Tile is not found!';
  SAS_ERR_Noconnectionstointernet = 'Error connecting to server';
  SAS_ERR_RepeatProcess = 'Retrying the operation';
  SAS_ERR_FileExistsShort = 'The tile is exist';
  SAS_ERR_Memory = 'Not enough memory to complete the task';
  SAS_ERR_UseADifferentFormat = 'Please decide to use another file formats (ecw,bmp,jp2) while stitching big images';
  SAS_ERR_BadMIMEForDownloadRastr  =  'Сервис вернул тип "%0:s", это не растровое изображение';
  SAS_ERR_BadMIME  =  'Сервис вернул тип "%0:s", а не один из ожидаемых';
  SAS_ERR_MapGUIDEmpty = 'Empty GUID';
  SAS_ERR_MapGUIDBad = 'Incorrect GUID: %0:s';
  SAS_ERR_MapGUIDError = 'There is an error %1:s in the map: %0:s';
  SAS_ERR_MapGUIDDuplicate = 'Files %0:s and %1:s have equal GUIDs';
  SAS_ERR_MainMapNotExists = 'You need at least one MAP among your zmp files';
  SAS_ERR_CategoryNameDoubling = 'Category name already exist';
  SAS_ERR_UrlScriptError = 'Ошибка скрипта карты %0:s (%2:s):'#13#10'%1:s';
  SAS_ERR_UrlScriptUnexpectedError = 'Ошибка скрипта карты %0:s (%2:s):'#13#10'Неожиданная ошибка';
  SAS_ERR_UrlScriptCompileError = 'Ошибка в скрипте при компиляции'#13#10'%0:s';
  SAS_ERR_UrlScriptByteCodeLoad = 'Error at script bytecode loading';
  SAS_ERR_MapProjectionUnexpectedType = 'Error in projection \"%0:s\" of the map';
  SAS_ERR_MapDownloadByError = 'Downloading of map %0:s disabled by error: %1:s';
  SAS_ERR_TileDownloadContentTypeUnexpcted = 'Unexpeced conent type';
  SAS_ERR_TileDownloadUnexpectedError = 'Unknown error while downloading';
  SAS_ERR_CoordinatesInput='Error in geographical coordinates!';

  SAS_STR_MarshLen = 'Distance:';
  SAS_STR_Marshtime = 'Time to reach:';
  SAS_STR_coordinates = 'Coordinates';
  SAS_STR_time = 'Time';
  SAS_STR_load = 'Downloaded';
  SAS_STR_Scale = 'Zoom';
  SAS_STR_Speed = 'Speed';
  SAS_STR_LenPath = 'Disctance';
  SAS_STR_LenToMark = 'Distance to the placemark';
  SAS_STR_filesnum = 'Tiles not over than';
  SAS_STR_activescale = 'Current zoom';
  SAS_STR_for = 'For';
  SAS_STR_savetreck = 'Saving track...';
  SAS_STR_loadhl = 'Loading selection...';
  SAS_STR_notfound = 'Nothing found on current map.';
  SAS_STR_foundplace = 'Found';
  SAS_STR_Process = 'Processing...';
  SAS_STR_WiteLoad = 'Please wait while downloading...';
  SAS_STR_Processed = 'Processed';
  SAS_STR_Saves = 'Saved';
  SAS_STR_AllProcessed = 'Processed total:';
  SAS_STR_AllLoad = 'Downloaded total:';
  SAS_STR_TimeRemained = 'Time remaining:';
  SAS_STR_LoadRemained = 'Approx.to download:';
  SAS_STR_ProcessedNoMore = 'Process not over than';
  SAS_STR_AllDelete = 'Deleted total:';
  SAS_STR_AllSaves = 'Total to save:';
  SAS_STR_files = 'files';
  SAS_STR_file = 'file';
  SAS_STR_No = 'No';
  SAS_STR_Yes = 'Yes';
  SAS_STR_Deleted = 'Deleting:';
  SAS_STR_Gamma = 'Gamma';
  SAS_STR_Contrast = 'Contrast';
  SAS_STR_NewPath = 'Path %0:d';
  SAS_STR_NewMark = 'Point %0:d';
  SAS_STR_NewPoly = 'Poly %0:d';
  SAS_STR_NewCategory = 'New category';
  SAS_STR_AddNewPath = 'Add new track';
  SAS_STR_AddNewMark = 'Add new placemark';
  SAS_STR_AddNewPoly = 'Add new polygon';
  SAS_STR_AddNewCategory = 'Add new category';
  SAS_STR_EditPath = 'Edit route';
  SAS_STR_EditMark = 'Edit placemark';
  SAS_STR_EditPoly = 'Edit polygon';
  SAS_STR_EditCategory = 'Edit category';
  SAS_STR_Add = 'Add';
  SAS_STR_Edit = 'Edit';
  SAS_STR_EditMap = 'Edit map description:';
  SAS_STR_Stop = 'Abort';
  SAS_STR_Stop1 = 'Paused';
  SAS_STR_Continue = 'Resume';
  SAS_STR_ExportTiles = 'Export';
  SAS_STR_DivideInto = 'Split to';
  SAS_STR_Resolution = 'Quality';
  SAS_STR_UserStop = 'Paused by user...';
  SAS_STR_LoadProcess = 'Downloading';
  SAS_STR_LoadProcessRepl = 'Downloading with overwriting...';
  SAS_STR_ProcessedFile = 'Processing file: %0:s ...';
  SAS_STR_Wite = 'Wait';
  SAS_STR_S = 'Area';
  SAS_STR_L = 'Length';
  SAS_STR_Lat = 'Latitude';
  SAS_STR_Lon = 'Longitude';
  SAS_STR_OnVertical = 'Vertically';
  SAS_STR_OnHorizontal = 'Horizontally';
  SAS_STR_P = 'Perimeter';
  SAS_STR_Whole = 'Total';
  SAS_STR_Maps = 'Maps';
  SAS_STR_Layers = 'Layers';
  SAS_STR_InputLacitp = 'Enter comma-separated mnc, mcc, LAC, CellID (example: 02,250,17023,13023)';
  SAS_STR_InputLacitpCaption = 'Enter parameters';
  SAS_STR_ExportYaMapsCaption = 'Mobile Yandex.Maps (ver.3)';
  SAS_STR_ExportGEKmlExportCaption = 'KML (open in Google Earth)';
  SAS_STR_ExportIPhone128Caption = 'iPhone (ver.2.2 and above, 128х128)';
  SAS_STR_ExportIPhone64Caption = 'iPhone (below ver.2.2, 64х64)';
  SAS_STR_ExportAUXGeoServerCaption = 'AUX for LizardTech GeoExpress Server';
  SAS_STR_ExportZipPackCaption = 'Compressing to Zip';
  SAS_STR_ExportTarPackCaption = 'Compressing to Tar';
  SAS_STR_OperationDeleteCaption = 'Deleting';
  SAS_STR_OperationGenPrevCaption = 'Generate upper zooms';
  SAS_STR_OperationTilesCopyCaption = 'Copying';
  SAS_STR_OperationMapCombineCaption = 'Stitching the map';
  SAS_STR_OperationDownloadCaption = 'Downloading';
  SAS_STR_ApplicationTitle = 'SAS.Planet';
  SAS_STR_BattaryStateOnLine = 'OnLine';
  SAS_STR_BattaryStateCharge = 'Charge';
  SAS_STR_BattaryStateUnknown = 'Unknown';
  SAS_STR_MapCombineProgressLine0 = 'Stitch: %0:dx%1:d (%2:d) files';
  SAS_STR_MapCombineProgressCaption = 'Quality: %0:dx%1:d Split to %2:d files';
  SAS_STR_MiniMapAsMainMap = 'Same as main map';
  SAS_STR_SensorReset = 'Reset';

  SAS_STR_SensorGPSRecorderLastSpeedCaption = 'Current speed, km/h:';
  SAS_STR_SensorGPSRecorderLastSpeedDescription = 'Shows current speed';
  SAS_STR_SensorGPSRecorderLastSpeedMenuItemName = 'Speed';

  SAS_STR_SensorGPSRecorderAvgSpeedCaption = 'Average speed, km/h:';
  SAS_STR_SensorGPSRecorderAvgSpeedDescription = 'Shows average speed';
  SAS_STR_SensorGPSRecorderAvgSpeedMenuItemName = 'Average speed';

  SAS_STR_SensorGPSRecorderMaxSpeedCaption = 'Max.speed, km/h:';
  SAS_STR_SensorGPSRecorderMaxSpeedDescription = 'Shows maximum speed';
  SAS_STR_SensorGPSRecorderMaxSpeedMenuItemName = 'Maximum speed';

  SAS_STR_SensorGPSRecorderDistCaption = 'Distance:';
  SAS_STR_SensorGPSRecorderDistDescription = 'Shows distance since last GPS connection';
  SAS_STR_SensorGPSRecorderDistMenuItemName = 'Distance';

  SAS_STR_SensorGPSRecorderOdometer1Caption = 'Distance, km:';
  SAS_STR_SensorGPSRecorderOdometer1Description = 'Shows the total distance';
  SAS_STR_SensorGPSRecorderOdometer1MenuItemName = 'Odometer';

  SAS_STR_SensorGPSRecorderOdometer2Caption = 'Distance №2, km:';
  SAS_STR_SensorGPSRecorderOdometer2Description = 'Shows the total distance';
  SAS_STR_SensorGPSRecorderOdometer2MenuItemName = 'Odometer №2';

  SAS_STR_SensorGPSRecorderAltitudeCaption = 'Altitude';
  SAS_STR_SensorGPSRecorderAltitudeDescription = 'Shows GPS altitude';
  SAS_STR_SensorGPSRecorderAltitudeMenuItemName = 'Altitude';

  SAS_STR_SensorGPSRecorderHeadingCaption = 'Bearing:';
  SAS_STR_SensorGPSRecorderHeadingDescription = 'Shows the bearing';
  SAS_STR_SensorGPSRecorderHeadingMenuItemName = 'Bearing';

  SAS_STR_SensorNavToPointCaption = 'Distance to the placemark:';
  SAS_STR_SensorNavToPointDescription = 'Shows distance to the selected placemark';
  SAS_STR_SensorNavToPointMenuItemName = 'Distance to the placemark';

  SAS_STR_SensorBatteryStatusCaption = 'Battery:';
  SAS_STR_SensorBatteryStatusDescription = 'Shows battery status';
  SAS_STR_SensorBatteryStatusMenuItemName = 'Battery';

  SAS_UNITS_kb = 'Kb';
  SAS_UNITS_mb = 'Mb';
  SAS_UNITS_gb = 'Gb';
  SAS_UNITS_kmperh = 'km/h';
  SAS_UNITS_mperp = '/pix.';
  SAS_UNITS_km = 'km';
  SAS_UNITS_sm = 'cm';
  SAS_UNITS_m = 'm';
  SAS_UNITS_m2 = 'm2';
  SAS_UNITS_km2 = 'km2';
  SAS_UNITS_Secund = 'seconds';
  SAS_UNITS_Min = 'min.';
implementation

end.
