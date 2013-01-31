{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ResStrings;

interface

ResourceString
  SAS_MSG_need_reload_application_curln = 'SAS.Planet must be restarted for the changes to take effect';

  SAS_MSG_coution = 'Attention!';

  SAS_MSG_DeleteMarkCategoryAsk = 'Are you sure you want to delete category with name "%0:s"?';
  SAS_MSG_DeleteMarkPointAsk = 'Are you sure you want to delete point with name "%0:s"?';
  SAS_MSG_DeleteMarkPathAsk = 'Are you sure you want to delete path with name "%0:s"?';
  SAS_MSG_DeleteMarkPolyAsk = 'Are you sure you want to delete polygon with name "%0:s"?';
  SAS_MSG_DeleteManyMarksAsk = 'Are you sure you want to delete %0:d placemarks?';


  SAS_MSG_DeleteTileOneTileAsk = 'Are you sure you want to delete tile with name "%0:s"?';
  SAS_MSG_DeleteTilesInRegionAsk = 'Are you sure you want to delete tiles in selected region?';

  SAS_MSG_ResetSensorAsk = 'Are you sure you want to reset sensor?';
  SAS_MSG_TileExists = 'Tile %0:s is available in cache.'#13#10 +
    'Replace?';
  SAS_MSG_FileExists = 'File %0:s is available on disk.'#13#10 +
    'Replace?';
  SAS_MSG_NeedHL = 'Please select at least one region';
  SAS_MSG_FunExForPoly = 'The feature can be used on polygons and polylines';
  SAS_MSG_FileBeCreateTime = 'The tile is newer than the entered age, skipping.';
  SAS_MSG_FileBeCreateLen = 'The tile size is equal to the existing one, skipping.';
  SAS_MSG_ProcessFilesComplete = 'The task is completed!';
  SAS_MSG_LoadComplete = 'Download completed';
  SAS_MSG_NoGPSdetected = 'No GPS receiver found';
  SAS_MSG_GarminMax1Mp = 'The number of JPEG files will exceed 100. If your'#13#10 +
    'navigation device split the image using'#13#10 +
    '"Manage Selection - Stitch" window'#13#10 +
    'and use resulting KMZ files separately';
  SAS_MSG_HotKeyExists = 'Hotkey in use. Please select another one';
  SAS_MSG_CantGetTileFileName = 'This is not simple file storage. Tile''s file name does not exist.';

  SAS_ERR_Nopoints = 'Required pointset is not found!';
  SAS_ERR_Save = 'Error while saving!';
  SAS_ERR_Code = 'Error code:';
  SAS_ERR_NoMaps = 'Unable to load map(s)! Aborting...';
  SAS_ERR_PortOpen = 'Error opening port!';
  SAS_ERR_Communication = 'Error communicating with device';
  SAS_ERR_UnablePposition = 'Unable to fix position';
  SAS_ERR_ParamsInput = 'Error in parameters!';
  SAS_ERR_LonLat1 = 'Longitude of upper left corner must be less than '#13#10 +
    'longitude of lower right corner';
  SAS_ERR_LonLat2 = 'Latitude of upper left corner must be less than '#13#10 +
    'latitude of lower right corner';
  SAS_ERR_Authorization = 'Proxy authorization error';
  SAS_ERR_WaitTime = 'Wait %0:d secons...';
  SAS_ERR_Ban = 'Most likely you''ve been banned by the server!';
  SAS_ERR_TileNotExists = 'Tile is not found!';
  SAS_ERR_Noconnectionstointernet = 'Error connecting to server';
  SAS_ERR_FileExistsShort = 'The tile already exists';
  SAS_ERR_BadMIMEForDownloadRastr = 'Server returned type "%0:s", this is not bitmap image';
  SAS_ERR_BadMIME = 'Server returned unexpected type "%0:s"';
  SAS_ERR_MapGUIDEmpty = 'Empty GUID';
  SAS_ERR_MapGUIDBad = 'Incorrect GUID: %0:s';
  SAS_ERR_MapGUIDError = 'There is an error %1:s in map %0:s';
  SAS_ERR_MapGUIDDuplicate = 'Files %0:s and %1:s have the same GUID';
  SAS_ERR_MainMapNotExists = 'You need at least one MAP among your ZMP files';
  SAS_ERR_UrlScriptCompileError = 'Compilation error in script '#13#10'%0:s';
  SAS_ERR_UrlScriptByteCodeLoad = 'Error at script bytecode loading';
  SAS_ERR_MapProjectionUnexpectedType = 'Error in projection "%0:s" of the map';
  SAS_ERR_TileDownloadUnexpectedError = 'Unknown error during download';
  SAS_ERR_CoordinatesInput = 'Error in geographic coordinates';
  SAS_ERR_TimeZoneInfoDisabled = '(%s not found!)';

  SAS_ERR_EmptyZMPFileName = 'Empty ZMP file name';
  SAS_ERR_FileNotFoundFmt = 'File %0:s not found';
  SAS_ERR_DirectoryNotExistFmt = 'Directory %0:s not exist';
  SAS_ERR_CantLoadBitmapFromSourceType = 'Can''t load bitmap from source type';
  SAS_ERR_CantSaveBitmapToTargetType = 'Can''t save bitmap to target type';
  SAS_ERR_EmptyServerResponse = 'Empty server response';
  SAS_ERR_CoordParseError = 'Error parsing coordinates: Lat=%s Lon=%s';

  SAS_ERR_ETS_NotImplemented  = 'Interface to external tile storage not implemented';
  SAS_ERR_ETS_CriticalError   = 'Unknown critical error at external tile storage';
  SAS_ERR_ETS_CannotConnect   = 'Cannot connect to external tile storage';
  SAS_ERR_ETS_CannotParseTile = 'Cannot parse tile to get its version';
  SAS_ERR_ETS_FailToSaveTile  = 'Failed to save to external tile storage: error ';
  SAS_ERR_ETS_NoSpaceAvailable  = 'No space available at external tile storage';
  SAS_ERR_ETS_ReadOnlyConnect   = 'External tile storage is readonly';
  SAS_ERR_ETS_DataTruncation    = 'Data truncation occured because of external tile storage misconfiguration';
  SAS_ERR_ETS_CannotCreateTable = 'Table not found and cannot be created (possibly due to insufficient privileges or readonly external tile storage)';
  SAS_ERR_ETS_UnknownError      = 'Unknown error at external tile storage, see Storage Options for more information';
  SAS_ERR_ETS_ConnectionIsDead  = 'Connection to external tile storage is dead and cannot be reestablished';

  SAS_ERR_ImageIsTooBig = 'Selected resolution is too big for %s format!'#13#10 +
    'Widht = %d (max = %d)'#13#10 + 'Height = %d (max = %d)'#13#10 +
    'Try select smaller region to stitch in %s or select other output format (ECW is the best).';

  SAS_ERR_ContentTypeMismatch = 'Cache converter aborted with error: Content-Type mismatch!' + #13#10 +
    'Source: %s' + #13#10 + 'Destination: %s';

  SAS_STR_MarshLen = 'Distance:';
  SAS_STR_Marshtime = 'Time to reach:';
  SAS_STR_load = 'Downloaded';
  SAS_STR_queue = 'Queue';
  SAS_STR_filesnum = 'Number of tiles';
  SAS_STR_activescale = 'Current zoom';
  SAS_STR_for = 'For';
  SAS_STR_notfound = 'Nothing found on current map.';
  SAS_STR_WiteLoad = 'Please wait, download in progress...';
  SAS_STR_Processed = 'Processed';
  SAS_STR_ProcessedNoMore = 'Process not more than';
  SAS_STR_AllDelete = 'Deleted total:';
  SAS_STR_AllSaves = 'Total to save:';
  SAS_STR_Files = 'files';
  SAS_STR_file = 'file';
  SAS_STR_No = 'No';
  SAS_STR_Yes = 'Yes';
  SAS_STR_Deleted = 'Deleting:';
  SAS_STR_Gamma = 'Gamma';
  SAS_STR_Contrast = 'Contrast';
  SAS_STR_NewPath = 'Path %0:d';
  SAS_STR_NewMark = 'Placemark %0:d';
  SAS_STR_NewPoly = 'Polygon %0:d';
  SAS_STR_NewCategory = 'New Category';
  SAS_STR_AddNewPath = 'Add New Path';
  SAS_STR_AddNewMark = 'Add New Placemark';
  SAS_STR_AddNewPoly = 'Add New Polygon';
  SAS_STR_AddNewCategory = 'Add New Category';
  SAS_STR_EditPath = 'Edit Path';
  SAS_STR_EditMark = 'Edit Placemark';
  SAS_STR_EditPoly = 'Edit Polygon';
  SAS_STR_EditCategory = 'Edit Category';
  SAS_STR_ExtendedPointCaption = '%0:s (Placemark)';
  SAS_STR_ExtendedPathCaption = '%0:s (Path)';
  SAS_STR_ExtendedPolygonCaption = '%0:s (Polygon)';
  SAS_STR_Finished = 'Finished';
  SAS_STR_CacheConvertionIsFinished = 'Cache conversion is finished!';
  SAS_STR_EditMap = 'Map Settings:';
  SAS_STR_Pause = 'Pause';
  SAS_STR_Paused = 'Paused... (%0:s)';
  SAS_STR_Continue = 'Resume';
  SAS_STR_ExportTiles = 'Export';
  SAS_STR_Resolution = 'size:';
  SAS_STR_UserStop = 'Paused by user...';
  SAS_STR_DownloadingCaption = '%0:s Downloading';
  SAS_STR_LoadProcess = 'Downloading...';
  SAS_STR_LoadProcessRepl = 'Downloading with overwrite...';
  SAS_STR_ProcessedFile = 'Processing tile: %0:s ...';
  SAS_STR_LoadAttachmentsBegin = 'Downloading attachments...';
  SAS_STR_LoadAttachmentsEnd_Skipped = 'Skipped';
  SAS_STR_LoadAttachmentsEnd_Failed = 'Failed';
  SAS_STR_LoadAttachmentsEnd_Cancelled = 'Cancelled';
  SAS_STR_LoadAttachmentsEnd_Nothing = 'No attachments';
  SAS_STR_S = 'Area';
  SAS_STR_L = 'Length';
  SAS_STR_P = 'Perimeter';
  SAS_STR_Whole = 'Total';
  SAS_STR_Azimuth = 'Azimuth';
  SAS_STR_Maps = 'Maps';
  SAS_STR_Layers = 'Layers';
  SAS_STR_InputLacitp = 'Enter comma-separated MNC, MCC, LAC, CellID (example: 02,250,7718,11942)';
  SAS_STR_InputLacitpCaption = 'Enter parameters';
  SAS_STR_ExportYaMobileV3Caption = 'Mobile Yandex.Maps (version 3)';
  SAS_STR_ExportYaMobileV4Caption = 'Mobile Yandex.Maps (version 3.91 and above)';
  SAS_STR_ExportGEKmlExportCaption = 'KML (for Google Earth)';
  SAS_STR_ExportRMapsSQLiteExportCaption = 'RMaps (SQLite)';
  SAS_STR_ExportIPhone128Caption = 'iPhone (version 2.2 and above, 128x128)';
  SAS_STR_ExportIPhone64Caption = 'iPhone (version prior to 2.2, 64x64)';
  SAS_STR_ExportAUXGeoServerCaption = 'AUX for LizardTech GeoExpress Server';
  SAS_STR_ExportZipPackCaption = 'Compressing to ZIP';
  SAS_STR_ExportTarPackCaption = 'Compressing to TAR';
  SAS_STR_ExportJNXPackCaption = 'JNX raster map for Garmin';
  SAS_STR_ExportOgf2PackCaption = 'Ogf2 map for SmartComGPS 1.5x';
  SAS_STR_ExportCEPackCaption = 'Packed cache for SAS4WinCE/SAS4Android';
  SAS_STR_OperationDeleteCaption = 'Deleting';
  SAS_STR_OperationGenPrevCaption = 'Generating upper zooms';
  SAS_STR_OperationTilesCopyCaption = 'Copying';
  SAS_STR_OperationMapCombineCaption = 'Stitching the map';
  SAS_STR_OperationDownloadCaption = 'Downloading';
  SAS_STR_ApplicationTitle = 'SAS.Planet';
  SAS_STR_BattaryStateOnLine = 'From mains';
  SAS_STR_BattaryStateCharge = 'Charging';
  SAS_STR_BattaryStateUnknown = 'Unknown';
  SAS_STR_MapCombineProgressLine0 = 'Stitch: %0:dx%1:d (%2:d) files';
  SAS_STR_MapCombineProgressCaption = 'Size: %0:dx%1:d. Split to %2:d files';
  SAS_STR_MiniMapAsMainMap = 'Displayed Main Map';
  SAS_STR_SensorReset = 'Reset';
  SAS_STR_Zoom = 'Zoom';
  SAS_STR_Tiles = 'Tiles';

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
  SAS_STR_SensorGPSRecorderOdometer1Description = 'Shows total distance';
  SAS_STR_SensorGPSRecorderOdometer1MenuItemName = 'Odometer';

  SAS_STR_SensorGPSRecorderOdometer2Caption = 'Distance #2, km:';
  SAS_STR_SensorGPSRecorderOdometer2Description = 'Shows total distance';
  SAS_STR_SensorGPSRecorderOdometer2MenuItemName = 'Odometer #2';

  SAS_STR_SensorGPSRecorderAltitudeCaption = 'Altitude';
  SAS_STR_SensorGPSRecorderAltitudeDescription = 'Shows GPS altitude';
  SAS_STR_SensorGPSRecorderAltitudeMenuItemName = 'Altitude';

  SAS_STR_SensorGPSRecorderHeadingCaption = 'Course:';
  SAS_STR_SensorGPSRecorderHeadingDescription = 'Shows the course';
  SAS_STR_SensorGPSRecorderHeadingMenuItemName = 'Course';

  SAS_STR_SensorNavToPointCaption = 'Distance to placemark:';
  SAS_STR_SensorNavToPointDescription = 'Shows distance to selected placemark';
  SAS_STR_SensorNavToPointMenuItemName = 'Distance to placemark';

  SAS_STR_SensorBatteryStatusCaption = 'Battery:';
  SAS_STR_SensorBatteryStatusDescription = 'Shows battery status';
  SAS_STR_SensorBatteryStatusMenuItemName = 'Battery';

  SAS_STR_SensorGPSRecorderHDOPCaption = 'HDOP:';
  SAS_STR_SensorGPSRecorderHDOPDescription = 'Shows HDOP';
  SAS_STR_SensorGPSRecorderHDOPMenuItemName = 'HDOP'; // Horizontal Dilution of Precision

  SAS_STR_SensorGPSRecorderVDOPCaption = 'VDOP:';
  SAS_STR_SensorGPSRecorderVDOPDescription = 'Shows VDOP';
  SAS_STR_SensorGPSRecorderVDOPMenuItemName = 'VDOP'; // Vertical Dilution of Precision

  SAS_STR_SensorGPSRecorderUTCTimeCaption = 'UTC time:';
  SAS_STR_SensorGPSRecorderUTCTimeDescription = 'Shows UTC time';
  SAS_STR_SensorGPSRecorderUTCTimeMenuItemName = 'UTC time'; // UTC time

  SAS_STR_SensorGPSRecorderLocalTimeCaption = 'Local time:';
  SAS_STR_SensorGPSRecorderLocalTimeDescription = 'Shows local time';
  SAS_STR_SensorGPSRecorderLocalTimeMenuItemName = 'Local time'; // Local time

  SAS_STR_SensorGPSRecorderDGPSCaption = 'DGPS:';
  SAS_STR_SensorGPSRecorderDGPSDescription = 'Shows DGPS';
  SAS_STR_SensorGPSRecorderDGPSMenuItemName = 'DGPS'; // DGPS

  SAS_STR_SensorGPSRecorderGPSUnitInfoCaption = 'Unit info:';
  SAS_STR_SensorGPSRecorderGPSUnitInfoDescription = 'Shows GPS Unit info';
  SAS_STR_SensorGPSRecorderGPSUnitInfoMenuItemName = 'Unit info'; // DGPS

  SAS_STR_SensorGPSRecorderGPSSatellitesCaption = 'Satellite Signal Strength:';
  SAS_STR_SensorGPSRecorderGPSSatellitesDescription = 'Shows Satellite Signal Strength bars';
  SAS_STR_SensorGPSRecorderGPSSatellitesMenuItemName = 'Satellite Signal Strength'; // DGPS

  SAS_STR_GoogleSearchLanguage = '&hl=en';

  SAS_UNITS_kb = 'KB';
  SAS_UNITS_mb = 'MB';
  SAS_UNITS_gb = 'GB';
  SAS_UNITS_kmperh = 'km/h';
  SAS_UNITS_mperp = '/pixel';
  SAS_UNITS_km = 'km';
  SAS_UNITS_sm = 'cm';
  SAS_UNITS_m = 'm';
  SAS_UNITS_m2 = 'm2';
  SAS_UNITS_km2 = 'km2';
  SAS_UNITS_Ha = 'ha';

implementation

end.
