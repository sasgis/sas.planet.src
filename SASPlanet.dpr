program SASPlanet;

uses
  reinit,
  Forms,
  iniFiles,
  sysutils,
  windows,
  Langs in 'src\Langs.pas',
  cUnicode in 'src\cUnicode.pas',
  cUnicodeChar in 'src\cUnicodeChar.pas',
  i_ILogSimple in 'i_ILogSimple.pas',
  i_ILogForTaskThread in 'i_ILogForTaskThread.pas',
  u_LogForTaskThread in 'u_LogForTaskThread.pas',
  u_WideStrings in 'src\u_WideStrings.pas',
  Unit1 in 'Unit1.pas' {Fmain},
  Unit2 in 'Unit2.pas' {FGoTo},
  UAbout in 'UAbout.pas' {Fabout},
  Usettings in 'Usettings.pas' {FSettings},
  USaveas in 'USaveas.pas' {Fsaveas},
  UProgress in 'UProgress.pas' {FProgress},
  UaddPoint in 'UaddPoint.pas' {FaddPoint},
  UTrAllLoadMap in 'UTrAllLoadMap.pas',
  UThreadScleit in 'UThreadScleit.pas',
  Unit4 in 'Unit4.pas' {Fprogress2},
  ULogo in 'ULogo.pas' {FLogo},
  UWikiLayer in 'UWikiLayer.pas',
  UOzi in 'UOzi.pas',
  USelLonLat in 'USelLonLat.pas' {FSelLonLat},
  Ugeofun in 'Ugeofun.pas',
  UKmlParse in 'UKmlParse.pas',
  Ubrowser in 'Ubrowser.pas' {Fbrowser},
  Uimgfun in 'Uimgfun.pas',
  UMapType in 'UMapType.pas',
  UTimeZones in 'UTimeZones.pas',
  UaddLine in 'UaddLine.pas' {FaddLine},
  UaddPoly in 'UaddPoly.pas' {FAddPoly},
  UThreadExport in 'UThreadExport.pas',
  UEditMap in 'UEditMap.pas' {FEditMap},
  UResStrings in 'UResStrings.pas',
  USearchResult in 'USearchResult.pas' {FSearchResult},
  UOpDelTiles in 'UOpDelTiles.pas',
  UOpGenPreviousZoom in 'UOpGenPreviousZoom.pas',
  UFillingMap in 'UFillingMap.pas',
  UMarksExplorer in 'UMarksExplorer.pas' {FMarksExplorer},
  UImport in 'UImport.pas' {FImport},
  UAddCategory in 'UAddCategory.pas' {FAddCategory},
  UPLT in 'UPLT.pas',
  UFDGAvailablePic in 'UFDGAvailablePic.pas' {FDGAvailablePic},
  u_MemFileCache in 'u_MemFileCache.pas',
  UYaMobile in 'UYaMobile.pas',
  u_UrlGenerator in 'u_UrlGenerator.pas',
  i_ICoordConverter in 'i_ICoordConverter.pas',
  u_CoordConverterAbstract in 'u_CoordConverterAbstract.pas',
  u_CoordConverterMercatorOnEllipsoid in 'u_CoordConverterMercatorOnEllipsoid.pas',
  u_CoordConverterMercatorOnSphere in 'u_CoordConverterMercatorOnSphere.pas',
  u_CoordConverterSimpleLonLat in 'u_CoordConverterSimpleLonLat.pas',
  ImgMaker in 'ImgMaker.pas',
  t_GeoTypes in 't_GeoTypes.pas',
  t_CommonTypes in 't_CommonTypes.pas',
  u_GeoToStr in 'u_GeoToStr.pas',
  u_GlobalState in 'u_GlobalState.pas',
  u_WindowLayerBasic in 'u_WindowLayerBasic.pas',
  u_MiniMap in 'u_MiniMap.pas',
  u_CenterScale in 'u_CenterScale.pas',
  u_LayerStatBar in 'u_LayerStatBar.pas',
  u_MapLayerBasic in 'u_MapLayerBasic.pas',
  u_MapMarksLayer in 'u_MapMarksLayer.pas',
  u_SelectionLayer in 'u_SelectionLayer.pas',
  u_MapGPSLayer in 'u_MapGPSLayer.pas',
  u_TileDownloaderBase in 'u_TileDownloaderBase.pas',
  u_TileDownloaderUI in 'u_TileDownloaderUI.pas',
  t_LoadEvent in 't_LoadEvent.pas',
  i_Marks in 'i_Marks.pas',
  u_MarkBasic in 'u_MarkBasic.pas',
  u_MarkCategory in 'u_MarkCategory.pas',
  u_MarksDb in 'u_MarksDb.pas' {DMMarksDb: TDataModule},
  u_EnumUnknownEmpty in 'u_EnumUnknownEmpty.pas',
  i_ITileFileNameGenerator in 'i_ITileFileNameGenerator.pas',
  u_TileFileNameSAS in 'u_TileFileNameSAS.pas',
  u_TileFileNameGMV in 'u_TileFileNameGMV.pas',
  u_TileFileNameES in 'u_TileFileNameES.pas',
  u_TileFileNameGM1 in 'u_TileFileNameGM1.pas',
  u_TileFileNameGM2 in 'u_TileFileNameGM2.pas',
  i_ITileFileNameGeneratorsList in 'i_ITileFileNameGeneratorsList.pas',
  u_TileFileNameGeneratorsSimpleList in 'u_TileFileNameGeneratorsSimpleList.pas',
  u_TileDownloaderUIOneTile in 'u_TileDownloaderUIOneTile.pas',
  u_TileDownloaderThreadBase in 'u_TileDownloaderThreadBase.pas',
  CPDrv in 'src\CPDrv.pas',
  UGSM in 'UGSM.pas';

var
  loc:integer;
   {$R *.res}{$R SASR.RES}
begin
  GState := TGlobalState.Create;
  if FileExists(GState.ProgramPath+'SASPlanet.RUS') then
   begin
    RenameFile(GState.ProgramPath+'SASPlanet.RUS',GState.ProgramPath+'SASPlanet.~RUS');
   end;
  if SysLocale.PriLangID<>CProgram_Lang_Default then loc:=LANG_ENGLISH
                                       else loc:=CProgram_Lang_Default;
  GState.Localization:= GState.MainIni.Readinteger('VIEW','localization',loc);
  GState.WebReportToAuthor:=GState.MainIni.ReadBool('NPARAM','stat',true);
  Application.Initialize;
  Application.Title := 'SAS.Планета';
  //logo
  LoadNewResourceModule(GState.Localization);
  if GState.MainIni.ReadBool('VIEW','Show_logo',true) then
   begin
    FLogo:=TFLogo.Create(application);
    FLogo.Label1.Caption:='v '+SASVersion;
    FLogo.Show;
    Application.ProcessMessages;
   end;
  LoadMaps;
  //xLogo
  Application.HelpFile := '';
  Application.CreateForm(TFmain, Fmain);
  Application.CreateForm(TFGoTo, FGoTo);
  Application.CreateForm(TFabout, Fabout);
  Application.CreateForm(TFSettings, FSettings);
  Application.CreateForm(TFsaveas, Fsaveas);
  Application.CreateForm(TFSearchResult, FSearchResult);
  Application.CreateForm(TFMarksExplorer, FMarksExplorer);
  Application.CreateForm(TFImport, FImport);
  Application.CreateForm(TFAddCategory, FAddCategory);
  Application.CreateForm(TFDGAvailablePic, FDGAvailablePic);
  Application.CreateForm(TFaddPoint, FaddPoint);
  Application.CreateForm(TFprogress2, Fprogress2);
  Application.CreateForm(TFbrowser, Fbrowser);
  Application.CreateForm(TFaddLine, FaddLine);
  Application.CreateForm(TFAddPoly, FAddPoly);
  Application.CreateForm(TFEditMap, FEditMap);
  Application.CreateForm(TDMMarksDb, DMMarksDb);
  Fmain.WebBrowser1.Navigate('about:blank');
  Fbrowser.EmbeddedWB1.Navigate('about:blank');
  Application.Run;
  FreeAndNil(GState);
end.
