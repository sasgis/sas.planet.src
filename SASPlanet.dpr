program SASPlanet;

uses
  reinit,
  Forms,
  iniFiles,
  sysutils,
  windows,
  Unit1 in 'Unit1.pas' {Fmain},
  Unit2 in 'Unit2.pas' {Form2},
  Unit7 in 'Unit7.pas' {Fabout},
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
  UKMLmap in 'UKMLmap.pas',
  Ubrowser in 'Ubrowser.pas' {Fbrowser},
  UKMLExplorer in 'UKMLExplorer.pas' {FKMLExplorer},
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
  UFillingMap in 'UFillingMap.pas';

var Ini: Tinifile;
{$R *.res} {$R SASPlanetIcons.Res} {$R 0map.res} {$R Icons.RES}
begin
  Ini:=TiniFile.Create(copy(paramstr(0),1,length(paramstr(0))-4)+'.ini');
  localization:=Ini.Readinteger('VIEW','localization',SysLocale.PriLangID);
  Application.Initialize;
  Application.Title := 'SAS.Планета';
  //logo
  LoadNewResourceModule(localization);
  if Ini.ReadBool('VIEW','Show_logo',true) then
   begin
    FLogo:=TFLogo.Create(application);
    FLogo.Label1.Caption:='v '+SASVersion;
    FLogo.Show;
    Application.ProcessMessages;
   end;
  Ini.Free;
  //xLogo
  Application.HelpFile := '';
  Application.CreateForm(TFmain, Fmain);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TFabout, Fabout);
  Application.CreateForm(TFSettings, FSettings);
  Application.CreateForm(TFsaveas, Fsaveas);
  Application.CreateForm(TFSearchResult, FSearchResult);
  //  Application.CreateForm(TFProgress, FProgress);
  Application.CreateForm(TFaddPoint, FaddPoint);
  Application.CreateForm(TFprogress2, Fprogress2);
  Application.CreateForm(TFbrowser, Fbrowser);
  Application.CreateForm(TFKMLExplorer, FKMLExplorer);
  Application.CreateForm(TFaddLine, FaddLine);
  Application.CreateForm(TFAddPoly, FAddPoly);
  Application.CreateForm(TFEditMap, FEditMap);
  Application.Run;
end.
