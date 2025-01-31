program SASPlanetTests;

{$WARN DUPLICATE_CTOR_DTOR OFF}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  u_GlobalDllName,
  i_IGUIDInterfaceSet_Test in 'i_IGUIDInterfaceSet_Test.pas',
  i_IGUIDObjectSet_Test in 'i_IGUIDObjectSet_Test.pas',
  u_BitmapTileLibPng_Test in 'u_BitmapTileLibPng_Test.pas',
  u_BitmapTileSaveLoadFactory_Test in 'u_BitmapTileSaveLoadFactory_Test.pas',
  u_CalcTilesInPolygon_Test in 'u_CalcTilesInPolygon_Test.pas',
  u_CoordToStringConverter_Test in 'u_CoordToStringConverter_Test.pas',
  u_Datum_Test in 'u_Datum_Test.pas',
  u_EnumDoublePointClosePoly_Test in 'u_EnumDoublePointClosePoly_Test.pas',
  u_EnumDoublePointFilterEqual_Test in 'u_EnumDoublePointFilterEqual_Test.pas',
  u_EnumDoublePointLine2Poly_Test in 'u_EnumDoublePointLine2Poly_Test.pas',
  u_EnumDoublePointWithClip_Test in 'u_EnumDoublePointWithClip_Test.pas',
  u_GUIDSetStatic_Test in 'u_GUIDSetStatic_Test.pas',
  u_GUIDSet_Test in 'u_GUIDSet_Test.pas',
  u_GeoJsonParser_Test in 'u_GeoJsonParser_Test.pas',
  u_GeometrySaveLoadWKB_Test in 'u_GeometrySaveLoadWKB_Test.pas',
  u_HashCacheWithQueuesAbstract_Test in 'u_HashCacheWithQueuesAbstract_Test.pas',
  u_MarkCategoryListToTree_Test in 'u_MarkCategoryListToTree_Test.pas',
  u_PascalScriptUrlTemplate_Test in 'u_PascalScriptUrlTemplate_Test.pas',
  u_PathConfig_Test in 'u_PathConfig_Test.pas',
  u_ProjectedPolygonWithRect_Test in 'u_ProjectedPolygonWithRect_Test.pas',
  u_ProjectedSingleLine_Test in 'u_ProjectedSingleLine_Test.pas',
  u_ProjectionSet_Test in 'u_ProjectionSet_Test.pas',
  u_ProjectionType_Test in 'u_ProjectionType_Test.pas',
  u_RegularExpressions_Test in 'u_RegularExpressions_Test.pas',
  u_SASTestCase in 'u_SASTestCase.pas',
  u_UpdateChecker_Test in 'u_UpdateChecker_Test.pas',
  u_VectorItmesFactorySimple_Test in 'u_VectorItmesFactorySimple_Test.pas',
  u_WikimapiaPlainTextParser_Test in 'u_WikimapiaPlainTextParser_Test.pas';

begin
  GDllName.Init;

  Application.Initialize;

  if IsConsole then begin
    TextTestRunner.RunRegisteredTests;
  end else begin
    GUITestRunner.RunRegisteredTests;
  end;
end.

