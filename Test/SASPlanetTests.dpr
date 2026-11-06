program SASPlanetTests;

{$WARN DUPLICATE_CTOR_DTOR OFF}

{$IFDEF CONSOLE_TESTRUNNER}
  {$APPTYPE CONSOLE}
{$ENDIF}

uses
  Classes,
  Forms,
  SysUtils,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  u_GlobalDllName,
  u_TestFilter,
  i_IGUIDInterfaceSet_Test in 'i_IGUIDInterfaceSet_Test.pas',
  i_IGUIDObjectSet_Test in 'i_IGUIDObjectSet_Test.pas',
  u_BitmapTileLibPng_Test in 'u_BitmapTileLibPng_Test.pas',
  u_BitmapTileSaveLoadFactory_Test in 'u_BitmapTileSaveLoadFactory_Test.pas',
  u_CalcTilesInPolygon_Test in 'u_CalcTilesInPolygon_Test.pas',
  u_ContentDecoder_Test in 'u_ContentDecoder_Test.pas',
  u_CoordToStringConverter_Test in 'u_CoordToStringConverter_Test.pas',
  u_Datum_Test in 'u_Datum_Test.pas',
  u_EnumDoublePointClosePoly_Test in 'u_EnumDoublePointClosePoly_Test.pas',
  u_EnumDoublePointFilterEqual_Test in 'u_EnumDoublePointFilterEqual_Test.pas',
  u_EnumDoublePointLine2Poly_Test in 'u_EnumDoublePointLine2Poly_Test.pas',
  u_EnumDoublePointWithClip_Test in 'u_EnumDoublePointWithClip_Test.pas',
  u_GUIDSetStatic_Test in 'u_GUIDSetStatic_Test.pas',
  u_GUIDSet_Test in 'u_GUIDSet_Test.pas',
  u_GeoJsonParser_Test in 'u_GeoJsonParser_Test.pas',
  u_GeometryMetaSaveLoadJson_Test in 'u_GeometryMetaSaveLoadJson_Test.pas',
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
  u_TestFilter_Test in 'u_TestFilter_Test.pas',
  u_UpdateChecker_Test in 'u_UpdateChecker_Test.pas',
  u_VectorItmesFactorySimple_Test in 'u_VectorItmesFactorySimple_Test.pas',
  u_WikimapiaPlainTextParser_Test in 'u_WikimapiaPlainTextParser_Test.pas',
  u_XmlEscape_Test in 'u_XmlEscape_Test.pas';

const
  CExitNoTestsMatched = 1;
  CExitTestsFailed = 2;

procedure PrintHelp;
begin
  WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' [--list] [--pause] [mask ...]');
  WriteLn('  --list   print available tests and exit');
  WriteLn('  --pause  wait for Enter before exiting');
  WriteLn('  mask     case-insensitive substring of "Suite.Method" (several = OR)');
  WriteLn('  (none)   run all tests');
  WriteLn('Exit codes:');
  WriteLn('  0  all tests passed');
  WriteLn(Format('  %d  no tests matched', [CExitNoTestsMatched]));
  WriteLn(Format('  %d  some tests failed or raised errors', [CExitTestsFailed]));
end;

procedure RunConsoleTests;
var
  VFilter: TSASTestFilter;
  VList: TStringList;
  VResult: TTestResult;
  VEnabledCount: Integer;
begin
  VFilter := TSASTestFilter.Create;
  try
    VFilter.ParseCommandLine;
    try
      if VFilter.HelpRequested then begin
        PrintHelp;
        Exit;
      end;

      if VFilter.ListOnly then begin
        VList := TStringList.Create;
        try
          VFilter.CollectList(RegisteredTests, VList);
          Write(VList.Text);
        finally
          VList.Free;
        end;
        Exit;
      end;

      VEnabledCount := VFilter.Apply(RegisteredTests);
      if VFilter.HasMasks then begin
        WriteLn(Format(
          'Filter: %s -> %d of %d tests',
          [VFilter.MasksAsText, VEnabledCount, RegisteredTests.CountTestCases]
        ));
        if VEnabledCount = 0 then begin
          WriteLn('ERROR: no tests matched');
          ExitCode := CExitNoTestsMatched;
          Exit;
        end;
      end;

      VResult := TextTestRunner.RunRegisteredTests;
      try
        if not VResult.WasSuccessful then begin
          ExitCode := CExitTestsFailed;
        end;
      finally
        VResult.Free;
      end;
    finally
      if VFilter.Pause then begin
        WriteLn('Press Enter to exit...');
        ReadLn;
      end;
    end;
  finally
    VFilter.Free;
  end;
end;

begin
  GDllName.Init;

  Application.Initialize;

  if IsConsole then begin
    RunConsoleTests;
  end else begin
    GUITestRunner.RunRegisteredTests;
  end;
end.

