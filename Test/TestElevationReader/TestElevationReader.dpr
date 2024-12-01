program TestElevationReader;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  System.Diagnostics,
  u_ByteSwapFunc,
  u_GlobalDllName,
  u_ExternalTerrainFile in '..\..\Src\Terrain\External\u_ExternalTerrainFile.pas',
  u_ElevationReader in '..\..\Src\Terrain\External\u_ElevationReader.pas',
  u_ElevationReaderRAW in '..\..\Src\Terrain\External\u_ElevationReaderRAW.pas',
  u_ElevationReaderTIFF in '..\..\Src\Terrain\External\u_ElevationReaderTIFF.pas',
  u_ElevationValue in '..\..\Src\Terrain\External\u_ElevationValue.pas';

{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

type
  TParams = record
    FileName: string;
    VoidValue: Integer;
    RowsCount: Integer;
    ColsCount: Integer;
    ByteOrder: Integer;
  end;

procedure DoTest(const AParams: TParams);
var
  I, J: Integer;
  P: PSmallInt;
  VData: SmallInt;
  VFileName: string;
  VStream: TMemoryStream;
  VTerrain: TExternalTerrainFile;
  VElevation: Single;
  VMin, VMax: Single;
  VVoidCount: Integer;
  VTimer: TStopwatch;
begin
  VFileName :=
    TPath.GetFullPath(ExtractFilePath(AParams.FileName)) +
    ExtractFileName(AParams.FileName);

  Writeln('Processing file: "' + VFileName + '"');

  VTerrain := TExternalTerrainFile.Create(AParams.ByteOrder, AParams.VoidValue);
  try
    if not VTerrain.Open(VFileName, AParams.RowsCount, AParams.ColsCount) then begin
      Writeln('Error: Can''t open input file!');
      Exit;
    end;

    VStream := TMemoryStream.Create;
    try
      VStream.SetSize(AParams.RowsCount * AParams.ColsCount * SizeOf(VData));
      P := VStream.Memory;

      VMin := AParams.VoidValue;
      VMax := AParams.VoidValue;
      VVoidCount := 0;

      VTimer := TStopwatch.StartNew;
      for I := AParams.RowsCount - 1 downto 0 do begin
        for J := 0 to AParams.ColsCount - 1 do begin
          if VTerrain.FindElevation(I, J, VElevation) then begin
            if (VMin > VElevation) or (VMin = AParams.VoidValue) then begin
              VMin := VElevation;
            end;
            if (VMax < VElevation) or (VMax = AParams.VoidValue) then begin
              VMax := VElevation;
            end;
            VData := Round(VElevation);
          end else begin
            VData := AParams.VoidValue;
            Inc(VVoidCount);
          end;
          P^ := VData;
          Inc(P);
        end;
      end;
      VTimer.Stop;

      Writeln('Min elevation: ' + FormatFloat('0.00', VMin) + ' m');
      Writeln('Max elevation: ' + FormatFloat('0.00', VMax) + ' m');
      Writeln('Void count: ' + VVoidCount.ToString);
      Writeln('Elapsed: ' + VTimer.Elapsed.ToString);

      VStream.SaveToFile(ChangeFileExt(VFileName, '.raw'));
    finally
      VStream.Free;
    end;
  finally
    VTerrain.Free;
  end;
end;

const
  CTestDataRootDir = '.\TestData\Terrain\';

procedure TestTiledTiff;
var
  VParams: TParams;
begin
  // Example data provider: FABDEM (tiled tiff, deflate compression)
  // https://data.bris.ac.uk/data/dataset/s5hqmjcdj8yo2ibzi9b4ew3sn

  with VParams do begin
    FileName  := CTestDataRootDir + 'tiled.tif';
    VoidValue := -9999;
    RowsCount := 3600;
    ColsCount := 3600;
    ByteOrder := 0;
  end;

  DoTest(VParams);
  Writeln;
end;

procedure TestStrippedTiff;
var
  VParams: TParams;
begin
  // Example data provider: ALOS AW3D30 (stripped tiff, no compression)
  // https://www.eorc.jaxa.jp/ALOS/en/aw3d30/data/index.htm

  with VParams do begin
    FileName  := CTestDataRootDir + 'stripped.tif';
    VoidValue := -9999;
    RowsCount := 3600;
    ColsCount := 3600;
    ByteOrder := 0;
  end;

  DoTest(VParams);
  Writeln;
end;

begin
  IsMultiThread := True;
  ReportMemoryLeaksOnShutdown := True;

  try
    GDllName.Init;

    TestTiledTiff;
    TestStrippedTiff;

    Writeln('Done!');
  except
    on E: Exception do begin
      Writeln(E.ClassName, ': ', E.Message);
    end;
  end;

  Writeln('Press ENTER to exit...');
  Readln;
end.
