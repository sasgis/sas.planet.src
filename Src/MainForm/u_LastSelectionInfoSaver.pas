{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_LastSelectionInfoSaver;

interface

uses
  Classes,
  SysUtils,
  i_NotifierOperation,
  i_Listener,
  i_PathConfig,
  i_SimpleFlag,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_LastSelectionInfo,
  u_BackgroundTask;

type
  TLastSelectionInfoSaver = class(TBackgroundTask)
  private
    FLastSelection: ILastSelectionInfo;
    FFileName: IPathConfig;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;

    FListener: IListener;
    FNeedReadFlag: ISimpleFlag;
    FNeedWriteFlag: ISimpleFlag;
    procedure _SaveContour(
      const AGeometry: IGeometryLonLatContour;
      const AStringList: TStringList;
      const AFormatSettings: TFormatSettings;
      var AStartIndex: Integer
    );
    procedure _SaveSinglePolygon(
      const AGeometry: IGeometryLonLatSinglePolygon;
      const AStringList: TStringList;
      const AFormatSettings: TFormatSettings;
      var AStartIndex: Integer
    );
    procedure _SaveMultiPolygon(
      const AGeometry: IGeometryLonLatMultiPolygon;
      const AStringList: TStringList;
      const AFormatSettings: TFormatSettings;
      var AStartIndex: Integer
    );
    procedure _SavePolygon(
      const AGeometry: IGeometryLonLatPolygon;
      const AStringList: TStringList;
      const AFormatSettings: TFormatSettings
    );

    procedure OnNeedSave;
    procedure ProcessSave(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const ALastSelection: ILastSelectionInfo;
      const AFileName: IPathConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  IniFiles,
  i_ThreadConfig,
  i_ConfigDataWriteProvider,
  i_EnumDoublePoint,
  u_ThreadConfig,
  u_ConfigProviderHelpers,
  u_ListenerByEvent,
  u_SimpleFlagWithInterlock,
  u_ConfigDataWriteProviderByIniFile;

{ TLastSelectionInfoSaver }

constructor TLastSelectionInfoSaver.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const ALastSelection: ILastSelectionInfo;
  const AFileName: IPathConfig
);
var
  VThreadConfig: IThreadConfig;
begin
  Assert(ALastSelection <> nil);
  Assert(AFileName <> nil);
  VThreadConfig := TThreadConfig.Create(tpIdle);
  inherited Create(AAppClosingNotifier, Self.ProcessSave, VThreadConfig, Self.ClassName);

  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FLastSelection := ALastSelection;
  FFileName := AFileName;


  FNeedReadFlag := TSimpleFlagWithInterlock.Create;
  FNeedWriteFlag := TSimpleFlagWithInterlock.Create;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnNeedSave);

  FLastSelection.ChangeNotifier.Add(FListener);
  FFileName.ChangeNotifier.Add(FListener);

  FNeedReadFlag.SetFlag;
  StartExecute;
end;

destructor TLastSelectionInfoSaver.Destroy;
begin
  if Assigned(FLastSelection) and Assigned(FListener) then begin
    FLastSelection.ChangeNotifier.Remove(FListener);
    FLastSelection := nil;
  end;

  if Assigned(FFileName) and Assigned(FListener) then begin
    FFileName.ChangeNotifier.Remove(FListener);
    FFileName := nil;
  end;

  FListener := nil;
  inherited;
end;

procedure TLastSelectionInfoSaver.OnNeedSave;
begin
  Self.StartExecute;
  FNeedWriteFlag.SetFlag;
end;

procedure TLastSelectionInfoSaver.ProcessSave(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VFileName: string;
  VPath: string;
  VZoom: Byte;
  VPolygon: IGeometryLonLatPolygon;
  VNeedRead: Boolean;
  VNeedWrite: Boolean;
  VIniFile: TMemIniFile;
  VProvider: IConfigDataWriteProvider;
  VStringList: TStringList;
  VFormatSettings: TFormatSettings;
begin
  if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
    Exit;
  end;
  VFileName := FFileName.FullPath;
  VNeedRead := FNeedReadFlag.CheckFlagAndReset;
  VNeedWrite := FNeedWriteFlag.CheckFlagAndReset;
  VZoom := 0;
  VPolygon := nil;
  if VNeedRead then begin
    if FileExists(VFileName) then begin
      try
        VIniFile := TMemIniFile.Create(VFileName);
        try
          VProvider := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
          VIniFile := nil;
        finally
          VIniFile.Free;
        end;
        VProvider := VProvider.GetOrCreateSubItem('HIGHLIGHTING');
        VPolygon := ReadPolygon(VProvider, FVectorGeometryLonLatFactory);
        if Assigned(VPolygon) then begin
          VZoom := VProvider.Readinteger('Zoom', VZoom);
        end;
      except
        VZoom := 0;
        VPolygon := nil;
      end;
      if VPolygon <> nil then begin
        FLastSelection.SetPolygon(VPolygon, VZoom);
      end;
    end;
  end;
  if VNeedWrite then begin
    VZoom := FLastSelection.Zoom;
    VPolygon := FLastSelection.Polygon;
    if VPolygon = nil then begin
      Exit;
    end;
    VPath := ExtractFilePath(VFileName);
    if not ForceDirectories(VPath) then begin
      Exit;
    end;
    try
      VFormatSettings.DecimalSeparator := '.';
      VFormatSettings.DateSeparator := '.';
      VFormatSettings.ShortDateFormat := 'dd.MM.yyyy';
      VFormatSettings.TimeSeparator := ':';
      VFormatSettings.LongTimeFormat := 'HH:mm:ss';
      VFormatSettings.ShortTimeFormat := 'HH:mm:ss';
      VFormatSettings.ListSeparator := ';';
      VFormatSettings.TwoDigitYearCenturyWindow := 50;
      VStringList := TStringList.Create;
      try
        VStringList.Add('[HIGHLIGHTING]');
        VStringList.Add('Zoom=' + IntToStr(VZoom));
        _SavePolygon(VPolygon, VStringList, VFormatSettings);
        VStringList.SaveToFile(VFileName);
      finally
        VStringList.Free;
      end;
    except
    end;
  end;
end;

procedure TLastSelectionInfoSaver._SaveContour(
  const AGeometry: IGeometryLonLatContour;
  const AStringList: TStringList;
  const AFormatSettings: TFormatSettings;
  var AStartIndex: Integer
);
var
  VEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  VEnum := AGeometry.GetEnum;
  while VEnum.Next(VPoint) do begin
    AStringList.Add('PointLon_' + IntToStr(AStartIndex) + '=' + FloatToStr(VPoint.X, AFormatSettings));
    AStringList.Add('PointLat_' + IntToStr(AStartIndex) + '=' + FloatToStr(VPoint.Y, AFormatSettings));
    Inc(AStartIndex);
  end;
end;

procedure TLastSelectionInfoSaver._SaveSinglePolygon(
  const AGeometry: IGeometryLonLatSinglePolygon;
  const AStringList: TStringList;
  const AFormatSettings: TFormatSettings;
  var AStartIndex: Integer
);
var
  i: Integer;
begin
  _SaveContour(AGeometry.OuterBorder, AStringList, AFormatSettings, AStartIndex);
  for i := 0 to AGeometry.HoleCount - 1 do begin
    AStringList.Add('PointLon_' + IntToStr(AStartIndex) + '=NAN');
    AStringList.Add('PointLat_' + IntToStr(AStartIndex) + '=-1');
    Inc(AStartIndex);
    _SaveContour(AGeometry.HoleBorder[i], AStringList, AFormatSettings, AStartIndex);
  end;
end;

procedure TLastSelectionInfoSaver._SaveMultiPolygon(
  const AGeometry: IGeometryLonLatMultiPolygon;
  const AStringList: TStringList;
  const AFormatSettings: TFormatSettings;
  var AStartIndex: Integer
);
var
  i: Integer;
begin
  Assert(AGeometry.Count > 0);
  _SaveSinglePolygon(AGeometry.Item[0], AStringList, AFormatSettings, AStartIndex);
  for i := 1 to AGeometry.Count - 1 do begin
    AStringList.Add('PointLon_' + IntToStr(AStartIndex) + '=NaN');
    AStringList.Add('PointLat_' + IntToStr(AStartIndex) + '=NaN');
    Inc(AStartIndex);
    _SaveSinglePolygon(AGeometry.Item[i], AStringList, AFormatSettings, AStartIndex);
  end;
end;

procedure TLastSelectionInfoSaver._SavePolygon(
  const AGeometry: IGeometryLonLatPolygon;
  const AStringList: TStringList;
  const AFormatSettings: TFormatSettings
);
var
  VStartIndex: Integer;
  VSinglePolygon: IGeometryLonLatSinglePolygon;
  VMultiPolygon: IGeometryLonLatMultiPolygon;
begin
  VStartIndex := 1;
  if Supports(AGeometry, IGeometryLonLatSinglePolygon, VSinglePolygon) then begin
    _SaveSinglePolygon(VSinglePolygon, AStringList, AFormatSettings, VStartIndex);
  end else if Supports(AGeometry, IGeometryLonLatMultiPolygon, VMultiPolygon) then begin
    _SaveMultiPolygon(VMultiPolygon, AStringList, AFormatSettings, VStartIndex);
  end else begin
    Assert(False);
  end;
end;

end.
