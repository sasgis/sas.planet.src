unit u_LastSelectionInfoSaver;

interface

uses
  Classes,
  i_NotifierOperation,
  i_Listener,
  i_PathConfig,
  i_SimpleFlag,
  i_VectorItemsFactory,
  i_LastSelectionInfo,
  u_BackgroundTask;

type
  TLastSelectionInfoSaver = class(TBackgroundTask)
  private
    FLastSelection: ILastSelectionInfo;
    FFileName: IPathConfig;
    FVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;

    FListener: IListener;
    FNeedReadFlag: ISimpleFlag;
    FNeedWriteFlag: ISimpleFlag;
    procedure OnNeedSave;
    procedure ProcessSave(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
      const ALastSelection: ILastSelectionInfo;
      const AFileName: IPathConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  SysUtils,
  IniFiles,
  i_ThreadConfig,
  i_VectorItemLonLat,
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
  const AVectorGeometryLonLatFactory: IVectorGeometryLonLatFactory;
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

procedure TLastSelectionInfoSaver.ProcessSave(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation);
var
  VFileName: string;
  VPath: string;
  VZoom: Byte;
  VPolygon: IGeometryLonLatMultiPolygon;
  VNeedRead: Boolean;
  VNeedWrite: Boolean;
  VIniFile: TMemIniFile;
  VProvider: IConfigDataWriteProvider;
  VStringList: TStringList;
  VFormatSettings: TFormatSettings;
  VEnum: IEnumDoublePoint;
  i: Integer;
  VPoint: TDoublePoint;
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
        if VPolygon.Count > 0 then begin
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
    FLastSelection.LockRead;
    try
      VZoom := FLastSelection.Zoom;
      VPolygon := FLastSelection.Polygon;
    finally
      FLastSelection.UnlockRead;
    end;
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
        VStringList.Add('Zoom='+IntToStr(VZoom));
        VEnum := VPolygon.GetEnum;
        i := 1;
        while VEnum.Next(VPoint) do begin
          VStringList.Add('PointLon_' + IntToStr(i)+ '=' +  FloatToStr(VPoint.X, VFormatSettings));
          VStringList.Add('PointLat_' + IntToStr(i)+ '=' +  FloatToStr(VPoint.Y, VFormatSettings));
          Inc(i);
        end;
        VStringList.SaveToFile(VFileName);
      finally
        VStringList.Free;
      end;
    except
    end;
  end;
end;

end.
