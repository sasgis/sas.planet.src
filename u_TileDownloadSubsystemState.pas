unit u_TileDownloadSubsystemState;

interface

uses
  i_TileDownloaderState,
  i_MapAbilitiesConfig,
  u_ConfigDataElementComplexBase;

type
  TTileDownloadSubsystemState = class(TConfigDataElementComplexBase, ITileDownloaderStateChangeble)
  private
    FZmpDownloadEnabled: Boolean;
    FSaverState: ITileDownloaderStateChangeble;
    FRequestBuilderState: ITileDownloaderStateChangeble;
    FMapAbilitiesConfig: IMapAbilitiesConfig;
    FStatic: ITileDownloaderStateStatic;

    function CreateStatic: ITileDownloaderStateStatic;
  protected
    procedure DoBeforeChangeNotify; override;
  protected
    function GetStatic: ITileDownloaderStateStatic;
  public
    constructor Create(
      AZmpDownloadEnabled: Boolean;
      ARequestBuilderState: ITileDownloaderStateChangeble;
      ASaverState: ITileDownloaderStateChangeble;
      AMapAbilitiesConfig: IMapAbilitiesConfig
    );
  end;

implementation

uses
  u_TileDownloaderStateStatic;

{ TTileDownloadSubsystemState }

constructor TTileDownloadSubsystemState.Create(
  AZmpDownloadEnabled: Boolean;
  ARequestBuilderState: ITileDownloaderStateChangeble;
  ASaverState: ITileDownloaderStateChangeble;
  AMapAbilitiesConfig: IMapAbilitiesConfig
);
begin
  inherited Create;
  FZmpDownloadEnabled := AZmpDownloadEnabled;
  if FZmpDownloadEnabled then begin
    FSaverState := ASaverState;
    Add(FSaverState);

    FRequestBuilderState := ARequestBuilderState;
    Add(FRequestBuilderState);

    FMapAbilitiesConfig := AMapAbilitiesConfig;
    Add(FMapAbilitiesConfig);
  end;
  FStatic := CreateStatic;
end;

function TTileDownloadSubsystemState.CreateStatic: ITileDownloaderStateStatic;
var
  VState: ITileDownloaderStateStatic;
begin
  if not FZmpDownloadEnabled then begin
    if FStatic = nil then begin
      Result := TTileDownloaderStateStatic.Create(False, 'Disabled by Zmp');
    end else begin
      Result := FStatic;
    end;
  end else begin
    if not FMapAbilitiesConfig.UseDownload then begin
      Result := TTileDownloaderStateStatic.Create(False, 'Disabled by map params');
    end else begin
      VState := FRequestBuilderState.GetStatic;
      if not VState.Enabled then begin
        Result := TTileDownloaderStateStatic.Create(False, VState.DisableReason);
      end else begin
        VState := FSaverState.GetStatic;
        if not VState.Enabled then begin
          Result := TTileDownloaderStateStatic.Create(False, VState.DisableReason);
        end else begin
          Result := TTileDownloaderStateStatic.Create(True, '');
        end;
      end;
    end;
  end;
end;

procedure TTileDownloadSubsystemState.DoBeforeChangeNotify;
begin
  inherited;
  FStatic := CreateStatic;
end;

function TTileDownloadSubsystemState.GetStatic: ITileDownloaderStateStatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

end.
