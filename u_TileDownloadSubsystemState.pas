unit u_TileDownloadSubsystemState;

interface

uses
  i_TileDownloaderState,
  i_MapAbilitiesConfig,
  u_ConfigDataElementComplexBase;

type
  TTileDownloadSubsystemState = class(TConfigDataElementComplexWithStaticBase, ITileDownloaderStateChangeble)
  private
    FZmpDownloadEnabled: Boolean;
    FSaverState: ITileDownloaderStateChangeble;
    FRequestBuilderState: ITileDownloaderStateChangeble;
    FMapAbilitiesConfig: IMapAbilitiesConfig;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetStatic: ITileDownloaderStateStatic;
  public
    constructor Create(
      AZmpDownloadEnabled: Boolean;
      const ARequestBuilderState: ITileDownloaderStateChangeble;
      const ASaverState: ITileDownloaderStateChangeble;
      const AMapAbilitiesConfig: IMapAbilitiesConfig
    );
  end;

implementation

uses
  u_TileDownloaderStateStatic;

{ TTileDownloadSubsystemState }

constructor TTileDownloadSubsystemState.Create(
  AZmpDownloadEnabled: Boolean;
  const ARequestBuilderState: ITileDownloaderStateChangeble;
  const ASaverState: ITileDownloaderStateChangeble;
  const AMapAbilitiesConfig: IMapAbilitiesConfig
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
end;

function TTileDownloadSubsystemState.CreateStatic: IInterface;
var
  VStatic: ITileDownloaderStateStatic;
  VState: ITileDownloaderStateStatic;
begin
  VStatic := ITileDownloaderStateStatic(GetStaticInternal);
  if not FZmpDownloadEnabled then begin
    if VStatic = nil then begin
      VStatic := TTileDownloaderStateStatic.Create(False, 'Disabled by Zmp');
    end;
  end else begin
    if not FMapAbilitiesConfig.UseDownload then begin
      VStatic := TTileDownloaderStateStatic.Create(False, 'Disabled by map params');
    end else begin
      VState := FRequestBuilderState.GetStatic;
      if not VState.Enabled then begin
        VStatic := TTileDownloaderStateStatic.Create(False, VState.DisableReason);
      end else begin
        VState := FSaverState.GetStatic;
        if not VState.Enabled then begin
          VStatic := TTileDownloaderStateStatic.Create(False, VState.DisableReason);
        end else begin
          VStatic := TTileDownloaderStateStatic.Create(True, '');
        end;
      end;
    end;
  end;
  Result := VStatic;
end;

function TTileDownloadSubsystemState.GetStatic: ITileDownloaderStateStatic;
begin
  Result := ITileDownloaderStateStatic(GetStaticInternal);
end;

end.
