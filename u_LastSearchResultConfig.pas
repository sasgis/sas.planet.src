unit u_LastSearchResultConfig;

interface

uses
  i_LastSearchResultConfig,
  i_GeoCoder,
  t_GeoTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TLastSearchResultConfig = class(TConfigDataElementBase, ILastSearchResultConfig)
  private
    FIsActive: Boolean;
    FGeoCodeResult: IGeoCodeResult;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsActive: Boolean;
    function GetGeoCodeResult:IGeoCodeResult;
    procedure SetGeoCodeResult(const AValue: IGeoCodeResult);
    procedure ClearGeoCodeResult;
  end;

implementation


procedure TLastSearchResultConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
end;

procedure TLastSearchResultConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
end;

function TLastSearchResultConfig.GetGeoCodeResult:IGeoCodeResult;
begin
  LockRead;
  try
    Result := FGeoCodeResult;
  finally
    UnlockRead;
  end;
end;

function TLastSearchResultConfig.GetIsActive: Boolean;
begin
  LockRead;
  try
    Result := FIsActive;
  finally
    UnlockRead;
  end;
end;

procedure TLastSearchResultConfig.SetGeoCodeResult(const AValue: IGeoCodeResult);
begin
  LockWrite;
  try
    if FGeoCodeResult <> AValue then begin
      FIsActive := True;
      FGeoCodeResult:=AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TLastSearchResultConfig.ClearGeoCodeResult;
begin
  LockWrite;
  try
    FIsActive := false;
    FGeoCodeResult:=nil;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;


end.
