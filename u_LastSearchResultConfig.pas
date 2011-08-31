unit u_LastSearchResultConfig;

interface

uses
  i_LastSearchResultConfig,
  i_GeoCoder,
  u_ConfigDataElementBase;

type
  TLastSearchResultConfig = class(TConfigDataElementBaseEmptySaveLoad, ILastSearchResultConfig)
  private
    FIsActive: Boolean;
    FGeoCodeResult: IGeoCodeResult;
  protected
    function GetIsActive: Boolean;
    function GetGeoCodeResult:IGeoCodeResult;
    procedure SetGeoCodeResult(const AValue: IGeoCodeResult);
    procedure ClearGeoCodeResult;
  end;

implementation

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
