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

unit u_VectorItemSubsetChangeableBySearchResult;

interface


uses
  SysUtils,
  i_VectorItemSubset,
  i_VectorItemSubsetChangeable,
  i_LastSearchResultConfig,
  i_Listener,
  u_ChangeableBase;

type
  TVectorItemSubsetChangeableBySearchResult = class(TChangeableBase, IVectorItemSubsetChangeable)
  private
    FLastSearchResults: ILastSearchResultConfig;
    FSearchResultListener: IListener;

    FResultCS: IReadWriteSync;
    FResult: IVectorItemSubset;
    procedure OnSearchResultChange;
  private
    function GetStatic: IVectorItemSubset;
  public
    constructor Create(
      const ALastSearchResults: ILastSearchResultConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_Synchronizer;

{ TVectorItemSubsetChangeableBySearchResult }

constructor TVectorItemSubsetChangeableBySearchResult.Create(
  const ALastSearchResults: ILastSearchResultConfig
);
begin
  Assert(Assigned(ALastSearchResults));
  inherited Create;
  FLastSearchResults := ALastSearchResults;

  FResultCS := MakeSyncRW_Var(Self, False);
  FSearchResultListener := TNotifyNoMmgEventListener.Create(Self.OnSearchResultChange);
  FLastSearchResults.ChangeNotifier.Add(FSearchResultListener);
end;

destructor TVectorItemSubsetChangeableBySearchResult.Destroy;
begin
  if Assigned(FLastSearchResults) and Assigned(FSearchResultListener) then begin
    FLastSearchResults.ChangeNotifier.Remove(FSearchResultListener);
    FLastSearchResults := nil;
    FSearchResultListener := nil;
  end;

  inherited;
end;

function TVectorItemSubsetChangeableBySearchResult.GetStatic: IVectorItemSubset;
begin
  FResultCS.BeginRead;
  try
    Result := FResult;
  finally
    FResultCS.EndRead;
  end;
end;

procedure TVectorItemSubsetChangeableBySearchResult.OnSearchResultChange;
var
  VNewResult: IVectorItemSubset;
  VNeedNotify: Boolean;
begin
  VNewResult := nil;
  FLastSearchResults.LockRead;
  try
    if FLastSearchResults.IsActive then begin
      VNewResult := FLastSearchResults.GeoCodeResult;
    end;
  finally
    FLastSearchResults.UnlockRead;
  end;

  FResultCS.BeginWrite;
  try
    if FResult <> nil then begin
      VNeedNotify := not FResult.IsEqual(VNewResult);
    end else begin
      VNeedNotify := Assigned(VNewResult);
    end;
    if VNeedNotify then begin
      FResult := VNewResult;
    end;
  finally
    FResultCS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
