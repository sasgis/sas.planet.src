{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_VectorTileProviderChangeableForLastSearchResult;

interface

uses
  Types,
  SysUtils,
  i_VectorTileProvider,
  i_VectorTileProviderChangeable,
  i_Listener,
  i_LastSearchResult,
  i_VectorItemSubsetBuilder,
  u_ChangeableBase;

type
  TVectorTileProviderChangeableForLastSearchResult = class(TChangeableWithSimpleLockBase, IVectorTileUniProviderChangeable)
  private
    FSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FSource: ILastSearchResult;
    FItemSelectOversize: TRect;

    FSourceListener: IListener;

    FResult: IVectorTileUniProvider;

    procedure OnSourceChange;
  private
    function GetStatic: IVectorTileUniProvider;
  public
    constructor Create(
      const ASource: ILastSearchResult;
      const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AItemSelectOversize: TRect
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_VectorItemSubset,
  u_ListenerByEvent,
  u_VectorTileProviderByFixedSubset;

{ TVectorLayerProviderChangeableForMainLayer }

constructor TVectorTileProviderChangeableForLastSearchResult.Create(
  const ASource: ILastSearchResult;
  const ASubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AItemSelectOversize: TRect
);
begin
  Assert(Assigned(ASource));
  Assert(Assigned(ASubsetBuilderFactory));
  Assert(AItemSelectOversize.Left >= 0);
  Assert(AItemSelectOversize.Left < 4096);
  Assert(AItemSelectOversize.Top >= 0);
  Assert(AItemSelectOversize.Top < 4096);
  Assert(AItemSelectOversize.Right >= 0);
  Assert(AItemSelectOversize.Right < 4096);
  Assert(AItemSelectOversize.Bottom >= 0);
  Assert(AItemSelectOversize.Bottom < 4096);
  inherited Create;
  FSubsetBuilderFactory := ASubsetBuilderFactory;
  FSource := ASource;
  FItemSelectOversize := AItemSelectOversize;

  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FSource.ChangeNotifier.Add(FSourceListener);
  OnSourceChange;
end;

destructor TVectorTileProviderChangeableForLastSearchResult.Destroy;
begin
  if Assigned(FSource) and Assigned(FSourceListener) then begin
    FSource.ChangeNotifier.Remove(FSourceListener);
    FSource := nil;
    FSourceListener := nil;
  end;
  inherited;
end;

function TVectorTileProviderChangeableForLastSearchResult.GetStatic: IVectorTileUniProvider;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TVectorTileProviderChangeableForLastSearchResult.OnSourceChange;
var
  VSource: IVectorItemSubset;
  VResult: IVectorTileUniProvider;
begin
  VSource := FSource.GeoCodeResult;
  VResult := nil;
  if Assigned(VSource) and (VSource.Count > 0) then begin
    VResult :=
      TVectorTileProviderByFixedSubset.Create(
        FSubsetBuilderFactory,
        FItemSelectOversize,
        VSource
      );
  end;

  CS.BeginWrite;
  try
    FResult := VResult;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

end.
