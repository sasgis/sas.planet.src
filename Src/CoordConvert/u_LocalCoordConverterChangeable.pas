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

unit u_LocalCoordConverterChangeable;

interface

uses
  SysUtils,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  u_ChangeableBase;

type
  TLocalCoordConverterChangeable = class(TChangeableBase, ILocalCoordConverterChangeable, ILocalCoordConverterChangeableInternal)
  private
    FConverter: ILocalCoordConverter;
    FCS: IReadWriteSync;
    FChangeCounter: IInternalPerformanceCounter;
  private
    function GetStatic: ILocalCoordConverter;
    procedure SetConverter(const AValue: ILocalCoordConverter);
  protected
    procedure DoChangeNotify; override;
  public
    constructor Create(
      const ASource: ILocalCoordConverter;
      const AChangeCounter: IInternalPerformanceCounter
    );
  end;

implementation

uses
  u_Synchronizer;

{ TLocalCoordConverterChangeable }

constructor TLocalCoordConverterChangeable.Create(
  const ASource: ILocalCoordConverter;
  const AChangeCounter: IInternalPerformanceCounter
);
var
  VCS: IReadWriteSync;
begin
  VCS := GSync.SyncVariable.Make(ClassName);
  inherited Create(VCS);
  FCS := VCS;
  FConverter := ASource;
  FChangeCounter := AChangeCounter;
end;

procedure TLocalCoordConverterChangeable.DoChangeNotify;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FChangeCounter.StartOperation;
  try
    inherited;
  finally
    FChangeCounter.FinishOperation(VCounterContext);
  end;
end;

function TLocalCoordConverterChangeable.GetStatic: ILocalCoordConverter;
begin
  FCS.BeginRead;
  try
    Result := FConverter;
  finally
    FCS.EndRead;
  end;
end;

procedure TLocalCoordConverterChangeable.SetConverter(
  const AValue: ILocalCoordConverter
);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  FCS.BeginWrite;
  try
    if (Assigned(FConverter) and not FConverter.GetIsSameConverter(AValue))
      or (Assigned(AValue) and not Assigned(FConverter))
    then begin
      FConverter := AValue;
      VNeedNotify := True;
    end;
  finally
    FCS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

end.
