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
  i_SimpleFlag,
  i_InternalPerformanceCounter,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  u_ConfigDataElementBase;

type
  TLocalCoordConverterChangeable = class(TConfigDataElementWithStaticBaseEmptySaveLoad, ILocalCoordConverterChangeable, ILocalCoordConverterChangeableInternal)
  private
    FConverter: ILocalCoordConverter;
    FChangeCounter: IInternalPerformanceCounter;
  private
    function GetStatic: ILocalCoordConverter;
    procedure SetConverter(const AValue: ILocalCoordConverter);
  protected
    function CreateStatic: IInterface; override;
    procedure DoChangeNotify; override;
  public
    constructor Create(
      const AChangedFlag: ISimpleFlag;
      const ASource: ILocalCoordConverter;
      const AChangeCounter: IInternalPerformanceCounter
    );
  end;

implementation

{ TLocalCoordConverterChangeable }

constructor TLocalCoordConverterChangeable.Create(
  const AChangedFlag: ISimpleFlag;
  const ASource: ILocalCoordConverter;
  const AChangeCounter: IInternalPerformanceCounter
);
begin
  inherited Create(AChangedFlag);
  FConverter := ASource;
  FChangeCounter := AChangeCounter;
end;

function TLocalCoordConverterChangeable.CreateStatic: IInterface;
begin
  Result := FConverter;
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
  Result := ILocalCoordConverter(GetStaticInternal);
end;

procedure TLocalCoordConverterChangeable.SetConverter(
  const AValue: ILocalCoordConverter);
begin
  LockWrite;
  try
    if (Assigned(FConverter) and not FConverter.GetIsSameConverter(AValue))
      or (Assigned(AValue) and not Assigned(FConverter))
    then begin
      FConverter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
