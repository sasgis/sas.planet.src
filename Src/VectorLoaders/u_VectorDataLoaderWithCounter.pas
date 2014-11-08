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

unit u_VectorDataLoaderWithCounter;

interface

uses
  i_InternalPerformanceCounter,
  i_BinaryData,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorDataLoader,
  u_BaseInterfacedObject;

type
  TVectorDataLoaderWithCounter = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FLoader: IVectorDataLoader;
    FLoadCounter: IInternalPerformanceCounter;
  private
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AFactory: IVectorDataItemMainInfoFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const ALoader: IVectorDataLoader;
      const ALoadCounter: IInternalPerformanceCounter
    );
  end;

implementation

{ TVectorDataLoaderWithCounter }

constructor TVectorDataLoaderWithCounter.Create(
  const ALoader: IVectorDataLoader;
  const ALoadCounter: IInternalPerformanceCounter
);
begin
  Assert(Assigned(ALoader));
  Assert(Assigned(ALoadCounter));
  inherited Create;
  FLoader := ALoader;
  FLoadCounter := ALoadCounter;
end;

function TVectorDataLoaderWithCounter.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AFactory: IVectorDataItemMainInfoFactory
): IVectorItemSubset;
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  Result := nil;
  VCounterContext := FLoadCounter.StartOperation;
  try
    // read from single simple source
    Result :=
      FLoader.Load(
        AData,
        AIdData,
        AFactory
      )
  finally
    FLoadCounter.FinishOperation(VCounterContext);
  end;
end;

end.
