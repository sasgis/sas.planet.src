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

unit u_NotifierFactorySimple;

interface

uses
  i_InternalPerformanceCounter,
  i_ReadWriteSyncFactory,
  i_Notifier,
  i_NotifierFactory,
  u_BaseInterfacedObject;

type
  TNotifierFactorySimple = class(TBaseInterfacedObject, INotifierFactory)
  private
    FSyncFactory: IReadWriteSyncFactory;
  private
    function Make(const AName: string): INotifierInternal;
  public
    constructor Create(
      const ASyncFactory: IReadWriteSyncFactory
    );
  end;

  TNotifierFactoryWrapperWithCounter = class(TBaseInterfacedObject, INotifierFactory)
  private
    FSource: INotifierFactory;
    FCounter: IInternalPerformanceCounter;
  private
    function Make(const AName: string): INotifierInternal;
  public
    constructor Create(
      const ASource: INotifierFactory;
      const ACounter: IInternalPerformanceCounter
    );
  end;


implementation

uses
  u_Notifier;

{ TNotifierFactorySimple }

constructor TNotifierFactorySimple.Create(
  const ASyncFactory: IReadWriteSyncFactory);
begin
  inherited Create;
  FSyncFactory := ASyncFactory;
end;

function TNotifierFactorySimple.Make(const AName: string): INotifierInternal;
begin
  Result := TNotifierBase.Create;
end;

{ TNotifierFactoryWrapperWithCounter }

constructor TNotifierFactoryWrapperWithCounter.Create(
  const ASource: INotifierFactory;
  const ACounter: IInternalPerformanceCounter
);
begin
  Assert(ASource <> nil);
  Assert(ACounter <> nil);
  inherited Create;
  FSource := ASource;
  FCounter := ACounter;
end;

function TNotifierFactoryWrapperWithCounter.Make(
  const AName: string
): INotifierInternal;
var
  VContext: TInternalPerformanceCounterContext;
begin
  VContext := FCounter.StartOperation;
  try
    Result := FSource.Make(AName);
  finally
    FCounter.FinishOperation(VContext);
  end;
end;

end.
