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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BackgroundTaskLayerDrawBase;

interface

uses
  i_JclNotify,
  i_OperationNotifier,
  i_ThreadConfig,
  u_BackgroundTask;

type
  TBgPaintLayerEvent =
    procedure(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ) of object;

  TBackgroundTaskLayerDrawBase = class(TBackgroundTask)
  private
    FOnBgPaintLayer: TBgPaintLayerEvent;
  protected
    procedure ExecuteTask(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier
    ); override;
  public
    constructor Create(
      const AAppClosingNotifier: IJclNotifier;
      AOnBgPaintLayer: TBgPaintLayerEvent;
      const AThreadConfig: IThreadConfig
    );
  end;

implementation

{ TBackgroundTaskLayerDrawBase }

constructor TBackgroundTaskLayerDrawBase.Create(
  const AAppClosingNotifier: IJclNotifier;
  AOnBgPaintLayer: TBgPaintLayerEvent;
  const AThreadConfig: IThreadConfig
);
begin
  inherited Create(AAppClosingNotifier, AThreadConfig);
  FOnBgPaintLayer := AOnBgPaintLayer;
end;

procedure TBackgroundTaskLayerDrawBase.ExecuteTask(
  AOperationID: Integer;
  const ACancelNotifier: IOperationNotifier
);
begin
  inherited;
  if Assigned(FOnBgPaintLayer) then begin
    FOnBgPaintLayer(AOperationID, ACancelNotifier);
  end;
end;

end.
