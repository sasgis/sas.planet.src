{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ElevationProfilePresenterOnPanel;

interface

uses
  Controls,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_ElevationProfilePresenter,
  i_LanguageManager,
  u_BaseInterfacedObject;

type
  TElevationProfilePresenterOnPanel = class(TBaseInterfacedObject, IElevationProfilePresenter)
  private
    FDrawParent: TWinControl;
    FLanguageManager: ILanguageManager;
    FGeometryLonLatFactory: IGeometryLonLatFactory;

    procedure HideParent;
  private
    { IElevationProfilePresenter }
    procedure ShowProfile(const ALine: IGeometryLonLatLine);
  public
    constructor Create(
      const ADrawParent: TWinControl;
      const ALanguageManager: ILanguageManager;
      const AGeometryLonLatFactory: IGeometryLonLatFactory
    );
    destructor Destroy; override;
  end;

implementation

{ TElevationProfilePresenterOnPanel }

constructor TElevationProfilePresenterOnPanel.Create(
  const ADrawParent: TWinControl;
  const ALanguageManager: ILanguageManager;
  const AGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;

  FDrawParent := ADrawParent;
  FLanguageManager := ALanguageManager;
  FGeometryLonLatFactory := AGeometryLonLatFactory;

  HideParent;
end;

destructor TElevationProfilePresenterOnPanel.Destroy;
begin

  inherited Destroy;
end;

procedure TElevationProfilePresenterOnPanel.HideParent;
begin
  FDrawParent.Visible := False;
end;

procedure TElevationProfilePresenterOnPanel.ShowProfile(
  const ALine: IGeometryLonLatLine
);
begin
  // ToDo:

  FDrawParent.Visible := True;
end;

end.
