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

unit u_BitmapLayerProviderChangeableBase;

interface

uses
  i_BitmapLayerProvider,
  i_BitmapLayerProviderChangeable,
  i_ListenerNotifierLinksList,
  u_ConfigDataElementBase;

type
  TBitmapLayerProviderChangeableBase = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IBitmapLayerProviderChangeable)
  private
    FLinksList: IListenerNotifierLinksList;
  private
    function GetStatic: IBitmapLayerProvider;

  protected
    property LinksList: IListenerNotifierLinksList read FLinksList;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create;
  end;

implementation

uses
  u_ListenerNotifierLinksList;

{ TBitmapLayerProviderChangeableBase }

constructor TBitmapLayerProviderChangeableBase.Create;
begin
  inherited Create;

  FLinksList := TListenerNotifierLinksList.Create;
end;

procedure TBitmapLayerProviderChangeableBase.AfterConstruction;
begin
  inherited;
  FLinksList.ActivateLinks;
end;

procedure TBitmapLayerProviderChangeableBase.BeforeDestruction;
begin
  inherited;
  FLinksList.DeactivateLinks;
end;

function TBitmapLayerProviderChangeableBase.GetStatic: IBitmapLayerProvider;
begin
  Result := IBitmapLayerProvider(GetStaticInternal);
end;

end.
