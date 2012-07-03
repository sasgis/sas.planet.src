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

unit u_UserInterfaceItemBase;

interface

uses
  i_ListenerNotifierLinksList,
  i_StringConfigDataElement,
  u_ConfigDataElementBase;

type
  TUserInterfaceItemBase = class(TConfigDataElementBaseEmptySaveLoad)
  private
    FGUID: TGUID;
    FCaption: IStringConfigDataElement;
    FDescription: IStringConfigDataElement;
    FMenuItemName: IStringConfigDataElement;

    FLinksList: IListenerNotifierLinksList;
    procedure OnTextChange;
  protected
    property LinksList: IListenerNotifierLinksList read FLinksList;
  protected
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetDescription: string;
    function GetMenuItemName: string;
  public
    constructor Create(
      const AGUID: TGUID;
      const ACaption: IStringConfigDataElement;
      const ADescription: IStringConfigDataElement;
      const AMenuItemName: IStringConfigDataElement
    );
  end;

implementation

uses
  u_ListenerNotifierLinksList,
  u_ListenerByEvent;

{ TUserInterfaceItemBase }

constructor TUserInterfaceItemBase.Create(
  const AGUID: TGUID;
  const ACaption: IStringConfigDataElement;
  const ADescription: IStringConfigDataElement;
  const AMenuItemName: IStringConfigDataElement
);
begin
  inherited Create;
  FGUID := AGUID;
  FCaption := ACaption;
  FDescription := ADescription;
  FMenuItemName := AMenuItemName;
  FLinksList := TListenerNotifierLinksList.Create;
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTextChange),
    FCaption.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTextChange),
    FDescription.ChangeNotifier
  );
  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTextChange),
    FMenuItemName.ChangeNotifier
  );

  FLinksList.ActivateLinks;
end;

function TUserInterfaceItemBase.GetCaption: string;
begin
  Result := FCaption.Value;
end;

function TUserInterfaceItemBase.GetDescription: string;
begin
  Result := FDescription.Value;
end;

function TUserInterfaceItemBase.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TUserInterfaceItemBase.GetMenuItemName: string;
begin
  Result := FMenuItemName.Value;
end;

procedure TUserInterfaceItemBase.OnTextChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
