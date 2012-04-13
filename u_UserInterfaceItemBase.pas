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
  i_JclListenerNotifierLinksList,
  i_LanguageManager,
  u_ConfigDataElementBase;

type
  TUserInterfaceItemBase = class(TConfigDataElementBaseEmptySaveLoad)
  private
    FGUID: TGUID;
    FCaption: string;
    FDescription: string;
    FMenuItemName: string;

    FLinksList: IJclListenerNotifierLinksList;
    procedure OnLangChange;
  protected
    function GetCaptionTranslated: string; virtual; abstract;
    function GetDescriptionTranslated: string; virtual; abstract;
    function GetMenuItemNameTranslated: string; virtual; abstract;

    property LinksList: IJclListenerNotifierLinksList read FLinksList;
  protected
    function GetGUID: TGUID;
    function GetCaption: string;
    function GetDescription: string;
    function GetMenuItemName: string;
  public
    constructor Create(
      const AGUID: TGUID;
      const ALanguageManager: ILanguageManager
    );
  end;

implementation

uses
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener;

{ TUserInterfaceItemBase }

constructor TUserInterfaceItemBase.Create(
  const AGUID: TGUID;
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create;
  FGUID := AGUID;

  FLinksList := TJclListenerNotifierLinksList.Create;
  FLinksList.ActivateLinks;
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLangChange),
    ALanguageManager.GetChangeNotifier
  );
  OnLangChange;
end;

function TUserInterfaceItemBase.GetCaption: string;
begin
  LockRead;
  try
    Result := FCaption;
  finally
    UnlockRead;
  end;
end;

function TUserInterfaceItemBase.GetDescription: string;
begin
  LockRead;
  try
    Result := FDescription;
  finally
    UnlockRead;
  end;
end;

function TUserInterfaceItemBase.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TUserInterfaceItemBase.GetMenuItemName: string;
begin
  LockRead;
  try
    Result := FMenuItemName;
  finally
    UnlockRead;
  end;
end;

procedure TUserInterfaceItemBase.OnLangChange;
begin
  LockWrite;
  try
    FCaption := GetCaptionTranslated;
    FDescription := GetDescriptionTranslated;
    FMenuItemName := GetMenuItemNameTranslated;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
