{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_ActionListByLanguageManager;

interface

uses
  Classes,
  ActnList,
  Actions,
  i_Listener,
  i_LanguageManager;

type
  TActionListByLanguageManager = class(TActionList)
  private
    FLanguageManager: ILanguageManager;
    FListener: IListener;
    procedure OnLangChange;
    procedure OnItemExecute(Sender: TObject);
  public
    constructor Create(
      const AOwner: TComponent;
      const ALanguageManager: ILanguageManager
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  i_LanguageListStatic,
  u_ListenerByEvent;

{ TActionListByLanguageManager }

constructor TActionListByLanguageManager.Create(
  const AOwner: TComponent;
  const ALanguageManager: ILanguageManager
);
var
  VList: ILanguageListStatic;
  i: Integer;
  VAction: TAction;
begin
  Assert(Assigned(ALanguageManager));
  inherited Create(AOwner);
  FLanguageManager := ALanguageManager;

  VList := FLanguageManager.LanguageList;
  for i := 0 to VList.Count - 1 do begin
    VAction := TAction.Create(Self);
    VAction.Caption := FLanguageManager.GetLangNameByIndex(i);
    VAction.Tag := i;
    VAction.OnExecute := Self.OnItemExecute;
    VAction.ActionList := Self;
  end;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FListener);

  OnLangChange;
end;

destructor TActionListByLanguageManager.Destroy;
begin
  if Assigned(FLanguageManager) and Assigned(FListener) then begin
    FLanguageManager.GetChangeNotifier.Remove(FListener);
    FListener := nil;
    FLanguageManager := nil;
  end;
  inherited;
end;

procedure TActionListByLanguageManager.OnItemExecute(Sender: TObject);
begin
  Assert(Assigned(Sender));
  FLanguageManager.SetCurrentLanguageIndex(TComponent(Sender).Tag);
end;

procedure TActionListByLanguageManager.OnLangChange;
var
  VIndex: Integer;
  i: Integer;
begin
  VIndex := FLanguageManager.GetCurrentLanguageIndex;
  Assert(VIndex >= 0);
  Assert(VIndex < Self.ActionCount);
  for i := 0 to ActionCount - 1 do begin
    TCustomAction(Actions[i]).Checked := i = VIndex;
  end;
end;

end.
