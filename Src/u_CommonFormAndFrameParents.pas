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

unit u_CommonFormAndFrameParents;

interface

uses
  Messages,
  Classes,
  Forms,
  i_Notifier,
  i_Listener,
  i_LanguageManager;

type
  TBaseForm = class(TForm)
  public
    procedure WMSetIcon(var Message: TWMSetIcon); message WM_SETICON;
  end;

  TCommonFormParent = class(TBaseForm)
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshTranslation; virtual;
  end;

  TFormWitghLanguageManager = class(TBaseForm)
  private
    FLanguageChangeListener: IListener;
    FLanguageManager: ILanguageManager;
    procedure OnLangChange;
  protected
    procedure RefreshTranslation; virtual;
  protected
    property LanguageManager: ILanguageManager read FLanguageManager;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create(const ALanguageManager: ILanguageManager); reintroduce;
  end;

  TCommonFrameParent = class(Forms.TFrame)
  private
    FLanguageChangeListener: IListener;
    FLanguageManager: ILanguageManager;
    procedure OnLangChange;
  protected
    procedure RefreshTranslation; virtual;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  public
    constructor Create(const ALanguageManager: ILanguageManager); reintroduce;
  end;

  TFrame = class(TCommonFrameParent);

implementation

uses
  gnugettext,
  u_ListenerByEvent;

{ TBaseForm }

procedure TBaseForm.WMSetIcon(var Message: TWMSetIcon);
begin
  if (csDesigning in ComponentState) or not (csDestroying in ComponentState) then begin
    inherited;
  end;
end;

{ TCommonFormParent }

constructor TCommonFormParent.Create(AOwner: TComponent);
begin
  inherited;
  TranslateComponent(self);
end;

procedure TCommonFormParent.RefreshTranslation;
begin
  ReTranslateComponent(self);
end;

{ TFrame }

constructor TCommonFrameParent.Create(const ALanguageManager: ILanguageManager);
begin
  Assert(ALanguageManager <> nil);
  inherited Create(nil);
  TranslateComponent(self);
  FLanguageManager := ALanguageManager;
  FLanguageChangeListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
end;

procedure TCommonFrameParent.AfterConstruction;
begin
  inherited;
  FLanguageManager.ChangeNotifier.Add(FLanguageChangeListener);
end;

procedure TCommonFrameParent.BeforeDestruction;
begin
  inherited;
  if Assigned(FLanguageManager) and Assigned(FLanguageChangeListener) then begin
    FLanguageManager.ChangeNotifier.Remove(FLanguageChangeListener);
  end;
end;

procedure TCommonFrameParent.OnLangChange;
begin
  RefreshTranslation;
end;

procedure TCommonFrameParent.RefreshTranslation;
begin
  ReTranslateComponent(self);
end;

{ TFormWitghLanguageManager }

constructor TFormWitghLanguageManager.Create(
  const ALanguageManager: ILanguageManager
);
begin
  Assert(ALanguageManager <> nil);
  inherited Create(nil);
  TranslateComponent(self);
  FLanguageManager := ALanguageManager;
  FLanguageChangeListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
end;

procedure TFormWitghLanguageManager.AfterConstruction;
begin
  inherited;
  FLanguageManager.ChangeNotifier.Add(FLanguageChangeListener);
end;

procedure TFormWitghLanguageManager.BeforeDestruction;
begin
  inherited;
  if Assigned(FLanguageManager) and Assigned(FLanguageChangeListener) then begin
    FLanguageManager.ChangeNotifier.Remove(FLanguageChangeListener);
  end;
end;

procedure TFormWitghLanguageManager.OnLangChange;
begin
  RefreshTranslation;
end;

procedure TFormWitghLanguageManager.RefreshTranslation;
begin
  ReTranslateComponent(self);
end;

end.
