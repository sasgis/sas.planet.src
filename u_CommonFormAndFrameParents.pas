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

unit u_CommonFormAndFrameParents;

interface

uses
  Classes,
  Forms,
  i_Notifier,
  i_Listener,
  i_LanguageManager;

type
  TCommonFormParent = class(TForm)
  public
    constructor Create(AOwner: TComponent); override;
    procedure RefreshTranslation; virtual;
  end;

  TFormWitghLanguageManager = class(TForm)
  private
    FLanguageChangeListener: IListener;
    FLanguageManager: ILanguageManager;
    procedure OnLangChange;
  protected
    procedure RefreshTranslation; virtual;
  protected
    property LanguageManager: ILanguageManager read FLanguageManager;
  public
    constructor Create(const ALanguageManager: ILanguageManager); reintroduce;
    destructor Destroy; override;
  end;

  TCommonFrameParent = class(Forms.TFrame)
  private
    FLanguageChangeListener: IListener;
    FLanguageManager: ILanguageManager;
    procedure OnLangChange;
  protected
    procedure RefreshTranslation; virtual;
  public
    constructor Create(const ALanguageManager: ILanguageManager); reintroduce;
    destructor Destroy; override;
  end;

  TFrame = class(TCommonFrameParent);

implementation

uses
  gnugettext,
  u_ListenerByEvent;

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
  FLanguageManager.ChangeNotifier.Add(FLanguageChangeListener);
end;

destructor TCommonFrameParent.Destroy;
begin
  FLanguageManager.ChangeNotifier.Remove(FLanguageChangeListener);
  FLanguageChangeListener := nil;
  FLanguageManager := nil;

  inherited;
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
  FLanguageManager.ChangeNotifier.Add(FLanguageChangeListener);
end;

destructor TFormWitghLanguageManager.Destroy;
begin
  FLanguageManager.ChangeNotifier.Remove(FLanguageChangeListener);
  FLanguageChangeListener := nil;
  FLanguageManager := nil;

  inherited;
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
