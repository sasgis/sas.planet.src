{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit frm_InvisibleBrowser;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  SyncObjs,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  OleCtrls,
  SHDocVw_EWB,
  EwbCore,
  EmbeddedWB,
  i_ProxySettings;

type
  TfrmInvisibleBrowser = class(TForm)
    WebBrowser1: TEmbeddedWB;
    procedure FormCreate(Sender: TObject);
    procedure WebBrowser1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
  private
    FCS: TCriticalSection;
    FProxyConfig: IProxyConfig;
  public
    constructor Create(AOwner: TComponent; AProxyConfig: IProxyConfig); reintroduce;
    destructor Destroy; override;
    procedure NavigateAndWait(AUrl: WideString);
  end;

implementation

{$R *.dfm}

uses
  StrUtils,
  ShellAPI,
  WinInet;

constructor TfrmInvisibleBrowser.Create(AOwner: TComponent; AProxyConfig: IProxyConfig);
begin
  inherited Create(AOwner);
  FProxyConfig := AProxyConfig;
  FCS := TCriticalSection.Create;
end;

destructor TfrmInvisibleBrowser.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

procedure TfrmInvisibleBrowser.FormCreate(Sender: TObject);
begin
  WebBrowser1.Navigate('about:blank');
end;

{ TfrmInvisibleBrowser }

procedure TfrmInvisibleBrowser.NavigateAndWait(AUrl: WideString);
begin
  FCS.Acquire;
  try
    WebBrowser1.NavigateWait(AUrl, 10000);
  finally
    FCS.Release;
  end;
end;

procedure TfrmInvisibleBrowser.WebBrowser1Authenticate(
  Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName,
  szPassWord: WideString; var Rezult: HRESULT);
var
  VUseLogin: Boolean;
  VProxyConfig: IProxyConfigStatic;
begin
  if FProxyConfig <> nil then begin
    VProxyConfig := FProxyConfig.GetStatic;
    VUselogin := (not VProxyConfig.UseIESettings) and VProxyConfig.UseProxy and VProxyConfig.UseLogin;
    if VUselogin then begin
      szUserName := VProxyConfig.Login;
      szPassWord := VProxyConfig.Password;
    end;
  end;
end;

end.
