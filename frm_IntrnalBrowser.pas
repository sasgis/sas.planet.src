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

unit frm_IntrnalBrowser;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  OleCtrls,
  SysUtils,
  EwbCore,
  EmbeddedWB,
  SHDocVw_EWB,
  u_CommonFormAndFrameParents;

type
  TfrmIntrnalBrowser = class(TCommonFormParent)
    EmbeddedWB1: TEmbeddedWB;
    procedure EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB;
      var hwnd: HWND; var szUserName, szPassWord: WideString;
      var Rezult: HRESULT);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EmbeddedWB1KeyDown(Sender: TObject; var Key: Word;
      ScanCode: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  public
    procedure showmessage(ACaption, AText: string);
    procedure Navigate(ACaption, AUrl: string);
  end;

var
  frmIntrnalBrowser: TfrmIntrnalBrowser;

implementation

uses
  u_ResStrings,
  i_ProxySettings,
  u_GlobalState;

{$R *.dfm}

procedure TfrmIntrnalBrowser.EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
var
  VProxyConfig: IProxyConfigStatic;
  VUseLogin: Boolean;
begin
  VProxyConfig := GState.InetConfig.ProxyConfig.GetStatic;
  VUselogin := (not VProxyConfig.UseIESettings) and VProxyConfig.UseProxy and VProxyConfig.UseLogin;
  if VUselogin then begin
    szUserName := VProxyConfig.Login;
    szPassWord := VProxyConfig.Password;
  end;
end;

procedure TfrmIntrnalBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EmbeddedWB1.Stop;
end;

procedure TfrmIntrnalBrowser.FormCreate(Sender: TObject);
begin
  EmbeddedWB1.Navigate('about:blank');
end;

procedure TfrmIntrnalBrowser.Navigate(ACaption, AUrl: string);
begin
  EmbeddedWB1.HTMLCode.Text:=SAS_STR_WiteLoad;
  Caption:=StringReplace(ACaption,#13#10,', ',[rfReplaceAll]);
  show;
  EmbeddedWB1.Navigate(AUrl);
end;

procedure TfrmIntrnalBrowser.showmessage(ACaption,AText: string);
begin
  EmbeddedWB1.GoAboutBlank;
  EmbeddedWB1.HTMLCode.Text:=AText;
  Caption:=StringReplace(ACaption,#13#10,', ',[rfReplaceAll]);
  show;
end;

procedure TfrmIntrnalBrowser.EmbeddedWB1KeyDown(Sender: TObject; var Key: Word;
  ScanCode: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then begin
    close;
  end;
end;

end.
