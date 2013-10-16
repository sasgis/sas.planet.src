{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2013, SAS.Planet development team.                      *}
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

unit frm_About;

interface

uses
  Types,
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  GR32_Image,
  i_ContentTypeManager,
  i_ConfigDataProvider,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TfrmAbout = class(TFormWitghLanguageManager)
    Bevel1: TBevel;
    btnClose: TButton;
    lblVersionCatpion: TLabel;
    lblProgramName: TLabel;
    lblVersion: TLabel;
    lblWebSite: TLabel;
    pnlBottom: TPanel;
    lblCopyright: TLabel;
    lblLicense: TLabel;
    imgLogo: TImage32;
    lblCompiler: TLabel;
    lblTimeStamp: TLabel;
    lblBuildInfo: TLabel;
    lblBuildTimeValue: TLabel;
    lblBuildInfoValue: TLabel;
    lblCompilerValue: TLabel;
    btnLicense: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblWebSiteClick(Sender: TObject);
    procedure btnLicenseClick(Sender: TObject);
  private
    FContentTypeManager: IContentTypeManager;
    FConfigData: IConfigDataProvider;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AContentTypeManager: IContentTypeManager;
      const AConfigData: IConfigDataProvider
    ); reintroduce;
  end;

implementation

uses
  SysUtils,
  ExeInfo,
  i_Bitmap32Static,
  u_ConfigProviderHelpers,
  u_BitmapFunc,
  u_InetFunc;

const
  cHomePage = 'http://sasgis.ru/';

resourcestring
  rsDevelopmentTeam = 'SAS.Planet Development Team';

{$R *.dfm}

function GetCompilerInfoStr: string;
begin
  {$IFDEF VER185} Result := 'CodeGear' + #153 +' Delphi' + #174 + ' 2007'; {$ENDIF}
  {$IFDEF VER230} Result := 'Embarcadero' + #153 +' Delphi' + #174 + ' XE2'; {$ENDIF}
end;

function GetUnicodeInfoStr: string;
begin
  {$IF CompilerVersion < 190}
    Result := 'Non-Unicode';
  {$ELSE}
    Result := 'Unicode';
  {$IFEND}
end;

constructor TfrmAbout.Create(
  const ALanguageManager: ILanguageManager;
  const AContentTypeManager: IContentTypeManager;
  const AConfigData: IConfigDataProvider
);
begin
  inherited Create(ALanguageManager);
  FContentTypeManager := AContentTypeManager;
  FConfigData := AConfigData;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  VBuildDate: TDateTime;
  VBitmapSize: TPoint;
  VBitmapStatic: IBitmap32Static;
begin
  VBuildDate := GetBuildDateTime;

  lblCopyright.Caption := 'Copyright ' + #169 + ' 2007-' + FormatDateTime('yyyy', VBuildDate) + ', ' + rsDevelopmentTeam;
  lblWebSite.Caption := cHomePage;

  lblVersion.Caption := GetBuildVersionInfo;
  lblBuildTimeValue.Caption := FormatDateTime('yyyy-mm-dd hh:mm:ss', VBuildDate) + ' UTC';
  lblBuildInfoValue.Caption := 'Windows, 32-bit, ' + GetUnicodeInfoStr {$IFDEF DEBUG} + ', Debug'{$ENDIF};
  lblCompilerValue.Caption := GetCompilerInfoStr;

  VBitmapStatic := ReadBitmapByFileRef(FConfigData, 'sas:\Resource\ABOUTICON.png', FContentTypeManager, nil);

  if VBitmapStatic <> nil then begin
    AssignStaticToBitmap32(imgLogo.Bitmap, VBitmapStatic);
  end;

  VBitmapSize.X := imgLogo.Bitmap.Width;
  VBitmapSize.Y := imgLogo.Bitmap.Height;

  if VBitmapSize.X <> 64 then begin
    VBitmapSize.X := 64;
  end;
  if VBitmapSize.Y <> 64 then begin
    VBitmapSize.Y := 64;
  end;

  imgLogo.Bitmap.SetSize(VBitmapSize.X, VBitmapSize.Y);
end;

procedure TfrmAbout.lblWebSiteClick(Sender: TObject);
begin
  OpenUrlInBrowser(cHomePage);
end;

procedure TfrmAbout.btnLicenseClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://www.gnu.org/licenses/gpl.html');
end;

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
