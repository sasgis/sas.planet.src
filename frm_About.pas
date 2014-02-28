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

unit frm_About;

interface

uses
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  GR32_Image,
  i_BuildInfo,
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
    lblSources: TLabel;
    lblRequires: TLabel;
    lblSourcesValue: TLabel;
    lblRequiresValue: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblWebSiteClick(Sender: TObject);
    procedure btnLicenseClick(Sender: TObject);
    procedure lblSourcesValueClick(Sender: TObject);
    procedure lblRequiresValueClick(Sender: TObject);
  private
    FBuildInfo: IBuildInfo;
    FContentTypeManager: IContentTypeManager;
    FConfigData: IConfigDataProvider;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ABuildInfo: IBuildInfo;
      const AContentTypeManager: IContentTypeManager;
      const AConfigData: IConfigDataProvider
    ); reintroduce;
  end;

implementation

uses
  SysUtils,
  i_Bitmap32Static,
  u_ConfigProviderHelpers,
  u_BitmapFunc,
  u_InetFunc;

const
  cHomePage = 'http://sasgis.org/';
  cSrcRepoLink = 'https://bitbucket.org/sas_team/sas.planet.src/';
  cReqRepoLink = 'https://bitbucket.org/sas_team/sas.requires/';

resourcestring
  rsDevelopmentTeam = 'SAS.Planet Development Team';

{$R *.dfm}

constructor TfrmAbout.Create(
  const ALanguageManager: ILanguageManager;
  const ABuildInfo: IBuildInfo;
  const AContentTypeManager: IContentTypeManager;
  const AConfigData: IConfigDataProvider
);
begin
  inherited Create(ALanguageManager);

  FBuildInfo := ABuildInfo;
  Assert(Assigned(FBuildInfo));

  FContentTypeManager := AContentTypeManager;
  FConfigData := AConfigData;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
var
  VBuildDate: TDateTime;
  VBitmapStatic: IBitmap32Static;
  VRevision: Integer;
  VNode: string;
begin
  VBuildDate := FBuildInfo.GetBuildDate;

  lblCopyright.Caption := 'Copyright ' + #169 + ' 2007-' + FormatDateTime('yyyy', VBuildDate) + ', ' + rsDevelopmentTeam;
  lblWebSite.Caption := cHomePage;

  lblVersion.Caption := FBuildInfo.GetVersion + ' ' + FBuildInfo.GetBuildType;
  lblBuildTimeValue.Caption := FormatDateTime('yyyy-mm-dd hh:mm:ss', VBuildDate) + ' UTC';
  lblBuildInfoValue.Caption := FBuildInfo.GetDescription;
  lblCompilerValue.Caption := FBuildInfo.GetCompilerInfo;

  if FBuildInfo.GetBuildSrcInfo(VRevision, VNode) then begin
    if Length(VNode) > 12 then begin
      SetLength(VNode, 12);
    end;
    lblSourcesValue.Caption := 'rev.' + IntToStr(VRevision) + ' (' + VNode + ')';
    lblSourcesValue.Hint := cSrcRepoLink;
  end else begin
    lblSourcesValue.Caption := 'Unknown';
  end;

  if FBuildInfo.GetBuildReqInfo(VRevision, VNode) then begin
    if Length(VNode) > 12 then begin
      SetLength(VNode, 12);
    end;
    lblRequiresValue.Caption := 'rev.' + IntToStr(VRevision) + ' (' + VNode + ')';
    lblRequiresValue.Hint := cReqRepoLink;
  end else begin
    lblRequiresValue.Caption := 'Unknown';
  end;

  VBitmapStatic := ReadBitmapByFileRef(FConfigData, 'sas:\Resource\ABOUTICON.png', FContentTypeManager, nil);

  if VBitmapStatic <> nil then begin
    AssignStaticToBitmap32(imgLogo.Bitmap, VBitmapStatic);
  end;
end;

procedure TfrmAbout.lblRequiresValueClick(Sender: TObject);
var
  VNode: string;
  VRevision: Integer;
begin
  if FBuildInfo.GetBuildReqInfo(VRevision, VNode) then begin
    OpenUrlInBrowser(cReqRepoLink + 'commits/all?search=0%3A' + VNode);
  end;
end;

procedure TfrmAbout.lblSourcesValueClick(Sender: TObject);
var
  VNode: string;
  VRevision: Integer;
begin
  if FBuildInfo.GetBuildSrcInfo(VRevision, VNode) then begin
    OpenUrlInBrowser(cSrcRepoLink + 'commits/all?search=0%3A' + VNode);
  end;
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
