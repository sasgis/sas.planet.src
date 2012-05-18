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

unit frm_About;

interface

uses
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrmAbout = class(TFormWitghLanguageManager)
    Bevel1: TBevel;
    btnClose: TButton;
    lblVersionCatpion: TLabel;
    lblWebSiteCaption: TLabel;
    lblEMailCaption: TLabel;
    lblProgramName: TLabel;
    lblVersion: TLabel;
    lblEMail: TLabel;
    lblWebSite: TLabel;
    pnlBottom: TPanel;
    Label1: TLabel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lblWebSiteClick(Sender: TObject);
    procedure lblEMailClick(Sender: TObject);
  private
  public
  end;

implementation

uses
  c_SasVersion,
  u_InetFunc;

{$R *.dfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
 lblVersion.Caption:=SASVersion;
end;

procedure TfrmAbout.lblWebSiteClick(Sender: TObject);
begin
  OpenUrlInBrowser('http://sasgis.ru');
end;

procedure TfrmAbout.lblEMailClick(Sender: TObject);
begin
  OpenUrlInBrowser('mailto:'+lblEMail.Caption);
end;

end.
