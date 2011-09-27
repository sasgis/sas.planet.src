{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_About;

interface

uses
  Windows,
  Forms,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  u_CommonFormAndFrameParents;

type
  TfrmAbout = class(TCommonFormParent)
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

var
  frmAbout: TfrmAbout;

implementation

uses
  c_SasVersion,
  frm_InvisibleBrowser;

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
