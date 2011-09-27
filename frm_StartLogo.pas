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

unit frm_StartLogo;

interface

uses
  Forms,
  Classes,
  GR32_Image,
  Controls,
  ExtCtrls,
  StdCtrls,
  i_StartUpLogoConfig,
  u_CommonFormAndFrameParents;

type
  TfrmStartLogo = class(TCommonFormParent)
    tmrLogo: TTimer;
    imgLogo: TImage32;
    lblVersion: TLabel;
    lblWebSite: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrLogoTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgLogoClick(Sender: TObject);
  private
    FReadyToHide: Boolean;
    FConfig: IStartUpLogoConfig;
  public
    constructor Create(AOwner: TComponent; AConfig: IStartUpLogoConfig); reintroduce;
    destructor Destroy; override;
    
    class procedure ShowLogo(AConfig: IStartUpLogoConfig);
    class procedure ReadyToHideLogo;
  end;


implementation

uses
  Types,
  c_SasVersion;

var
  frmStartLogo: TfrmStartLogo;

{$R *.dfm}

procedure TfrmStartLogo.tmrLogoTimer(Sender: TObject);
begin
  tmrLogo.Enabled:=false;
  Self.Close;
end;

constructor TfrmStartLogo.Create(AOwner: TComponent;
  AConfig: IStartUpLogoConfig);
begin
  inherited Create(AOwner);
  FConfig := AConfig;
end;

destructor TfrmStartLogo.Destroy;
begin
  frmStartLogo := nil;
  inherited;
end;

procedure TfrmStartLogo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmStartLogo.FormShow(Sender: TObject);
var
  VBitmapSize: TPoint;
begin
  imgLogo.Bitmap.Assign(FConfig.Logo);
  VBitmapSize.X := imgLogo.Bitmap.Width;
  VBitmapSize.Y := imgLogo.Bitmap.Height;

  if VBitmapSize.X < 100 then begin
    VBitmapSize.X := 480;
  end;
  if VBitmapSize.Y < 100 then begin
    VBitmapSize.Y := 276;
  end;
  imgLogo.Bitmap.SetSize(VBitmapSize.X, VBitmapSize.Y);
  lblVersion.Caption:='v '+SASVersion;
  FReadyToHide := False;
end;

procedure TfrmStartLogo.imgLogoClick(Sender: TObject);
begin
  if FReadyToHide then begin
    tmrLogo.Enabled := false;
    Self.Close;
  end;
end;

class procedure TfrmStartLogo.ReadyToHideLogo;
begin
  if frmStartLogo <> nil then begin
    frmStartLogo.FReadyToHide := True;
    frmStartLogo.tmrLogo.Enabled := True;
  end;
end;

class procedure TfrmStartLogo.ShowLogo(AConfig: IStartUpLogoConfig);
begin
  if AConfig.IsShowLogo then begin
    frmStartLogo := TfrmStartLogo.Create(nil, AConfig);
    frmStartLogo.Show;
    Application.ProcessMessages;
  end;
end;

end.
