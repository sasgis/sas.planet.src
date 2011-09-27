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

unit frm_LonLatRectEdit;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  u_CommonFormAndFrameParents,
  t_GeoTypes,
  fr_LonLat;

type
  TfrmLonLatRectEdit = class(TCommonFormParent)
    btnOk: TButton;
    btnCancel: TButton;
    grpTopLeft: TGroupBox;
    grpBottomRight: TGroupBox;
    pnlBottomButtons: TPanel;
    grdpnlMain: TGridPanel;
    procedure FormShow(Sender: TObject);
  private
    FfrLonLatTopLeft: TfrLonLat;
    FfrLonLatBottomRight: TfrLonLat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(var ALonLatRect: TDoubleRect): Boolean;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  u_ResStrings,
  u_GlobalState;

{$R *.dfm}

constructor TfrmLonLatRectEdit.Create(AOwner: TComponent);
begin
  inherited;
  FfrLonLatTopLeft :=
    TfrLonLat.Create(
      nil,
      GState.MainFormConfig.ViewPortState,
      GState.ValueToStringConverterConfig,
      tssTopLeft
    );
  FfrLonLatBottomRight :=
    TfrLonLat.Create(
      nil,
      GState.MainFormConfig.ViewPortState,
      GState.ValueToStringConverterConfig,
      tssBottomRight
    );
end;

destructor TfrmLonLatRectEdit.Destroy;
begin
  FreeAndNil(FfrLonLatTopLeft);
  FreeAndNil(FfrLonLatBottomRight);
  inherited;
end;

function TfrmLonLatRectEdit.Execute(var ALonLatRect: TDoubleRect): Boolean;
begin
  FfrLonLatTopLeft.LonLat := ALonLatRect.TopLeft;
  FfrLonLatBottomRight.LonLat := ALonLatRect.BottomRight;
  Result := ShowModal = mrOK;
  if Result then begin
    ALonLatRect.TopLeft := FfrLonLatTopLeft.LonLat;
    ALonLatRect.BottomRight := FfrLonLatBottomRight.LonLat;
    if (ALonLatRect.Left>ALonLatRect.Right)then begin
      ShowMessage(SAS_ERR_LonLat2);
      result:=false;
    end else if (ALonLatRect.Top < ALonLatRect.Bottom)then begin
      ShowMessage(SAS_ERR_LonLat1);
      result:=false;
    end;
  end;
end;

procedure TfrmLonLatRectEdit.FormShow(Sender: TObject);
begin
  FfrLonLatTopLeft.Parent := grpTopLeft;
  FfrLonLatBottomRight.Parent := grpBottomRight;
end;

procedure TfrmLonLatRectEdit.RefreshTranslation;
begin
  inherited;
  FfrLonLatTopLeft.RefreshTranslation;
  FfrLonLatBottomRight.RefreshTranslation;
end;

end.
