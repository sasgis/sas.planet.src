{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit frm_FavoriteMapSetManager;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  fr_FavoriteMapSetManager,
  frm_FavoriteMapSetEditor,
  i_MapTypeSet,
  i_LanguageManager,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetHelper,
  u_CommonFormAndFrameParents;

type
  TfrmFavoriteMapSetManager = class(TFormWitghLanguageManager)
    pnlBottomButtons: TPanel;
    btnCancel: TButton;
    btnOk: TButton;
    pnlMapSets: TPanel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FfrFavoriteMapSetManager: TfrFavoriteMapSetManager;
    FFavoriteMapSetConfig: IFavoriteMapSetConfig;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapsSet: IMapTypeSet;
      const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
      const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
      const AFavoriteMapSetEditor: TfrmFavoriteMapSetEditor

    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfrmFavoriteMapSetManager }

constructor TfrmFavoriteMapSetManager.Create(
  const ALanguageManager: ILanguageManager;
  const AMapsSet: IMapTypeSet;
  const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
  const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
  const AFavoriteMapSetEditor: TfrmFavoriteMapSetEditor
);
begin
  inherited Create(ALanguageManager);
  FFavoriteMapSetConfig := AFavoriteMapSetConfig;
  FfrFavoriteMapSetManager :=
    TfrFavoriteMapSetManager.Create(
      ALanguageManager,
      AMapsSet,
      AFavoriteMapSetConfig,
      AFavoriteMapSetHelper,
      AFavoriteMapSetEditor
    );
end;

destructor TfrmFavoriteMapSetManager.Destroy;
begin
  FreeAndNil(FfrFavoriteMapSetManager);
  inherited Destroy;
end;

procedure TfrmFavoriteMapSetManager.FormHide(Sender: TObject);
begin
  FFavoriteMapSetConfig.StartNotify;
end;

procedure TfrmFavoriteMapSetManager.FormShow(Sender: TObject);
begin
  FFavoriteMapSetConfig.StopNotify;
  FfrFavoriteMapSetManager.Parent := pnlMapSets;
  FfrFavoriteMapSetManager.Init;
end;

procedure TfrmFavoriteMapSetManager.btnOkClick(Sender: TObject);
begin
  FfrFavoriteMapSetManager.ApplyChanges;
  Close;
end;

procedure TfrmFavoriteMapSetManager.btnCancelClick(Sender: TObject);
begin
  FfrFavoriteMapSetManager.CancelChanges;
  Close;
end;

end.
