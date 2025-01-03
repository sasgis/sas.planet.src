{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit frm_ArchiverSettings;

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
  fr_ArchiveWriteZipConfig,
  i_ArchiveReadWriteConfig,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TfrmArchiverSettings = class(TCommonFormParent)
    btnApply: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FWriterConfig: IArchiveWriteConfig;
    FfrArchiveWriteZipConfig: TfrArchiveWriteZipConfig;
  public
    function GetWriterConfig: IArchiveWriteConfig;
  public
    constructor Create(
      AOwner: TComponent;
      const ALanguageManager: ILanguageManager
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfrmArchiverSettings }

constructor TfrmArchiverSettings.Create(
  AOwner: TComponent;
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create(AOwner);

  FfrArchiveWriteZipConfig := TfrArchiveWriteZipConfig.Create(ALanguageManager);
  FfrArchiveWriteZipConfig.Parent := pnlMain;

  FWriterConfig := nil;

  if AOwner = nil then begin
    FPropertyState := CreateComponentPropertyState(
      Self, [], [], True, False, True, True
    );
    FPropertyState.ExcludeAll(Self.Name);
  end;
end;

destructor TfrmArchiverSettings.Destroy;
begin
  FreeAndNil(FfrArchiveWriteZipConfig);
  inherited;
end;

procedure TfrmArchiverSettings.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FfrArchiveWriteZipConfig.Hide;
end;

procedure TfrmArchiverSettings.FormShow(Sender: TObject);
begin
  if FWriterConfig <> nil then begin
    (FfrArchiveWriteZipConfig as IArchiveWriteConfigFrame).Reset(FWriterConfig);
  end;
  FfrArchiveWriteZipConfig.Show;
end;

procedure TfrmArchiverSettings.btnApplyClick(Sender: TObject);
begin
  FWriterConfig := (FfrArchiveWriteZipConfig as IArchiveWriteConfigFrame).GetWriteConfig;
  Close;
end;

procedure TfrmArchiverSettings.btnCancelClick(Sender: TObject);
begin
  Close;
end;

function TfrmArchiverSettings.GetWriterConfig: IArchiveWriteConfig;
begin
  Result := FWriterConfig;
end;

end.
