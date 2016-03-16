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

unit u_TileDownloaderStateInternal;

interface

uses
  i_TileDownloaderState,
  i_Changeable,
  u_ChangeableBase;

type
  ITileDownloaderStateInternal = interface(IChangeable)
    ['{BD4A155B-92AC-4E61-8A1D-A0F8516E1340}']
    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetDisableReason: string;
    property DisableReason: string read GetDisableReason;

    procedure Disable(const AReason: string);
    procedure Enable;

    function GetStatic: ITileDownloaderStateStatic;
  end;

type
  TTileDownloaderStateInternal = class(TChangeableWithSimpleLockBase, ITileDownloaderStateInternal, ITileDownloaderStateChangeble)
  private
    FEnabled: Boolean;
    FReason: string;
    FStatic: ITileDownloaderStateStatic;
    function CreateStatic: ITileDownloaderStateStatic;
  private
    function GetEnabled: Boolean;
    function GetDisableReason: string;

    procedure Disable(const AReason: string);
    procedure Enable;

    function GetStatic: ITileDownloaderStateStatic;
  public
    constructor Create;
  end;


implementation

uses
  gnugettext,
  u_TileDownloaderStateStatic;

{ TTileDownloaderStateInternal }

constructor TTileDownloaderStateInternal.Create;
begin
  inherited Create;
  FEnabled := True;
  FStatic := CreateStatic;
end;

function TTileDownloaderStateInternal.CreateStatic: ITileDownloaderStateStatic;
begin
  Result :=
    TTileDownloaderStateStatic.Create(
      FEnabled,
      FReason
    );
end;

procedure TTileDownloaderStateInternal.Disable(const AReason: string);
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if FEnabled then begin
      FEnabled := False;
      FReason := AReason;
      FStatic := CreateStatic;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

procedure TTileDownloaderStateInternal.Enable;
var
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  CS.BeginWrite;
  try
    if not FEnabled then begin
      FEnabled := True;
      FReason := '';
      FStatic := CreateStatic;
      VNeedNotify := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VNeedNotify then begin
    DoChangeNotify;
  end;
end;

function TTileDownloaderStateInternal.GetDisableReason: string;
begin
  CS.BeginRead;
  try
    if FEnabled then begin
      Result := '';
    end else begin
      Result := gettext_NoExtract(FReason);
    end;
  finally
    CS.EndRead;
  end;
end;

function TTileDownloaderStateInternal.GetEnabled: Boolean;
begin
  CS.BeginRead;
  try
    Result := FEnabled;
  finally
    CS.EndRead;
  end;
end;

function TTileDownloaderStateInternal.GetStatic: ITileDownloaderStateStatic;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

end.
