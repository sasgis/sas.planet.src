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

unit u_SpinEditExt;

interface

uses
  Classes,
  Spin;

type
  // TSpinEdit with the OnUpClick and OnDownClick event notifiers

  TSpinEditExt = class(TSpinEdit)
  private
    FOnUpClick: TNotifyEvent;
    FOnDownClick: TNotifyEvent;
  protected
    procedure UpClick(Sender: TObject); override;
    procedure DownClick(Sender: TObject); override;
  published
    property OnDownClick: TNotifyEvent read FOnDownClick write FOnDownClick;
    property OnUpClick: TNotifyEvent read FOnUpClick write FOnUpClick;
  end;

  TSpinEdit = class(TSpinEditExt);

implementation

{ TSpinEditExt }

procedure TSpinEditExt.UpClick(Sender: TObject);
Begin
  inherited;
  if Assigned(FOnUpClick) then begin
    FOnUpClick(Self);
  end;
end;

procedure TSpinEditExt.DownClick(Sender: TObject);
begin
  inherited;
  if Assigned(FOnDownClick) then begin
    FOnDownClick(Self);
  end;
end;

end.
