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

unit fr_ZoomsSelect;

interface

uses
  Classes,
  SysUtils,
  Types,
  CheckLst,
  StdCtrls,
  Controls,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TfrZoomsSelect = class(TFrame)
    chkAllZooms: TCheckBox;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;

    procedure chklstZoomsDblClick(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure chklstZoomsClick(Sender: TObject);
  private
    FOnClick: TNotifyEvent;
  public
    procedure Show(AParent: TWinControl);
    function GetZoomList: TByteDynArray;
    function Validate: Boolean;
    procedure Init(AminZoom, AmaxZoom: Byte);
    procedure DisableZoom(AitemValue: Byte);
    procedure CheckZoom(AZoom: Byte);
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AOnClick: TNotifyEvent = nil
    ); reintroduce;
  end;

implementation

{$R *.dfm}

{ TfrZoomsSelect }

constructor TfrZoomsSelect.Create(
  const ALanguageManager: ILanguageManager;
  const AOnClick: TNotifyEvent
);
begin
  inherited Create(ALanguageManager);
  FOnClick := AOnClick;
end;

procedure TfrZoomsSelect.chkAllZoomsClick(Sender: TObject);
var
  i: byte;
begin
  if chkAllZooms.state <> cbGrayed then begin
    for i := 0 to chklstZooms.Count - 1 do begin
      if chklstZooms.ItemEnabled[i]// Select only enabled items
        or (not TCheckBox(Sender).Checked and chklstZooms.Checked[i]) // deselect disabled items
      then begin
        chklstZooms.Checked[i] := TCheckBox(Sender).Checked;
      end;
    end;
    if Assigned(FOnClick) then begin
      FOnClick(Sender);
    end;
  end;
end;

procedure TfrZoomsSelect.Init(AminZoom, AmaxZoom: Byte);
var
  i: integer;
begin
  chklstZooms.Items.Clear;
  if AminZoom <= AmaxZoom then begin
    for i := AminZoom to AmaxZoom do begin
      chklstZooms.items.Add(IntToStr(i + 1));
    end;
  end else begin
    for i := AminZoom downto AmaxZoom do begin
      chklstZooms.items.Add(IntToStr(i + 1));
    end;
  end;
end;

procedure TfrZoomsSelect.DisableZoom(AitemValue: Byte);
var
  i: integer;
begin
  for i := 0 to chklstZooms.Count - 1 do begin
    if chklstZooms.items[i] = inttostr(AitemValue + 1) then begin
      chklstZooms.ItemEnabled[i] := false;
    end;
  end;
end;

procedure TfrZoomsSelect.chklstZoomsClick(Sender: TObject);
var
  i, VCountChecked, VDisabledCut: Integer;
begin
  VCountChecked := 0;
  VDisabledCut := 0;
  for i := 0 to chklstZooms.Count - 1 do begin
    if chklstZooms.Checked[i] then begin
      inc(VCountChecked);
    end;
    if not chklstZooms.ItemEnabled[i] then begin
      inc(VDisabledCut);
    end;
  end;
  if chkAllZooms.state <> cbGrayed then begin
    if (VCountChecked > 0) then begin
      chkAllZooms.state := cbGrayed;
    end;
  end else begin
    if VCountChecked + VDisabledCut = chklstZooms.Count then begin
      chkAllZooms.State := cbChecked;
    end;
    if VCountChecked = 0 then begin
      chkAllZooms.State := cbUnchecked;
    end;
  end;
  if Assigned(FOnClick) then begin
    FOnClick(Sender);
  end;
end;

procedure TfrZoomsSelect.chklstZoomsDblClick(Sender: TObject);
var
  i, VCountChecked, VDisabledCut: Integer;
begin
  VCountChecked := 0;
  VDisabledCut := 0;
  for i := 0 to chklstZooms.ItemIndex do begin //select items 0 -> click
    if chklstZooms.ItemEnabled[i] then begin // Select only enabled items
      chklstZooms.Checked[i] := true;
      inc(VCountChecked);
    end else begin
      chklstZooms.Checked[i] := false;
    end;
    if not chklstZooms.ItemEnabled[i] then begin
      inc(VDisabledCut);
    end;
  end;

  if chklstZooms.ItemIndex < chklstZooms.count - 1 then begin //deselect click -> 24
    for i := chklstZooms.ItemIndex + 1 to chklstZooms.count - 1 do begin
      chklstZooms.Checked[i] := false;
      if not chklstZooms.ItemEnabled[i] then begin
        inc(VDisabledCut);
      end;
    end;
  end;

  if VCountChecked + VDisabledCut = 0 then begin   // mark chekbox All
    chkAllZooms.state := cbUnchecked;
  end else begin
    if VCountChecked + VDisabledCut = chklstZooms.Items.Count then begin
      chkAllZooms.state := cbChecked;
    end else begin
      chkAllZooms.state := cbGrayed;
    end;
  end;

  if Assigned(FOnClick) then begin
    FOnClick(Sender);
  end;
end;

function TfrZoomsSelect.GetZoomList: TByteDynArray;
var
  i: Integer;
  VCount: Integer;
begin
  Result := nil;
  VCount := 0;
  for i := 0 to chklstZooms.count - 1 do begin
    if chklstZooms.Checked[i] then begin
      SetLength(Result, VCount + 1);
      Result[VCount] := strtoint(chklstZooms.Items[i]) - 1;
      Inc(VCount);
    end;
  end;
end;

procedure TfrZoomsSelect.Show(AParent: TWinControl);
begin
  Parent := AParent;
end;

function TfrZoomsSelect.Validate: Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to chklstZooms.Count - 1 do begin
    if chklstZooms.Checked[i] then begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TfrZoomsSelect.CheckZoom(AZoom: Byte);
begin
  Assert(AZoom <= chklstZooms.Count);
  Assert(chklstZooms.ItemEnabled[AZoom]);
  chklstZooms.Checked[AZoom] := True;
  chklstZooms.TopIndex := AZoom;
  chkAllZooms.state := cbGrayed;
end;

end.
