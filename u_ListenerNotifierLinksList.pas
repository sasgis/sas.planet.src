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

unit u_ListenerNotifierLinksList;

interface

uses
  Classes,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_ListenerTTLCheck,
  i_NotifierTTLCheck,
  i_ListenerNotifierLinksList,
  u_BaseInterfacedObject;

type
  TListenerNotifierLinksList = class(TBaseInterfacedObject, IListenerNotifierLinksList)
  private
    FCS: IReadWriteSync;
    FLinksActive: Boolean;
    FListenerList: IInterfaceList;
    FNotifierList: IInterfaceList;
    function GetCount: Integer;
    procedure ActivateLink(AIndex: Integer);
    procedure DeactivateLink(AIndex: Integer);
    procedure DoActivateLinks;
    procedure DoDeactivateLinks;
  private
    procedure Add(
      const AListener: IListener;
      const ANotifier: INotifier
    );overload;
    procedure Add(
      const AListener: IListenerTime;
      const ANotifier: INotifierTime
    );overload;
    procedure ActivateLinks;
    procedure DeactivateLinks;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer;

{ TListenerNotifierLinksList }

constructor TListenerNotifierLinksList.Create;
begin
  inherited Create;
  FCS := MakeSyncRW_Sym(Self, TRUE);
  FListenerList := TInterfaceList.Create;
  FNotifierList := TInterfaceList.Create;
  FLinksActive := False;
end;

destructor TListenerNotifierLinksList.Destroy;
begin
  FCS := nil;
  FListenerList := nil;
  FNotifierList := nil;
  inherited;
end;

procedure TListenerNotifierLinksList.DoActivateLinks;
var
  i: Integer;
begin
  if not FLinksActive then begin
    for i := 0 to GetCount - 1 do begin
      ActivateLink(i);
    end;
    FLinksActive := True;
  end;
end;

procedure TListenerNotifierLinksList.DoDeactivateLinks;
var
  i: Integer;
begin
  if FLinksActive then begin
    for i := 0 to GetCount - 1 do begin
      DeactivateLink(i);
    end;
    FLinksActive := False;
  end;
end;

function TListenerNotifierLinksList.GetCount: Integer;
begin
  Result := FListenerList.Count;
end;

procedure TListenerNotifierLinksList.ActivateLink(AIndex: Integer);
var
  VListener: IInterface;
  VNotifier: IInterface;
  VListenerBasic: IListener;
  VNotifierBasic: INotifier;
  VListenerTime: IListenerTime;
  VNotifierTime: INotifierTime;
begin
  VListener := FListenerList.Items[AIndex];
  VNotifier := FNotifierList.Items[AIndex];
  if (VListener <> nil) and (VNotifier <> nil) then begin
    if Supports(VListener, IListener, VListenerBasic) then begin
      if Supports(VNotifier, INotifier, VNotifierBasic) then begin
        VNotifierBasic.Add(VListenerBasic);
      end;
    end else if Supports(VListener, IListenerTime, VListenerTime) then begin
      if Supports(VNotifier, INotifierTime, VNotifierTime) then begin
        VNotifierTime.Add(VListenerTime);
      end;
    end;
  end;
end;


procedure TListenerNotifierLinksList.ActivateLinks;
begin
  FCS.BeginRead;
  try
    if not FLinksActive then begin
      DoActivateLinks;
    end;
  finally
    FCS.EndRead;
  end;
end;

procedure TListenerNotifierLinksList.Add(const AListener: IListenerTime;
  const ANotifier: INotifierTime);
var
  VListenerIndex: Integer;
  VNotifierIndex: Integer;
begin
  FCS.BeginWrite;
  try
    VListenerIndex := FListenerList.Add(AListener);
    VNotifierIndex := FNotifierList.Add(ANotifier);
    Assert(VListenerIndex = VNotifierIndex);
    if FLinksActive then begin
      ActivateLink(VListenerIndex);
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TListenerNotifierLinksList.Add(
  const AListener: IListener;
  const ANotifier: INotifier
);
var
  VListenerIndex: Integer;
  VNotifierIndex: Integer;
begin
  FCS.BeginWrite;
  try
    VListenerIndex := FListenerList.Add(AListener);
    VNotifierIndex := FNotifierList.Add(ANotifier);
    Assert(VListenerIndex = VNotifierIndex);
    if FLinksActive then begin
      ActivateLink(VListenerIndex);
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TListenerNotifierLinksList.DeactivateLink(AIndex: Integer);
var
  VListener: IInterface;
  VNotifier: IInterface;
  VListenerBasic: IListener;
  VNotifierBasic: INotifier;
  VListenerTime: IListenerTime;
  VNotifierTime: INotifierTime;
begin
  VListener := FListenerList.Items[AIndex];
  VNotifier := FNotifierList.Items[AIndex];
  if (VListener <> nil) and (VNotifier <> nil) then begin
    if Supports(VListener, IListener, VListenerBasic) then begin
      if Supports(VNotifier, INotifier, VNotifierBasic) then begin
        VNotifierBasic.Remove(VListenerBasic);
      end;
    end else if Supports(VListener, IListenerTime, VListenerTime) then begin
      if Supports(VNotifier, INotifierTime, VNotifierTime) then begin
        VNotifierTime.Remove(VListenerTime);
      end;
    end;
  end;
end;

procedure TListenerNotifierLinksList.DeactivateLinks;
begin
  FCS.BeginRead;
  try
    if FLinksActive then begin
      DoDeactivateLinks;
    end;
  finally
    FCS.EndRead;
  end;
end;

end.





