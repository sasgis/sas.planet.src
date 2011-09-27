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

unit u_ConfigDataProviderVirtualWithSubItem;

interface

uses
  Classes,
  i_ConfigDataProvider;

type
  TConfigDataProviderVirtualWithSubItem = class(TInterfacedObject, IConfigDataProvider)
  private
    FSubItemName: string;
    FSubItem: IConfigDataProvider;
  protected
    function GetSubItem(const AIdent: string): IConfigDataProvider; virtual;
    function ReadBinaryStream(const AIdent: string; AValue: TStream): Integer; virtual;
    function ReadString(const AIdent: string; const ADefault: string): string; virtual;
    function ReadInteger(const AIdent: string; const ADefault: Longint): Longint; virtual;
    function ReadBool(const AIdent: string; const ADefault: Boolean): Boolean; virtual;
    function ReadDate(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadDateTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadFloat(const AIdent: string; const ADefault: Double): Double; virtual;
    function ReadTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;

    procedure ReadSubItemsList(AList: TStrings); virtual;
    procedure ReadValuesList(AList: TStrings); virtual;
  public
    constructor Create(
      ASubItemName: string;
      ASubItem: IConfigDataProvider
    );
    destructor Destroy; override;
  end;

implementation

{ TConfigDataProviderVirtualWithSubItem }

constructor TConfigDataProviderVirtualWithSubItem.Create(ASubItemName: string;
  ASubItem: IConfigDataProvider);
begin
  FSubItemName := ASubItemName;
  FSubItem := ASubItem;
end;

destructor TConfigDataProviderVirtualWithSubItem.Destroy;
begin
  FSubItem := nil;
  inherited;
end;

function TConfigDataProviderVirtualWithSubItem.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  if AIdent = FSubItemName then begin
    Result := FSubItem;
  end else begin
    Result := nil;
  end;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBinaryStream(
  const AIdent: string; AValue: TStream): Integer;
begin
  Result := 0;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderVirtualWithSubItem.ReadSubItemsList(AList: TStrings);
begin
  AList.Clear;
  AList.Add(FSubItemName);
end;

function TConfigDataProviderVirtualWithSubItem.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderVirtualWithSubItem.ReadValuesList(AList: TStrings);
begin
  AList.Clear;
end;

end.
