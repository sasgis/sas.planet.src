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

unit u_ConfigDataWriteProviderWithGlobal;

interface

uses
  Classes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataProviderWithGlobal;

type
  TConfigDataWriteProviderWithGlobal = class(TConfigDataProviderWithGlobal, IConfigDataWriteProvider)
  private
    FProviderMain: IConfigDataWriteProvider;
  protected
    function GetOrCreateSubItem(const AIdent: string): IConfigDataWriteProvider;
    procedure DeleteSubItem(const AIdent: string);
    procedure DeleteValue(const AIdent: string);
    procedure DeleteValues;
    procedure WriteBinaryStream(const AIdent: string; AValue: TStream);
    procedure WriteString(const AIdent: string; const AValue: string);
    procedure WriteInteger(const AIdent: string; const AValue: Longint);
    procedure WriteBool(const AIdent: string; const AValue: Boolean);
    procedure WriteDate(const AIdent: string; const AValue: TDateTime);
    procedure WriteDateTime(const AIdent: string; const AValue: TDateTime);
    procedure WriteFloat(const AIdent: string; const AValue: Double);
    procedure WriteTime(const AIdent: string; const AValue: TDateTime);
  public
    constructor Create(
      AProviderMain: IConfigDataWriteProvider;
      AProviderGlobalPrefix: string;
      AProviderGlobal: IConfigDataProvider
    );
  end;

implementation

uses
  SysUtils;

{ TConfigDataWriteProviderWithGlobal }

constructor TConfigDataWriteProviderWithGlobal.Create(
  AProviderMain: IConfigDataWriteProvider;
  AProviderGlobalPrefix: string;
  AProviderGlobal: IConfigDataProvider
);
begin
  inherited Create(AProviderMain, AProviderGlobalPrefix, AProviderGlobal);
  FProviderMain := AProviderMain;
end;

procedure TConfigDataWriteProviderWithGlobal.DeleteSubItem(
  const AIdent: string);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.DeleteSubItem(VIdent);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.DeleteValue(const AIdent: string);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.DeleteValue(VIdent);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.DeleteValues;
begin
  if FProviderMain <> nil then begin
    FProviderMain.DeleteValues;
  end;
end;

function TConfigDataWriteProviderWithGlobal.GetOrCreateSubItem(
  const AIdent: string): IConfigDataWriteProvider;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      Result := TConfigDataWriteProviderWithGlobal.Create(FProviderMain.GetOrCreateSubItem(VIdent), ProviderGlobalPrefix, ProviderGlobal);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteBinaryStream(
  const AIdent: string; AValue: TStream);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteBinaryStream(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteBool(const AIdent: string;
  const AValue: Boolean);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteBool(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteDate(const AIdent: string;
  const AValue: TDateTime);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteDate(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteDateTime(const AIdent: string;
  const AValue: TDateTime);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteDateTime(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteFloat(const AIdent: string;
  const AValue: Double);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteFloat(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteInteger(const AIdent: string;
  const AValue: Integer);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteInteger(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteString(const AIdent,
  AValue: string);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteString(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

procedure TConfigDataWriteProviderWithGlobal.WriteTime(const AIdent: string;
  const AValue: TDateTime);
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      FProviderMain.WriteTime(VIdent, AValue);
    end;
  end else begin
    raise Exception.Create('Not expected');
  end;
end;

end.
