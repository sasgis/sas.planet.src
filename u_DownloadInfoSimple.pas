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

unit u_DownloadInfoSimple;

interface

uses
  SyncObjs,
  i_DownloadInfoSimple;

type
  TDownloadInfoSimple = class(TInterfacedObject, IDownloadInfoSimple)
  private
    FParentInfo: IDownloadInfoSimple;
    FCS: TCriticalSection;
    FTileCount: UInt64;
    FSize: UInt64;
  protected
    function GetTileCount: UInt64;
    function GetSize: UInt64;

    procedure Reset;
    procedure Add(ACount: UInt64; ASize: UInt64);
  public
    constructor Create(
      AParent: IDownloadInfoSimple;
      ATileCount: UInt64 = 0;
      ASize: UInt64 = 0
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TDownloadInfoSimple }

constructor TDownloadInfoSimple.Create(
  AParent: IDownloadInfoSimple;
  ATileCount: UInt64;
  ASize: UInt64
);
begin
  inherited Create;
  FParentInfo := AParent;
  FCS := TCriticalSection.Create;
  FTileCount := ATileCount;
  FSize := ASize;
end;

destructor TDownloadInfoSimple.Destroy;
begin
  FreeAndNil(FCS);
  FParentInfo := nil;
  inherited;
end;

procedure TDownloadInfoSimple.Add(ACount, ASize: UInt64);
begin
  FCS.Acquire;
  try
    Inc(FTileCount, ACount);
    Inc(FSize, ASize);
  finally
    FCS.Release;
  end;
  if FParentInfo <> nil then begin
    FParentInfo.Add(ACount, ASize);
  end;
end;

function TDownloadInfoSimple.GetSize: UInt64;
begin
  FCS.Acquire;
  try
    Result := FSize;
  finally
    FCS.Release;
  end;
end;

function TDownloadInfoSimple.GetTileCount: UInt64;
begin
  FCS.Acquire;
  try
    Result := FTileCount;
  finally
    FCS.Release;
  end;
end;

procedure TDownloadInfoSimple.Reset;
begin
  FCS.Acquire;
  try
    FSize := 0;
    FTileCount := 0;
  finally
    FCS.Release;
  end;
end;

end.
