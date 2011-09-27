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

unit u_ImageResamplerFactoryListStatic;

interface

uses
  SyncObjs,
  Classes,
  i_ImageResamplerFactory;

type
  TImageResamplerFactoryListStatic = class(TInterfacedObject, IImageResamplerFactoryList)
  private
    FList: TStringList;
    FCS: TCriticalSection;
  protected
    procedure Add(AFactory: IImageResamplerFactory; ACaption: string);
  protected
    function Count: Integer;
    function Get(AIndex: Integer): IImageResamplerFactory;
    function GetCaption(AIndex: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TImageResamplerFactoryListStatic }

constructor TImageResamplerFactoryListStatic.Create;
begin
  inherited;
  FCS := TCriticalSection.Create;
  FList := TStringList.Create;
end;

destructor TImageResamplerFactoryListStatic.Destroy;
var
  i: Integer;
begin
  if FList <> nil then begin
    for i := 0 to FList.Count - 1 do begin
      if FList.Objects[i] <> nil then begin
        IInterface(Pointer(FList.Objects[i]))._Release;
        FList.Objects[i] := nil;
      end;
    end;
  end;
  FreeAndNil(FList);
  FreeAndNil(FCS);
  inherited;
end;

procedure TImageResamplerFactoryListStatic.Add(AFactory: IImageResamplerFactory;
  ACaption: string);
begin
  FCS.Acquire;
  try
    FList.AddObject(ACaption, TObject(Pointer(AFactory)));
    AFactory._AddRef;
  finally
    FCS.Release;
  end;
end;

function TImageResamplerFactoryListStatic.Count: Integer;
begin
  FCS.Acquire;
  try
    Result := FList.Count;
  finally
    FCS.Release;
  end;
end;

function TImageResamplerFactoryListStatic.Get(
  AIndex: Integer): IImageResamplerFactory;
begin
  FCS.Acquire;
  try
    Result := IImageResamplerFactory(Pointer(FList.Objects[AIndex]));
  finally
    FCS.Release;
  end;
end;

function TImageResamplerFactoryListStatic.GetCaption(AIndex: Integer): string;
begin
  FCS.Acquire;
  try
    Result := FList.Strings[AIndex];
  finally
    FCS.Release;
  end;
end;

end.
