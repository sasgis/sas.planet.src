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

unit u_ImageResamplerFactory;

interface

uses
  GR32,
  GR32_Resamplers,
  i_ImageResamplerFactory;

type
  TImageResamplerFactoryLinear = class(TInterfacedObject, IImageResamplerFactory)
  protected
    function CreateResampler: TCustomResampler;
  end;

  TImageResamplerFactoryNil = class(TInterfacedObject, IImageResamplerFactory)
  protected
    function CreateResampler: TCustomResampler;
  end;

  TImageResamplerFactoryKernel = class(TInterfacedObject, IImageResamplerFactory)
  private
    FKernelClass: TCustomKernelClass;
  protected
    function CreateResampler: TCustomResampler;
  public
    constructor Create(AKernelClass: TCustomKernelClass);
  end;

implementation

{ TImageResamplerFactoryLinear }

function TImageResamplerFactoryLinear.CreateResampler: TCustomResampler;
begin
  Result := TLinearResampler.Create;
end;

{ TImageResamplerFactoryNil }

function TImageResamplerFactoryNil.CreateResampler: TCustomResampler;
begin
  Result := nil;
end;

{ TImageResamplerFactoryKernel }

constructor TImageResamplerFactoryKernel.Create(
  AKernelClass: TCustomKernelClass);
begin
  FKernelClass := AKernelClass;
end;

function TImageResamplerFactoryKernel.CreateResampler: TCustomResampler;
var
  VResult: TKernelResampler;
begin
  VResult := TKernelResampler.Create;
  if Assigned(FKernelClass) then begin
    VResult.Kernel := FKernelClass.Create;
  end;
  Result := VResult;
end;

end.
