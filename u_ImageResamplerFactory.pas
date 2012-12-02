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

unit u_ImageResamplerFactory;

interface

uses
  GR32,
  GR32_Resamplers,
  i_ImageResamplerFactory,
  u_BaseInterfacedObject;

type
  TImageResamplerFactoryLinear = class(TBaseInterfacedObject, IImageResamplerFactory)
  private
    function CreateResampler: TCustomResampler;
  end;

  TImageResamplerFactoryNearest = class(TBaseInterfacedObject, IImageResamplerFactory)
  private
    function CreateResampler: TCustomResampler;
  end;

  TImageResamplerFactoryKernel = class(TBaseInterfacedObject, IImageResamplerFactory)
  private
    FKernelClass: TCustomKernelClass;
  private
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

{ TImageResamplerFactoryNearest }

function TImageResamplerFactoryNearest.CreateResampler: TCustomResampler;
begin
  Result := TNearestResampler.Create;
end;

{ TImageResamplerFactoryKernel }

constructor TImageResamplerFactoryKernel.Create(
  AKernelClass: TCustomKernelClass);
begin
  inherited Create;
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
