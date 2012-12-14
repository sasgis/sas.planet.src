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

unit i_InternalDomainOptions;

interface

const
  // internal domain options - request types:
  c_IDO_RT_None                        = 0;
  c_IDO_RT_MakeVersionByDescriptionURL = 1;
  c_IDO_RT_SelectVersionByDescription  = 2;

type
  TDomainOptionsResponseFlag = (
    dorf_HtmlDecorated,
    dorf_ClearMemCache,
    dorf_Refresh,
    dorf_Restart
  );
  TDomainOptionsResponseFlags = set of TDomainOptionsResponseFlag;

  IInternalDomainOptions = interface
    ['{76A0B7AE-D75B-44BE-8983-69AF835E47A9}']
    function DomainHtmlOptions(
      const AFullPrefix, ARequest: String;
      out AResponse: String;
      out AFlags: TDomainOptionsResponseFlags;
      const ARequestType: LongWord = c_IDO_RT_None
    ): Boolean;
  end;

implementation

end.
