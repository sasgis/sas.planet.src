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

unit t_ETS_AuthKind;

interface

const
  // supported authentication kinds
  ETS_AK_PROXY = $01; // basic proxy authentication
  ETS_AK_UNC   = $02; // basic net (share) authentication
  ETS_AK_DBMS  = $03; // basic database authentication

  // and (auxillary) authentication flags
  ETS_AF_ANSI_VERSION      = $10000000; // use ansi if set, use unicode if not set (by default)
  ETS_AF_ALLOW_DOMAIN_USER = $20000000; // storage supports domain user credentials (integrated login) and provider can use it
  ETS_AF_KEEP              = $40000000; // storage provider can keep auth info

  // mask
  ETS_MASK_AK_ALL_DBMS = (ETS_AK_DBMS or ETS_AF_ALLOW_DOMAIN_USER or ETS_AF_KEEP);

  // type of resultant credentials
  ETS_AT_NO               = $00000001; // not required or empty - no info
  ETS_AT_INTEGRATED       = $00000002; // use current domain user credentials (integrated login) - no additional info required
  ETS_AT_LOGIN_PWD        = $00000004; // use login and password authentification
  ETS_AT_DOMAIN_LOGIN_PWD = $00000008; // use domain, login and password authentification

  // and auxillary flags
  ETS_RF_USE_DOMAIN       = $10000000; // use domain from buffer
  ETS_RF_USE_LOGIN        = $20000000; // use login from buffer
  ETS_RF_USE_PASSWORD     = $40000000; // use password from buffer
  ETS_RF_CUR_DOMAIN       = $01000000; // use current domain
  ETS_RF_CUR_LOGIN        = $02000000; // use current login
  ETS_RF_ALLOW_KEEP       = $00100000; // allow keep auth info in storage provider

implementation

end.