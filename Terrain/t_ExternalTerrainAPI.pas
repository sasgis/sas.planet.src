unit t_ExternalTerrainAPI;

interface

// AsterGdemV2
// STRM
// BIL

// Open
// Close
// GetElevation

type
  // callback for ExternalTerrainsEnum
  TExternalTerrainsEnumCallback = function(
    const AHostPointer: Pointer;
    const ACallPointer: Pointer;
    const AProviderGUID: TGUID;
    const AProviderName: PWideChar;
    const AProviderProj: PAnsiChar;
    const AOptions: LongWord // reserved
  ): Boolean; cdecl;

  // name 'ExternalTerrainsEnum'
  TExternalTerrainsEnum = function(
    const AHostPointer: Pointer;
    const ACallPointer: Pointer;
    const AHostCallback: TExternalTerrainsEnumCallback
  ): Boolean; cdecl;

  // to work with terrain

  TExternalTerrainsHandle = Pointer;
  PExternalTerrainsHandle = ^TExternalTerrainsHandle;

  // name 'ExternalTerrainsOpen'
  TExternalTerrainsOpen = function(
    const ATerrain: PExternalTerrainsHandle;
    const AReserved: Pointer;
    const AProviderGuid: TGUID;
    const AConvertAtHost: Boolean
  ): Boolean; cdecl;

  // name 'ExternalTerrainsClose'
  TExternalTerrainsClose = procedure(
    const ATerrain: PExternalTerrainsHandle
  ); cdecl;

  // name 'ExternalTerrainsGetElevation'
  TExternalTerrainsGetElevation = function(
    const ATerrain: PExternalTerrainsHandle;
    const ALon: Double;
    const ALat: Double;
    out AElevation: Single
  ): Boolean; cdecl;

implementation

end.
