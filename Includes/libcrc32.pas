unit libcrc32;

interface

const
  libcrc32_dll = 'libcrc32.dll';

function crc32(
  const APrev: Cardinal;
  const AData: Pointer;
  const ALength: Cardinal
): Cardinal; cdecl; external libcrc32_dll;

implementation

end.
