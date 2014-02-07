unit i_MapVersionRequest;

interface

uses
  i_MapVersionInfo;

type
  IMapVersionRequest = interface
    function GetBaseVersion: IMapVersionInfo;
    property BaseVersion: IMapVersionInfo read GetBaseVersion;

    function GetShowPrevVersion: Boolean;
    property ShowPrevVersion: Boolean read GetShowPrevVersion;

    function GetIsValidVersion(const AVersion: IMapVersionInfo): Boolean;
  end;

implementation

end.
