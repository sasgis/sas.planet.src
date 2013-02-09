unit i_MapSvcScanStorage;

interface

type
  IMapSvcScanStorage = interface
    ['{2320B6DB-151D-4602-8265-33D4BEAA34D6}']
    // check if storage available
    function Available: Boolean;
    // check if image exists (and returns fetch date if requested)
    function ItemExists(
      const AServiceName: String;
      const AIdentifier: WideString;
      const AFetchedDate: PDateTime
    ): Boolean;
    // add image to storage
    function AddItem(
      const AServiceName: String;
      const AIdentifier: WideString;
      const AFetchedDate: TDateTime
    ): Boolean;
  end;

implementation

end.