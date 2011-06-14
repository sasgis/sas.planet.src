unit u_TileDownloaderThreadBase;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  Types,
  i_JclNotify,
  i_TileDownlodSession,
  u_MapType;

type
  TTileDownloaderThreadBase = class(TThread)
  protected
    FMapType: TMapType;
    FLoadXY: TPoint;
    FZoom: byte;
    FLoadUrl: string;
    FCancelEvent: TEvent;
    FCancelNotifier: IJclNotifier;

    procedure SleepCancelable(ATime: Cardinal);
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Terminate; reintroduce;
    property MapType: TMapType read FMapType;
  end;

implementation

uses
  SysUtils,
  u_JclNotify,
  u_ResStrings;

constructor TTileDownloaderThreadBase.Create(CreateSuspended: Boolean);
begin
  inherited Create(CreateSuspended);
  FCancelEvent := TEvent.Create;
  FCancelNotifier := TJclBaseNotifier.Create;
end;

destructor TTileDownloaderThreadBase.Destroy;
begin
  Terminate;
  inherited;
  FreeAndNil(FCancelEvent);
end;

procedure TTileDownloaderThreadBase.SleepCancelable(ATime: Cardinal);
begin
  if  ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderThreadBase.Terminate;
begin
  inherited;
  FCancelEvent.SetEvent;
  FCancelNotifier.Notify(nil);
end;

end.
