unit u_InetFunc;

interface

procedure OpenUrlInBrowser(const URL: string);

implementation

uses
  Windows,
  ShellAPI;

procedure OpenUrlInBrowser(const URL: string);
begin
  ShellExecute(0, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

end.
