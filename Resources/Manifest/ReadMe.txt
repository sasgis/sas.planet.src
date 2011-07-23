requestedExecutionLevel / level поддерживает несколько параметров:

- asInvoker - приложение будет запущено с привелегиями пользователя
- highestAvailable - приложение будет запущено с максимально доступными привелегиями
- requireAdministrator- приложение будет запущено с максимально привелегиями администратора, появится окно UAC для подтверждения

За подробностями по параметрам манифеста следует обращаться в MSDN: Application Manifest http://msdn.microsoft.com/en-us/library/bb756929.aspx