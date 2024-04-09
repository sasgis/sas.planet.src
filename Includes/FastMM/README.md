# FastMM5
FastMM is a fast replacement memory manager for Embarcadero Delphi applications that scales well across multiple threads and CPU cores, is not prone to memory fragmentation, and supports shared memory without the use of external .DLL files.

![FastMM5-social.png](Images/FastMM5-social.png "FastMM5-social.png")

Version 5 is a complete rewrite of FastMM. It is designed from the ground up to simultaneously keep the strengths and address the shortcomings of version 4.992:
* Multithreaded scaling across multiple CPU cores is massively improved, without memory usage blowout. It can be configured to scale close to linearly for any number of CPU cores.
* In the Fastcode memory manager benchmark tool FastMM 5 scores 15% higher than FastMM 4.992 on the single threaded benchmarks, and 30% higher on the multithreaded benchmarks. (I7-8700K CPU, EnableMMX and AssumeMultithreaded options enabled.)
* It is fully configurable runtime. There is no need to change conditional defines and recompile to change options. (It is however backward compatible with many of the version 4 conditional defines.) 
* Debug mode uses the same debug support library as version 4 (FastMM_FullDebugMode.dll) by default, but custom stack trace routines are also supported. Call FastMM_EnterDebugMode to switch to debug mode ("FullDebugMode") and call FastMM_ExitDebugMode to return to performance mode. Calls may be nested, in which case debug mode will be exited after the last FastMM_ExitDebugMode call.
* Supports 8, 16, 32 or 64 byte alignment of all blocks. Call FastMM_EnterMinimumAddressAlignment to request a minimum block alignment, and FastMM_ExitMinimumAddressAlignment to rescind a prior request. Calls may be nested, in which case the coarsest alignment request will be in effect.
* All event notifications (errors, memory leak messages, etc.) may be routed to the debugger (via OutputDebugString), a log file, the screen or any combination of the three. Messages are built using templates containing mail-merge tokens. Templates may be changed runtime to facilitate different layouts and/or translation into any language. Templates fully support Unicode, and the log file may be configured to be written in UTF-8 or UTF-16 format, with or without a BOM.
* It may be configured runtime to favour speed, memory usage efficiency or a blend of the two via the FastMM_SetOptimizationStrategy call.

Homepage: https://github.com/pleriche/FastMM5

### Developed by
Pierre le Riche, copyright 2004 - 2023, all rights reserved

### Sponsored by
[gs-soft AG](https://www.gs-soft.com/)

### Licence
FastMM 5 is dual-licensed.  You may choose to use it under the restrictions of the GPL v3 licence at no cost to you, or you may purchase a commercial licence.  A commercial licence grants you the right to use FastMM5 in your own applications, royalty free, and without any requirement to disclose your source code nor any modifications to FastMM to any other party.  A commercial licence lasts into perpetuity, and entitles you to all future updates, free of charge.  A commercial licence is sold per developer developing applications that use FastMM, as follows:
<table>
<tr><td>Number Of Developers</td><td>Price (USD)</td></tr>
<tr><td>1 developer</td><td>$99</td></tr>
<tr><td>2 developers</td><td>$189</td></tr>
<tr><td>3 developers</td><td>$269</td></tr>
<tr><td>4 developers</td><td>$339</td></tr>
<tr><td>5 developers</td><td>$399</td></tr>
<tr><td>More than 5 developers</td><td>$399 + $50 per developer from the 6th onwards</td></tr>
<tr><td>Site licence (unlimited number of developers affiliated with the owner of the licence, i.e. employees, co-workers, interns and contractors)</td><td>$999</td></tr>
</table>

Please send an e-mail to fastmm@leriche.org to request an invoice before or after payment is made.  Payment may be made via PayPal at https://www.paypal.me/fastmm (paypal@leriche.org), or via bank transfer.  Bank details will be provided on the invoice.

Support (via e-mail) is available for users with a commercial licence.  Enhancement requests submitted by users with a commercial licence will be prioritized.

### Usage Instructions
Add FastMM5.pas as the first unit in your project's DPR file.  It will install itself automatically during startup, replacing the default memory manager.

In order to share the memory manager between the main application and libraries call FastMM_AttemptToUseSharedMemoryManager (in order to use the memory manager of another module in the process) or FastMM_ShareMemoryManager (to share the memory manager instance of the current module with other modules).  It is important to share the memory manager between modules where memory allocated in the one module may be freed by the other.

If the application requires memory alignment greater than the default, call FastMM_EnterMinimumAddressAlignment and once the greater alignment is no longer required call FastMM_ExitMinimumAddressAlignment.  Calls may be nested.  The coarsest memory alignment requested takes precedence.

At the cost of performance and increased memory usage FastMM can log additional metadata together with every block.  In order to enable this mode call FastMM_EnterDebugMode and to exit debug mode call FastMM_ExitDebugMode.  Calls may be nested in which case debug mode will be active as long as the number of FastMM_EnterDebugMode calls exceed the number of FastMM_ExitDebugMode calls.  In debug mode freed memory blocks will be filled with the byte pattern $808080... so that usage of a freed memory block or object, as well as corruption of the block header and/or footer will likely be detected.  If the debug support library, FastMM_FullDebugMode.dll, is available and the application has not specified its own handlers for FastMM_GetStackTrace and FastMM_ConvertStackTraceToText then the support library will be loaded during the first call to FastMM_EnterDebugMode.

Events (memory leaks, errors, etc.) may be logged to file, displayed on-screen, passed to the debugger or any combination of the three.  Specify how each event should be handled via the FastMM_LogToFileEvents, FastMM_MessageBoxEvents and FastMM_OutputDebugStringEvents variables.  The default event log filename will be built from the application filepath, but may be overridden via FastMM_SetEventLogFilename.  Messages are built from templates that may be changed/translated by the application.

The optimization strategy of the memory manager may be tuned via FastMM_SetOptimizationStrategy.  It can be set to favour performance, low memory usage, or a blend of both.  The default strategy is to blend the performance and low memory usage goals.

The default configuration should scale close to linearly up to between 8 and 16 threads, so for most applications there should be no need to tweak any performance settings. Beyond 16 threads you may consider increasing the number of arenas (CFastMM_...BlockArenaCount), but inspect the thread contention counts first (FastMM_...BlockThreadContentionCount), before assuming that it is necessary.


### The following conditional defines are supported
* FastMM_FullDebugMode (or FullDebugMode) - If defined then FastMM_EnterDebugMode will be called on startup so that the memory manager starts up in debug mode.  If FullDebugMode is defined then the FastMM_DebugLibraryStaticDependency define is also implied.
* FastMM_FullDebugModeWhenDLLAvailable (or FullDebugModeWhenDLLAvailable) - If defined an attempt will be made to load the debug support library during startup.  If successful then FastMM_EnterDebugMode will be called so that the memory manager starts up in debug mode.
* FastMM_DebugLibraryStaticDependency - If defined there will be a static dependency on the debug support library, FastMM_FullDebugMode.dll (32-bit) or FastMM_FullDebugMode64.dll (64-bit).  If FastMM_EnterDebugMode will be called in the startup code while and the memory manager will also be shared between an application and libraries, then it may be necessary to enable this define in order to avoid DLL unload order issues during application shutdown (typically manifesting as an access violation when attempting to report on memory leaks during shutdown).  It is a longstanding issue with Windows that it is not always able to unload DLLs in the correct order on application shutdown when DLLs are loaded dynamically during startup.  Note that while enabling this define will introduce a static dependency on the debug support library, it does not actually enter debug mode by default - FastMM_EnterDebugMode must still be called to enter debug mode, and FastMM_ExitDebugMode can be called to exit debug mode at any time.
* FastMM_ClearLogFileOnStartup (or ClearLogFileOnStartup) - When defined FastMM_DeleteEventLogFile will be called during startup, deleting the event log file (if it exists).
* FastMM_Align16Bytes (or Align16Bytes) - When defined FastMM_EnterMinimumAddressAlignment(maa16Bytes) will be called during startup, forcing a minimum of 16 byte alignment for memory blocks.  Note that this has no effect under 64 bit, since 16 bytes is already the minimum alignment.
* FastMM_5Arenas, FastMM_6Arenas .. FastMM_16Arenas - Increases the number of arenas from the default values.  See the notes for the CFastMM_SmallBlockArenaCount constant for guidance on the appropriate number of arenas.
* FastMM_DisableAutomaticInstall - Disables automatic installation of FastMM as the memory manager.  If defined then FastMM_Initialize should be called from application code in order to install FastMM, and FastMM_Finalize to uninstall and perform the leak check (if enabled), etc.
* FastMM_EnableMemoryLeakReporting (or EnableMemoryLeakReporting) - If defined then the memory leak summary and detail will be added to the set of events logged to file (FastMM_LogToFileEvents) and the leak summary will be added to the set of events displayed on-screen (FastMM_MessageBoxEvents).
* FastMM_RequireDebuggerPresenceForLeakReporting (or RequireDebuggerPresenceForLeakReporting) - Used in conjunction with EnableMemoryLeakReporting - if the application is not running under the debugger then the EnableMemoryLeakReporting define is ignored.
* FastMM_NoMessageBoxes (or NoMessageBoxes) - Clears the set of events that will cause a message box to be displayed (FastMM_MessageBoxEvents) on startup.
* FastMM_ShareMM (or ShareMM) - If defined then FastMM_ShareMemoryManager will be called during startup, sharing the memory manager of the module if the memory manager of another module is not already being shared.
* FastMM_ShareMMIfLibrary (or ShareMMIfLibrary) - If defined and the module is not a library then the ShareMM define is disabled.
* FastMM_AttemptToUseSharedMM (or AttemptToUseSharedMM) - If defined FastMM_AttemptToUseSharedMemoryManager will be called during startup, switching to using the memory manager shared by another module (if there is a shared memory manager).
* FastMM_NeverUninstall (or NeverUninstall) - Sets the FastMM_NeverUninstall global variable to True.  Use this if any leaked pointers should remain valid after this unit is finalized.
* PurePascal - The assembly language code paths are disabled, and only the Pascal code paths are used.  This is normally used for debugging purposes only.

### Supported Compilers
Delphi XE3 and later

### Supported Platforms
Windows, 32-bit and 64-bit

### Change Log
##### Version 5.00
* First non-beta release of FastMM 5.

##### Version 5.01
* Enhancement: Log a stack trace for the virtual method call that lead to a "virtual method call on freed object" error

##### Version 5.02
* Backward compatibility improvement: If ReportMemoryLeaksOnShutdown = True then mmetUnexpectedMemoryLeakSummary will automatically be included in FastMM_MessageBoxEvents, and the the leak summary will thus be displayed on shutdown.
* FastMM in debug mode will now catch all TObject virtual method calls on a freed object. Previously it only caught some commonly used ones.
* Increase the number of virtual methods supported by TFastMM_FreedObject to 75. (There are some classes in the RTL that have more than 30 virtual methods, e.g. TStringList).
* Add a lock timeout for FastMM_LogStateToFile and FastMM_WalkBlocks. Some severe memory corruption crashes may leave an arena locked, in which case it was previously not possible to walk blocks or dump the memory manager state to file in the crash handler.
* Add backward compatibility support for the ClearLogFileOnStartup v4 define.

##### Version 5.03
* Add runtime support for configuring the number of entries in the debug block allocation and free stack traces (FastMM_SetDebugModeStackTraceEntryCount)
* Enhancements to BorlndMM.dll: Support for a "DEBUG" build configuration as well as additional exports
* Additional demo applications
* Add a FastMM_NeverUninstall boolean variable. It will be set to True on startup if "NeverUninstall" is defined (for backward compatibility with FastMM4). This is useful in the rare situation where live pointers are expected to remain valid after the FastMM unit is finalized.
* Change FastMM_ScanDebugBlocksForCorruption to a function that always returns a boolean result of True. This allows it to be used in a debug watch, thus scanning blocks every time the debugger stops on a breakpoint, etc.
* Expose DebugLibrary_GetRawStackTrace and DebugLibrary_GetFrameBasedStackTrace in order to allow runtime switching between raw and frame based stack traces.
* Add support for a new conditional define "FastMM_DisableAutomaticInstall". When defined FastMM will not be installed automatically, and instead the application should call FastMM_Initialize to initialize and install FastMM, and finally FastMM_Finalize to uninstall it. This allows the application runtime control over whether to use FastMM or not. Note that FastMM_Initialize has to be called very early in the unit initialization sequence, before any memory is allocated through the default memory manager. Practically this means it has to be called from either the first or second (after FastMM5.pas) unit in your project DPR.
* Add a new boolean configuration variable: FastMM_DebugMode_ScanForCorruptionBeforeEveryOperation. When this variable is True and debug mode is enabled, all debug blocks will be checked for corruption on entry to any memory manager operation (i.e. GetMem, FreeMem, AllocMem and ReallocMem). It is analogous to the v4 FullDebugModeScanMemoryPoolBeforeEveryOperation option. Note that this comes with an extreme performance penalty.
* Add the FastMM_5Arenas through FastMM_16Arenas defines in order to allow control of the number of arenas through conditional defines instead of requiring editing of the FastMM5.pas source file. As a rule of thumb, FastMM performs optimally if the number of arenas is between 0.5x to 1x the number of threads that are expected to call the memory manager simultaneously.
* Expose the FastMM_DetectStringData and FastMM_DetectClassInstance functions, which are used to determine whether a pointer potentially points to string data or a class instance. These may be useful inside the FastMM_WalkBlocks callback in order to collect more detailed statistics about the memory pool content.
* Add support for a "FastMM_DebugLibraryStaticDependency" define, which is automatically defined if the legacy "FullDebugMode" option is defined. When "FastMM_DebugLibraryStaticDependency" is defined the application will have a static dependency on the debug support library. This prevents the premature unloading of the debug support library (and crash on shutdown) when the memory manager is shared between the main application and a statically linked library.
* Add FastMM_GetCurrentMemoryUsage, FastMM_SetMemoryUsageLimit and FastMM_GetMemoryUsageLimit calls. FastMM_GetCurrentMemoryUsage returns the number of bytes of address space that is currently either committed or reserved by FastMM. This includes the total used by the heap, as well as all internal management structures. FastMM_SetMemoryUsageLimit allows the application to specify a maximum amount of memory that may be allocated through FastMM. An attempt to allocate more than this amount will fail and lead to an "Out of Memory" exception. Note that after the first failure the maximum amount of memory that may be allocated is slightly increased in order to allow the application to allocate some additional memory in subsequent attempts. This is to allow for a graceful shutdown. Specify 0 for no limit. FastMM_GetMemoryUsageLimit returns the current limit in effect. 0 = no limit (the default).

##### Version 5.04
* Implement a return address info cache for the LogStackTrace call in FastMM_FullDebugMode. This greatly speeds up logging of memory leak detail to file when there are many leaks with the same (or similar) stack traces.
* Avoid opening and reopening the event log file multiple times when logging leak detail. This improves performance significantly when logging multiple memory leaks.
* Ensure that the event log file is closed before showing any dialogs, so the user can access it while the dialog is displayed.
* Implement several 32-bit SSE2 move routines (64-bit already used SSE2)
* Make the static dependency on the FastMM_FullDebugMode library optional when FastMM_FullDebugMode is defined. When FastMM_DebugLibraryDynamicLoading (or LoadDebugDLLDynamically) is defined then the DLL will be loaded dynamically.

##### Version 5.05
* Add the FastMM_AllocateTopDown (Boolean, default False) option. When True, allocates all memory from the top of the address space downward.  This is useful to catch bad pointer typecasts in 64-bit code, where pointers would otherwise often fit in a 32-bit variable.  Note that this comes with a performance impact in the other of O(n^2), where n is the number of chunks obtained from the OS.
* Add support for Eurekalog 7 in FastMM_FullDebugMode.
* Add FastMM_BeginEraseFreedBlockContent and FastMM_EndEraseFreedBlockContent calls. These calls enable/disable the erasure of the content of freed blocks.  Calls may be nested, in which case erasure is only disabled when the number of FastMM_EndEraseFreedBlockContent calls equal the number of FastMM_BeginEraseFreedBlockContent calls.  When enabled the content of all freed blocks is filled with the debug pattern $80808080 before being returned to the memory pool.  This is useful for security purposes, and may also help catch "use after free" programming errors (like debug mode, but at reduced CPU cost).
* Add support for the FastMM_IncludeLegacyOptionsFile define. If defined the legacy FastMM4Options.inc will be included (and the version 4 options will be translated to the equivalent v5 options).
* Add the FastMM_NoDebugInfo option. If defined then debug info will not be emitted for FastMM5.pas, stopping the debugger from stepping into it.
* Fix a race condition in FastMM_ScanDebugBlocksForCorruption that could erroneously report a debug block that is in the process of being freed by another thread as corrupted.
