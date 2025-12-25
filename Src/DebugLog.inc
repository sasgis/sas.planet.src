
{******************************************************************************

                     IDebugLogger compile-time configuration

  Two logger types are available for use:

    1. TDebugLoggerAsync (asynchronous) - includes an internal message queue
       and a background thread for processing messages. To enable this logger,
       define ENABLE_ASYNC_LOGGER.

    2. TDebugLogger - a simple, synchronous logger that directly forwards
       messages to the consumer

  For each logger type, you can choose from the following consumers:

    1. TDebugLoggerConsumerSimple - outputs messages using OutputDebugString
       (this is the default consumer)

    2. TDebugLoggerConsumerToLogFile - writes messages to a file: log/debug.txt
       (enabled by defining USE_LOG_FILE_CONSUMER)

    3. TDebugLoggerConsumerFake - a dummy consumer that discards all messages
       and performs no output (enabled by defining USE_FAKE_CONSUMER)

******************************************************************************}

{$IFDEF RELEASE}
  {.$DEFINE ENABLE_ASYNC_LOGGER}
  {.$DEFINE USE_LOG_FILE_CONSUMER}
  {$DEFINE USE_FAKE_CONSUMER}
{$ELSE}
  {$DEFINE ENABLE_ASYNC_LOGGER}
  {.$DEFINE USE_LOG_FILE_CONSUMER}
  {.$DEFINE USE_FAKE_CONSUMER}
{$ENDIF}

{******************************************************************************}

{$IF DEFINED(DEBUG) AND NOT DEFINED(USE_FAKE_CONSUMER)}

  {.$DEFINE ENABLE_MAIN_FORM_STATE_LOGGING}
  
  {.$DEFINE ENABLE_TILE_MATRIX_LOGGING}
  {.$DEFINE ENABLE_TILED_MAP_LAYER_LOGGING}

{$IFEND}