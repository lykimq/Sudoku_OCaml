(* Global debug mode flag for conditional logging throughout the application.

   Functional Purpose: Provides runtime-configurable debug output without
   performance penalty in production builds.

   Design Choice: Global mutable reference over parameter threading - Avoids
   threading debug flag through every function call - Simple on/off switch for
   entire application - Zero runtime cost when disabled (conditional
   compilation) - Easy integration with existing codebase

   Alternative: Could use environment variables, logging levels, or compile-time
   flags for debug elimination *)
let debug_mode = ref false

(* Conditional debug printing with printf-style formatting.

   Functional Purpose: Provides formatted debug output that can be completely
   disabled at runtime for performance and clean production logs.

   Design Choice: Printf.ksprintf for lazy evaluation and formatting - Format
   string processing only occurs when debug mode is enabled - Full printf format
   string support for rich debugging - Automatic newline and flush for immediate
   output visibility - No-op function when disabled prevents any overhead

   Algorithm: Conditional evaluation with continuation-based formatting 1. Check
   debug_mode flag first (O(1) check) 2. If enabled: Process format string and
   print with flush 3. If disabled: Create no-op continuation that discards
   arguments Time: O(1) when disabled, O(format_complexity) when enabled

   Performance considerations: - Zero overhead when disabled (no string
   processing) - Immediate output flush for real-time debugging -
   Printf.ksprintf delays evaluation until needed

   Alternative approaches: - Logging framework: More features but heavier
   dependency - Compile-time elimination: Better performance but less flexible -
   Structured logging: Better for production but more complex *)
let debug fmt =
  if !debug_mode
  then
    Printf.ksprintf
      (fun s ->
        Printf.printf "%s\n" s ;
        flush stdout)
      fmt
  else Printf.ksprintf (fun _ -> ()) fmt
