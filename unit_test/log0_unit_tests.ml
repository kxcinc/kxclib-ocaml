open Alcotest

type 'a logger = ?modul:string -> ('a, ppf, return, return) format4 -> 'a

let loggers : ('a logger * string * _ * _) list =
  [
    (Log0.verbose, "VERBOSE", (Some `Thin), `Bright_cyan);
    (Log0.info, "INFO", (Some `Bold), `Bright_cyan);
    (Log0.warn, "WARN", (Some `Bold), `Yellow);
    (Log0.debug, "DEBUG", (Some `Bold), `Magenta);
    (Log0.error, "ERROR", (Some `Bold), `Red);
  ]

let make_expected style color label fmt =
  let pp_header ppf =
    Fmt.colored ?style color ppf "%s"
  in
  Printf.ksprintf (
    sprintf "@<1>%a @[<hov>%s@]@." pp_header (sprintf "[%s]" label)
  ) fmt

let logging buf =
  loggers
  |> List.map (fun ((f : _ logger), label, style, color) ->
    test_case label `Quick (fun () ->
      f "check logging";
      check' string
        ~expected:(make_expected style color label "check logging")
        ~actual:(Buffer.contents buf)
        ~msg:(sprintf "Logging of %s" label);
      Buffer.clear buf
    )
  )

let log_filter buf =
  loggers
  |> List.map (fun (_, label, style, color) ->
    test_case label `Quick (fun () ->
      Log0.LoggingConfig.set_entry_filter (
        Log0.LogFilter_by_label_whitelist [ label ]
      );

      loggers
      |> List.iter (fun ((g: _ logger), _, _, _) ->
        g "check log_filter"
      );

      check' string
        ~expected:(make_expected style color label "check log_filter")
        ~actual:(Buffer.contents buf)
        ~msg:(sprintf "Header of %s" label);

      Buffer.clear buf
    )
  )

let () =
  Printexc.record_backtrace true;

  let buf = Buffer.create 1024 in
  let fmt = Format.formatter_of_buffer buf in
  Log0.LoggingConfig.set_logging_formatter fmt;

  run "Kxclib_log0_unit_tests" ([
    "logging", logging buf;
    "log_filter", log_filter buf;
  ])
