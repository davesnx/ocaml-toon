let () =
  Testo.interpret_argv
    ~expectation_workspace_root:Fpath.(v "test" / "snapshots")
    ~project_name:"toon"
    (fun _opts ->
      Test_parse.test () @ Test_print.test () @ Test_roundtrip.test ()
      @ Test_spec.test ())
