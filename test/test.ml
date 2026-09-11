let () =
  Testo.interpret_argv
    ~expectation_workspace_root:Fpath.(v "test" / "snapshots")
    ~project_name:"toon"
    (fun _opts -> Test_roundtrip.test () @ Test_fixtures.test ())
