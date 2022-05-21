open Tyxml.Html

module Job = Current.Job

let render_row (id, job) =
  let url = Fmt.str "/job/%s" id in
  let start_time =
    match Lwt.state (Job.start_time job) with
    | Lwt.Sleep -> "(ready to start)"
    | Lwt.Return t -> Utils.string_of_timestamp (Unix.gmtime t)
    | Lwt.Fail f -> Printexc.to_string f
  in
  tr [
    td [ a ~a:[a_href url] [txt id] ];
    td [ txt start_time ];
  ]

let r = object
  inherit Resource.t

  val! can_get = `Viewer

  method! private get ctx =
    let jobs = Current.Job.jobs () in
    Context.respond_ok ctx ~refresh:60 (
      if Current.Job.Map.is_empty jobs then [
        txt "There are no active jobs."
      ] else [
        table ~a:[a_class ["table"]]
          ~thead:(thead [
              tr [
                th [txt "Job"];
                th [txt "Start time"];
              ]
            ])
          (Current.Job.Map.bindings jobs |> List.map render_row)
      ]
    )

  method! nav_link = Some "Jobs"
end

let s ~engine = object
  inherit Resource.t

  val! can_post = `Builder

  method! private post ctx body =
    let data = Uri.query_of_encoded body in
    let pick label ((x,y) : (uri * uri list_wrap)) =
      if x = label then Some y else None
    in
    match List.filter_map (pick "pipeline_id") data |> List.concat with
    | [] -> Context.respond_error ctx `Bad_request "No pipeline id specified"
    | pipeline_id :: _ ->
      match List.filter_map (pick "pipeline_source_id") data |> List.concat with
      | [] -> Context.respond_error ctx `Bad_request "No pipeline source id specified"
      | pipeline_source_id :: _ ->
      let pipeline_link =
        let uri = Fmt.str "/pipelines/%s/%s" pipeline_source_id pipeline_id in
        a ~a:[a_href (uri)] [txt @@ Fmt.str "Return to pipeline"] in
        match List.filter_map (pick "id") data |> List.concat with
        | [] -> Context.respond_error ctx `Bad_request "No jobs selected!"
        | jobs ->
          let failed = ref [] in
          let rebuilding = ref [] in
          jobs |> List.iter (fun job_id ->
              let state = Current.Engine.state engine in
              let jobs = state.Current.Engine.jobs in
              match Current.Job.Map.find_opt job_id jobs with
              | None -> failed := job_id :: !failed
              | Some actions ->
                match actions#rebuild with
                | None -> failed := job_id :: !failed
                | Some rebuild ->
                  let _new_id : string = rebuild () in
                  rebuilding := job_id :: !rebuilding;
                  ()
            );
          let fail_msg = match !failed with
          | [] -> div []
          | failed ->
              div [span [ txt @@
                Fmt.str "%d/%d jobs could not be restarted (because they are no longer active): %a"
                  (List.length failed) (List.length jobs)
                Fmt.(list ~sep:(any ", ") string) failed
            ]]
          in
          let success_msg = match !rebuilding with
          | [] -> div []
          | rebuilding ->
            let open Tyxml_html in
            div [span [ txt @@
              Fmt.str "%d/%d jobs were restarted: %a"
                (List.length rebuilding) (List.length jobs)
                Fmt.(list ~sep:(any ", ") string) rebuilding
            ]]
          in
          let body = [ fail_msg; success_msg; pipeline_link ] in
          Context.respond_ok ctx body

  method! nav_link = None
end
