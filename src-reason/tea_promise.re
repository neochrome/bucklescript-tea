let cmd = (promise, tagger) =>
  Vdom.(
    Tea_cmd.call(
      fun
      | callbacks => {
          let _ =
            promise
            |> Js.Promise.then_(
                 fun
                 | res =>
                   switch (tagger(res)) {
                   | Some(msg) =>
                     let () = callbacks^.enqueue(msg);
                     Js.Promise.resolve();
                   | None => Js.Promise.resolve()
                   },
               );

          ();
        },
    )
  );

let result = (promise, msg) =>
  Vdom.(
    Tea_cmd.call(
      fun
      | callbacks => {
          let enq = result => callbacks^.enqueue(msg(result));

          let _ =
            promise
            |> Js.Promise.then_(
                 fun
                 | res => {
                     let resolve = enq(Ok(res));
                     Js.Promise.resolve(resolve);
                   },
               )
            |> Js.Promise.catch(
                 fun
                 | err => {
                     let err_to_string = err => {j|$err|j};
                     let reject = enq(Error(err_to_string(err)));
                     Js.Promise.resolve(reject);
                   },
               );

          ();
        },
    )
  );
