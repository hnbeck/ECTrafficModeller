:- module(gpg, [
      gpg_load_file/1,
      gpg_load_file/2
   ]).

:- use_module(library(option)).

gpg_load_file(File) :-
   gpg_load_file(File,[]).

gpg_load_file(File,Opts) :-
   process_create(path(gpg),
                  ['-d',file(File)],
                  [  stdout(pipe(Stdout)),
                     stderr(null)
                  ]
                 ),
   option(module(M),Opts, user),
   load_files(M:File, [ stream(Stdout),
                        sandboxed(true),
                        if(not_loaded)
                      ] ).


