namespace std

open System

module Env =
    let argv () =
        Environment.GetCommandLineArgs()