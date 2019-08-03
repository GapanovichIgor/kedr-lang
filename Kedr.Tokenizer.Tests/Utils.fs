module Kedr.Tokenizer.Tests.Utils

open Hedgehog
open System

let (:=) name body : (string * Property<_>) = (name, body)

let printProperty (name : string, body : Property<_>) =
    let previousColor = Console.ForegroundColor
        
    Console.ForegroundColor <- ConsoleColor.DarkBlue
    Console.Write name
    Console.Write ": "
    
    let report = Property.report body
    
    Console.ForegroundColor <-
        match report.Status with
        | OK -> ConsoleColor.Green
        | GaveUp -> ConsoleColor.Yellow
        | Failed _ -> ConsoleColor.Red
        
    Console.WriteLine (Report.render report) 
    
    Console.ForegroundColor <- previousColor