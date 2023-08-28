open Passert
open Music
open NativeLazyStreams

let melody1 = [ quarter (C, 3); quarter_rest; half (E, 3) ]

let list_of_m1_events =
  [
    Tone (0., (C, 3), 60);
    Stop (0.25, (C, 3));
    Tone (0.25, (E, 3), 60);
    Stop (0.5, (E, 3));
    Tone (0., (C, 3), 60);
  ]

let melody2 : obj list =
  [
    half (C, 1);
    quarter (Db, 1);
    eighth (D, 2);
    eighth_rest;
    eighth (Eb, 2);
    quarter_rest;
    half (E, 3);
  ]

let list_of_m2_events : event list =
  [
    Tone (0., (C, 1), cDEFAULT_VOLUME);
    Stop (0.5, (C, 1));
    Tone (0., (Db, 1), cDEFAULT_VOLUME);
    Stop (0.25, (Db, 1));
    Tone (0., (D, 2), cDEFAULT_VOLUME);
    Stop (0.125, (D, 2));
    Tone (0.125, (Eb, 2), cDEFAULT_VOLUME);
    Stop (0.125, (Eb, 2));
    Tone (0.25, (E, 3), cDEFAULT_VOLUME);
    Stop (0.5, (E, 3));
  ]

let paired_harmony =
  [
    Tone (0., (C, 3), 60);
    Tone (0.25, (G, 3), 60);
    Stop (0., (C, 3));
    Stop (0.25, (G, 3));
    Tone (0., (E, 3), 60);
    Tone (0.25, (B, 3), 60);
    Stop (0.25, (E, 3));
    Tone (0., (C, 3), 60);
    Stop (0.25, (B, 3));
    Tone (0., (G, 3), 60);
  ]

let test_list_to_stream () =
  let expect = passert ~suite:"list_to_stream" in
  expect
    (first 5 (list_to_stream melody1) = list_of_m1_events)
    "melody1 to events";
  expect
    (first 10 (list_to_stream melody2) = list_of_m2_events)
    "melody2 to events"

let test_pair () =
  let expect = passert ~suite:"pair" in
  expect (first 10 harmony = paired_harmony) "harmony"

let tests () =
  test_list_to_stream ();
  test_pair ()

let _ = tests ()
