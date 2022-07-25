structure Proof = struct
val testfiles = 
    [(*"test/array.mldoc",*)
     "test/bool.mldoc",
     "test/byte.mldoc",
     "test/convert-int.mldoc",
     "test/convert-word.mldoc",
     "test/float.mldoc",
     "test/generic-sock.mldoc",
     "test/imperative-io.mldoc",
     "test/inet-db.mldoc",
     "test/integer.mldoc",
     "test/ip-sock.mldoc",
     "test/list-pair.mldoc",
     "test/math.mldoc",
   (*"test/mono-array.mldoc",
     "test/mono-vector.mldoc",*)
     "test/os-path.mldoc",
     "test/os-process.mldoc",
     "test/os.mldoc",
     "test/posix-file-sys.mldoc",
     "test/posix-flags.mldoc",
     "test/posix-io.mldoc",
     "test/posix-proc-env.mldoc",
     "test/posix-signal.mldoc",
     "test/posix.mldoc",
     "test/prim-io.mldoc",
     "test/real.mldoc",
     "test/socket.mldoc",
     "test/string-cvt.mldoc",
     "test/string.mldoc",
     "test/substring.mldoc",
     "test/time.mldoc",
     "test/timer.mldoc",
     "test/unix-sock.mldoc",
(*   "test/vector.mldoc", *)
     "test/word.mldoc"]

  fun doit [] = ()
    | doit (file::files) = 
       (print (file ^ " ...\n");
	Main.main ["_", file];
	doit files)

  fun doall () = doit testfiles
end

	       

