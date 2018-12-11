# Temporary folder with notes for issue #102

The full fork of the [zip library by Mark Karpov](https://github.com/mrkkrp/zip) can be found on my github: [https://github.com/arbertrary/zip](https://github.com/arbertrary/zip)
This includes (very simple) debugging output in the function `Codec.Archive.Zip.Internal.locateECD` and a `Main.hs` that wants to scan the Archive for its files.

## Files:

- testfile.zip (a normal zip archive)
- self-extracting-zip (a Haskell executable with appended zip archive)
- zipinfo_normal (output of the cli program `zipinfo` for testfile.zip)
- zipinfo_broken (output of `zipinfo` for self-extracting-zip)
- output_normal (debugging output from running the `Main.hs` for testfile.zip)
- output_broken (debugging output from running Main for self-extracting-zip)

## Notes:

- The ZIP format specifications: http://www.onicos.com/staff/iz/formats/zip.html
- 0x06054b50 =   0      4 bytes  End of central dir signature (0x06054b50 = 101010256)

- problem arises first in `locateECD`
- sizeCheck finds correct length of file (8234614)
- `hSeek h SeekFromEnd (-22)`
		this makes it so that EOCD record offset will be skipped
		or rather that the entry point is directly at the
- limit = max 0 (fsize - 0xffff - 22)
		= 8169057
		not sure what the limit does
- `pos <- subtract 4 <$> hTell h`
		outputs exactly the same as zipinfo for "Actual end-cent-dir record offset"
		8234592 = 8234614 -22
- `result <- runMaybeT ...`
		checkZip64 is not stepped into
		this means that Nothing is returned here
- `checkComment` returns Just pos once
		ich believe checkComment takes the current pospos (aka 8234592 = 8234614 -22), 
		adds 20, which gives the Comment
- `checkCDSig`:
		`if sigPos == 0xffffffff -- Zip64 is used`; does not apply here
		checkCDSig returns Nothing with the self-extracting zip
		-> 0x02014b50 (normal case: central directory file header signature -> ist True bei normaler zip)
		with the self-extracting-zip this is False. Why?
- after that: loops between getNum and loop functions (see `output` files

- `getNum` has the line `result <- runGet f <$> B.hGet h n`
		for the normal zip file, this returns Right 33639248 (which is 0x02014b50)
		therefore the if-clause in checkCDSig is true (if cdSig == 0x02014b50)
		for the self-extracting-zip, this returns Right 0

		BUT: 
		before that there is also the output of val 1101 in the correct example 
		for the self-extracting-zip this output is 227
		zipinfo says: "its (expected) offset in bytes from the beginning of the zipfile is 227"
		**That means the function DOES somehow recognize the zip/the ECD.**
		Maybe it just doesn't know how/where to stop looking when going from the end of the file?


what does the second val output 0x02014b50 mean? Why and where does this work for the normal zip file?
