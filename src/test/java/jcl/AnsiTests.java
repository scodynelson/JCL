package jcl;

import java.nio.file.Paths;

import jcl.compiler.function.InternalEval;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringInputStreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.InternalRead;
import jcl.system.LoadLispFilesExtension;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(LoadLispFilesExtension.class)
class AnsiTests {

	@Test
	@Disabled
	void runAnsiTests() {
		CommonLispSymbols.DEFAULT_PATHNAME_DEFAULTS_VAR.setfSymbolValue(PathnameStruct.toPathname(
				Paths.get("src/test/ansi-tests").toAbsolutePath().toString() + '/'
		));
		final String setVariable = "(load (merge-pathnames \"doit.lsp\"))";
		final StringInputStreamStruct stream = StringInputStreamStruct.toStringInputStream(
				StringStruct.toLispString(setVariable),
				IntegerStruct.ZERO,
				IntegerStruct.toLispInteger(setVariable.length())
		);
		final LispStruct whatRead = InternalRead.read(stream, false, NILStruct.INSTANCE, false);
		final LispStruct eval = InternalEval.eval(whatRead);
		System.out.println(eval);
	}
}