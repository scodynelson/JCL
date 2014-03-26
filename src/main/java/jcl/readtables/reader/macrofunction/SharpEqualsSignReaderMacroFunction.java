package jcl.readtables.reader.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.readtables.reader.Reader;
import jcl.syntax.CharacterConstants;
import jcl.variables.ReadSuppressVariable;

import java.util.UUID;

/**
 * Implements the '#=' Lisp reader macro.
 */
public class SharpEqualsSignReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.EQUALS_SIGN;

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for #=.");
		}

		if (SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.containsKey(numArg)
				|| SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.containsKey(numArg)) {
			throw new ReaderErrorException("Label already defined: #" + numArg + '=');
		}

		final UUID tag = UUID.randomUUID();

		SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.put(numArg, tag);
		final LispStruct token = reader.read();
		SharpTagReaderConstants.SHARP_EQUAL_REPL_TABLE.put(tag, token);

		SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.put(numArg, token);

		return null;
	}
}
