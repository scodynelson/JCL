package jcl.reader.macrofunction.impl;

import jcl.LispStruct;
import jcl.reader.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.variables.ReadSuppressVariable;

import java.util.Map;
import java.util.Random;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Implements the '#=' Lisp reader macro.
 */
public class SharpEqualsSignReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.EQUALS_SIGN;

		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for #=.");
		}

		if (reader.SHARP_EQUAL_FINAL_TABLE.containsKey(numArg) || reader.SHARP_EQUAL_TEMP_TABLE.containsKey(numArg)) {
			throw new ReaderErrorException("Label already defined: #" + numArg + '=');
		}

		final Random random = new Random();
		final int tag = random.nextInt();

		reader.SHARP_EQUAL_TEMP_TABLE.put(numArg, tag);

		final LispStruct token = reader.read();

		reader.SHARP_EQUAL_REPL_TABLE.put(tag, token);

		final Map<Long, LispStruct> sharpEqualReplTable = new ConcurrentHashMap<>();
		reader.circleSubst(sharpEqualReplTable, token);

		reader.SHARP_EQUAL_FINAL_TABLE.put(numArg, token);

		return null;
	}
}
