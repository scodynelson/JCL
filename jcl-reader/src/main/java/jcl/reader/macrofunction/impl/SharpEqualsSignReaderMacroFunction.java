package jcl.reader.macrofunction.impl;

import jcl.reader.macrofunction.MacroFunctionReader;
import jcl.reader.macrofunction.ReaderMacroFunction;
import jcl.reader.syntax.CharacterConstants;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.types.Variable;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

/**
 * Implements the '#=' Lisp reader macro.
 */
public class SharpEqualsSignReaderMacroFunction implements ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) throws ReaderErrorException {
		assert codePoint == CharacterConstants.EQUALS_SIGN;

		if (Variable.ReadSuppress) {
			return null;
		} else if (numArg == null) {
			throw new ReaderErrorException("Missing label for #=.");
		} else if (reader.SHARP_EQUAL_FINAL_TABLE.containsKey(numArg) || reader.SHARP_EQUAL_TEMP_TABLE.containsKey(numArg)) {
			throw new ReaderErrorException("Label already defined: #" + numArg + '=');
		} else {
			final Random random = new Random();
			final int tag = random.nextInt();

			reader.SHARP_EQUAL_TEMP_TABLE.put(numArg, tag);

			final LispStruct token = reader.read();

			reader.SHARP_EQUAL_REPL_TABLE.put(tag, token);

			final Map<Long, LispStruct> sharpEqualReplTable = new HashMap<>();
			reader.circleSubst(sharpEqualReplTable, token);

			reader.SHARP_EQUAL_FINAL_TABLE.put(numArg, token);

			return null;
		}
	}
}
