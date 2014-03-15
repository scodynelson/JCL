package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.state.MacroFunctionReader;
import jcl.syntax.CharacterConstants;
import jcl.reader.tokens.SpecialOperatorSymbolTokens;
import jcl.lists.ListStruct;
import jcl.symbols.SymbolStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.variables.ReadSuppressVariable;

/**
 * Implements the '#'' Lisp reader macro.
 */
public class SharpApostropheReaderMacroFunction extends ReaderMacroFunction {

	@Override
	public LispStruct readMacro(final int codePoint, final MacroFunctionReader reader, final Integer numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct expression = reader.read();
		if (ReadSuppressVariable.INSTANCE.getValue()) {
			return null;
		}

		if (expression == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		final SymbolStruct<?> quoteToken = SpecialOperatorSymbolTokens.FUNCTION;
		return ListStruct.buildProperList(quoteToken, expression);
	}
}
