package jcl.functions.readtable;

import java.math.BigInteger;

import jcl.functions.BuiltInFunctionStructImpl;
import jcl.lang.CharacterStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;

public final class ReadDispatchCharacterFunction extends BuiltInFunctionStructImpl {

	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String DISPATCH_CHAR_ARGUMENT = "DISPATCH-CHAR";
	private static final String IGNORED_ARGUMENT = "IGNORED";

	public ReadDispatchCharacterFunction() {
		super("Reads the dispatch character and invokes the appropriate dispatching character function.",
		      CommonLispSymbols.READ_DISPATCH_CHARACTER.getName(),
		      Parameters.forFunction(CommonLispSymbols.READ_DISPATCH_CHARACTER.getName())
		                .requiredParameter(INPUT_STREAM_ARGUMENT)
		                .requiredParameter(DISPATCH_CHAR_ARGUMENT)
		                .optionalParameter(IGNORED_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.READ_DISPATCH_CHARACTER;
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final InputStreamStruct inputStream = arguments.getRequiredArgument(INPUT_STREAM_ARGUMENT, InputStreamStruct.class);
		final CharacterStruct character = arguments.getRequiredArgument(DISPATCH_CHAR_ARGUMENT, CharacterStruct.class);
		final int dispatchCodePoint = character.toUnicodeCodePoint();

		ReadPeekResult readResult = inputStream.readChar(false, null, false);
		int codePoint = readResult.getResult();

		final StringBuilder digitStringBuilder = new StringBuilder();

		while (Character.isDigit(codePoint)) {
			digitStringBuilder.appendCodePoint(codePoint);

			readResult = inputStream.readChar(false, null, false);
			codePoint = readResult.getResult();
		}

		BigInteger numberArgument = null;
		if (digitStringBuilder.length() >= 1) {
			final String digitString = digitStringBuilder.toString();
			numberArgument = new BigInteger(digitString);
		}

		if (readResult.isEof()) {
			throw new ReaderErrorException("End of file reached when trying to determine read macro function.");
		}
		final int subCodePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final FunctionStruct macroFunction = readtable.getDispatchMacroCharacter(dispatchCodePoint, subCodePoint);

		if (macroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for: " + dispatchCodePoint + subCodePoint + '.');
		}

		if (numberArgument == null) {
			return macroFunction.apply(
					inputStream,
					CharacterStruct.toLispCharacter(subCodePoint),
					NILStruct.INSTANCE
			);
		} else {
			return macroFunction.apply(
					inputStream,
					CharacterStruct.toLispCharacter(subCodePoint),
					IntegerStruct.toLispInteger(numberArgument)
			);
		}
	}
}
