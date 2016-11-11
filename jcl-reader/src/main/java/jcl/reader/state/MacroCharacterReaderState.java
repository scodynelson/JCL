/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.lang.FunctionStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 4 of the Reader Algorithm.
 * <p>
 * If x is a terminating or non-terminating macro character then its associated reader macro function is called with
 * two arguments, the input stream and x.
 * <p>
 * The reader macro function may read characters from the input stream; if it does, it will see those characters
 * following the macro character. The Lisp reader may be invoked recursively from the reader macro function.
 * </p>
 * <p>
 * The reader macro function must not have any side effects other than on the input stream; because of backtracking and
 * restarting of the read operation, front ends to the Lisp reader (e.g., ``editors'' and ``rubout handlers'') may
 * cause the reader macro function to be called repeatedly during the reading of a single expression in which x only
 * appears once.
 * </p>
 * <p>
 * The reader macro function may return zero values or one value. If one value is returned, then that value is returned
 * as the result of the read operation; the algorithm is done. If zero values are returned, then step 1 is re-entered.
 * </p>
 */
//@Component
class MacroCharacterReaderState implements ReaderState {

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		// NOTE: This will throw errors when it reaches an EOF
		final ReadPeekResult readResult = tokenBuilder.getPreviousReadResult();
		final int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final FunctionStruct readerMacroFunction = readtable.getMacroCharacter(codePoint);

		if (readerMacroFunction == null) {
			throw new ReaderErrorException("No reader macro function exists for character: " + codePoint + '.');
		}

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final LispStruct token = readerMacroFunction.apply(
				inputStreamStruct,
				LispStructFactory.toCharacter(codePoint),
				NILStruct.INSTANCE
		);

		if (token == null) {
			return readerStateMediator.read(tokenBuilder);
		} else {
			return token;
		}
	}
}
