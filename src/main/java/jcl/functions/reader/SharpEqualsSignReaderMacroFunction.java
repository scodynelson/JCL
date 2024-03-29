/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import jcl.lang.ConsStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.Reader;
import jcl.reader.ReaderContext;
import jcl.reader.ReaderContextHolder;
import jcl.util.CodePointConstants;

/**
 * Implements the '#=' Lisp reader macro.
 */
public final class SharpEqualsSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpEqualsSignReaderMacroFunction() {
		super("SHARP-EQUALS");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.EQUALS_SIGN;

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			return null;
		}

		if (numberArgument == null) {
			throw new ReaderErrorException("Missing label for #=.");
		}

		final int numberArgumentInt = numberArgument.toJavaInt();

		final ReaderContext context = ReaderContextHolder.getContext();
		final Map<Integer, LispStruct> sharpEqualFinalTable = context.getSharpEqualFinalTable();
		final Map<Integer, SymbolStruct> sharpEqualTempTable = context.getSharpEqualTempTable();

		if (sharpEqualFinalTable.containsKey(numberArgumentInt)
				|| sharpEqualTempTable.containsKey(numberArgumentInt)) {
			throw new ReaderErrorException("Label already defined: #" + numberArgument + '=');
		}

		final String labelTagName = UUID.randomUUID().toString();
		final SymbolStruct labelTag = SymbolStruct.toLispSymbol(labelTagName);
		sharpEqualTempTable.put(numberArgumentInt, labelTag);

		final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);

		final Map<SymbolStruct, LispStruct> sharpEqualReplTable = context.getSharpEqualReplTable();
		sharpEqualReplTable.put(labelTag, token);

		final Set<LispStruct> sharpEqualCircleSet = new HashSet<>();
		replaceTagsWithTokens(token, sharpEqualReplTable, sharpEqualCircleSet);

		sharpEqualFinalTable.put(numberArgumentInt, token);

		return token;
	}

	/**
	 * Replaces the {@link SymbolStruct} tags located within the provided {@link LispStruct} token with the mapped token
	 * values located within the provided {@code sharpEqualReplTable} {@link Map}. Circularities are also accounted for
	 * by using the provided {@code sharpEqualCircleSet} {@link Set} to keep track of the {@link ConsStruct} tokens
	 * throughout the replacement process.
	 * <p>
	 * NOTE: This method destructively modified the provided {@link LispStruct} token if it is a {@link ConsStruct}
	 *
	 * @param token
	 * 		the {@link LispStruct} token to replace {@link SymbolStruct} tags with their mapped {@link LispStruct} tokens
	 * @param sharpEqualReplTable
	 * 		the {@link Map} of {@link SymbolStruct} tags to their mapped {@link LispStruct} tokens
	 * @param sharpEqualCircleSet
	 * 		the {@link Set} of {@link ConsStruct} tokens within the provided {@link LispStruct} token used to track
	 * 		circularities.
	 *
	 * @return the modified token with all {@link SymbolStruct} tags replaced with their corresponding
	 * {@link LispStruct} tokens
	 */
	private static LispStruct replaceTagsWithTokens(final LispStruct token,
	                                                final Map<SymbolStruct, LispStruct> sharpEqualReplTable,
	                                                final Set<LispStruct> sharpEqualCircleSet) {

		if (token instanceof SymbolStruct) {
			if (sharpEqualReplTable.containsKey(token)) {
				return sharpEqualReplTable.get(token);
			}
		} else if (token instanceof ConsStruct) {

			if (!sharpEqualCircleSet.contains(token)) {
				sharpEqualCircleSet.add(token);

				final ConsStruct consToken = (ConsStruct) token;

				final LispStruct car = consToken.car();
				final LispStruct carSubst = replaceTagsWithTokens(car, sharpEqualReplTable, sharpEqualCircleSet);

				if (!carSubst.eq(car)) {
					consToken.rplaca(carSubst);
				}

				final LispStruct cdr = consToken.cdr();
				final LispStruct cdrSubst = replaceTagsWithTokens(cdr, sharpEqualReplTable, sharpEqualCircleSet);

				if (!cdrSubst.eq(cdr)) {
					consToken.rplacd(cdrSubst);
				}
			}
		}

		return token;
	}
}
