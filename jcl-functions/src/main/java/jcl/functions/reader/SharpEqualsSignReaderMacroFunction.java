/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;

import jcl.lang.ConsStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.Reader;
import jcl.reader.ReaderContext;
import jcl.reader.ReaderContextHolder;
import jcl.util.CodePointConstants;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#=' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpEqualsSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	public SharpEqualsSignReaderMacroFunction(final Reader reader) {
		super("SHARP-EQUALS");
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.EQUALS_SIGN, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.EQUALS_SIGN;

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return null;
		}

		if (!numberArgument.isPresent()) {
			throw new ReaderErrorException("Missing label for #=.");
		}
		final BigInteger numberArgumentValue = numberArgument.get();

		final ReaderContext context = ReaderContextHolder.getContext();
		final Map<BigInteger, LispStruct> sharpEqualFinalTable = context.getSharpEqualFinalTable();
		final Map<BigInteger, SymbolStruct> sharpEqualTempTable = context.getSharpEqualTempTable();

		if (sharpEqualFinalTable.containsKey(numberArgumentValue)
				|| sharpEqualTempTable.containsKey(numberArgumentValue)) {
			throw new ReaderErrorException("Label already defined: #" + numberArgumentValue + '=');
		}

		final String labelTagName = UUID.randomUUID().toString();
		final SymbolStruct labelTag = SymbolStruct.toLispSymbol(labelTagName);
		sharpEqualTempTable.put(numberArgumentValue, labelTag);

		final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);

		final Map<SymbolStruct, LispStruct> sharpEqualReplTable = context.getSharpEqualReplTable();
		sharpEqualReplTable.put(labelTag, token);

		final Set<LispStruct> sharpEqualCircleSet = new HashSet<>();
		replaceTagsWithTokens(token, sharpEqualReplTable, sharpEqualCircleSet);

		sharpEqualFinalTable.put(numberArgumentValue, token);

		return token;
	}

	/**
	 * Replaces the {@link SymbolStruct} tags located within the provided {@link LispStruct} token with the mapped
	 * token values located within the provided {@code sharpEqualReplTable} {@link Map}. Circularities are also
	 * accounted for by using the provided {@code sharpEqualCircleSet} {@link Set} to keep track of the {@link
	 * ConsStruct} tokens throughout the replacement process.
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
	 * @return the modified token with all {@link SymbolStruct} tags replaced with their corresponding {@link
	 * LispStruct} tokens
	 */
	private static LispStruct replaceTagsWithTokens(final LispStruct token, final Map<SymbolStruct, LispStruct> sharpEqualReplTable,
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
