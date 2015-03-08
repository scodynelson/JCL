/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.UUID;

/**
 * Implements the '#=' Lisp reader macro.
 */
@Component
public class SharpEqualsSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -23240558522016014L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.EQUALS_SIGN, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.EQUALS_SIGN;

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for #=.");
		}

		final Map<BigInteger, LispStruct> sharpEqualFinalTable = reader.getSharpEqualFinalTable();
		final Map<BigInteger, SymbolStruct<?>> sharpEqualTempTable = reader.getSharpEqualTempTable();

		if (sharpEqualFinalTable.containsKey(numArg)
				|| sharpEqualTempTable.containsKey(numArg)) {
			throw new ReaderErrorException("Label already defined: #" + numArg + '=');
		}

		final String tagName = UUID.randomUUID().toString();
		final SymbolStruct<?> tag = new SymbolStruct<>(tagName);
		sharpEqualTempTable.put(numArg, tag);

		final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);

		final Map<SymbolStruct<?>, LispStruct> sharpEqualReplTable = reader.getSharpEqualReplTable();
		sharpEqualReplTable.put(tag, token);

		final Set<LispStruct> sharpEqualCircleSet = new HashSet<>();
		replaceTagsWithTokens(token, sharpEqualReplTable, sharpEqualCircleSet);

		sharpEqualFinalTable.put(numArg, token);

		return token;
	}

	/**
	 * Replaces the {@link SymbolStruct} tags located within the provided {@link LispStruct} token with the mapped
	 * token values located within the provided {@code sharpEqualReplTable} {@link Map}. Circularities are also
	 * accounted for by using the provided {@code sharpEqualCircleSet} {@link Set} to keep track of the {@link
	 * ConsStruct} tokens throughout the replacement process.
	 * </p>
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
	private static LispStruct replaceTagsWithTokens(final LispStruct token, final Map<SymbolStruct<?>, LispStruct> sharpEqualReplTable,
	                                                final Set<LispStruct> sharpEqualCircleSet) {

		if (token instanceof SymbolStruct) {
			if (sharpEqualReplTable.containsKey(token)) {
				return sharpEqualReplTable.get(token);
			}
		} else if (token instanceof ConsStruct) {

			if (!sharpEqualCircleSet.contains(token)) {
				sharpEqualCircleSet.add(token);

				final ConsStruct consTree = (ConsStruct) token;

				final LispStruct car = consTree.getCar();
				final LispStruct carSubst = replaceTagsWithTokens(car, sharpEqualReplTable, sharpEqualCircleSet);

				if (!Objects.equals(carSubst, car)) {
					consTree.setCar(carSubst);
				}

				final LispStruct cdr = consTree.getCdr();
				final LispStruct cdrSubst = replaceTagsWithTokens(cdr, sharpEqualReplTable, sharpEqualCircleSet);

				if (!Objects.equals(cdrSubst, cdr)) {
					consTree.setCdr(cdrSubst);
				}
			}
		}

		return token;
	}
}
