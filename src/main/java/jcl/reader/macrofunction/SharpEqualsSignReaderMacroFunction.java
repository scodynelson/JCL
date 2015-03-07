/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ConsStruct;
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

		final LispStruct token = reader.read();
		reader.getSharpEqualReplTable().put(tag, token);

		final Set<LispStruct> sharpEqualCircleSet = new HashSet<>();
		circleSubst(reader, sharpEqualCircleSet, token);

		sharpEqualFinalTable.put(numArg, token);

		return token;
	}

	private LispStruct circleSubst(final Reader reader, final Set<LispStruct> sharpEqualCircleSet, final LispStruct tree) {

		final Map<SymbolStruct<?>, LispStruct> sharpEqualReplTable = reader.getSharpEqualReplTable();
		if (tree instanceof SymbolStruct) {
			if (sharpEqualReplTable.containsKey(tree)) {
				return sharpEqualReplTable.get(tree);
			}
		} else if (tree instanceof ConsStruct) {

			if (!sharpEqualCircleSet.contains(tree)) {
				sharpEqualCircleSet.add(tree);

				final ConsStruct consTree = (ConsStruct) tree;

				final LispStruct car = consTree.getCar();
				final LispStruct cdr = consTree.getCdr();

				final LispStruct carSubst = circleSubst(reader, sharpEqualCircleSet, car);
				final LispStruct cdrSubst = circleSubst(reader, sharpEqualCircleSet, cdr);

				if (!Objects.equals(carSubst, car)) {
					consTree.setCar(carSubst);
				}

				if (!Objects.equals(cdrSubst, cdr)) {
					consTree.setCdr(cdrSubst);
				}
			}
		}

		return tree;
	}
}
