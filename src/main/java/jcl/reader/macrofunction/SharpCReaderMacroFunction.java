/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.numbers.RealStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;
import java.util.List;

/**
 * Implements the '#c' Lisp reader macro.
 */
@Component
public class SharpCReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2703333209830257710L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpCReaderMacroFunction.class);

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_C, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_C, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_C) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct lispToken = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", lispToken.printStruct());
			}
			return null;
		}

		if (!(lispToken instanceof ListStruct)) {
			throw new ReaderErrorException("Illegal complex number format: #C" + lispToken);
		}

		final ListStruct listToken = (ListStruct) lispToken;
		final List<LispStruct> lispTokens = listToken.getAsJavaList();

		final int maxNumberOfTokensForComplex = 2;
		if (lispTokens.size() != maxNumberOfTokensForComplex) {
			throw new ReaderErrorException("Illegal complex number format: #C" + lispToken);
		}

		final LispStruct real = lispTokens.get(0);
		if (!(real instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + real);
		}

		final LispStruct imaginary = lispTokens.get(1);
		if (!(imaginary instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + imaginary);
		}

		final SymbolStruct<?> complexFnSymbol = GlobalPackageStruct.COMMON_LISP.findSymbol("COMPLEX").getSymbolStruct();
		return ListStruct.buildProperList(complexFnSymbol, real, imaginary);
	}
}
