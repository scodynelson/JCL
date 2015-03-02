/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.StringElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#p' Lisp reader macro.
 */
@Component
public class SharpPReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3962629854177635283L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpPReaderMacroFunction.class);

	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_P, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_P, this);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_P) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_P);

		final SimpleElement lispToken = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				final String printedToken = printer.print(lispToken);
				LOGGER.debug("{} suppressed.", printedToken);
			}
			return null;
		}

		if (lispToken instanceof StringElement) {
			final StringElement pathnameString = (StringElement) lispToken;

			return new ConsElement(PATHNAME, pathnameString);
		} else {
			final String printedToken = printer.print(lispToken);
			throw new ReaderErrorException("Improper namestring provided to #P: " + printedToken);
		}
	}
}
