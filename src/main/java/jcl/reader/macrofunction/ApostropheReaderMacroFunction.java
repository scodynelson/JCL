/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the ''' Lisp reader macro.
 */
@Component
public class ApostropheReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 1319912697712324737L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ApostropheReaderMacroFunction.class);

	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.APOSTROPHE, this, false);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final SimpleElement lispToken = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				final String printedToken = printer.print(lispToken);
				LOGGER.debug("{} suppressed.", printedToken);
			}
			return null;
		}

		if (lispToken == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return new ConsElement(QUOTE, lispToken);
	}
}
