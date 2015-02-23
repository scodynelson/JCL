/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.ConsElement;
import jcl.compiler.real.element.SimpleElement;
import jcl.compiler.real.element.SymbolElement;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.packages.GlobalPackageStruct;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#'' Lisp reader macro.
 */
@Component
public class SharpApostropheReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -480798662778934982L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpApostropheReaderMacroFunction.class);

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.APOSTROPHE, this);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final SimpleElement lispToken = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", lispToken.toLispStruct().printStruct()); // TODO: fix
			}
			return null;
		}

		if (lispToken == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		final SymbolElement functionSpecialOperator = new SymbolElement(GlobalPackageStruct.COMMON_LISP.getName(), "FUNCTION"); // TODO: fix
		return new ConsElement(functionSpecialOperator, lispToken);
	}
}
