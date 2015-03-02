/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.SimpleElement;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#-' Lisp reader macro.
 */
@Component
public class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -582954823547720438L;

	@Autowired
	private FeaturesReaderMacroFunction featuresReaderMacroFunction;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.HYPHEN_MINUS, this);
	}

	@Override
	public SimpleElement readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		featuresReaderMacroFunction.readFeatures(reader, true);
		return null;
	}
}
