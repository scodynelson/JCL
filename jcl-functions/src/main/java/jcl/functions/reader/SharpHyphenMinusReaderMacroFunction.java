/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#-' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * {@link Autowired} {@link FeaturesReaderMacroFunction} used for reading features and either reading or
	 * suppressing the following {@link LispStruct}s based on whether or not the feature is present.
	 */
	private final FeaturesReaderMacroFunction featuresReaderMacroFunction;

	@Autowired
	public SharpHyphenMinusReaderMacroFunction(final FeaturesReaderMacroFunction featuresReaderMacroFunction) {
		super("SHARP-HYPHEN-MINUS");
		this.featuresReaderMacroFunction = featuresReaderMacroFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.HYPHEN_MINUS, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.HYPHEN_MINUS;

		featuresReaderMacroFunction.readFeatures(inputStreamStruct, true);
		return null;
	}
}
