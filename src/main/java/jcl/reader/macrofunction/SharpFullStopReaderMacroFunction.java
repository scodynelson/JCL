/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.compiler.functions.EvalFunction;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.NullStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#.' Lisp reader macro.
 */
@Component
public class SharpFullStopReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8806757995826578582L;

	/**
	 * {@link EvalFunction} singleton used to evaluate the expression passed to '#.'.
	 */
	@Autowired
	private EvalFunction evalFunction;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.FULL_STOP, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.FULL_STOP;

		final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (!ReaderVariables.READ_EVAL.getVariableValue().booleanValue()) {
			throw new ReaderErrorException("Attempt to read #. while *READ-EVAL* is bound to NIL.");
		}

		return evalFunction.eval(token);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(evalFunction)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final SharpFullStopReaderMacroFunction rhs = (SharpFullStopReaderMacroFunction) obj;
		return new EqualsBuilder().append(evalFunction, rhs.evalFunction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(evalFunction)
		                                                                .toString();
	}
}
