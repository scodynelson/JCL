package jcl.compiler.icg.emitter;

import java.io.Serial;

import jcl.lang.condition.exception.ErrorException;

public class EmitterException extends ErrorException {

	@Serial
	private static final long serialVersionUID = 1L;

	public EmitterException(final String message) {
		super(message);
	}
}