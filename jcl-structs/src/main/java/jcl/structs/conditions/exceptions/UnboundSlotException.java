package jcl.structs.conditions.exceptions;

public class UnboundSlotException extends CellErrorException {

	private static final long serialVersionUID = -607520610353032014L;

	public UnboundSlotException(final String message) {
		super(message);
	}

	public UnboundSlotException(final String message, final Throwable cause) {
		super(message, cause);
	}
}
