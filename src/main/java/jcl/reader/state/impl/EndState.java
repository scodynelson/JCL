package jcl.reader.state.impl;

import jcl.reader.state.ReaderState;
import jcl.reader.state.State;
import jcl.reader.StateReader;
import jcl.structs.conditions.exceptions.ReaderErrorException;

/**
 * Not Specified in the Reader Algorithm.
 * <p/>
 * This state should never be processed.  If it does, then a programming error has occurred.
 * <p/>
 * This is our Accepting state that says we are done with the Reader.
 */
public class EndState implements State {

	public static final State END_STATE = new EndState();

	/**
	 * Processes for the reader for the current State.
	 *
	 * @throws ReaderErrorException thrown if the process method ever gets called. This can only be done explicitly by a programmer.
	 */
	@Override
	public ReaderState process(final StateReader reader, final ReaderState readerState) {
		throw new ReaderErrorException("A PROGRAMMING ERROR HAS OCCURRED. The EndState process method has been explicitly and illegally called!");
	}
}
