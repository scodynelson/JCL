package jcl.reader.state;

import jcl.reader.LispReader;
import jcl.reader.state.impl.EndState;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.readtables.ReadtableStruct;
import jcl.structs.streams.InputStream;
import jcl.structs.streams.ReadResult;
import jcl.types.Variable;

public class StateReader extends LispReader {

	private final InputStream inputStream;
	private final ReadtableStruct readtable;

	public StateReader(final InputStream inputStream) {
		this(inputStream, Variable.Readtable);
	}

	public StateReader(final InputStream inputStream, final ReadtableStruct readtable) {
		this.inputStream = inputStream;
		this.readtable = readtable;
	}

	@Override
	public LispStruct read() throws ReaderErrorException {
		return read(true, null, true);
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP)
			throws ReaderErrorException {

		final StateReader stateReader = new StateReader(inputStream);

		ReaderState readerState = new ReaderState(eofErrorP, eofValue, recursiveP);
		State currentState = readerState.getNextState();

		while (currentState != EndState.END_STATE) {
			readerState = currentState.process(stateReader, readerState);
			currentState = readerState.getNextState();
		}

		return readerState.getReturnToken();
	}

	@Override
	public ReadResult readChar() throws ReaderErrorException {
		return readChar(true, null, true);
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws ReaderErrorException {
		try {
			return inputStream.readChar(eofErrorP, eofValue, recursiveP);
		} catch (StreamErrorException e) {
			throw new ReaderErrorException("Failed to read character.", e);
		}
	}

	@Override
	public void unreadChar(final int codePoint) throws ReaderErrorException {
		try {
			inputStream.unreadChar(codePoint);
		} catch (final StreamErrorException see) {
			throw new ReaderErrorException("Failed to read character.", see);
		}
	}

	public ReadtableStruct getReadtable() {
		return readtable;
	}
}
