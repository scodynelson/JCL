package jcl.reader.state;

import jcl.reader.LispReader;
import jcl.reader.state.impl.EndState;
import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.ReadtableStruct;
import jcl.structs.InputStream;
import jcl.structs.streams.ReadResult;
import jcl.structs.symbols.Variable;

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
	public LispStruct read() {
		return read(true, null, true);
	}

	@Override
	public LispStruct read(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
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
	public ReadResult readChar() {
		return readChar(true, null, true);
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		try {
			return inputStream.readChar(eofErrorP, eofValue, recursiveP);
		} catch (StreamErrorException e) {
			throw new ReaderErrorException("Failed to read character.", e);
		}
	}

	@Override
	public void unreadChar(final int codePoint) {
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
