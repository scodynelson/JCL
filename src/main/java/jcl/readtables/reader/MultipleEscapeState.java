package jcl.readtables.reader;

/**
 * Step 6 of the Reader Algorithm.
 */
public class MultipleEscapeState extends State {

	public static final State MULTIPLE_ESCAPE_STATE = new MultipleEscapeState();

	@Override
	public void process(final StateReader reader, final ReaderState readerState) {
		OddMultiEscapeState.ODD_MULTI_ESCAPE_STATE.process(reader, readerState);
	}
}
