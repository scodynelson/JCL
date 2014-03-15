package jcl.readtables.state;

/**
 * This interface defines a set of anonymous classes that comprise the states of the
 * Reader state machine as defined in CLtL: Ch 22.1.1 pp 511-515. These states are active
 * objects having a single {@code process} method. Each state returns a State object
 * that is the next state to process. The current Reader instance is passed to each State.
 * The Reader instance contains a reference to the current input Stream. A state processes
 * according to the specification and returns the next state. The states in CLtL are numbered.
 * The following is a correspondence list between the numbered states and the named states
 * in this interface.
 * <p/>
 * <ol start=0>
 * <li>InitialState
 * <li>ReadState
 * <li>IllegalCharState
 * <li>WhitespaceState
 * <li>MacroCharacterState
 * <li>SingleEscapeState
 * <li>MultipleEscapeState
 * <li>ConstituentState
 * <li>EvenMultiEscapeState
 * <li>OddMultiEscapeState
 * <li>TokenAccumulatedState
 * <li>EndState
 * <li>ErrorState
 * </ol>
 * <p/>
 * <p/>
 * For online specifications of these states, goto http://www.lispworks.com/documentation/HyperSpec/Body/02_b.htm
 * This site is the Reader Algorithm that is outlined within the CommonLisp HyperSpec (TM).
 */
public interface State {

	ReaderState process(StateReader reader, ReaderState readerState);
}
